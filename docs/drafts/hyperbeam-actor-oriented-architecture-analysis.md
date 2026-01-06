# HyperBEAM Actor-Oriented Architecture: Slots 概念深度解析

## 概述

本文档基于对 HyperBEAM 代码库的深入分析，系统性地阐述了 AO-Core 协议中 Actor-Oriented（面向 Actor）架构的核心概念，特别是 **Slots** 的准确定义与实现机制。

## 核心发现

### 1. Actor-Oriented Architecture（面向 Actor 的架构）

HyperBEAM 实现了 AO-Core 协议，这是一个建立在 Arweave 之上的**去中心化计算平台**。与传统区块链不同，它采用了**Actor-Oriented**的架构模式：

**核心定义**：
- **Process（进程）**：系统中基本的执行单元，每个进程都是一个独立的 **stateful actor（有状态的 Actor）**
- **每个进程具有**：
  - **ID**：唯一标识符
  - **Device Stack**：设备栈（可配置的执行模块，如 WASM 执行器、调度器等）
  - **State**：持久化状态
  - **Ordered slot sequence**：**有序的 slot 序列**

### 2. Slots 的准确定义

#### 传统误解纠正
❌ **错误理解**：Slot 是一个从 0 开始的递增整数，代表消息执行的全局顺序编号

#### 正确定义
✅ **准确理解**：**Slot 是每个 AO 进程内部的有序编号容器，从 0 开始递增，每个 slot 包含一个等待执行的消息 assignment，用于保证进程内消息的确定性顺序执行。**

#### 关键特性
```markdown
Slots are a fundamental concept in the `~scheduler@1.0` device, providing a structured mechanism for organizing and sequencing computation.

* **Sequential Ordering:** Slots act as numbered containers (starting at 0) that hold specific messages or tasks to be processed in a deterministic order.
* **Per-Process Isolation:** Each AO process maintains its own independent slot sequence.
* **Assignment Storage:** Each slot contains an "assignment" - the cryptographically verified message waiting to be executed.
```

### 3. Scheduler 的角色与机制

**Scheduler = Orders messages into slots**

调度器（`dev_scheduler@1.0`）扮演"**deliberate bottleneck（刻意瓶颈）**"的角色：

#### 核心职责
- **确保消息顺序**：每个进程的消息被严格地分配到递增的 slot 中
- **防止冲突**：避免多个消息被分配到同一个 slot
- **维护一致性**：通过 hash chain 链接 assignments

#### 技术实现

```erlang
% dev_scheduler_server.erl - 调度器服务器启动
start(ProcID, Opts) ->
    spawn_link(
        fun() ->
            hb_name:register({dev_scheduler, ProcID}),
            {CurrentSlot, HashChain} =
                case dev_scheduler_cache:latest(ProcID, Opts) of
                    not_found -> {-1, <<>>};
                    {Slot, Chain} -> {Slot, Chain}
                end,
            server(#{
                id => ProcID,
                current => CurrentSlot,
                wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts),
                hash_chain => HashChain,
                opts => Opts
            })
        end
    ).

% Slot 分配逻辑（do_assign函数片段）
do_assign(State, Message, ReplyPID) ->
    HashChain = next_hashchain(maps:get(hash_chain, State), Message),
    NextSlot = maps:get(current, State) + 1,
    {Timestamp, Height, Hash} = ar_timestamp:get(),
    Assignment = hb_message:commit(#{
        <<"data-protocol">> => <<"ao">>,
        <<"variant">> => <<"ao.N.1">>,
        <<"process">> => hb_util:id(maps:get(id, State)),
        <<"epoch">> => <<"0">>,
        <<"slot">> => NextSlot,
        <<"block-height">> => Height,
        <<"block-hash">> => hb_util:human_id(Hash),
        <<"block-timestamp">> => Timestamp,
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"hash-chain">> => hb_util:id(HashChain),
        <<"body">> => Message
    }, maps:get(wallet, State)),
    % 异步写入缓存和上传到Arweave
    spawn(fun() ->
        dev_scheduler_cache:write(Assignment, maps:get(opts, State)),
        hb_client:upload(Assignment, maps:get(opts, State))
    end),
    State#{current := NextSlot, hash_chain := HashChain}.
```

**并发控制机制**：
```erlang
% dev_process_worker.erl - 进程级并发隔离
process_to_group_name(Msg1, Opts) ->
    Initialized = dev_process:ensure_process_key(Msg1, Opts),
    ProcMsg = hb_ao:get(<<"process">>, Initialized, Opts#{ hashpath => ignore }),
    ID = hb_message:id(ProcMsg, all),
    hb_util:human_id(ID).  % 返回进程ID作为并发组标识
```

每个进程的消息处理都被分配到以进程ID命名的并发组，确保相同进程的执行串行化，不同进程可以并行执行。

### 4. 与区块链架构的对比

| 维度 | 区块链 | AO 进程（HyperBEAM） |
|------|--------|---------------------|
| **顺序保证** | 全局区块高度（block height） | 每个进程独立的 slot 序列 |
| **执行单元** | 全网统一的交易顺序 | 进程内消息的确定性顺序 |
| **并发性** | 串行执行（单链） | 并行执行（多进程） |
| **状态管理** | 全局状态 | 每个进程独立状态 |
| **扩展性** | 有限（TPS 瓶颈） | 无限扩展（进程可并行） |

### 5. 技术实现细节

#### Slot 分配流程
1. **消息到达** → 调度器接收
2. **分配 slot** → `NextSlot = CurrentSlot + 1`
3. **创建 Assignment** → 包含 slot 号、消息体、时间戳等
4. **签名存储** → 使用钱包签名，确保不可篡改
5. **执行计算** → 按 slot 顺序执行消息
6. **状态更新** → 更新进程状态

#### 关键数据结构

**Assignment（任务分配）结构**：
```erlang
Assignment = #{
    <<"data-protocol">> => <<"ao">>,                    % 协议标识
    <<"variant">> => <<"ao.N.1">>,                      % 协议版本
    <<"process">> => ProcessID,                         % 目标进程ID
    <<"epoch">> => <<"0">>,                             % 纪元（预留字段）
    <<"slot">> => SlotNumber,                           % Slot编号（从0开始递增）
    <<"block-height">> => ArweaveBlockHeight,           % Arweave区块高度
    <<"block-hash">> => ArweaveBlockHash,               % Arweave区块哈希
    <<"block-timestamp">> => ArweaveTimestamp,          % Arweave时间戳
    <<"timestamp">> => SystemTimestamp,                 % SU本地时间戳（毫秒）
    <<"hash-chain">> => HashChainID,                    % 哈希链标识（用于验证顺序）
    <<"body">> => Message                               % 原始消息内容
}
```

**Hash Chain机制**：
```erlang
% next_hashchain函数 - 生成哈希链的下一个元素
next_hashchain(PreviousHash, Message) ->
    MessageID = hb_message:id(Message, all),
    crypto:hash(sha256, <<PreviousHash/binary, MessageID/binary>>).
```

Hash Chain通过将前一个哈希值与当前消息ID连接后进行SHA256哈希，确保消息顺序的不可篡改性。

#### API 接口
```http
# 查询进程当前 slot
GET /<ProcessID>~process@1.0/slot

# 获取指定 slot 的状态
GET /<ProcessID>~process@1.0/slot/5/state

# 添加消息到调度队列
POST /<ProcessID>~process@1.0/schedule

# 计算到指定 slot 的状态
GET /<ProcessID>~process@1.0/compute/<TargetSlot>
```

### 6. 设计哲学与优势

#### Actor 模型的优势
- **隔离性**：每个进程的状态完全隔离
- **并发性**：不同进程可并行执行，无相互干扰
- **容错性**：单个进程失败不影响其他进程
- **扩展性**：可无限水平扩展进程数量

#### Slot 机制的精妙之处
- **确定性**：相同输入在相同 slot 产生相同结果
- **可验证性**：通过密码学保证执行顺序的不可篡改
- **效率**：支持缓存和预取优化
- **灵活性**：每个进程可独立调度策略

### 7. 关键代码位置

#### 核心模块
- `src/dev_scheduler.erl` - 调度器主逻辑
- `src/dev_scheduler_server.erl` - 调度器服务器实现
- `src/dev_process.erl` - 进程设备实现
- `src/dev_scheduler_cache.erl` - 调度缓存机制

#### 文档参考
- `docs/devices/scheduler-at-1-0.md` - 调度器设备文档
- `docs/devices/process-at-1-0.md` - 进程设备文档
- `docs/introduction/what-is-ao-core.md` - AO-Core 协议介绍

## 结论

HyperBEAM 的 Actor-Oriented 架构通过 **Slots** 机制巧妙地解决了去中心化计算中的顺序与并发问题：

1. **进程隔离**：每个 Actor（进程）维护独立的状态空间
2. **顺序保证**：Slots 确保每个进程内的消息确定性执行
3. **并发优化**：不同进程可并行处理，实现超高并发
4. **可扩展性**：理论上支持无限进程数量的水平扩展

这种设计实现了区块链式确定性与传统分布式系统并发性的完美融合，是 AO 生态系统的核心创新。

## 8. 最新实现细节补充

### 8.1 调度模式选择

HyperBEAM 支持多种调度模式，以适应不同的性能和可靠性需求：

```erlang
% 调度模式配置
case hb_opts:get(scheduling_mode, sync, maps:get(opts, State)) of
    aggressive ->
        % 激进模式：立即返回，异步处理
        spawn(AssignFun);
    sync ->
        % 同步模式：等待处理完成
        AssignFun();
    disabled ->
        % 禁用模式：拒绝调度请求
        throw({scheduling_disabled_on_node, {requested_for, ProcID}})
end
```

**调度确认机制**：
```erlang
maybe_inform_recipient(Mode, ReplyPID, Message, Assignment, State) ->
    case hb_opts:get(scheduling_mode, remote_confirmation, maps:get(opts, State)) of
        Mode ->
            % 根据模式发送确认消息
            ReplyPID ! {scheduled, Message, Assignment};
        _ -> ok
    end.
```

支持三种确认级别：`aggressive`、`local_confirmation`、`remote_confirmation`。

### 8.2 预取和缓存优化

调度器实现了高效的预取机制：

```erlang
% dev_scheduler.erl - 预取逻辑
check_lookahead_and_local_cache(Msg1, ProcID, TargetSlot, Opts) ->
    case dev_scheduler_cache:read(ProcID, TargetSlot, Opts) of
        {ok, Assignment} ->
            % 缓存命中，直接返回
            {ok, undefined, Assignment};
        not_found ->
            % 启动预取工作者
            Worker = spawn_lookahead_worker(ProcID, TargetSlot, Opts),
            {ok, Worker, undefined}
    end.
```

**预取工作者机制**：
- 在后台预取后续的slot分配
- 减少查询延迟
- 支持并发预取多个slot

### 8.3 进程状态一致性

通过`at-slot`指针确保状态一致性：

```erlang
% 获取当前处理到的slot
LastProcessed = hb_util:int(
    hb_ao:get(<<"at-slot">>, Msg1, Opts#{ hashpath => ignore })
)
```

**状态前进机制**：
1. 执行slot N的消息
2. 更新进程状态
3. 将`at-slot`指针前进到N+1
4. 生成状态快照（如果需要）

### 8.4 容错和恢复机制

调度器服务器具有容错能力：

```erlang
% 服务器启动时的状态恢复
{CurrentSlot, HashChain} =
    case dev_scheduler_cache:latest(ProcID, Opts) of
        not_found ->
            % 新进程，从slot -1开始
            {-1, <<>>};
        {Slot, Chain} ->
            % 现有进程，从上次保存的状态继续
            {Slot, Chain}
    end
```

**崩溃恢复**：
- 服务器重启时从缓存恢复最后状态
- Hash Chain验证消息顺序完整性
- 自动重建缺失的slot分配

## 结论

HyperBEAM的Actor-Oriented架构通过精心设计的Slots机制，实现了：

1. **进程级隔离**：每个进程维护独立的状态空间和执行序列
2. **确定性保证**：Slots确保消息的严格顺序执行
3. **并发优化**：不同进程可并行处理，相同进程串行执行
4. **可扩展性**：理论上支持无限数量的并发进程
5. **容错性**：通过缓存和快照实现状态持久化
6. **性能优化**：预取、缓存和异步处理机制

这种设计完美平衡了区块链的安全性要求与分布式系统的性能需求，为去中心化计算开辟了新的可能性。

---

*本文档基于 HyperBEAM v0.1 代码库深度分析，记录了Actor-Oriented架构和Slots机制的最新实现细节。如有更新，请及时修正。*

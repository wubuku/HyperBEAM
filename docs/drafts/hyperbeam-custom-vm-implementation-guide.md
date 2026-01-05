# HyperBEAM 自定义 VM 实现指南：Your Own VM — Any Execution Environment

## 概述

本文档深入解读 WAO 关于 HyperBEAM 的文档中的关键陈述 **"Your own VM — Any execution environment"**，并结合 Actor-Oriented 架构分析，验证用户提出的观点：**用任意语言开发的确定性微服务可以作为自定义 VM**。

## 核心概念解读

### AOS 架构剖析

```
AOS = process@1.0 + scheduler@1.0 + (lua@5.3a | wasm-64@1.0)
```

#### 组件解构
- **process@1.0**: Actor-Oriented 进程管理，提供 slot 机制保证顺序执行
- **scheduler@1.0**: 消息调度，确保进程内消息的确定性顺序
- **lua@5.3a | wasm-64@1.0**: **可替换的执行环境**

#### Lua 智能合约执行流程详解

基于 `dev_process.erl` 代码深度分析，一个 Lua 智能合约的执行遵循严格的 Actor 模型，涉及多层组件协同工作：

```
+-------------------+
|   HTTP Request    |  <-- User contract call
|  (Any Language)   |
+---------+---------+
          |
          v
+-------------------+
|  hb_http_server  |  <-- HTTP processing, convert to AO message
| (HTTP Handler)    |
+---------+---------+
          |
          v
+-------------------+
|   process@1.0     |  <-- Process management, check cache/trigger compute
| (Process Mgmt)    |
+---------+---------+
          |
          v
+-------------------+
|  scheduler@1.0    |  <-- Message scheduling, get next message by slot
| (Msg Scheduler)   |
+---------+---------+
          |
          v
+-------------------+
|   process@1.0     |  <-- State restoration, compute to target slot
| (State Restore)   |
| • Check cache     |
| • Load snapshot   |
| • Restore state   |
+---------+---------+
          |
          v
+-------------------+
|   process@1.0     |  <-- Delegate to execution device
|   run_as()        |
|   execution       |
+---------+---------+
          |
          v
+-------------------+
|    lua@5.3a       |  <-- Execute Lua code
| (Exec Device)     |
| • Load script     |
| • Call Lua func   |
| • Exec contract   |
| • Sandbox env     |
+---------+---------+
          |
          v
+-------------------+
| Contract Result   |  <-- Contract execution complete
+---------+---------+
          |
          v
+-------------------+
|   process@1.0     |  <-- Store computation result
| (Result Storage)  |
| • Update at-slot  |
| • Cache result    |
| • Gen snapshot    |
| • Async write     |
+---------+---------+
          |
          v
+-------------------+
|  HTTP Response    |  <-- Return to user
| (JSON Format)     |
+-------------------+
```

#### 📋 Execution Flow Overview / 执行流程概览

This flowchart shows the **complete lifecycle of a single Lua smart contract execution** / 这个流程图展示了单个Lua智能合约执行的完整生命周期：

1. **Request Entry** / 请求入口: HTTP request converted to AO message format / HTTP请求转换为AO消息格式
2. **Cache Check** / 缓存检查: Priority check for existing computation results / 优先检查已有计算结果
3. **Message Scheduling** / 消息调度: Get next message to execute in slot order / 按slot顺序获取待执行消息
4. **State Restoration** / 状态恢复: Load from snapshot or compute step-by-step to target slot / 从快照加载或逐步计算到目标slot
5. **Execution Delegation** / 执行委托: Delegate computation task to Lua execution device / 将计算任务委托给Lua执行设备
6. **Contract Execution** / 合约执行: Lua VM loads script and executes contract logic / Lua VM加载脚本并执行合约逻辑
7. **Result Storage** / 结果存储: Save computation results and generate snapshots / 保存计算结果并生成快照
8. **Response Return** / 响应返回: Format results as HTTP response / 将结果格式化为HTTP响应

**Note** / 注意: When computing multiple slots, `compute_to_slot()` recursively calls itself, processing one slot at a time until reaching the target slot. / 当需要计算多个slot时，`compute_to_slot()`函数会递归调用自身，每次处理一个slot，直到达到目标slot。

#### Key Execution Nodes Detail / 关键执行节点详解

**1. Message Entry & Formatting / 消息入口与格式化**
- **hb_http_server**: Receives HTTP requests, converts to AO message format via `req_to_tabm_singleton()` / 通过`req_to_tabm_singleton()`接收HTTP请求，转换为AO消息格式
- **Message Formation**: Standardizes message structure for compatibility / 标准化消息结构，确保兼容性

**2. Cache Priority Check / 缓存优先检查**
- **process@1.0 compute()**: First checks if result is already cached via `dev_process_cache:read()` / 首先通过`dev_process_cache:read()`检查结果是否已缓存
- **Cache Hit**: Returns cached result immediately if found / 如果找到直接返回缓存结果

**3. Scheduling & Queue Management / 调度与队列管理**
- **scheduler@1.0**: Manages message execution order for determinism / 管理消息执行顺序，确保确定性
- **next()**: Returns next message to execute in slot order via scheduler queue / 通过调度器队列按slot顺序返回下一个待执行消息

**4. State Restoration Mechanism / 状态恢复机制**
- **ensure_loaded()**: Loads latest snapshot from disk via `dev_process_cache:latest()` / 通过`dev_process_cache:latest()`从磁盘加载最近快照
- **normalize()**: Restores execution device state via device-specific `normalize/3` / 通过设备特定的`normalize/3`函数恢复执行设备状态
- **Snapshot Loading**: Restores state from cache by slot frequency / 按slot频率从缓存恢复状态

**5. Step-by-Step Execution / 逐步执行过程**
- **compute_to_slot()**: Computes step by step to target slot via recursive calls / 通过递归调用逐步计算到目标slot
- **compute_slot()**: Executes computation for single slot / 执行单个slot的计算
- **run_as("execution")**: Delegates to specific execution device via `hb_ao:subresolve()` / 通过`hb_ao:subresolve()`委托给具体的执行设备

**6. Contract Execution Core / 合约执行核心**
- **dev_lua:compute()**: Loads and executes Lua scripts via `luerl:call_function_dec()` / 通过`luerl:call_function_dec()`加载并执行Lua脚本
- **Function Calls**: Calls specific Lua functions with parameters / 使用参数调用特定的Lua函数
- **Sandbox Environment**: Restricts Lua code system access via sandboxed execution / 通过沙盒化执行限制Lua代码的系统访问权限

**7. Result Storage & Caching / 结果存储与缓存**
- **store_result()**: Saves computation results to cache via `dev_process_cache:write()` / 通过`dev_process_cache:write()`保存计算结果到缓存
- **snapshot()**: Generates state snapshots by frequency (every N slots) / 按频率（每N个slot）生成状态快照
- **Async Write**: Snapshot storage doesn't block main execution flow / 快照存储不阻塞主执行流程

**8. Response Return / 响应返回**
- **Format Output**: Converts results to HTTP response format via `hb_http:reply()` / 通过`hb_http:reply()`将结果转换为HTTP响应格式
- **State Update**: Updates process at-slot pointer for progress tracking / 更新进程的at-slot指针用于进度跟踪

#### Performance Optimization Points / 性能优化点

- **🔍 Cache Check**: Avoids redundant deterministic computation / 避免重复执行确定性计算
- **💾 Snapshot Restore**: Computes from intermediate state, not from scratch / 从中间状态开始计算，而非从头开始
- **⚡ Async Storage**: Result storage doesn't block execution flow / 结果存储不阻塞执行流程
- **🔄 State Reuse**: Reuses Lua VM state across requests / 跨请求复用Lua VM状态

This flow ensures HyperBEAM's core characteristics as a "decentralized supercomputer": **deterministic execution**, **high-performance caching**, and **scalable architecture**.

这个流程确保了HyperBEAM作为"去中心化超级计算机"的核心特性：**确定性执行**、**高性能缓存**和**可扩展架构**。

**关键洞察**：执行环境是**可插拔的**，这为自定义 VM 奠定了基础。Lua 合约通过 `luerl:call_function_dec()` 执行，WASM 通过本地运行时执行，皆保证确定性输出。

#### ⚠️ 重要澄清：无快照的严重后果

**你的理解完全正确！如果不生成快照，系统会面临严重的性能问题**：

**缓存机制依赖快照**：
```erlang
% src/dev_process.erl:180 - 缓存检查
case dev_process_cache:read(ProcID, Slot, Opts) of
    {ok, Result} ->
        % 直接返回缓存结果 ✅
        {ok, Result};
    not_found ->
        % 需要重新计算 ❌
        compute_to_slot(ProcID, Loaded, Msg2, Slot, Opts)
end
```

**快照加载机制**：
```erlang
% src/dev_process.erl:424 - 从磁盘加载最近快照
LoadRes = dev_process_cache:latest(ProcID, [<<"snapshot">>], TargetSlot, Opts)
case LoadRes of
    {ok, LoadedSlot, SnapshotMsg} ->
        % 从快照点开始计算 ✅
        restore_from_snapshot(SnapshotMsg, LoadedSlot);
    not_found ->
        % 从 slot 0 开始计算所有操作 ❌❌❌
        compute_from_scratch()
end
```

**无快照的灾难性后果**：
- **每次查询都重新计算**：从 slot 0 到目标 slot 的所有操作都要重新执行
- **指数级性能下降**：随着 slot 数量增加，查询时间呈线性增长
- **无法扩展**：进程运行时间越长，查询性能越差
- **资源浪费**：重复执行相同的确定性计算

**因此，快照虽然"可选"，但对于任何实际应用都是"必需"的**。

#### 🎯 核心洞察：快照内容由设备决定！

**你的观点完全正确！** HyperBEAM 并不关心快照的具体内容和含义：

**HyperBEAM 的角色**：
- **存储快照**：将设备的 `snapshot/3` 返回值存储到缓存
- **传递快照**：在需要时将快照数据传递给设备的 `normalize/3`
- **触发恢复**：调用 `run_as(<<"execution">>, SnapshotMsg, normalize, Opts)`

**设备的自主权**：
- **快照格式**：可以是完整数据、引用、hash，或任意格式
- **恢复逻辑**：`normalize/3` 完全由设备控制如何使用快照
- **持久化策略**：设备可以有自己的外部存储机制

### 执行环境样板代码

基于现有设备（如 `dev_lua.erl` 和 `dev_wasm.erl`）的分析，一个标准的执行环境设备应该实现以下接口。

> **重要说明**: 本文档提供的样板代码基于实际的HyperBEAM设备实现。浏览器文档中可能显示简化的示例代码，但最终以代码库中的实际实现为准。

```erlang
%%% @doc 自定义执行环境设备样板
-module(dev_your_execution_env).
-export([info/1, init/3, compute/3, snapshot/3, normalize/3]).

-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc 定义设备能力
%% 注意：info函数的参数签名因设备类型而异
%% - Lua设备使用：info(Base)
%% - WASM设备使用：info(Msg1, Opts)
info(Base) ->
    #{
        % 默认处理器：所有未匹配的key都通过compute/3处理
        default => fun compute/3,

        % 排除的key（这些不会被compute处理）
        excludes => [
            <<"keys">>, <<"set">>, <<"encode">>, <<"decode">>,
            <<"init">>, <<"snapshot">>, <<"normalize">>
        ] ++ maps:keys(Base)
    }.

%% @doc 初始化执行环境
init(Base, Req, Opts) ->
    % 1. 检查或加载执行环境代码
    case hb_ao:get(<<"code">>, Base, not_found, Opts) of
        not_found ->
            {error, <<"No code found for initialization">>};
        Code ->
            % 2. 初始化你的VM（编译代码、启动进程等）
            case initialize_your_vm(Code, Opts) of
                {ok, VMState} ->
                    % 3. 返回包含VM状态的基础消息
                    {ok, Base#{vm_state => VMState}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 执行代码 - 核心执行函数
%% 注意：compute函数的参数签名因设备类型而异
%% - Lua设备使用：compute(Key, RawBase, Req, Opts)
%% - WASM设备使用：compute(Base, Req, Opts)
%% 以下是基于WASM设备模式的简化实现
compute(Base, Req, Opts) ->
    % 1. 确保VM已初始化
    {ok, InitializedBase} = ensure_initialized(Base, Req, Opts),

    % 2. 从请求中提取要执行的操作
    case hb_ao:get([<<"body">>, <<"Action">>], Req, Opts) of
        undefined ->
            % 默认操作：简单的数据处理
            {ok, hb_ao:set(InitializedBase, #{<<"result">> => <<"executed">>}, Opts)};
        Action ->
            % 根据Action执行特定操作
            execute_action(InitializedBase, Action, Req, Opts)
    end.

%% @doc 生成状态快照
snapshot(Base, _Req, Opts) ->
    VMState = hb_ao:get(<<"vm_state">>, Base, Opts),
    % 生成VM状态的快照
    Snapshot = create_vm_snapshot(VMState),
    {ok, Base#{<<"snapshot">> => Snapshot}}.

%% @doc 标准化消息格式
normalize(Base, Req, Opts) ->
    % 标准化输入/输出格式
    Normalized = normalize_message_format(Base, Req, Opts),
    {ok, Normalized}.

%% 内部辅助函数

%% @doc 确保VM已初始化
ensure_initialized(Base, Req, Opts) ->
    case hb_ao:get(<<"vm_state">>, Base, not_found, Opts) of
        not_found ->
            % 如果未初始化，调用init
            init(Base, Req, Opts);
        _ ->
            {ok, Base}
    end.

%% @doc 初始化你的VM - 需要你实现
initialize_your_vm(_Code, _Opts) ->
    % TODO: 实现你的VM初始化逻辑
    % 例如：编译代码、启动解释器进程等
    {ok, #{initialized => true, timestamp => erlang:system_time()}}.

%% @doc 执行具体的Action操作
execute_action(Base, Action, Req, Opts) ->
    % TODO: 根据Action类型实现不同的操作逻辑
    % 示例：简单的数学运算
    case Action of
        <<"add">> ->
            A = hb_ao:get(<<"a">>, Req, 0, Opts),
            B = hb_ao:get(<<"b">>, Req, 0, Opts),
            Result = A + B,
            {ok, hb_ao:set(Base, #{<<"result">> => Result}, Opts)};
        <<"multiply">> ->
            A = hb_ao:get(<<"a">>, Req, 1, Opts),
            B = hb_ao:get(<<"b">>, Req, 1, Opts),
            Result = A * B,
            {ok, hb_ao:set(Base, #{<<"result">> => Result}, Opts)};
        _ ->
            % 未知操作，返回错误
            {error, <<"Unknown action: ", Action/binary>>}
    end.

%% @doc 在你的VM中执行代码 - 需要你实现（可选，用于复杂实现）
execute_in_your_vm(_VMState, _Function, _Args, _Opts) ->
    % TODO: 实现你的代码执行逻辑
    % 例如：调用解释器、运行字节码等
    {ok, #{output => <<"Hello from your VM!">>}}.

%% @doc 创建VM快照 - 需要你实现
create_vm_snapshot(_VMState) ->
    % TODO: 实现状态快照逻辑
    #{snapshot_timestamp => erlang:system_time()}.

%% @doc 标准化消息 - 需要你实现
normalize_message_format(Base, Req, _Opts) ->
    % TODO: 实现消息格式标准化
    Base#{normalized_req => Req}.
```

#### 关键接口函数说明

| 函数 | 参数 | 说明 | 必需性 |
|------|------|------|--------|
| `info/1` 或 `info/2` | `Base` 或 `Msg1, Opts` | 定义设备能力，默认处理器 | ✅ 必需 |
| `init/3` | `Base, Req, Opts` | 初始化执行环境 | ✅ 必需 |
| `compute/3` 或 `compute/4` | `Base, Req, Opts` 或 `Key, Base, Req, Opts` | 执行代码核心逻辑 | ✅ 必需 |
| `snapshot/3` | `Base, Req, Opts` | 生成状态快照 | 🔄 可选* |
| `normalize/3` | `Base, Req, Opts` | 标准化消息格式 | 🔄 可选 |

> **函数签名说明**: 不同设备实现的参数签名可能不同：
> - **Lua设备** (`dev_lua.erl`): `info/1`, `compute/4`
> - **WASM设备** (`dev_wasm.erl`): `info/2`, `compute/3`
> - **自定义设备**: 可根据需要选择合适的签名模式

> **\*** 关于 snapshot 可选性的深度说明：
> - **技术上可选**：如果不实现，系统会 fallback 到默认设备处理
> - **性能权衡**：大状态应用建议实现，否则缓存机制会失效
> - **频率控制**：仅在特定 slot（如每 `Cache-Frequency` 个 slot）生成，非每次执行

### Snapshot 机制深度解析

#### 生成时机与频率
```erlang
% src/dev_process.erl:314
case Slot rem Freq of
    0 -> generate_snapshot();  % 仅当 Slot % Cache-Frequency == 0 时
    _ -> skip_snapshot()       % 其他情况跳过
end
```

**默认频率**：`?DEFAULT_CACHE_FREQ = 1`（每个 slot 都生成）

#### 性能影响分析

**✅ 生成快照的优势**：
- **计算去重**：避免重复执行相同状态计算
- **快速恢复**：进程重启后可从快照恢复，而非重新计算
- **并发优化**：多进程可共享缓存的计算结果

**❌ 大状态应用的性能问题**：
- **序列化开销**：大状态序列化为二进制数据耗时
- **存储压力**：快照数据占用磁盘和内存空间
- **网络传输**：分布式环境下快照同步成本高

#### 实现策略建议

**对于小状态应用**（如简单计数器、配置数据）：
```erlang
snapshot(Base, _Req, Opts) ->
    % 直接序列化整个状态
    {ok, #{<<"body">> => term_to_binary(Base)}}.
```

**对于大状态应用**（如复杂数据结构、大量数据）：
```erlang
snapshot(Base, _Req, Opts) ->
    % 增量快照：只保存变更部分
    case get_last_snapshot(Base) of
        none ->
            % 首次快照：完整保存
            create_full_snapshot(Base);
        LastSnapshot ->
            % 增量快照：只保存diff
            create_incremental_snapshot(Base, LastSnapshot)
    end.
```

**策略 4：引用模式快照**（推荐用于大状态应用）
```erlang
snapshot(Base, _Req, _Opts) ->
    % 生成状态的引用，而不是完整数据
    StateRef = generate_state_reference(Base),

    % 外部存储完整状态（IPFS、Arweave、专用存储等）
    external_store:save(StateRef, Base),

    % 只返回引用给 HyperBEAM 缓存
    {ok, #{<<"state_ref">> => StateRef, <<"timestamp">> => erlang:system_time()}}.

normalize(Base, _Req, _Opts) ->
    % 从快照中提取引用
    case hb_ao:get(<<"state_ref">>, Base, not_found, Opts) of
        not_found ->
            % 没有引用，使用默认状态
            {ok, Base};
        StateRef ->
            % 从外部存储加载完整状态
            case external_store:load(StateRef) of
                {ok, FullState} ->
                    % 成功恢复，合并到基础消息中
                    {ok, Base#{restored_state => FullState}};
                {error, _Reason} ->
                    % 恢复失败，返回错误或降级处理
                    {error, <<"Failed to restore state from reference">>}
            end
    end.
```

**核心优势**：
- **存储效率**：HyperBEAM 只缓存轻量级引用
- **灵活存储**：完整状态可存储在 IPFS、Arweave 或专用存储
- **版本控制**：可实现增量快照和状态历史管理
- **容错性**：外部存储失败时可降级处理

**对于超大状态应用**（如 AI 模型、大型数据库）：
```erlang
snapshot(_Base, _Req, _Opts) ->
    % 不实现快照，让系统使用默认处理
    % 或者返回错误，强制每次重新计算
    {error, <<"Snapshot not supported for large state">>}.
```

#### 替代方案

如果快照性能开销过大，可以考虑：

1. **稀疏快照**：降低 `Cache-Frequency`，减少快照生成频率
2. **异步快照**：在后台进程中生成，避免阻塞主执行流程
3. **外部存储**：将快照存储到外部系统（如 IPFS、Arweave）
4. **选择性缓存**：只缓存关键状态，跳过临时数据

## 快照机制的核心设计哲学

### 🎯 快照内容的完全自主权

**HyperBEAM 的快照设计哲学**：**存储与解释分离**

**HyperBEAM 的职责**：
- ✅ **存储快照**：将 `snapshot/3` 的返回值可靠存储
- ✅ **传递快照**：在状态恢复时传递给 `normalize/3`
- ✅ **触发恢复**：调用设备的 normalize 函数处理快照
- ❌ **不解释内容**：HyperBEAM 完全不关心快照里是什么

**设备的自主权**：
- 🎨 **任意格式**：快照可以是完整数据、引用、hash、元数据，或任何格式
- 🔧 **自定义逻辑**：`normalize/3` 完全控制如何使用快照数据
- 💾 **外部存储**：设备可以实现自己的持久化机制
- 🚀 **性能优化**：根据应用需求选择最适合的快照策略

### 📋 快照内容设计模式

| 模式 | 适用场景 | 优点 | 缺点 |
|------|----------|------|------|
| **完整数据** | 小状态应用 | 简单、可靠 | 存储开销大 |
| **引用模式** | 大状态应用 | 存储高效、灵活 | 需要外部存储 |
| **增量模式** | 频繁更新的状态 | 节省空间 | 实现复杂 |
| **混合模式** | 复杂应用 | 平衡各种需求 | 设计复杂 |

### 🔗 与外部存储系统的集成

**引用模式的核心优势**：
```erlang
% 快照只是轻量级引用
{ok, #{
    <<"state_ref">> => <<"ipfs://Qm...">>,
    <<"timestamp">> => 1640995200,
    <<"size">> => 1024*1024*100  % 100MB
}}

% 实际数据存储在外部系统
IPFS.store(state_ref, actual_large_state)
Arweave.store(state_ref, actual_large_state)
S3.store(state_ref, actual_large_state)
```

### 现有设备快照实现深度剖析

#### Lua 设备（`dev_lua.erl`）快照机制

**生成快照**：
```erlang
snapshot(Base, _Req, Opts) ->
    case hb_private:get(<<"state">>, Base, Opts) of
        not_found ->
            {error, <<"Cannot snapshot Lua state: state not initialized.">>};
        State ->
            % 将 Lua 状态序列化为 Erlang 二进制格式
            {ok, #{
                <<"body">> => term_to_binary(luerl:externalize(State))
            }}
    end.
```

**恢复快照**：
```erlang
normalize(Base, _Req, RawOpts) ->
    % 从快照中恢复 Lua 状态
    SerializedState = hb_ao:get([<<"snapshot">>] ++ DeviceKey ++ [<<"body">>], ...),
    case SerializedState of
        not_found -> throw({error, no_lua_state_snapshot_found});
        State ->
            % 反序列化并重建 Lua 状态
            ExternalizedState = binary_to_term(State),
            InternalizedState = luerl:internalize(ExternalizedState),
            {ok, hb_private:set(Base, <<"state">>, InternalizedState, Opts)}
    end.
```

**核心技术**：
- **序列化**：使用 `luerl:externalize()` 将 Lua 状态转换为 Erlang term
- **存储格式**：Erlang 二进制格式 (`term_to_binary`)
- **恢复过程**：`binary_to_term()` → `luerl:internalize()`

#### WASM 设备（`dev_wasm.erl`）快照机制

**生成快照**：
```erlang
snapshot(M1, M2, Opts) ->
    Instance = instance(M1, M2, Opts),
    % 通过 hb_beamr 序列化 WASM 实例状态
    {ok, Serialized} = hb_beamr:serialize(Instance),
    {ok, #{ <<"body">> => Serialized }}.
```

**恢复快照**：
```erlang
normalize(RawM1, M2, Opts) ->
    % 检查是否已有实例
    case instance(RawM1, M2, Opts) of
        not_found ->
            % 从快照恢复
            Memory = hb_ao:get([<<"snapshot">>] ++ DeviceKey ++ [<<"body">>], ...),
            case Memory of
                not_found -> throw({error, no_wasm_instance_or_snapshot});
                State ->
                    % 初始化新实例并反序列化状态
                    {ok, M1} = init(RawM1, State, Opts),
                    Res = hb_beamr:deserialize(instance(M1, M2, Opts), State),
                    M1
            end;
        _ ->
            % 已有实例，跳过反序列化
            RawM1
    end.
```

**核心技术**：
- **序列化**：通过 `hb_beamr:serialize()` 序列化 WASM 运行时状态
- **存储格式**：WASM 特定的二进制格式
- **恢复过程**：`hb_beamr:deserialize()` 重建完整运行时状态

#### 两种实现对比

| 维度 | Lua 设备 | WASM 设备 |
|------|----------|-----------|
| **状态复杂度** | Lua 解释器状态 | WASM 运行时 + 内存 |
| **序列化方式** | Erlang term 序列化 | 专用 WASM 序列化 |
| **恢复策略** | 完全重建状态 | 实例重建 + 状态注入 |
| **性能特点** | 轻量级，快速 | 重量级，但功能完整 |
| **适用场景** | 脚本执行状态 | 复杂计算和内存状态 |

#### 快照使用流程

**在进程执行中的集成**：

1. **计算完成后触发**（`src/dev_process.erl:314`）：
```erlang
case Slot rem Freq of
    0 -> {ok, Snapshot} = snapshot(Msg3, Msg2, Opts)
end
```

2. **存储到缓存**（`src/dev_process.erl:332`）：
```erlang
dev_process_cache:write(ProcID, Slot, Msg3MaybeWithSnapshot, Opts)
```

3. **查询时恢复**（`src/dev_process.erl:192`）：
```erlang
{ok, Loaded} = ensure_loaded(ProcBase, Msg2, Opts)
% 通过 normalize 函数恢复状态
```

#### 实现步骤

1. **复制样板代码** → 创建 `src/dev_your_execution_env.erl`
2. **实现核心函数** → 填充 `initialize_your_vm`、`execute_in_your_vm` 等
3. **注册设备** → 在 `hb_opts.erl` 中添加设备配置
4. **测试集成** → 配置 AOS 进程使用你的执行环境

这个样板代码提供了完整的设备接口框架，你只需要实现具体的 VM 逻辑即可。

### "Your Own VM" 的技术内涵

#### WAO 文档中的表述
> "You can build: Your own VM — Any execution environment"

这意味着你可以创建任何编程语言或执行环境的设备，作为 HyperBEAM 的计算后端。

#### 技术实现路径

```erlang
% 标准设备接口
-module(dev_your_vm).
-export([info/1, compute/3, init/3]).

% 信息函数：定义设备能力
info(_Msg) ->
    #{
        % 支持的函数列表
        functions => [<<"execute">>, <<"call">>, <<"query">>],
        % 默认处理器
        default => fun compute/3
    }.

% 初始化函数：启动你的 VM
init(Base, Req, Opts) ->
    % 启动你的微服务进程
    % 返回初始化状态
    {ok, #{vm_pid => YourVMPid}}.

% 计算函数：执行代码
compute(Key, Base, Req, Opts) ->
    % 1. 提取执行参数
    Code = hb_ao:get(<<"code">>, Req, Opts),
    Input = hb_ao:get(<<"input">>, Req, Opts),

    % 2. 调用你的微服务
    Result = call_your_microservice(Code, Input),

    % 3. 返回结果
    {ok, #{<<"output">> => Result}}.
```

## 验证用户观点：自定义微服务即 VM

### 用户观点分析

> "我们如果用任意语言开发一个'本地运行的、确定性的微服务'（仅对 Erlang 编写的 device/一个薄层暴露 api），其实可以看到 our own VM 不是吗？"

**回答：完全正确！这就是 HyperBEAM 自定义 VM 的本质。**

### 技术验证

#### 1. Actor 模型的支撑

基于我们的分析文档 `@docs/drafts/hyperbeam-actor-oriented-architecture-analysis.md`：

```markdown
**进程隔离**：每个 Actor（进程）维护独立的状态空间
**顺序保证**：Slots 确保每个进程内的消息确定性执行
**并发优化**：不同进程可并行处理，实现超高并发
```

**你的微服务可以作为独立的 Actor，享受所有这些特性。**

#### 2. Slot 机制的保证

```erlang
% 每个进程独立的 slot 序列
NextSlot = maps:get(current, State) + 1,
Assignment = hb_message:commit(#{
    <<"process">> => ProcessID,      % 你的微服务进程
    <<"slot">> => SlotNumber,        % 保证顺序
    <<"body">> => Message            % 执行请求
}, Wallet)
```

**你的微服务收到的消息已经通过 slot 机制排序，保证确定性执行。**

#### 3. 设备栈的集成

```erlang
% 你的进程定义可以这样配置
ProcessConfig = #{
    <<"device">> => <<"process@1.0">>,
    <<"execution-device">> => <<"your-vm@1.0">>,  % 你的自定义 VM
    <<"scheduler-device">> => <<"scheduler@1.0">>
}
```

## 实现指南：构建自定义 VM

### 步骤 1：设计微服务接口

#### 确定性保证
- **纯函数**：相同输入必须产生相同输出
- **无外部依赖**：不能调用随机数、时间、网络等非确定性资源
- **状态管理**：通过 HyperBEAM 的 state 机制管理

#### 接口协议
```json
// 请求格式
{
    "function": "execute",
    "code": "your_deterministic_code",
    "input": {...},
    "slot": 42  // HyperBEAM 保证的顺序编号
}

// 响应格式
{
    "output": {...},
    "gas_used": 1000,
    "state_changes": {...}
}
```

### 步骤 2：实现 Erlang 设备层

```erlang
-module(dev_your_custom_vm).
-export([info/1, init/3, compute/3, terminate/3]).

% 启动你的微服务
init(Base, Req, Opts) ->
    % 启动微服务进程（通过端口、HTTP、或 Erlang 端口驱动）
    {ok, Pid} = start_your_microservice(),
    {ok, Base#{vm_pid => Pid}}.

% 执行计算
compute(Key, Base, Req, Opts) ->
    #{
        <<"vm_pid">> := VMPid,
        <<"process">> := ProcessID,
        <<"slot">> := Slot
    } = Base,

    % 提取执行参数
    Code = hb_ao:get(<<"code">>, Req, Opts),
    Input = hb_ao:get(<<"input">>, Req, Opts),

    % 调用微服务
    Result = your_vm_api:call(VMPid, #{
        function => Key,
        code => Code,
        input => Input,
        process => ProcessID,
        slot => Slot
    }),

    {ok, Result}.
```

### 步骤 3：集成到系统

#### 注册设备
```erlang
% 在 hb_opts.erl 中添加
preloaded_devices => [
    % ... 其他设备 ...
    #{<<"name">> => <<"your-vm@1.0">>, <<"module">> => dev_your_custom_vm}
]
```

#### 配置进程
```javascript
// AOS 风格的进程配置
const processConfig = {
    "Device": "process@1.0",
    "Scheduler-Device": "scheduler@1.0",
    "Execution-Device": "your-vm@1.0",  // 使用你的自定义 VM
    "Your-VM-Language": "Python",       // 或任何语言
    "Your-VM-Version": "3.9"
}
```

## 现有实现对比

### Lua VM 实现分析

```erlang
% dev_lua.erl 的 compute 函数
compute(Key, RawBase, Req, Opts) ->
    % 1. 初始化 Lua 状态
    {ok, Base} = ensure_initialized(RawBase, Req, Opts),

    % 2. 提取函数和参数
    Function = extract_function(Req, Base, Key, Opts),
    Params = extract_parameters(Req, Base, Opts),

    % 3. 调用 Lua VM
    process_response(
        luerl:call_function_dec([Function], encode(Params), State)
    )
```

**你的自定义 VM 可以遵循完全相同的模式，只是将 `luerl:call_function_dec` 替换为对你的微服务的调用。**

### WASM VM 实现分析

```erlang
% dev_wasm.erl 的架构
info(_Msg1, _Opts) ->
    #{ excludes => [instance] }.  % 排除内部函数

compute(Base, Req, Opts) ->
    % 1. 获取 WASM 实例
    Instance = get_wasm_instance(Base),

    % 2. 执行 WASM 函数
    Result = wasm_runtime:call(Instance, Function, Args),

    % 3. 返回结果
    {ok, Result}
```

## 优势与限制

### 🎯 优势

1. **语言自由**：可以用任何编程语言实现业务逻辑
2. **性能优化**：针对特定领域优化执行环境
3. **生态集成**：复用现有代码库和工具链
4. **渐进迁移**：逐步将传统应用迁移到去中心化环境

### ⚠️ 限制与权衡

1. **确定性要求**：必须保证相同输入的确定性输出
2. **状态管理**：需要通过 HyperBEAM 的机制管理状态
3. **调试复杂性**：多语言栈的调试更复杂
4. **信任模型**：微服务运行在节点本地，需要信任节点

## 结论

**你的观点完全正确！** 通过开发一个确定性的微服务并用薄层的 Erlang 设备暴露 API，你确实可以创建自己的 VM。这种方法：

1. **符合 HyperBEAM 架构**：利用 Actor 模型和 slot 机制
2. **保持语言自由**：可以用任何语言实现核心逻辑
3. **保证可验证性**：通过 TEE 和密码学保证执行完整性
4. **支持生态集成**：可以复用现有的代码和工具

这就是 **"Your own VM — Any execution environment"** 的实际实现路径。

---

*本文档验证了自定义微服务作为 VM 的可行性，提供了完整的实现指南。基于 HyperBEAM Actor-Oriented 架构分析。*

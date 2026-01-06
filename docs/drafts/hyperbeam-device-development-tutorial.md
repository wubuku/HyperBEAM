# HyperBEAM 设备开发教程 - Erlang 小白版

## 🎯 前言

如果你是Erlang小白，但想在HyperBEAM中添加自定义设备功能，这篇教程就是为你准备的。我们会一步步从零开始，创建一个简单的测试设备。

## ⚠️ 重要提醒

**HyperBEAM的HTTP响应数据通常在HTTP头中，但也可以在响应体中返回！**

- **默认行为**：使用 `httpsig@1.0` codec，数据在HTTP头中（便于数字签名）
- **可选行为**：某些设备函数可以返回 `<<"body">>` 字段，数据将在响应体中
- **编解码器选择**：通过 `hb_http:accept_to_codec/2` 根据Accept头选择编解码器
- 使用 `curl -v` 查看完整响应（头 + 体）
- 这是HyperBEAM的设计特点，便于数字签名和类型安全

## 📚 基础概念（用大白话解释）

### 什么是设备（Device）？
想象一下HyperBEAM是一个大公司，设备就是公司的各个部门：
- `json@1.0` 部门：专门处理JSON数据
- `lua@5.3a` 部门：专门运行Lua代码
- `mydev@1.0` 部门：我们新创建的部门

### Erlang中的Map是什么？
```erlang
#{<<"name">> => <<"张三">>, <<"age">> => 25}
```
就像Python的字典：
```python
{"name": "张三", "age": 25}
```

### Erlang中的二进制字符串
```erlang
<<"hello">>  % 这就是二进制字符串
```
相当于其他语言的字符串，但更高效。

## 🛠️ 实战步骤

### 步骤1：理解项目结构

```
HyperBEAM/
├── src/                    # 源代码目录
│   ├── hb_opts.erl        # 系统配置（重要！）
│   ├── dev_*.erl          # 各种设备文件
│   └── hb_ao.erl          # 核心解析器
├── Makefile              # 编译脚本
└── rebar.config          # 项目配置
```

### 步骤2：创建设备文件

**文件名**: `src/dev_mydev.erl`

```erlang
-module(dev_mydev).                    % 模块名，必须和文件名一致
-export([info/3]).                     % 导出函数列表

-include_lib("eunit/include/eunit.hrl"). % 单元测试头文件
-include("include/hb.hrl").             % HyperBEAM头文件

info(Msg1, Msg2_, Opts) ->              % 主要的info函数
    {ok, #{<<"version">> => <<"1.0">>}}. % 返回版本信息
```

#### 代码解释：
- **`-module(dev_mydev).`**: 定义模块名，就像Python的`class MyDev`
- **`-export([info/3]).`**: 告诉Erlang这个函数可以被外部调用
- **`info/3`**: 函数名后面跟`/`和参数个数，表示这个函数接收3个参数
- **`{ok, Data}`**: Erlang的标准返回格式，`ok`表示成功，后面是数据

### 步骤3：注册设备到系统

**文件**: `src/hb_opts.erl`

在`preloaded_devices`列表中添加：

```erlang
preloaded_devices => [
    #{<<"name">> => <<"ans104@1.0">>, <<"module">> => dev_codec_ans104},
    #{<<"name">> => <<"json@1.0">>, <<"module">> => dev_codec_json},
    % ... 其他设备 ...
    #{<<"name">> => <<"mydev@1.0">>, <<"module">> => dev_mydev},  % ← 新增的！
    % ... 更多设备 ...
],
```

#### 注册表解释：
- **`<<"mydev@1.0">>`**: 设备名称，版本号用`@`分隔
- **`dev_mydev`**: 对应的Erlang模块名
- **为什么需要注册？**: 系统通过这个表知道哪个名字对应哪个模块

### 步骤4：编译项目

```bash
# 在项目根目录执行
make compile
```

或者直接用rebar3：
```bash
rebar3 compile
```

### 步骤5：测试设备

#### 方法1：在Erlang shell中测试

```bash
# 启动HyperBEAM（使用默认端口8734）
cd /path/to/HyperBEAM
rebar3 shell

# 如果端口被占用，使用自定义端口
HB_PORT=18735 rebar3 shell
```

启动成功后，你会看到类似这样的输出：
```
===> Booted hb
(hb@your-host)1>
```

在Erlang提示符下输入测试命令：
```erlang
% 测试基本功能
hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, <<"info">>, #{}).

% 测试hello功能
hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, #{<<"path">> => <<"hello">>, <<"name">> => <<"小明">>}, #{}).

% 测试counter功能
hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, #{<<"path">> => <<"counter">>}, #{}).
```

**预期输出**:
```erlang
{ok,#{<<"priv">> => #{<<"hashpath">> => <<"xxx">>}, <<"version">> => <<"1.0">>}}
```

#### 方法2：通过HTTP API测试

HyperBEAM启动时会显示实际监听的端口，请查看启动日志中的 "Node activate at: http://localhost:XXXXX"。

```bash
# 查看启动日志获取实际端口
# 例如：如果显示10000端口，则使用：

# ⚠️ 重要：使用 -v 参数查看HTTP头中的响应数据！

# 基本测试（替换XXXX为实际端口）
curl -v "http://localhost:XXXX/~mydev@1.0/info"

# 测试hello功能（使用POST，因为有body参数）
curl -v -X POST "http://localhost:XXXX/~mydev@1.0/hello" \
     -H "Content-Type: application/json" \
     -d '{"name": "小明"}'

# 测试counter功能
curl -v -X POST "http://localhost:XXXX/~mydev@1.0/counter"
```

**预期响应**:
```bash
# 注意：HyperBEAM默认将响应数据放在HTTP头中（使用httpsig@1.0 codec）
# 如果设备返回body字段，则该字段内容在响应体中
# 使用 curl -v 查看完整响应（头 + 体）

HTTP/1.1 200 OK
features: "hello", "counter"
status: 200
version: 1.0
```

## ⚠️ 重要说明：HTTP响应机制

### HyperBEAM的独特响应方式

**HyperBEAM默认使用HTTP头返回数据，但也支持在响应体中返回数据！**

#### 默认行为（httpsig@1.0 codec）：
- 将消息的所有字段（除`body`外）放在HTTP响应头中
- 如果设备函数返回`<<"body">>`字段，则该字段内容在响应体中
- 这是HyperBEAM的设计特点，便于数字签名和类型安全

#### 其他 codec 行为：
- **ans104@1.0**: 使用二进制格式，完整数据在响应体中
- **其他 codec**: 根据编码器实现，可能在头或体中返回

#### 为什么这样设计？
1. **性能优化**：HTTP头传输更快，无需解析JSON
2. **类型安全**：头信息包含类型注解（如`ao-types`）
3. **数字签名**：便于对响应进行数字签名验证
4. **灵活性**：支持不同的编码格式和传输方式

#### 如何查看响应数据？

```bash
# ❌ 错误：只显示响应体（空的）
curl "http://localhost:8734/~mydev@1.0/info"

# ✅ 正确：查看完整HTTP头
curl -v "http://localhost:8734/~mydev@1.0/info"

# ✅ 或者只查看关键头信息
curl -v "http://localhost:8734/~mydev@1.0/info" 2>&1 | grep -E "(version:|features:|status:)"
```

#### 响应头格式示例：
```
< HTTP/1.1 200 OK
< version: 1.0
< features: "hello", "counter"
< status: 200
< ao-types: features="list", status="integer"
< signature: http-sig-xxxxx...  # 数字签名
```

### 📝 开发时的注意事项

- **调试时总是使用 `curl -v`** 来查看HTTP头
- **数据在头中**：`version`、`features`、`status` 等字段
- **类型信息**：`ao-types` 头包含字段类型信息
- **安全验证**：`signature` 头用于验证响应完整性

### 端口配置

HyperBEAM的端口配置有三层优先级（从高到低）：

1. **环境变量**（最高优先级）:
   ```bash
   HB_PORT=18735 rebar3 shell  # 使用端口18735
   ```

2. **配置文件** `config.flat`:
   ```yaml
   port: 10000  # 当前设置为10000
   ```

3. **代码默认值**（最低优先级）:
   ```erlang
   port => 8734  % src/hb_opts.erl 中的默认值
   ```

**查看实际端口**: 启动时查找日志中的 "Node activate at: http://localhost:XXXXX"

### 常见启动问题

#### 端口被占用
```bash
# 一行命令：自动杀死占用指定端口的所有进程
lsof -ti :8734 | xargs kill -9 2>/dev/null || true

# 或者手动操作：
# 查看端口占用
lsof -i :8734

# 杀死占用进程（替换<PID>为实际进程ID）
kill -9 <PID>

# 或者使用不同端口启动
HB_PORT=18735 rebar3 shell
```

#### 退出Erlang shell
```erlang
q().  % 输入并按回车
```

## 📋 理解 HyperBEAM 的三个核心参数

### 设备函数的参数详解

HyperBEAM 的设备函数使用 **arity 3**（三个参数）的签名：`function_name(Msg1, Msg2, Opts)`。

这三个参数分工明确，各司其职：

#### 1. **`Opts` - 系统配置参数**
```erlang
% 示例内容：
#{
    "mode": "debug",                    % 运行模式
    "store": #{...},                    % 存储配置
    "port": 8734,                       % 端口配置
    "cache": #{...},                    % 缓存配置
    "priv_wallet": #{...},              % 私钥钱包
    "trace": <TracePID>,                % 跟踪进程ID
    % ... 其他系统级配置
}
```
**作用**：
- 节点级别的全局配置
- 影响整个系统的行为
- 包含权限、缓存、存储等设置

#### 2. **`Msg1` - 状态消息（State Message）**
```erlang
% 示例内容：
#{
    "device": "mydev@1.0",              % 目标设备标识
    "count": 5,                         % 当前计数器值
    "already-seen": [...],               % 历史数据
    "hashpath": "xxx",                   % 数据哈希路径
    % ... 设备状态和历史数据
}
```
**作用**：
- 包含设备的状态信息
- 存储历史数据和上下文
- 通常用于读操作（`hb_ao:get/3`）

#### 3. **`Msg2` - 请求消息（Request Message）**
```erlang
% 示例内容：
#{
    "path": "counter",                   % 请求路径
    "name": "小明",                      % 用户参数
    "method": "POST",                    % HTTP方法
    "commitments": #{                    % 数字签名
        "alg": "rsa-pss-sha512",
        "signature": "..."
    },
    "host": "localhost:8734",            % 主机信息
    % ... 请求相关信息
}
```
**作用**：
- 包含用户的请求参数
- 路径信息和签名数据
- 通常用于写操作和参数提取

### 🔄 参数处理流程

```erlang
% 典型的使用模式：
counter(Msg1, Msg2, Opts) ->
    % 从 Msg1 读取当前状态
    Current = hb_ao:get(<<"count">>, Msg1, 0, Opts),

    % 从 Msg2 提取请求参数
    Increment = hb_ao:get(<<"increment">>, Msg2, 1, Opts),

    % 计算新状态
    NewCount = Current + Increment,

    % 将新状态保存到 Msg1 并返回
    {ok, hb_ao:set(Msg1, #{<<"count">> => NewCount}, Opts)}.
```

### 💡 实际应用示例

#### HTTP API 调用时：
```bash
curl -X POST "http://localhost:8734/~mydev@1.0/counter" \
     -H "Content-Type: application/json" \
     -d '{"increment": 3}'
```

**此时参数内容**：
- **`Msg1`**: `{<<"device">> => <<"mydev@1.0">>, <<"count">> => 0}`
- **`Msg2`**: `{<<"path">> => <<"counter">>, <<"increment">> => 3, <<"method">> => <<"POST">>, ...}`
- **`Opts`**: `{<<"port">> => 8734, <<"mode">> => <<"debug">>, ...}`

#### Erlang 直接调用时：
```erlang
hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, #{<<"path">> => <<"counter">>}, #{}).
```

**此时参数内容**：
- **`Msg1`**: `{<<"device">> => <<"mydev@1.0">>}`
- **`Msg2`**: `{<<"path">> => <<"counter">>}`
- **`Opts`**: `#{} `（空配置）

### 🎯 参数设计理念

- **`Msg1`**: "状态容器" - 存放设备的状态和历史数据
- **`Msg2`**: "请求容器" - 存放用户的请求信息和参数
- **`Opts`**: "系统容器" - 存放全局配置和系统级设置

这种设计使得设备函数能够：
1. **读取状态**：从 Msg1 获取当前状态
2. **处理请求**：从 Msg2 提取参数
3. **系统集成**：通过 Opts 访问系统功能

## 🔍 核心机制详解

### HyperBEAM如何找到你的设备？

1. **用户请求**: `hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, <<"info">>, #{})`

2. **系统查找**:
   - 在`preloaded_devices`列表中搜索`<<"mydev@1.0">>`
   - 找到对应的`dev_mydev`模块

3. **函数调用**:
   - 调用`dev_mydev:info(Msg1, Msg2, Opts)`
   - `Msg1` = `#{<<"device">> => <<"mydev@1.0">>}`
   - `Msg2` = `<<"info">>`
   - `Opts` = `#{} `

4. **返回结果**:
   - 你的函数返回: `{ok, #{<<"version">> => <<"1.0">>}}`
   - 系统添加私有数据，返回给用户

### arity 3的重要性

```erlang
info(Msg1, Msg2, Opts) -> ...  % ← 这是arity 3的函数
```

- **Msg1**: 状态消息（包含设备信息、历史数据等）
- **Msg2**: 请求消息（包含路径、参数等）
- **Opts**: 选项配置（缓存、权限等）

HyperBEAM**只路由arity 3的函数**到HTTP API！

## 🚀 扩展你的设备

### 添加更多功能

```erlang
-module(dev_mydev).
-export([info/3, hello/3, counter/3]).  % 导出更多函数

info(Msg1, Msg2, Opts) ->
    {ok, #{<<"version">> => <<"1.0">>, <<"features">> => [<<"hello">>, <<"counter">>]}}.

hello(Msg1, Msg2, Opts) ->
    Name = hb_ao:get(<<"name">>, Msg2, <<"World">>, Opts),  % 获取参数
    Message = <<"Hello, ", Name/binary, "!">>,
    {ok, #{<<"message">> => Message}}.

counter(Msg1, Msg2, Opts) ->
    Current = hb_ao:get(<<"count">>, Msg1, 0, Opts),  % 获取当前计数
    NewCount = Current + 1,
    {ok, hb_ao:set(Msg1, #{<<"count">> => NewCount}, Opts)}.  % 保存新状态
```

### 测试新功能

```erlang
% 测试hello功能
hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, #{<<"path">> => <<"hello">>, <<"name">> => <<"小明">>}, #{}).

% 测试counter功能
hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, #{<<"path">> => <<"counter">>}, #{}).
```

## 🐛 常见问题

### Q: 编译时出现警告？
A: 忽略吧！我们还没用到那些参数。

### Q: 端口被占用怎么办？
```bash
# 找到占用进程
lsof -i :8734

# 杀死进程
kill -9 <PID>

# 重新启动
rebar3 shell
```

### Q: 怎么退出Erlang shell？
```erlang
q().  % 输入这个并按回车
```

### Q: 忘记导出函数怎么办？
A: 在`-export([...])`中添加函数名，如`-export([info/3, hello/3]).`

## 🎉 总结

恭喜！你现在知道如何在HyperBEAM中添加自定义设备了！

**核心步骤**:
1. 创建`dev_你的设备名.erl`文件
2. 实现arity 3的函数
3. 在`hb_opts.erl`中注册设备
4. 编译并测试

**记住**: HyperBEAM就像一个模块化的超级计算机，你创建的每个设备都是一个"小程序"，可以通过HTTP API或Erlang代码调用。

继续探索，创造更多有趣的设备吧！🚀

---

## 📋 完整代码参考

### src/dev_mydev.erl
```erlang
-module(dev_mydev).
-export([info/3, hello/3, counter/3]).

-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

info(Msg1, Msg2, Opts) ->
    {ok, #{<<"version">> => <<"1.0">>, <<"features">> => [<<"hello">>, <<"counter">>]}}.

hello(Msg1, Msg2, Opts) ->
    Name = hb_ao:get(<<"name">>, Msg2, <<"World">>, Opts),
    Message = <<"Hello, ", Name/binary, "!">>,
    {ok, #{<<"message">> => Message}}.

counter(Msg1, Msg2, Opts) ->
    Current = hb_ao:get(<<"count">>, Msg1, 0, Opts),
    NewCount = Current + 1,
    {ok, hb_ao:set(Msg1, #{<<"count">> => NewCount}, Opts)}.
```

### src/hb_opts.erl (preloaded_devices部分)
```erlang
preloaded_devices => [
    % ... 其他设备 ...
    #{<<"name">> => <<"mydev@1.0">>, <<"module">> => dev_mydev},  % ← 新增的设备
    % ... 更多设备 ...
],
```

### 测试命令
```bash
# 启动HyperBEAM
rebar3 shell

# 测试基本功能
hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, <<"info">>, #{}).

# 测试hello功能
hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, #{<<"path">> => <<"hello">>, <<"name">> => <<"小明">>}, #{}).

# 测试counter功能
hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, #{<<"path">> => <<"counter">>}, #{}).

# HTTP API测试（基本功能，替换XXXX为实际端口）
curl "http://localhost:XXXX/~mydev@1.0/info"

# HTTP API测试（hello功能，需要POST）
curl -X POST "http://localhost:XXXX/~mydev@1.0/hello" \
     -H "Content-Type: application/json" \
     -d '{"name": "小明"}'

# HTTP API测试（counter功能）
curl -X POST "http://localhost:XXXX/~mydev@1.0/counter"

# 查看实际端口的方法：
# 启动后查看日志："Node activate at: http://localhost:XXXXX"
```

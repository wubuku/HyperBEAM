# HyperBEAM 计数器设备开发完整教程

## 🎯 概述

本教程详细介绍如何在HyperBEAM中开发一个完整的**有状态计数器设备**。通过这个实践，我们将深入理解HyperBEAM设备开发的完整流程，包括状态管理、HTTP API设计、错误排查等关键环节。

**最终成果**：一个支持GET查看计数、POST递增计数的完整设备。

---

## 📋 设备规格

### 功能要求
- **GET** `/~counter@1.0/info` - 获取设备元数据
- **GET** `/~counter@1.0/value` - 获取当前计数器值
- **POST** `/~counter@1.0/increment` - 递增计数器（+1）

### 技术要求
- ✅ 持久化状态存储
- ✅ HTTP状态码正确性
- ✅ 完整的单元测试
- ✅ 错误处理

---

## 🛠️ 完整实现代码

### dev_counter.erl - 完整代码

```erlang
%%%-------------------------------------------------------------------
%%% @doc Simple Counter Device
%%%
%%% A simple stateful counter device. Each increment request increases
%%% the counter by 1.
%%%
%%% API:
%%%   GET  /~counter@1.0/info              Device metadata
%%%   GET  /~counter@1.0/value             Get current counter value
%%%   POST /~counter@1.0/increment         Increment counter by 1
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dev_counter).
-export([info/3, value/3, increment/3]).
-include("include/hb.hrl").

-define(STATE_KEY, <<"counter-state-id">>).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Device metadata
info(_M1, _M2, _Opts) ->
    {ok, #{
        <<"name">> => <<"counter">>,
        <<"version">> => <<"1.0">>,
        <<"description">> => <<"Simple Counter Device - increments by 1 on each request">>,
        <<"author">> => <<"HyperBEAM Tutorial">>
    }}.

%% @doc Get the current counter value
value(M1, _M2, Opts) ->
    State = load_state(M1, Opts),
    CounterValue = maps:get(<<"counter">>, State, 0),
    {ok, integer_to_binary(CounterValue)}.

%% @doc Handle counter operations based on HTTP method
increment(M1, M2, Opts) ->
    % Load current state
    State = load_state(M1, Opts),
    CurrentValue = maps:get(<<"counter">>, State, 0),

    % Increment by 1
    NewValue = CurrentValue + 1,

    % Save updated state
    NewState = maps:put(<<"counter">>, NewValue, State),
    M1Updated = save_state(M1, NewState, Opts),

    {ok, maps:merge(M1Updated, #{
        <<"value">> => NewValue,
        <<"status">> => 200
    })}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Load state from private storage
load_state(M1, Opts) ->
    case hb_private:get(?STATE_KEY, M1, not_found, Opts) of
        not_found ->
            #{};
        StateID ->
            case hb_cache:read(StateID, Opts) of
                {ok, State} ->
                    hb_cache:ensure_all_loaded(State, Opts);
                not_found ->
                    #{}
            end
    end.

%% @private Save state to private storage
save_state(M1, State, Opts) ->
    {ok, StateID} = hb_cache:write(State, Opts),
    hb_private:set(M1, #{?STATE_KEY => StateID}, Opts).

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_test_env() ->
    application:ensure_all_started(hb),
    Store = hb_test_utils:test_store(hb_store_fs),
    #{store => [Store]}.

%% Test device info
info_test() ->
    {ok, Info} = info(#{}, #{}, #{}),
    ?assertEqual(<<"counter">>, maps:get(<<"name">>, Info)),
    ?assertEqual(<<"1.0">>, maps:get(<<"version">>, Info)).

%% Test counter operations
counter_test() ->
    Opts = setup_test_env(),
    M1 = #{},

    % Initial value should be 0
    {ok, Initial} = value(M1, #{}, Opts),
    ?assertEqual(<<"0">>, Initial),

    % Increment counter by 1
    {ok, IncResult1} = increment(M1, #{}, Opts),
    ?assertEqual(1, maps:get(<<"value">>, IncResult1)),
    ?assertEqual(200, maps:get(<<"status">>, IncResult1)),

    % Value should now be 1
    M1AfterInc1 = IncResult1,
    {ok, ValueAfterInc1} = value(M1AfterInc1, #{}, Opts),
    ?assertEqual(<<"1">>, ValueAfterInc1),

    % Increment again
    {ok, IncResult2} = increment(M1AfterInc1, #{}, Opts),
    ?assertEqual(2, maps:get(<<"value">>, IncResult2)),

    % Final value should be 2
    M1AfterInc2 = IncResult2,
    {ok, FinalValue} = value(M1AfterInc2, #{}, Opts),
    ?assertEqual(<<"2">>, FinalValue).

-endif.
```

---

## 🏗️ 开发过程详解

### 步骤1：创建设备文件

创建 `src/dev_counter.erl` 文件，定义模块和导出函数：

```erlang
-module(dev_counter).
-export([info/3, value/3, increment/3]).
-include("include/hb.hrl").
```

### 步骤2：实现基础功能

#### 2.1 Info函数 - 设备元数据

```erlang
info(_M1, _M2, _Opts) ->
    {ok, #{
        <<"name">> => <<"counter">>,
        <<"version">> => <<"1.0">>,
        <<"description">> => <<"Simple Counter Device - increments by 1 on each request">>,
        <<"author">> => <<"HyperBEAM Tutorial">>
    }}.
```

**参数说明**：
- `_M1`: 消息1，通常包含上下文信息
- `_M2`: 消息2，包含请求信息（如HTTP方法、路径等）
- `_Opts`: 选项，包含运行时配置

#### 2.2 Value函数 - 读取计数器

```erlang
value(M1, _M2, Opts) ->
    State = load_state(M1, Opts),
    CounterValue = maps:get(<<"counter">>, State, 0),
    {ok, integer_to_binary(CounterValue)}.
```

**关键点**：
- 使用 `load_state/2` 从持久化存储加载状态
- 默认值为0（首次使用）
- 返回二进制字符串格式

### 步骤3：实现状态管理

#### 3.1 状态键定义

```erlang
-define(STATE_KEY, <<"counter-state-id">>).
```

#### 3.2 状态加载函数

```erlang
load_state(M1, Opts) ->
    case hb_private:get(?STATE_KEY, M1, not_found, Opts) of
        not_found ->
            #{};
        StateID ->
            case hb_cache:read(StateID, Opts) of
                {ok, State} ->
                    hb_cache:ensure_all_loaded(State, Opts);
                not_found ->
                    #{}
            end
    end.
```

**存储架构**：
1. `hb_private` - 存储状态ID引用
2. `hb_cache` - 存储实际状态数据

#### 3.3 状态保存函数

```erlang
save_state(M1, State, Opts) ->
    {ok, StateID} = hb_cache:write(State, Opts),
    hb_private:set(M1, #{?STATE_KEY => StateID}, Opts).
```

### 步骤4：实现Increment函数

```erlang
increment(M1, M2, Opts) ->
    % Load current state
    State = load_state(M1, Opts),
    CurrentValue = maps:get(<<"counter">>, State, 0),

    % Increment by 1
    NewValue = CurrentValue + 1,

    % Save updated state
    NewState = maps:put(<<"counter">>, NewValue, State),
    M1Updated = save_state(M1, NewState, Opts),

    {ok, maps:merge(M1Updated, #{
        <<"value">> => NewValue,
        <<"status">> => 200
    })}.
```

**重要细节**：
- 返回新的M1（包含更新后的私有状态）
- 返回新计数值
- **状态码必须是整数**（见下文坑点）

### 步骤5：注册设备

在 `src/hb_opts.erl` 中添加：

```erlang
preloaded_devices => [
    % ... 现有设备 ...
    #{<<"name">> => <<"counter@1.0">>, <<"module">> => dev_counter}
],
```

### 步骤6：编译测试

```bash
rebar3 compile
rebar3 shell
```

---

## 🧪 测试过程

### 单元测试

```erlang
counter_test() ->
    Opts = setup_test_env(),
    M1 = #{},

    % Initial value should be 0
    {ok, Initial} = value(M1, #{}, Opts),
    ?assertEqual(<<"0">>, Initial),

    % Increment counter by 1
    {ok, IncResult1} = increment(M1, #{}, Opts),
    ?assertEqual(1, maps:get(<<"value">>, IncResult1)),
    ?assertEqual(200, maps:get(<<"status">>, IncResult1)),

    % Value should now be 1
    M1AfterInc1 = IncResult1,
    {ok, ValueAfterInc1} = value(M1AfterInc1, #{}, Opts),
    ?assertEqual(<<"1">>, ValueAfterInc1),

    % Increment again
    {ok, IncResult2} = increment(M1AfterInc1, #{}, Opts),
    ?assertEqual(2, maps:get(<<"value">>, IncResult2)),

    % Final value should be 2
    M1AfterInc2 = IncResult2,
    {ok, FinalValue} = value(M1AfterInc2, #{}, Opts),
    ?assertEqual(<<"2">>, FinalValue).
```

### HTTP API测试

```bash
# 启动节点
rebar3 shell

# 测试info
curl "http://localhost:8734/~counter@1.0/info"

# 测试value
curl "http://localhost:8734/~counter@1.0/value"

# 测试increment
curl -X POST "http://localhost:8734/~counter@1.0/increment"
```

---

## 💥 遇到的坑和解决方案

### 坑1：POST Increment 返回500错误

**现象**：
```bash
$ curl -X POST "http://localhost:8734/~counter@1.0/increment"
# 返回: HTTP/1.1 500 Internal Server Error
```

**错误信息**：
```
erlang, binary_to_integer, incremented
```

**原因**：
在 `increment/3` 函数中，返回的status字段使用了字符串：
```erlang
<<"status">> => <<"incremented">>  % ❌ 错误！
```

HyperBEAM的 `dev_meta:message_to_status/1` 函数会尝试将status转换为HTTP状态码，但无法将 `"incremented"` 字符串转换为整数。

**解决方案**：
```erlang
<<"status">> => 200  % ✅ 正确：使用HTTP状态码
```

### 坑2：错误的权限分析

**误区**：
起初认为POST失败是因为缓存写入权限不足，需要客户端签名。

**真相**：
HyperBEAM节点默认配置 `force_signed => true`，服务端会自动为所有响应签名，客户端无需额外签名。

**教训**：
不要过早下结论，要仔细分析错误堆栈和日志。

### 坑3：状态管理复杂度

**挑战**：
HyperBEAM的状态管理涉及两层存储：
- `hb_private` - 私有状态引用
- `hb_cache` - 实际状态数据

**解决方案**：
封装为 `load_state/2` 和 `save_state/3` 辅助函数，隐藏实现细节。

---

## 🎯 核心概念理解

### 1. 设备函数签名

所有设备函数都遵循 `(M1, M2, Opts) -> Result` 模式：

- **M1**: 输入消息，包含上下文和历史状态
- **M2**: 请求消息，包含HTTP信息（方法、路径、body等）
- **Opts**: 运行时选项
- **Result**: `{ok, Response}` 或 `{error, Reason}`

### 2. 状态持久化

HyperBEAM使用两层架构：
- **私有层** (`hb_private`): 存储状态ID引用
- **缓存层** (`hb_cache`): 存储实际状态数据

### 3. HTTP状态码处理

设备返回的状态码必须是**整数**，不能是字符串。HyperBEAM会自动将其转换为HTTP响应状态码。

### 4. 消息传递

- **M1**: 包含私有状态和历史
- **返回的M1**: 可能包含更新后的私有状态
- **响应数据**: 通过maps返回给客户端

---

## 🚀 最佳实践

### 1. 错误处理
```erlang
% 始终检查函数返回值
case load_state(M1, Opts) of
    not_found -> handle_missing_state();
    State -> process_state(State)
end
```

### 2. 类型一致性
```erlang
% 统一使用二进制字符串作为键
#{<<"status">> => 200, <<"data">> => <<"value">>}
```

### 3. 状态封装
```erlang
% 隐藏状态管理细节
load_state(M1, Opts) -> % 封装逻辑
save_state(M1, State, Opts) -> % 封装逻辑
```

### 4. 完整的测试覆盖
```erlang
% 测试所有代码路径
info_test() -> % 元数据测试
counter_test() -> % 功能测试
```

---

## 📚 参考资料

- [HyperBEAM设备开发官方文档](https://docs.wao.eco/hyperbeam/custom-devices-codecs#building-custom-devices)
- `src/dev_meta.erl` - 响应处理逻辑
- `src/hb_private.erl` - 私有状态管理
- `src/hb_cache.erl` - 缓存系统

---

## 🎉 总结

通过这个计数器设备的开发，我们掌握了：

✅ **HyperBEAM设备开发基础**
✅ **状态持久化管理**
✅ **HTTP API设计**
✅ **错误排查技巧**
✅ **最佳实践应用**

这个设备虽然简单，但涵盖了HyperBEAM设备开发的核心概念和常见坑点，为开发更复杂的设备奠定了基础。

**记住**：开发过程中遇到问题时，要仔细分析错误信息，不要轻易下结论！ 🔍
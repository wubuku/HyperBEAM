# HyperBEAM 设备开发教程

参考：[https://docs.wao.eco/hyperbeam/custom-devices-codecs#building-custom-devices](https://docs.wao.eco/hyperbeam/custom-devices-codecs#building-custom-devices)

## 🎯 教程概述

## 📚 前置知识

### HTTP API特性
- **响应数据在HTTP头中**：使用 `curl -v` 查看完整响应
- **JSON参数处理**：HTTP请求的JSON body会被放在Msg2的`<<"body">>`字段中，需要手动解析

## 🛠️ 完整实现步骤

### 步骤1：创建设备文件

创建文件 `src/dev_mydev.erl`：

```erlang
-module(dev_mydev).
-export([info/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% 返回设备信息和功能列表
info(_Msg1, _Msg2, _Opts) ->
    {ok, #{
        <<"version">> => <<"1.0">>
    }}.
```

### 步骤2：注册设备

在 `src/hb_opts.erl` 的 `preloaded_devices` 列表中添加：

```erlang
preloaded_devices => [
    % ... 现有设备 ...
    #{<<"name">> => <<"mydev@1.0">>, <<"module">> => dev_mydev}
],
```

### 步骤3：编译项目

```bash
cd /path/to/HyperBEAM
rebar3 compile
```

## 🧪 完整测试过程

### 方法1：Erlang Shell测试

```bash
# 可以在 `config.flat` 文件中配置端口号
# 启动HyperBEAM
rebar3 shell
```

启动成功后会显示：
```
===> Booted hb
== Node activate at:              http://localhost:XXXXX
(hb@your-host)1>
```

#### 测试info功能
```erlang
hb_ao:resolve(#{<<"device">> => <<"mydev@1.0">>}, <<"info">>, #{}).
```
**预期输出**：
```erlang
{ok,#{<<"version">> => <<"1.0">>}}
```

### 方法2：HTTP API测试

#### 获取实际端口
启动时查找日志中的端口号：
```
== Node activate at:              http://localhost:10006
```

#### 测试info功能
```bash
curl -v "http://localhost:10006/~mydev@1.0/info"
```
**预期响应头**：
```
< HTTP/1.1 200 OK
< version: 1.0
< status: 200
```

## 🔧 故障排除

### 端口被占用
```bash
# 检查端口占用
lsof -i :10006

# 使用不同端口
echo "port: 10007" > config.flat
rebar3 shell

# 杀掉占用端口的进程
ps aux | grep "rebar3 shell" | grep -v grep | awk '{print $2}' | xargs -r kill -9
```

### 编译错误
```bash
# 清理并重新编译
rebar3 clean
rebar3 compile
```

### HTTP请求失败
- 确保使用 `-v` 参数查看完整响应
- 检查端口号是否正确
- 确认设备已正确注册

## 🎯 核心概念详解

### HTTP参数传递机制

**重要**：HTTP请求的JSON body会被完整地放在Msg2的`<<"body">>`字段中，而不是直接解析为Map字段。

```erlang
% ❌ 错误方式
Name = hb_ao:get(<<"name">>, Msg2, <<"World">>, Opts)

% ✅ 正确方式
JsonBody = hb_ao:get(<<"body">>, Msg2, <<"{}">>, Opts),
Params = hb_json:decode(JsonBody),
Name = maps:get(<<"name">>, Params, <<"World">>)
```


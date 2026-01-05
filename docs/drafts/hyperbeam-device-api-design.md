# HyperBEAM设备API设计机制

## 概述

本文档基于对HyperBEAM代码库的深入分析，系统性地阐述了HyperBEAM设备API的设计机制，包括URL路径解析、设备路径设计的灵活性以及HTTP方法的全面支持。

## URL路径解析机制

### 基础URL结构分析

以具体的URL路径为例：
```
GET /~process@1.0/ABC123/slot/5/state
     ↑          ↑      ↑   ↑   ↑
   device    process  func arg key
```

#### 路径组成部分
- **`~process@1.0`**: 设备标识符，表示使用`process@1.0`设备
- **`ABC123`**: 进程ID，作为资源标识符
- **`slot`**: 操作类型（function），表示要执行slot相关操作
- **`5`**: 参数（argument），表示slot编号
- **`state`**: 键（key），表示要获取的内容类型

#### 技术实现
路径解析通过以下步骤完成：

1. **路径分割**: `hb_path:term_to_path_parts()`按`/`分割路径为列表
2. **Key提取**: `hb_path:hd()`获取第一个路径段作为设备调用的key
3. **设备路由**: 根据key调用相应的设备函数

### Key的含义分析

在上述URL中，最后的`state`作为key，表示要获取的内容类型：

- **完整状态快照**: 返回指定slot的完整进程状态
- **包含内容**: 包括`at-slot`、计算结果、缓存快照等
- **用途**: 允许访问进程在任意历史slot的完整状态

## 设备路径设计的灵活性

### 非强制性设计约束

**重要发现**: `func`、`arg`、`key`三段式结构并非所有设备的强制要求。

#### 设计自由度
设备开发者可以根据具体需求设计完全不同的路径结构：

**方式A：简单Key-Value模式**（如`dev_message`设备）
```
GET /message/some-key
→ get("some-key", Message)
```

**方式B：RESTful风格**（如`process@1.0`设备）
```
GET /process/ABC123/slot/5/state
→ slot(5, "state", Process)
```

**方式C：命令式风格**
```
GET /calculator/add/2/3
→ add(2, 3, Calculator)
```

**方式D：默认处理器模式**
```erlang
info() -> #{ default => fun handle_any_path/3 }
```

#### 设备调用机制

HyperBEAM提供了5种设备函数查找和调用方式：

1. **显式处理器函数**: 设备定义`handler`函数
2. **直接导出函数**: 设备直接导出与key同名的函数
3. **默认处理器**: 使用`default`处理器处理所有key
4. **默认模块**: 切换到默认设备处理
5. **递归处理**: 继续查找其他处理方式

### 实际设备对比

| 设备 | 路径结构 | 设计理念 |
|------|----------|----------|
| `process@1.0` | `/{processId}/{func}/{arg}/{key}` | 多层次资源访问 |
| `dev_message` | `/{key}` | 简单键值存储 |
| `dev_scheduler` | `/{operation}` | 操作导向 |
| `dev_wasm` | `/{action}` | 功能导向 |

## HTTP方法的全面支持

### 方法支持范围

HyperBEAM支持所有标准HTTP方法：

```erlang
allowed_methods(Req, State) ->
    {
        [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>, <<"PATCH">>],
        Req,
        State
    }.
```

### 方法传递机制

HTTP方法被转换为AO-Core消息中的`method`字段：

```erlang
Method = cowboy_req:method(Req),
Msg#{ <<"method">> => Method, <<"path">> => MsgPath }.
```

### 设备级方法处理

设备可以根据HTTP方法实现不同的行为：

#### Scheduler设备示例
```erlang
case hb_ao:get(<<"method">>, Msg2, <<"GET">>, Opts) of
    <<"POST">> -> post_schedule(Msg1, Msg2, Opts);
    <<"GET">> -> get_schedule(Msg1, Msg2, Opts)
end.
```

#### Patch设备示例
```erlang
Method = hb_ao:get(<<"method">>, Msg, Opts) == <<"PATCH">>
```

### RESTful API实现

设备可以实现完整的RESTful API：

| HTTP方法 | 典型用途 | 示例 |
|----------|----------|------|
| **GET** | 查询/读取数据 | `GET /~process@1.0/ABC123/slot/5/state` |
| **POST** | 创建资源/执行操作 | `POST /~process@1.0/ABC123/schedule` |
| **PUT** | 更新/替换资源 | `PUT /~resource@1.0/item/123` |
| **DELETE** | 删除资源 | `DELETE /~resource@1.0/item/123` |
| **PATCH** | 部分更新 | `PATCH /~process@1.0/ABC123/cache` |
| **OPTIONS** | 获取支持的方法 | `OPTIONS /~device@1.0/*` |

## 核心设计原则

### 1. 最大化灵活性
- 设备开发者可以自由设计API接口
- 路径结构可以完全自定义
- HTTP方法使用不受限制

### 2. 统一的消息模型
- 所有请求都转换为AO-Core消息格式
- 设备通过标准接口进行交互
- 支持签名验证和权限控制

### 3. 分层架构
- HTTP层：处理网络协议
- 消息层：标准化请求格式
- 设备层：实现具体业务逻辑
- 存储层：持久化数据

## 结论

HyperBEAM的设备API设计体现了高度的灵活性和可扩展性：

1. **路径设计完全自主**: 设备可以根据需求设计任意复杂的路径结构
2. **HTTP方法全面支持**: 支持所有标准HTTP方法，实现RESTful API
3. **统一的消息抽象**: 通过AO-Core消息模型实现设备间的互操作
4. **渐进式复杂性**: 从简单的key-value接口到复杂的多层资源访问

这种设计使得HyperBEAM能够支持从简单的数据存储到复杂计算引擎的各种设备类型，同时保持统一的编程模型和网络接口。

## 附录：代码引用

### 路径解析核心函数
```erlang
% hb_path.erl
term_to_path_parts(Binary, Opts) when is_binary(Binary) ->
    case binary:match(Binary, <<"/">>) of
        nomatch -> [Binary];
        _ ->
            term_to_path_parts(
                binary:split(Binary, <<"/">>, [global, trim_all]),
                Opts
            )
    end.
```

### 设备函数调用
```erlang
% hb_ao.erl - 设备函数查找
message_to_fun(Msg, Key, Opts) ->
    Dev = message_to_device(Msg, Opts),
    Info = info(Dev, Msg, Opts),
    % 5种不同的处理方式...
```

### HTTP方法传递
```erlang
% hb_http.erl
Method = cowboy_req:method(Req),
Msg#{ <<"method">> => Method, <<"path">> => MsgPath }.
```

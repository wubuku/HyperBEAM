# HyperBEAM Erlang 基础教程 - 专为 Java 开发者打造

## 前言

本教程专为对 Erlang 一无所知的 Java 开发者编写。通过对比 Java 语法，帮助您快速理解[docs.wao.eco/book/setup](https://docs.wao.eco/book/setup)中的 Erlang 代码。

## 目录

1. [Erlang 模块系统与依赖管理](#erlang-模块系统与依赖管理)
2. [Erlang 语法基础](#erlang-语法基础)
3. [数据类型与操作](#数据类型与操作)
4. [函数定义与调用](#函数定义与调用)
5. [HyperBEAM 代码示例解析](#hyperbeam-代码示例解析)

---

## Erlang 模块系统与依赖管理

### 1.1 模块声明与包含

#### 基本语法

```erlang
-module(my_module).                    % 模块声明，相当于Java的 class
-export([function1/2, function2/1]).   % 导出函数，相当于 public 方法
-include("include/my_header.hrl").     % 包含本地头文件
-include_lib("eunit/include/eunit.hrl"). % 包含库头文件
```

#### Java 对比
```java
// Java 风格
public class MyModule {
    public Object function1(Object arg1, Object arg2) { ... }
    public Object function2(Object arg1) { ... }
}
```

### 1.2 Erlang 的"包管理器" - rebar3

#### 什么是 rebar3？
- Erlang 的官方构建工具和包管理器
- 类似 Java 的 Maven/Gradle
- 管理依赖、编译、测试、发布

#### 依赖声明 (rebar.config)

```erlang
{deps, [
    % Hex.pm 包（全局缓存）
    {prometheus, "4.11.0"},              % 版本号格式
    {luerl, "1.3.0"},
    {b64fast, "0.2.2"},

    % Git 仓库依赖（项目本地）
    {cowboy, {git, "https://github.com/ninenines/cowboy",
              {ref, "022013b6c4e967957c7e0e7e7cdefa107fc48741"}}},
    {gun, {git, "https://github.com/ninenines/gun",
           {ref, "8efcedd3a089e6ab5317e4310fed424a4ee130f8"}}}
]}.
```

#### 依赖存储位置

**全局缓存** (`~/.cache/rebar3/hex/hexpm/packages/`):
- 存放从 Hex.pm 下载的包
- 如：`prometheus-4.11.0.tar`, `luerl-1.3.0.tar`
- 避免重复下载，提高效率

**项目本地** (`_build/default/lib/`):
- 存放编译后的依赖
- Git 依赖只存在于项目本地
- 每个项目使用特定版本

#### 如何判断依赖位置？

```bash
# 查看依赖树（推荐）
rebar3 tree

# 全局缓存中的 Hex 包
ls ~/.cache/rebar3/hex/hexpm/packages/

# 项目本地依赖
ls _build/default/lib/
```

---

## Erlang 语法基础

### 2.1 语句与表达式

#### 核心规则

**每个 Erlang 语句必须以 `.` 结束**
```erlang
X = 5.          % ✅ 正确
Y = X + 1.      % ✅ 正确
```

**Java 对比：**
```java
int x = 5;      // Java 用分号 ;
x = x + 1;      // Java 用分号 ;
```

**函数内的多个表达式：**
```erlang
function_name(Param) ->
    Expr1,       % 逗号分隔
    Expr2,       % 逗号分隔
    Result.      % 句点结束函数
```

### 2.2 变量与赋值

```erlang
% Erlang: 单次赋值（变量不可变）
X = 5.          % X 绑定到 5
Y = X + 1.      % Y 绑定到 6

% Java 对比
int x = 5;      // 可修改
x = x + 1;      // 修改 x 的值
```

**关键差异：**
- Erlang 变量**不可变**（Immutable）
- `=` 是**模式匹配**，不是赋值
- 变量名必须以**大写字母**开头

---

## 数据类型与操作

### 3.1 基本数据类型

```erlang
% 整数（任意精度）
X = 42.
Y = 16#FF.      % 255 (十六进制)
Z = 2#1010.     % 10 (二进制)

% 原子（类似枚举，常量）
Status = ok.
Error = error.
Name = 'my-atom'. % 有特殊字符时用引号

% Java 对比
int x = 42;
String status = "ok";  // Java 用字符串
enum Status { OK, ERROR } // Java 用枚举
```

### 3.2 字符串与二进制

#### 两种字符串表示

```erlang
% 传统字符串（字符列表）
String1 = "hello".        % [104, 101, 108, 108, 111]

% 二进制字符串（字节序列，更高效）
Binary1 = <<"hello">>.     % 真正的字节数据
Binary2 = <<72, 101, 108, 108, 111>>.  % 字节字面量
```

#### Java 对比
```java
// Java 字符串
String str = "hello";

// Java 字节数组
byte[] bytes = "hello".getBytes(StandardCharsets.UTF_8);
byte[] bytes2 = {72, 101, 108, 108, 111};
```

#### 二进制拼接（重点）

```erlang
Name = <<"Alice">>.
Greeting = <<"Hello, ", Name/binary, "!">>.
% 结果：<<"Hello, Alice!">>

% Java 对比
String name = "Alice";
String greeting = "Hello, " + name + "!";
```

**语法分解：**
- `<<` `>>` - 二进制字面量界定符
- `,` - 在二进制中是连接操作符
- `/binary` - 明确指定变量为二进制类型

### 3.3 Map（映射）- Java 的 HashMap

```erlang
% 创建 Map
User = #{
    <<"name">> => <<"Alice">>,
    <<"age">> => 25,
    <<"active">> => true
}.

% 访问 Map
Name = maps:get(<<"name">>, User).

% 更新 Map（返回新 Map，因为数据不可变）
User2 = User#{ <<"age">> => 26 }.
```

**Java 对比：**
```java
// 创建 HashMap
Map<String, Object> user = new HashMap<>();
user.put("name", "Alice");
user.put("age", 25);
user.put("active", true);

// 访问
String name = (String) user.get("name");

// 更新（Java 会修改原对象）
user.put("age", 26);
```

---

## 函数定义与调用

### 4.1 函数定义语法

```erlang
% 基本函数定义
function_name(Parameter1, Parameter2) ->
    Expression1,
    Expression2,
    ResultExpression.

% 多子句函数（模式匹配）
factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
```

**Java 对比：**
```java
public Object functionName(Object param1, Object param2) {
    // expression1
    // expression2
    return resultExpression;
}

public int factorial(int n) {
    if (n == 0) return 1;
    return n * factorial(n - 1);
}
```

### 4.2 函数调用语法

```erlang
% 模块函数调用
Result = module_name:function_name(Arg1, Arg2).

% 本模块函数调用
Result = function_name(Arg1, Arg2).
```

**Java 对比：**
```java
Object result = ModuleName.functionName(arg1, arg2);
Object result = this.functionName(arg1, arg2);
```

### 4.3 返回值模式

**Erlang 用元组返回结果：**
```erlang
{ok, Data}     % 成功
{error, Reason} % 失败
```

**Java 用异常或特殊返回值：**
```java
return data;              // 成功
throw new Exception(reason); // 失败
// 或
return new Result(true, data); // 自定义结果类
```

---

## HyperBEAM 代码示例解析

基于浏览器当前页面的 WizardAO Setup 教程，以下是完整代码的逐行解析。

### 5.1 测试文件 (`src/test/my_first_test.erl`)

```erlang
-module(my_first_test).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    ?assertEqual(4, 2 + 2).

hb_util_test() ->
    % Test base64url encoding
    Encoded = hb_util:encode(<<"hello">>),
    ?assertEqual(<<"hello">>, hb_util:decode(Encoded)).

message_test() ->
    % Create a simple message
    Msg = #{ <<"key">> => <<"value">> },
    ?assertEqual(<<"value">>, maps:get(<<"key">>, Msg)).

debug_test() ->
    Value = 42,
    ?debugFmt("Value is ~p~n", [Value]),
    ?assertEqual(42, Value).
```

**逐行解析：**

1. **`-module(my_first_test).`**
   - 模块声明
   - Java: `public class MyFirstTest`

2. **`-include_lib("eunit/include/eunit.hrl").`**
   - 包含 EUnit 测试框架头文件
   - Java: `import org.junit.*;`

3. **`basic_test() ->`**
   - 测试函数定义（函数名以 `_test` 结尾）
   - Java: `@Test public void basicTest()`

4. **`?assertEqual(4, 2 + 2).`**
   - EUnit 断言：检查两个值是否相等
   - Java: `assertEquals(4, 2 + 2)`

5. **`Encoded = hb_util:encode(<<"hello">>)`**
   - 调用 `hb_util` 模块的 `encode` 函数
   - `<<"hello">>` 是二进制字符串
   - Java: `String encoded = HbUtil.encode("hello".getBytes())`

6. **`maps:get(<<"key">>, Msg)`**
   - 从 Map 中获取值
   - Java: `msg.get("key")`

7. **`?debugFmt("Value is ~p~n", [Value])`**
   - 调试输出，`~p` 是格式化占位符
   - Java: `System.out.printf("Value is %s\n", value)`

### 5.2 设备模块 (`src/dev_hello.erl`)

```erlang
-module(dev_hello).
-export([info/3, greet/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

info(_Msg, _Msg2, _Opts) ->
    {ok, #{
        <<"name">> => <<"hello">>,
        <<"version">> => <<"1.0">>
    }}.

greet(Msg, _Msg2, Opts) ->
    Name = hb_ao:get(<<"name">>, Msg, Opts),
    Greeting = <<"Hello, ", Name/binary, "!">>,
    {ok, hb_ao:set(Msg, #{ <<"greeting">> => Greeting }, Opts)}.

%% Tests
info_test() ->
    {ok, Result} = info(#{}, #{}, #{}),
    ?assertEqual(<<"hello">>, maps:get(<<"name">>, Result)).

greet_test() ->
    Msg = #{ <<"name">> => <<"Alice">> },
    {ok, Result} = greet(Msg, #{}, #{}),
    ?assertEqual(<<"Hello, Alice!">>, maps:get(<<"greeting">>, Result)).
```

**逐行解析：**

1. **`-export([info/3, greet/3]).`**
   - 导出函数，`/3` 表示函数接受3个参数
   - Java: `public Object info(Object msg, Object msg2, Object opts)`

2. **`info(_Msg, _Msg2, _Opts) ->`**
   - 函数定义，下划线前缀表示参数未使用
   - 返回设备信息 Map

3. **`{ok, #{...}}`**
   - 返回元组：`{ok, Data}`
   - Java: `return new SuccessResponse(map)`

4. **`Name = hb_ao:get(<<"name">>, Msg, Opts)`**
   - 从消息 Map 中获取 name 字段
   - Java: `byte[] name = (byte[]) HbAo.get("name", msg, opts)`

5. **`Greeting = <<"Hello, ", Name/binary, "!">>`**
   - 二进制字符串拼接
   - `Name/binary` 确保 Name 被当作二进制处理
   - Java: `"Hello, ".getBytes() + name + "!".getBytes()`

6. **`hb_ao:set(Msg, #{ <<"greeting">> => Greeting }, Opts)`**
   - 调用 hb_ao:set 函数添加 greeting 字段
   - 返回新的 Map（数据不可变）
   - Java: `Map updatedMsg = HbAo.set(msg, Map.of("greeting", greeting), opts)`

### 5.3 配置文件 (`src/hb_opts.erl`)

```erlang
preloaded_devices => [
    % ... existing devices ...
    #{<<"name">> => <<"hello@1.0">>, <<"module">> => dev_hello}
],
```

**解析：**
- 设备配置列表
- 每个设备包含名称和对应的模块名
- 相当于 Java 的配置文件

### 5.4 运行测试

```bash
# 运行特定模块的测试
rebar3 eunit --module=my_first_test
rebar3 eunit --module=dev_hello

# 运行所有测试
rebar3 eunit
```

---

## 总结：Erlang vs Java 思维差异

### 数据不可变性
```erlang
% Erlang: 返回新数据
UpdatedMap = hb_ao:set(OldMap, #{<<"key">> => <<"value">>}).
```

```java
// Java: 修改原对象
oldMap.put("key", "value");
```

### 错误处理
```erlang
% Erlang: 返回值模式
{ok, Result} = some_function().
{error, Reason} = some_function().
```

```java
// Java: 异常模式
try {
    Object result = someFunction();
} catch (Exception e) {
    // handle error
}
```

### 并发模型
```erlang
% Erlang: 轻量级进程
Pid = spawn(fun() -> worker_function() end).
```

```java
// Java: 重量级线程
Thread thread = new Thread(() -> workerFunction());
thread.start();
```

## Erlang 宏（Macros）详解

### 6.1 什么是宏？

在 Erlang 中，以 `?` 开头的标识符表示**宏（Macro）**，这是一种编译时展开的代码片段。

**宏 vs 普通函数：**
```erlang
% 宏调用（编译时展开）
?assertEqual(A, B)

% 普通函数调用（运行时调用）
assertEqual(A, B)
```

**Java 对比：**
```java
// Java 注解（编译时处理）
@Test
public void myTest() { ... }

// 普通方法调用
assertEquals(a, b);
```

### 6.2 为什么测试断言用宏？

EUnit 的断言函数都用宏的原因：

1. **编译时代码生成**：宏在编译时展开，可以生成更多调试信息
2. **行号信息**：宏可以捕获调用处的文件名和行号
3. **条件编译**：可以根据编译选项启用/禁用断言
4. **性能优化**：在生产环境中可以完全移除断言代码

### 6.3 宏展开示例

```erlang
% 源代码
?assertEqual(4, 2 + 2)

% 编译时展开为
case (2 + 2) of
    4 -> ok;
    ActualValue ->
        erlang:error({assertEqual_failed,
                     [{module, ?MODULE},
                      {line, ?LINE},
                      {expression, "2 + 2"},
                      {expected, 4},
                      {value, ActualValue}]})
end
```

**包含的信息：**
- 模块名 (`?MODULE`)
- 行号 (`?LINE`)
- 原始表达式字符串
- 期望值和实际值

### 6.4 常用宏类型

```erlang
% EUnit 断言宏
?assert(Expression)           % 检查表达式为 true
?assertEqual(A, B)            % 检查 A == B
?assertNotEqual(A, B)         % 检查 A != B
?assertMatch(Pattern, Value)  % 检查值匹配模式
?assertException(Class, Term, Expr) % 检查异常

% 调试宏
?debugFmt(Fmt, Args)          % 调试输出
?debugVal(Var)                % 打印变量值

% 预定义宏
?MODULE                       % 当前模块名
?LINE                         % 当前行号
?FILE                         % 当前文件名
```

### 6.5 宏定义语法

```erlang
% 在 .hrl 头文件中定义宏
-define(assertEqual(A, B),
    case (B) of
        A -> ok;
        Actual ->
            erlang:error({assertEqual_failed, [
                {module, ?MODULE},
                {line, ?LINE},
                {expected, A},
                {actual, Actual}
            ]})
    end).

% 使用宏
?assertEqual(4, 2 + 2)
```

**Java 对比（最接近的概念）：**
```java
// Lombok 注解（编译时代码生成）
@Data
public class User {
    private String name;
    // 编译时自动生成 getter/setter
}

// 或预处理器宏（C语言风格）
#define ASSERT_EQUAL(a, b) \
    if ((a) != (b)) { \
        printf("Assertion failed: %s != %s at %s:%d\n", #a, #b, __FILE__, __LINE__); \
        exit(1); \
    }
```

### 6.6 宏的优缺点

**优点：**
- 编译时展开，无运行时开销
- 可以访问编译时信息（文件名、行号）
- 条件编译支持
- 代码生成能力

**缺点：**
- 调试困难（展开后的代码与源码不同）
- 错误信息可能不够直观
- 过度使用会使代码复杂

## Erlang 文件类型详解

### 7.1 .erl vs .hrl 文件的区别

#### .erl 文件（源代码文件）
```erlang
% my_module.erl
-module(my_module).
-export([hello/0]).

hello() ->
    "Hello, World!".
```

- **包含**：完整的模块实现、函数定义
- **编译**：直接编译为 .beam 文件
- **作用**：程序的主要逻辑代码

#### .hrl 文件（头文件）
```erlang
% my_header.hrl
% 宏定义
-define(TIMEOUT, 5000).
-define(DEBUG(Msg), io:format("DEBUG: ~p~n", [Msg])).

% 记录定义
-record(user, {
    id :: integer(),
    name :: binary(),
    email :: binary()
}).

% 类型定义
-type user_id() :: integer().
-type user() :: #user{}.
```

- **包含**：宏定义、记录定义、类型定义、常量
- **不编译**：被其他 .erl 文件包含
- **作用**：共享的声明和定义

### 7.2 为什么要使用头文件？

#### 1. 代码复用与模块化

```erlang
% user.erl
-module(user).
-include("user.hrl").  % 包含共享定义
-export([create/2]).

create(Name, Email) ->
    #user{id = generate_id(), name = Name, email = Email}.
```

```erlang
% user_db.erl
-module(user_db).
-include("user.hrl").  % 复用相同的定义
-export([save/1]).

save(User = #user{}) ->  % 使用相同的记录定义
    % 保存用户到数据库
    ok.
```

**Java 对比：**
```java
// Java 通过 package 和 import 实现类似功能
// 但需要每个文件重复定义或使用继承
public class User {
    private int id;
    private String name;
    private String email;
    // ... 重复的字段定义
}

public class UserDB {
    // 需要重复的 User 类定义或 import
}
```

#### 2. 避免重复定义

**不使用头文件的问题：**
```erlang
% user.erl
-record(user, {id, name, email}).

% user_db.erl
-record(user, {id, name, email}).  % 重复定义！

% user_api.erl
-record(user, {id, name, email}).  % 又重复了！
```

**使用头文件：**
```erlang
% user.hrl
-record(user, {id, name, email}).

% 所有模块都包含这个头文件
-include("user.hrl").
```

#### 3. 编译优化

- 头文件在编译时被展开（宏）或复制（记录）
- 避免运行时查找
- 更好的类型检查和优化

### 7.3 现代编程语言如何替代头文件？

#### Java 的解决方案
```java
// 1. 接口定义共享行为
public interface UserService {
    User create(String name, String email);
}

// 2. 抽象类定义共享实现
public abstract class BaseEntity {
    protected Long id;
    protected LocalDateTime createdAt;
}

// 3. Lombok 等工具生成样板代码
@Data
@Builder
public class User {
    private Long id;
    private String name;
    private String email;
}
```

#### Python 的解决方案
```python
# 1. 直接在类中定义
class User:
    def __init__(self, name, email):
        self.id = generate_id()
        self.name = name
        self.email = email

# 2. 类型注解
from typing import NamedTuple

class User(NamedTuple):
    id: int
    name: str
    email: str
```

#### TypeScript/JavaScript 的解决方案
```typescript
// 1. 接口定义类型
interface User {
    id: number;
    name: string;
    email: string;
}

// 2. 类实现接口
class User implements UserInterface {
    constructor(public name: string, public email: string) {
        this.id = generateId();
    }
}
```

### 7.4 Erlang 头文件的独特优势

#### 1. 宏系统（Macro System）
```erlang
% 条件编译
-ifdef(DEBUG).
-define(LOG(Msg), io:format("DEBUG: ~p~n", [Msg])).
-else.
-define(LOG(Msg), ok).
-endif.
```

#### 2. 记录系统（Record System）
```erlang
% 编译时生成高效的元组操作
-record(user, {id, name, email}).
% 编译为：{user, Id, Name, Email}

User = #user{id = 1, name = <<"Alice">>},
Id = User#user.id.  % 高效的字段访问
```

#### 3. 运行时反射
```erlang
% 记录信息在运行时可用
user_record_info() ->
    record_info(fields, user).  % 返回 [id, name, email]
```

### 7.5 HyperBEAM 中的实际应用

```erlang
% include/hb.hrl
-record(device, {
    id :: binary(),
    type :: atom(),
    config :: map(),
    state :: map()
}).

-define(DEVICE_TIMEOUT, 30000).
-define(DEBUG_DEVICE(Msg), ?debugFmt("Device: ~p", [Msg])).

% src/hb_device.erl
-module(hb_device).
-include("include/hb.hrl").
-export([create/2]).

create(Type, Config) ->
    #device{
        id = hb_util:generate_id(),
        type = Type,
        config = Config,
        state = #{}
    }.
```

### 7.6 总结：头文件的价值

**尽管现代语言减少了头文件的使用，但 Erlang 的头文件仍然有独特价值：**

1. **性能**：编译时展开，无运行时开销
2. **类型安全**：记录提供编译时类型检查
3. **宏威力**：强大的元编程能力
4. **代码组织**：清晰的接口分离
5. **向后兼容**：Erlang 从 1980s 就开始使用这种模式

**现代语言的"替代方案"：**
- 通过 IDE 和语言服务器提供智能提示
- 通过依赖注入和注解减少样板代码
- 通过模块系统提供更好的封装

但 Erlang 的头文件系统在性能和表达力方面仍然很有优势！

## Erlang 命名规则详解

### 8.1 大小写规则的硬性要求

#### 变量必须大写开头

```erlang
% ✅ 正确：变量以大写字母开头
Name = "Alice".
Age = 25.
UserList = [user1, user2, user3].

% ❌ 错误：小写开头的被认为是原子（常量）
name = "Alice".     % 这不是变量，而是原子 name
age = 25.          % 这不是变量，而是原子 age
```

**为什么？**
- Erlang 使用**单赋值变量**（single assignment）
- 大写字母开头表示"这是一个可以被赋值的变量"
- 小写字母开头表示"这是一个不变的原子常量"

#### 函数名、模块名必须小写开头

```erlang
% ✅ 正确
-module(my_module).           % 模块名小写
-export([hello_world/0]).     % 函数名小写
hello_world() -> "Hello".

% ❌ 错误
-module(My_Module).           % 大写模块名
Hello_World() -> "Hello".     % 大写函数名
```

### 8.2 全大写变量的含义（命名约定）

澄清：和首字母大写相比，语法上没有区别，只有命名约定！

```erlang
% 从语法角度看，这些都是等价的变量：
Result = compute().     % 驼峰命名
RESULT = compute().     % 全大写命名

result = compute().     % ❌ 错误：这是原子，不是变量！
```

**Erlang 编译器只关心首字母大小写，其他字母大小写只是命名约定。**

#### 全大写变量的常见用途

##### 1. 表示常量或重要值

```erlang
% 全大写表示"这个值很重要，不要轻易修改"
RESULT = compute_final_answer(),
TOTAL = sum_all_values(),
CONFIG = load_configuration().

% 普通变量（都必须大写开头）
TempValue = compute_temp_value(),  % 临时值
Index = 0.                         % 计数器
Counter = 1.                       % 另一个计数器
```

##### 2. 宏展开结果（约定俗成）

```erlang
% 宏定义
-define(TIMEOUT, 5000).
-define(PI, 3.14159).

% 使用宏时常用全大写（约定）
TIMEOUT = ?TIMEOUT,
PI = ?PI.

% 但也可以不用全大写（语法允许）
timeout = ?TIMEOUT,
pi = ?PI.
```

##### 3. 记录字段访问（个人偏好）

```erlang
% 记录定义
-record(user, {id, name, email}).

% 访问字段时，有人喜欢全大写（个人风格）
User = #user{id = 1, name = "Alice"},
ID = User#user.id,      % 全大写字段名
NAME = User#user.name.  % 全大写字段名

% 也有人用驼峰
Id = User#user.id,      % 驼峰字段名
Name = User#user.name.  % 驼峰字段名
```

#### 实际项目中的使用模式

```erlang
% HyperBEAM 风格（观察现有代码）
create_device(Config) ->
    % 重要结果用大写开头
    DeviceId = generate_id(),
    Timestamp = current_time(),

    % 配置和临时值也用大写开头（因为变量必须大写！）
    DeviceConfig = maps:get(config, Config, #{}),
    Retries = 0,

    % 构建结果
    % 创建记录（Record）- Erlang 的结构体
    Device = #device{
        id = DeviceId,           % 字段赋值
        config = DeviceConfig,   % 相当于 Java: device.setConfig(config)
        created_at = Timestamp   % 相当于 Java: device.setCreatedAt(timestamp)
    },

    {ok, Device}.
```

#### 关键理解

**全大写变量 ≠ 特殊语法**

- `Result` 和 `RESULT` 在语法上完全相同
- 全大写只是一种**命名约定**，表示：
  - "这个值很重要"
  - "这个是常量"
  - "这个来自宏展开"

**就像 Java 中的常量命名：**
```java
final int MAX_SIZE = 100;  // 全大写表示常量
String userName = "alice"; // 驼峰表示普通变量
```

**Erlang 没有语法强制，只有社区约定！**

### 8.3 完整的命名规则对照表

| 标识符类型 | 命名规则 | 示例 | 说明 |
|-----------|---------|------|------|
| **变量** | 大写开头 | `Name`, `UserList` | 可被赋值 |
| **函数名** | 小写开头 | `hello/0`, `get_user/1` | 可调用 |
| **模块名** | 小写开头 | `user_db`, `hb_util` | 文件名 |
| **原子** | 小写开头 | `ok`, `error`, `true` | 常量符号 |
| **宏** | 以 `?` 开头 | `?assertEqual`, `?MODULE` | 编译时展开 |
| **记录** | 以 `#` 开头 | `#user{}`, `#device{}` | 结构体 |

### 8.4 最佳实践

#### 1. 变量命名

```erlang
% ✅ 推荐：驼峰命名，描述性强
UserName = "alice".
UserAge = 25.
ConfigMap = #{key => value}.

% ❌ 不推荐：太简短或无意义
U = "alice".        % 不知道是什么
X = 25.            % 不知道代表什么
M = #{key => value}. % 不知道是什么Map
```

#### 2. 函数命名

```erlang
% ✅ 推荐：小写 + 下划线分隔
create_user/2
get_user_by_id/1
validate_email/1

% ❌ 不推荐：驼峰式（虽然语法允许）
createUser/2
getUserById/1
```

#### 3. 模块命名

```erlang
% ✅ 推荐：全小写，下划线分隔
user_db
hb_util
json_parser

% ❌ 不推荐：驼峰式
UserDB
HBUtil
```

#### 4. 原子使用

```erlang
% ✅ 推荐：小写，描述性
{ok, Data}
{error, Reason}
true, false

% ❌ 不推荐：大写原子（除非特殊含义）
{OK, Data}
{ERROR, Reason}
```

### 8.5 与其他语言的对比

#### Java 对比
```java
// Java: 大小写都有意义，但不强制
String userName;      // 驼峰
int USER_COUNT;       // 常量大写
public void getUser() // 方法小写开头
private int userId;   // 字段驼峰
```

```erlang
% Erlang: 严格的大小写规则
UserName.        % 变量：大写开头
user_name().     % 函数：小写开头
ok.             % 原子：小写开头
```

#### Python 对比
```python
# Python: 变量可以任意命名
user_name = "alice"    # 变量
def get_user():       # 函数
    return user_name

USER_COUNT = 100      # 常量（约定）
```

```erlang
% Erlang: 强制规则
UserName = "alice".   % 变量必须大写
get_user() -> ok.     % 函数必须小写
```

### 8.6 HyperBEAM 中的实际应用

```erlang
% src/hb_device.erl
-module(hb_device).
-export([create/2, get_info/1]).

create(DeviceType, Config) ->
    % 变量大写开头
    DeviceId = hb_util:generate_id(),
    Timestamp = hb_util:timestamp(),

    % 记录使用
    Device = #device{
        id = DeviceId,
        type = DeviceType,      % 小写原子
        config = Config,
        created_at = Timestamp
    },

    {ok, Device}.

get_info(Device) ->
    % 模式匹配变量
    #device{id = Id, type = Type} = Device,

    % 返回结果
    #{id => Id, type => Type}.
```

### 8.7 常见错误与调试

```erlang
% ❌ 错误：试图重新赋值变量
X = 5,
X = 6.        % 错误！变量不能重新赋值

% ✅ 正确：使用不同变量名
X = 5,
Y = X + 1.

% ❌ 错误：变量名小写
result = compute().  % 这不是变量，而是原子！

% ✅ 正确
Result = compute().

% ❌ 错误：函数名大写
CreateUser() -> ok.  % 语法错误！

% ✅ 正确
create_user() -> ok.
```

### 8.8 总结

**Erlang 的命名规则：**
- **变量**：大写开头（`Name`, `UserList`）
- **函数/模块**：小写开头（`create_user()`, `user_db`）
- **原子**：小写开头（`ok`, `error`）
- **全大写**：通常表示常量、宏结果或记录字段

**设计哲学：**
1. **明确区分**：一眼就能看出标识符的类型
2. **编译时检查**：变量使用前必须先绑定
3. **函数式思维**：变量不可变，函数是第一公民

理解这些规则后，你就能避免常见的 Erlang 新手错误，并在 HyperBEAM 代码中正确使用命名约定！

## Erlang 记录（Records）详解

### 9.1 什么是记录？

**记录是 Erlang 的结构体，类似于 Java 的类或 C 的 struct。**

#### 定义记录（在 .hrl 头文件中）

```erlang
% 定义记录结构
-record(device, {
    id,           % 字段名（可选默认值）
    type,         % 另一个字段
    config = #{}, % 字段有默认值
    created_at    % 最后一个字段
}).
```

**Java 对比：**
```java
public class Device {
    private String id;
    private String type;
    private Map<String, Object> config = new HashMap<>();
    private long createdAt;

    // 构造函数、getter、setter...
}
```

#### 使用记录

```erlang
% 创建记录实例
Device1 = #device{
    id = "dev-123",
    type = "sensor",
    config = #{temp_threshold => 25}
}.

% 使用默认值
Device2 = #device{
    id = "dev-456",
    type = "actuator"
}.  % config 会使用默认值 #{}

% 访问字段
DeviceId = Device1#device.id,        % 相当于 Java: device.getId()
DeviceType = Device1#device.type,    % 相当于 Java: device.getType()

% 更新记录（返回新记录，因为数据不可变）
UpdatedDevice = Device1#device{
    config = #{temp_threshold => 30}  % 只更新 config 字段
}.
```

### 9.2 记录 vs Map

| 特性 | 记录 (Record) | Map |
|------|---------------|-----|
| **定义** | 需要预定义字段 | 动态字段 |
| **性能** | 更快（编译时优化） | 稍慢 |
| **类型检查** | 编译时检查 | 运行时检查 |
| **语法** | `#record{field = value}` | `#{key => value}` |

```erlang
% 记录（结构化数据）
-record(user, {id, name, email}).

% Map（灵活数据）
UserMap = #{id => 1, name => "Alice", extra_field => "anything"}.
```

### 9.3 HyperBEAM 中的记录使用

```erlang
% 在 include/hb.hrl 中定义
-record(device, {
    id :: binary(),           % 类型注解
    type :: atom(),
    config :: map(),
    state :: map(),
    created_at :: integer()
}).

% 在代码中使用
create_device(Type, Config) ->
    Device = #device{
        id = hb_util:generate_id(),
        type = Type,
        config = Config,
        state = #{status => inactive},
        created_at = hb_util:timestamp()
    },
    {ok, Device}.

% 更新设备状态
activate_device(Device) ->
    Device#device{state = #{status => active}}.
```

### 9.4 记录的优缺点

**优点：**
- ✅ 编译时类型检查
- ✅ 性能优化（字段访问更快）
- ✅ IDE 支持（自动补全）
- ✅ 结构清晰

**缺点：**
- ❌ 需要预定义字段
- ❌ 修改记录定义需要重新编译
- ❌ 不如 Map 灵活

### 9.5 记录模式匹配

```erlang
% 函数参数模式匹配
handle_device(#device{type = sensor, state = #{status := active}} = Device) ->
    % 只处理活跃的传感器
    process_sensor_data(Device);

handle_device(#device{type = actuator} = Device) ->
    % 处理执行器
    control_actuator(Device).
```

**Java 对比（模拟模式匹配）：**
```java
public void handleDevice(Device device) {
    if ("sensor".equals(device.getType()) &&
        "active".equals(device.getState().get("status"))) {
        processSensorData(device);
    } else if ("actuator".equals(device.getType())) {
        controlActuator(device);
    }
}
```

### 9.5 记录更新语法详解

#### 记录更新：`State#state{}`

```erlang
% 语法：ExistingRecord#record_type{field1 = new_value, field2 = new_value}
UpdatedState = State#state{
    processes = [PID | State#state.processes],
    activity = [Act | State#state.activity]
}.
```

**Java 对比：**
```java
// Java 中修改对象属性
state.setProcesses(newProcesses);
state.setActivity(newActivity);

// Erlang 中创建新记录（数据不可变）
NewState = State#state{
    processes = NewProcesses,
    activity = NewActivity
}.
```

#### 工作原理

1. **取出现有记录**：`State` 是当前的 state 记录
2. **指定记录类型**：`#state{}` 表示创建新的 state 记录
3. **只更新指定字段**：大括号内只列出要修改的字段
4. **其他字段保持不变**：未提到的字段自动从原记录复制

#### HyperBEAM 中的实际例子

```erlang
% state 记录定义
-record(state, {
    client = undefined,     % 客户端进程
    activity = [],          % 活动日志列表
    processes = waiting,    % 进程列表
    console = true          % 是否输出到控制台
}).

% 记录更新示例
loop(State) ->
    receive
        {register, PID} ->
            % 创建新记录，只更新 processes 和 activity 字段
            NewState = State#state{
                processes = [PID | case State#state.processes of
                                       waiting -> [];
                                       L -> L
                                    end],
                activity = [Act | State#state.activity]
            },
            loop(NewState);
        % ... 其他消息处理
    end.
```

**分解解释：**
```erlang
State#state{
    processes = [PID | case State#state.processes of waiting -> []; L -> L end],
    activity = [Act | State#state.activity]
}
```

1. **`State#state`**：基于现有 State 创建新 state 记录
2. **`processes = [...]`**：更新 processes 字段
3. **`activity = [...]`**：更新 activity 字段
4. **其他字段**（client, console）保持原值不变

#### 为什么需要这种语法？

**因为 Erlang 数据不可变：**
- 不能直接修改现有记录
- 必须创建新的记录实例
- 记录更新语法让这变得简洁高效

**性能优化：**
- 编译器会优化字段复制
- 只复制修改的字段数据
- 共享不变的字段数据

通过本教程，您应该能够理解 HyperBEAM 项目中的基本 Erlang 代码。如果需要更深入的某个主题，请参考相关的 HyperBEAM 文档。

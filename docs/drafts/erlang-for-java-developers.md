# Erlang 教程 - 专为 Java 开发者打造

> 听说过 Erlang 就像 Java 的"分布式版"？来吧，让我们用 Java 的思维来学习 Erlang！

## 🎯 前言：Erlang 对 Java 开发者来说是什么？

想象一下：**Java 是面向对象的巨人，Erlang 是并发的小精灵**。

| Java                           | Erlang                          | 说明          |
| ------------------------------ | ------------------------------- | ------------- |
| `public class User {}`         | `-module(user).`                | 类 → 模块     |
| `public void sendMessage() {}` | `send_message() -> ...`         | 方法 → 函数   |
| `throw new Exception()`        | `{error, reason}`               | 异常 → 返回值 |
| `Thread thread = new Thread()` | `Pid = spawn(fun my_module:my_task/0)` | 线程 → 进程   |
| `List<User> users`             | `[User1, User2, User3]`         | 集合 → 列表   |

**Erlang 的优势：**
- 🚀 **轻松支持百万级并发连接**（远超 Java 的高并发能力）
- 🛡️ **可实现高达 99.9999999% 的系统可用性**（源于其在电信领域的传奇表现）
- 🌐 **天生支持分布式**

**这个教程的目标：** 让你能看懂 [HyperBEAM Erlang 教程](https://hbdocs.vercel.app/hyperbeam/erlang) 中的所有代码！

## 📚 完整学习路径（涵盖 HyperBEAM 教程的所有基础知识）

- Day 0: Erlang 语法基础 - 别急，先学语法！
- Day 1: 基础数据类型 - 数字、原子、字符串、变量
- Day 2: 数据结构 - 列表、元组、映射、记录
- Day 3: 函数基础 - 函数、守卫、模式匹配
- Day 4: 控制流 - 分支、递归、高阶函数
- Day 5: 并发编程 - 进程、消息传递、OTP
- Day 6: 标准库 - 错误处理、文件、二进制、加密等
- Day 7: 高级特性 - ETS、定时器、端口、引用等
- Day 8: 模块系统与 NIFs

---

## Day 0: Erlang 语法基础 - 从零开始！

> **重要提醒**：别急着学数据类型，先学语法规则！这就像学英语先学字母和标点符号一样。

### 0.1 语句结束符：句点（Period）

```erlang
% ❌ Java 风格（错误！）
int x = 5;
x = x + 1;

% ✅ Erlang 风格（正确）
X = 5.          % 语句必须以句点结束！
Y = X + 1.      % 每个语句都要有句点！
```

**规则**：**每个 Erlang 语句都必须以句点 `.` 结束**，就像英语句子以句点结束一样。

**澄清**：在函数体内，多个表达式用逗号分隔，最后一个表达式后跟句点结束整个函数子句：

```erlang
% 函数内的多个表达式用逗号分隔，最后一个表达式后跟句点
add(A, B) ->
    Temp = A + 10,     % 逗号分隔
    Temp + B.          % 句点结束函数子句
```

### 0.2 参数分隔符：逗号（Comma）

```erlang
% 函数调用
add(A, B).      % 参数用逗号分隔

% 变量绑定
X = 1, Y = 2, Z = 3.  % 可以一行写多个，用逗号分隔

% 函数参数
calculate(A, B, C).    % 参数用逗号分隔
```

**规则**：**参数和多个表达式用逗号 `,` 分隔**。

### 0.3 函数定义语法

```erlang
% 函数定义 - 多行写法（推荐）
function_name(Parameter1, Parameter2) ->
    Expression1,
    Expression2,
    ResultExpression.  % 最后一行是返回值

% 函数定义 - 一行写法（也可以）
function_name(Parameter1, Parameter2) -> Expression1, Expression2, ResultExpression.
```

**🎯 核心概念：Erlang 中一切都是表达式！**
- **没有语句，只有表达式**
- **每个表达式都有返回值**
- **函数的返回值是最后一个表达式的值**
- **不需要 `return` 关键字**

```erlang
% Java 风格（语句）
public int add(int a, int b) {
    int temp = a + 10;  // 语句
    return temp + b;    // 显式返回
}

% Erlang 风格（表达式）
add(A, B) ->
    Temp = A + 10,      % 表达式，返回 A+10
    Temp + B.           % 表达式，返回最终结果
```

**规则**：
- 函数名后跟括号和参数
- 用 `->` 分隔参数和函数体
- 函数体可以有多行表达式，用逗号分隔
- **也可以写成一行**，效果完全一样
- **最后一个表达式自动作为返回值**（无论是多行还是单行，最后计算的表达式作为返回值）
- **一行写法中**：`Expr1, Expr2, ResultExpr.` → 返回 `ResultExpr`
- **不需要 return 语句**
- 函数定义以句点结束

**返回值规则**：
```erlang
% 多行写法：最后一行是返回值
add(A, B) ->
    Temp = A + B,    % 这行执行但不返回值
    Temp.            % 这一行是返回值

% 一行写法：最后一个表达式是返回值
add(A, B) -> Temp = A + B, Temp.  % 返回 Temp
add(A, B) -> A + B.               % 返回 A + B
```

### 0.4 代码块和作用域

```erlang
% ✅ 情况1：case 后面还有语句，必须用逗号
Result = case X of
    1 -> "one";
    2 -> "two";
    _ -> "other"
end,  % 用逗号连接，后续还有语句
io:format("Result: ~p~n", [Result]).

% ✅ 情况2：case 是函数最后部分，可以用句点
func_returning_case(X) ->
    case X of
        1 -> "one";
        2 -> "two";
        _ -> "other"
    end.  % 用句点结束，因为 case 结果就是返回值

% ✅ 情况3：if 语句结尾，用句点
Value = if
    X > 10 -> "big";
    X < 0 -> "negative";
    true -> "normal"  % 必须有 true 子句
end.  % 用句点结束

% ❌ 语法错误：Erlang 函数中不能用句点分隔语句！
% bad_example(X) ->
%     Result = case X of
%         1 -> "one";
%         2 -> "two";
%         _ -> "other"
%     end.  % ❌ 用句点结束，这里就成了语法错误！
%     io:format("Result: ~p~n", [Result]).  % ❌ 这行无法编译！
%
% % 错误原因：函数定义中只能有一个语句序列，不能有多个独立的语句

% 函数体
my_function(A, B) ->
    Temp = A + B,     % 中间变量
    Temp * 2.         % 返回值
```

#### **✅ 正确理解：变量范围**
```erlang
% 在同一个函数中，变量可以在后续语句中访问
my_function(X) ->
    A = X + 1,        % 绑定变量 A
    B = A * 2,        % 可以使用变量 A
    C = B + 5,        % 可以使用变量 B
    C.               % 返回 C（最后一行）
```

#### **❌ 语法错误示例**
```erlang
% 这个函数无法编译，会报语法错误
bad_function(X) ->
    A = X + 1.        % ❌ 用句点结束第一个语句
    B = A * 2.        % ❌ 这行无法编译：变量 A 未定义
```

**编译错误信息**：
```
bad_function.erl:4: syntax error before: 'B'
```

**错误原因**：在 Erlang 函数定义中，句点 `.` 结束了整个函数定义，后面的代码被当作新的函数定义，但没有函数头。

#### **🎯 核心区别：**
- **逗号 `,`**：连接**同一个语句序列**中的多个语句
- **句点 `.`**：结束**整个逻辑单元**（如函数定义）

**变量范围**：在同一个函数（逻辑单元）内，所有用逗号连接的语句可以共享变量。但你不能用句点把语句分开，因为函数是一个不可分割的逻辑单元。

---

**`if` 语句为什么必须有 `true` 子句？**

Erlang 的 `if` 不是条件分支，而是**守卫序列**。它是**表达式**，必须总是返回一个值：

- 每个条件都是**守卫表达式**，按顺序求值
- 如果没有 `true -> ` 子句，当所有条件都不匹配时，程序会崩溃
- `true` 总是匹配，作为"兜底"默认情况
- 这体现了 Erlang 的**函数式编程**特性：表达式必须总是有值

```erlang
% ❌ 缺少 true 子句，会导致运行时错误
BadIf = if
    X > 10 -> "big";
    X < 0 -> "negative"
end.  % 如果 X 在 0-10 之间，会抛出异常！
```

> **📚 Erlang 逻辑单元总结**
>
> Erlang 中的句点 `.` 用于结束以下**逻辑单元**：
>
> - **模块级声明**：`-module(my_module).`, `-export([...]).`, `-record(...)`
> - **函数定义**：`func(Args) -> Body.` 或多子句函数
> - **类型规范**：`-spec func(...) -> ...`
> - **宏定义**：`-define(NAME, value).`
> - **条件编译**：`-ifdef(...)`, `-endif.`
> - **语句序列**：函数体中最后一个语句用句点结束

### 0.5 注释语法

```erlang
% 单行注释：以百分号开头，到行尾结束
X = 5. % 行内注释

% Erlang 没有真正的多行注释语法！
% 以下每一行都是独立的单行注释：
%%
%% 虽然看起来像多行注释，但每一行都是独立的单行注释
%% 只是为了视觉上看起来像多行注释而已
%%

% 实际效果和下面一样：
% 多行注释：第一行
% 多行注释：第二行
% 多行注释：第三行
```

### 0.6 模块结构

```erlang
% 文件名必须是：my_module.erl
-module(my_module).                    % 模块声明

-export([public_function/1]).          % 导出的函数

% 记录定义
-record(user, {name, age = 0}).

% 函数定义
public_function(X) ->
    private_function(X).

private_function(X) ->
    X + 1.
```

### 0.7 常见语法错误

```erlang
% ❌ 缺少句点
X = 5  % 错误！语句必须以句点结束

% ❌ 多余的句点
case X of
    1 -> "one".
    _ -> "other"  % 错误！case 内部不能有句点
end.

% ❌ 错误的逗号使用
add(A; B).  % 错误！应该用逗号
```

### 0.8 Java vs Erlang 语法对比

| Java 语法 | Erlang 语法 | 说明 |
|-----------|-------------|------|
| `int x = 5;` | `X = 5.` | 变量赋值 |
| `add(a, b);` | `add(A, B).` | 函数调用 |
| `return x;` | `X.` (最后一行) | 返回值 |
| `{ stmt1; stmt2; }` | `stmt1, stmt2.` | 多条语句 |
| `// comment` | `% comment` | 注释 |

**现在你知道语法规则了，可以开始学数据类型了！** 🎯

## Day 1: 基础数据类型（Primitives）

### 1.1 数字 (Numbers) - Java 的 int/long/float/double

```java
// Java 的数字类型
int age = 25;
long bigNum = 1234567890123L;
float pi = 3.14159f;
double precise = 3.141592653589793;
```

```erlang
% Erlang 的数字（任意精度！）
Age = 25,                    % 整数
BigNum = 1234567890123,       % 大整数（自动处理，不会溢出）
Pi = 3.14159,               % 浮点数

% 算术运算
3 + 4,        % 7
10 div 3,     % 3 (整数除法)
10 rem 3,     % 1 (余数)
5 / 2,        % 2.5 (浮点除法)

% 比较操作符（注意语法差异！）
5 < 10,       % true
5 =< 5,       % true (注意：=< 而不是 <=)
5 >= 10,      % false
5 == 5.0,     % true (类型转换)
5 =:= 5.0,    % false (精确匹配)
5 =/= 6,      % true (不等于)

% 进制表示
255,          % 十进制
16#FF,        % 十六进制 (255)
2#1010,       % 二进制 (10)
1_000_000.    % 可读性分隔符
```

**与 Java 的区别：**
- Erlang 整数**任意精度**，不会溢出（Java 的 long 也有上限）
- 除法 `/` 总是返回浮点数（Java 的 `/` 对整数是整数除法）
- 使用 `div` 和 `rem` 进行整数运算
- 浮点数通常采用 IEEE 754 双精度标准，与 Java 的 double 类似

### 1.2 原子 (Atoms) - Java 的 enum/常量

```java
// Java 的枚举和常量
enum Status { OK, ERROR, PENDING }
public static final String STATUS_OK = "ok";
```

```erlang
% Erlang 的原子（轻量级常量）
% 原子不需要预定义，直接使用！
ok,           % 原子 - 直接写小写名称
error,        % 原子
pending,      % 原子
my_custom_atom. % 自定义原子

% 原子作为布尔值（true/false 其实是原子！）
is_atom(true),    % true - true 是预定义原子
is_atom(false),   % true - false 是预定义原子
is_atom(ok).      % true（所有原子！）

% 原子命名规则：
% 1. 以小写字母开头
% 2. 可以包含字母、数字、下划线
% 3. 不需要预先声明，直接使用

% 原子在模式匹配中的使用
Status = ok,
case Status of
    ok -> "操作成功";
    error -> "操作失败";
    pending -> "等待中";
    _ -> "未知状态"
end.
```

**原子特性：**
- **常量**：运行时创建，但比较是**指针比较**（非常快）
- **轻量级**：常用于状态、标签、消息类型
- **内存效率高**：相同的原子在内存中只存储一次

**⚠️ 布尔操作符的陷阱：**
Erlang 有两种布尔操作符，行为完全不同！

```erlang
% ❌ and/or - 总是计算两个操作数（危险！）
true and (1/0 == 0),     % 崩溃！即使左边是 true，也会计算 1/0
false or (io:format("不会执行")), % 仍然会执行 io:format

% ✅ andalso/orelse - 短路求值（推荐使用）
true andalso (1/0 == 0),  % false - 不会计算 1/0
false orelse (io:format("不会执行")), % false - 不会执行 io:format

% 记住：总是使用 andalso/orelse！
```

**⚠️ 重要警告：** 原子不会被垃圾回收。如果从外部输入（如用户请求的参数）动态创建原子，可能会导致原子表耗尽，使整个系统崩溃。**绝不要将不受信任的、动态的数据转换为原子**。

### 1.3 二进制与字符串 (Binaries & Strings)

```java
// Java 的字符串和字节数组
String name = "Alice";
byte[] data = "Hello".getBytes();
```

```erlang
% Erlang 的二进制（高效的字节序列）
Name = <<"Alice">>,           % UTF-8 二进制字符串
Data = <<"Hello">>,           % 字节序列
Empty = <<>>,                % 空二进制

% 操作
byte_size(<<"Hello">>),       % 5（字节数）
<<"Hello", " ", "World">>,    % 连接：<<"Hello World">>

% 模式匹配（超强大！）
<<H:8, Rest/binary>> = <<"Hello">>,
% H = 72 (ASCII 'H'), Rest = <<"ello">>

<<R:8, G:8, B:8>> = <<255, 0, 128>>,
% R=255, G=0, B=128 (解析RGB颜色)

% UTF-8 字符解析
<<C/utf8, Rest/binary>> = <<"你好世界">>,
% C = 20320 (码点'你'), Rest = <<"好世界">>
```

**二进制 vs 传统字符串：**

Erlang 有两种表示文本的方式：

**现代推荐：二进制（Binary）**
```erlang
Name = <<"Alice">>  % UTF-8 编码，高效，节省内存
```
- `<<"text">>` → 二进制（字节序列）
- 支持高效的位操作和模式匹配
- **强烈推荐**用于现代 Erlang 代码

**历史遗留：字符列表（String）**
```erlang
Name = "Alice"      % 实际上是 [65, 108, 105, 99, 101]
```
- `"text"` → 字符的整数列表
- 内存占用大，操作效率低
- 主要用于兼容老代码

**为什么推荐二进制？**
- **性能**：二进制操作更快
- **内存**：更节省内存
- **现代**：符合 Erlang 最佳实践

> **Pro Tip: 处理 IO 列表**
>
> 某些 I/O 操作，如 `io_lib:format/2`，返回一个复杂的字符和二进制列表，称为"io列表"。为了高效处理和存储，应将其转换为单个二进制：
>
> ```erlang
> DeepList = io_lib:format("Request from ~s", ["127.0.0.1"]),
> FlatBinary = iolist_to_binary(DeepList).
> ```

### 1.4 变量与模式匹配 (Variables & Pattern Matching)

```java
// Java 的变量（可变）
int x = 5;
x = 10;        // 可以重新赋值
```

```erlang
% Erlang 的变量（单次赋值）
X = 5,         % 绑定到 5
% X = 10,     % 错误！不能重新绑定

Y = 5,         % Y 永远是 5
Z = X + Y.     % Z = 10

% 模式匹配（= 是匹配，不是赋值）
{A, B} = {10, 20},  % A=10, B=20
{ok, Result} = {ok, "success"},  % Result="success"
[H|T] = [1,2,3,4],  % H=1, T=[2,3,4]

% 函数参数中的模式匹配
handle_response({ok, Data}) ->
    "成功: " ++ Data;
handle_response({error, Reason}) ->
    "失败: " ++ Reason.

% 模式匹配失败的例子（强化理解）
X = 5,
Y = 6,
X = Y.  % ❌ badmatch 错误！X(5) 不等于 Y(6)
```

**重要规则：**
- 变量**只能绑定一次**（像 Java 的 final）
- `=` 是**模式匹配**，不是赋值
- 鼓励**不可变编程**

## Day 2: 数据结构（Data Structures）

### 2.1 列表 (Lists) - Java 的 ArrayList/LinkedList

```java
// Java 的 ArrayList
List<String> names = Arrays.asList("Alice", "Bob", "Charlie");
names.get(0);      // "Alice"
names.size();      // 3
```

```erlang
% Erlang 的列表（链表实现）
Names = ["Alice", "Bob", "Charlie"],
[H|T] = Names,     % H="Alice", T=["Bob","Charlie"]
length(Names),     % 3
hd(Names),         % "Alice" (头元素)
tl(Names),         % ["Bob","Charlie"] (尾列表)

% 操作
[0|Names],         % [0,"Alice","Bob","Charlie"] (头部添加 O(1))
Names ++ ["Dave"], % ["Alice","Bob","Charlie","Dave"] (连接 O(n))
lists:nth(2, Names), % "Bob" (1索引！)
lists:reverse(Names). % ["Charlie","Bob","Alice"]
```

**列表特性：**
- **链表结构**：头部操作 O(1)，尾部操作 O(n)
- 支持**模式匹配**：`[H|T]` 语法
- 可以包含**任意类型**的元素

### 2.2 元组 (Tuples) - Java 的固定数组

```java
// Java 的固定数组
String[] person = {"Alice", "25", "Engineer"};
```

```erlang
% Erlang 的元组（类型安全，固定大小）
Person = {"Alice", 25, engineer},     % 注意：原子不用引号
{Name, Age, Job} = Person,            % 解构赋值
element(2, Person),                   % 25 (1索引！)
tuple_size(Person),                   % 3
setelement(3, Person, manager).       % {"Alice",25,manager}
```

**元组 vs 列表：**
- 元组：**固定大小**，随机访问快，类型安全
- 列表：**可变长度**，头部操作快
- 元组常用于**复合数据**（如函数返回值）

### 2.3 映射 (Maps) - Java 的 HashMap

```java
// Java 的 HashMap
Map<String, Object> user = new HashMap<>();
user.put("name", "Alice");
user.put("age", 25);
```

```erlang
% Erlang 的映射（现代，高效）
% 映射的键可以是二进制，也可以是原子，如 #{name => <<"Alice">>, ...}
User = #{
    <<"name">> => <<"Alice">>,
    <<"age">> => 25,
    <<"active">> => true
},

% 访问
maps:get(<<"name">>, User),              % <<"Alice">>
maps:get(<<"email">>, User, <<"N/A">>),  % 默认值

% 更新
User#{<<"email">> => <<"alice@example.com">>},  % 添加/更新
User#{<<"age">> := 26},                        % 更新（必须存在）
maps:remove(<<"active">>, User),               % 删除

% 操作
maps:size(User),         % 大小
maps:keys(User),         % [<<"name">>, <<"age">>, <<"active">>]
maps:values(User).       % [<<"Alice">>, 25, true]
```

### 2.4 记录 (Records) - Java 的类/struct

```java
// Java 的类
public class User {
    private String name;
    private int age;
    private boolean active = true;
    // getters/setters...
}
```

**与 Java 的区别**：Erlang 记录更像一个公共的、不可变的结构体。由于数据不可变，没有传统意义上的"setter"方法，访问字段是直接的。

```erlang
% Erlang 的记录（编译时类型）
-record(user, {
    name,
    age = 0,
    active = true
}).

% 使用
User = #user{name = <<"Alice">>, age = 25},
User#user.name,      % <<"Alice">>
User#user.age,       % 25
User#user.active,    % true

% 更新
User2 = User#user{age = 26, active = false}. % 创建一个新的记录副本，User 保持不变
```

**记录特性：**
- **编译时检查**：字段名在编译时验证
- **类型安全**：比 Map 更安全
- **模式匹配**：`#user{name = Name}`

#### Maps vs Records：如何选择？

| 特性 | 映射 (Maps) | 记录 (Records) |
| :--- | :--- | :--- |
| 字段检查 | 运行时 (键可能不存在) | 编译时 (字段名错误会被捕获) |
| 键 | 动态，任意类型 | 预定义的原子 |
| 定义 | 无需预定义 | 必须用 `-record(...)` 预定义 |
| 适用场景 | 处理动态或非结构化数据，如 JSON payload | 定义结构化的内部状态，API 数据模型 |
| 结论 | 灵活，用于外部数据 | 安全，用于内部数据 |

## Day 3: 函数基础（Functions）

### 3.1 函数定义与调用

```java
// Java 的方法
public int add(int a, int b) {
    return a + b;
}
```

```erlang
% Erlang 的函数（多子句）
add(A, B) ->
    A + B.

% 多子句函数（模式匹配）
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

% 匿名函数（Java 的 lambda）
Double = fun(X) -> X * 2 end,
Double(5).          % 10

% 高阶函数
lists:map(fun(X) -> X * 2 end, [1,2,3]). % [2,4,6]
```

### 3.2 守卫 (Guards) - Java 的条件判断

**什么是守卫表达式？**
- 守卫（Guard）是 Erlang 中特殊的**条件表达式**
- 只能使用**纯函数**和**内置守卫函数**
- **不能有副作用**（不能修改变量、调用普通函数）
- `when` 关键字用来**引入守卫表达式**

```java
// Java 的条件判断
public boolean isAdult(int age) {
    return age >= 18 && age <= 120;
}
```

```erlang
% Erlang 的守卫表达式（函数头中的额外条件）
is_adult(Age) when Age >= 18, Age =< 120 -> true;
is_adult(_) -> false.

% 守卫表达式的例子
is_valid(Age) when is_integer(Age), Age > 0 -> true;
is_valid(_) -> false.

% 内置守卫函数（只能在守卫中使用）
is_number(X), is_integer(X), is_float(X),
is_atom(X), is_binary(X), is_list(X), is_tuple(X).

% 守卫中不能调用自定义函数
is_special_number(N) -> N =:= 42.

% ❌ 下面的代码无法编译！
% my_func(X) when is_special_number(X) -> true;
% my_func(_) -> false.

% 守卫 vs 普通条件
check_age(Age) ->
    if Age >= 18 -> adult;    % 普通条件（可以有副作用）
       true -> minor
    end.

check_age_guard(Age) when Age >= 18 -> adult;  % 守卫（纯函数）
check_age_guard(_) -> minor.
```

**守卫可以检查：**
- 类型：`is_integer(X)`, `is_atom(X)`
- 值：`X > 0`, `X =:= 42`
- 多个条件：用逗号分隔（AND）

### 3.3 函数中的模式匹配

```erlang
% 参数中的模式匹配 (使用高效的二进制构建)
handle_result({ok, Data}) when is_binary(Data) ->
    <<"成功: ", Data/binary>>;
handle_result({error, Reason}) when is_binary(Reason) ->
    <<"失败: ", Reason/binary>>.

% 列表处理
sum([]) -> 0;
sum([H|T]) -> H + sum(T).

% 二进制模式匹配
parse_header(<<Length:16, Type:8, Rest/binary>>) ->
    {Length, Type, Rest}.

% 记录模式匹配
handle_user(#user{name = Name, age = Age}) when Age >= 18 andalso is_binary(Name) ->
    <<"成人用户: ", Name/binary>>;
handle_user(#user{name = Name}) when is_binary(Name) ->
    <<"未成年用户: ", Name/binary>>.
```

## Day 4: 控制流（Control Flow）

### 4.1 case 表达式

```java
// Java 的 switch
switch (status) {
    case "ok": return "成功";
    case "error": return "失败";
    default: return "未知";
}
```

```erlang
% Erlang 的 case（更强大）
Result = case Status of
    ok -> "成功";
    error -> "失败";
    pending -> "等待中";
    _ -> "未知"  % 通配符
end,
Result.
```

### 4.2 if 表达式（不常用）

```erlang
% Erlang 的 if（带守卫）
Result = if
    X > 10 -> "大";
    X < 0 -> "负";
    X == 0 -> "零";
    true -> "正"  % 必须有 true 子句
end.

% 为什么 if 不常用？
% 因为 if 是 case 的语法糖，只能使用守卫表达式
% case 可以匹配任意模式，功能更强大

% 等价的 case 写法（推荐）
Result2 = case X of
    X when X > 10 -> "大";
    X when X < 0 -> "负";
    0 -> "零";
    _ -> "正"
end.
```

### 4.3 递归 - Erlang 的循环

```java
// Java 的循环
public int sum(List<Integer> list) {
    int total = 0;
    for (int num : list) {
        total += num;
    }
    return total;
}
```

```erlang
% Erlang 的尾递归（优化）
sum(List) -> sum(List, 0).
sum([], Acc) -> Acc;
sum([H|T], Acc) -> sum(T, H + Acc).

% 阶乘
factorial(N) -> factorial(N, 1).
factorial(0, Acc) -> Acc;
factorial(N, Acc) -> factorial(N-1, N*Acc).

% 列表反转
reverse(List) -> reverse(List, []).
reverse([], Acc) -> Acc;
reverse([H|T], Acc) -> reverse(T, [H|Acc]).
```

### 4.4 高阶函数

```java
// Java 的函数式接口
list.stream()
    .map(x -> x * 2)
    .filter(x -> x > 10)
    .collect(Collectors.toList());
```

```erlang
% Erlang 的高阶函数
Double = fun(X) -> X * 2 end,
IsBig = fun(X) -> X > 10 end,

Numbers = [1, 2, 3, 4, 5],
Doubled = lists:map(Double, Numbers),     % [2,4,6,8,10]
BigOnes = lists:filter(IsBig, Doubled),   % [12,14,16,18,20]
Sum = lists:foldl(fun(X, Acc) -> X + Acc end, 0, BigOnes). % 70

% 列表推导式（强大的语法糖）
[X*2 || X <- [1,2,3,4,5]],             % [2,4,6,8,10] - 基础用法
[X || X <- [1,2,3,4,5], X > 2],        % [3,4,5] - 带条件
[{X,Y} || X <- [1,2], Y <- [a,b]].     % [{1,a},{1,b},{2,a},{2,b}] - 笛卡尔积
```

## Day 5: 并发编程（Concurrency）

### 5.1 进程与消息传递

```java
// Java 的线程
Thread thread = new Thread(() -> {
    System.out.println("Hello from thread!");
});
thread.start();
```

```erlang
% Erlang 的进程（超轻量！）
Pid = spawn(fun() ->
    io:format("Hello from process!~n")
end),
io:format("Created process: ~p~n", [Pid]).

% 消息传递
Sender = self(),
spawn(fun() ->
    Sender ! {hello, "from child process"}
end),

receive
    {hello, Message} ->
        io:format("Received: ~s~n", [Message])
after 1000 ->
    timeout
end.
```

### 5.2 进程链接与监控

**链接 (Linking)**：可以想象成"共享命运"。如果一对链接进程中有一个因错误死亡，它会发送退出信号（默认情况下）终止另一个。这创建了一种依赖关系，就像两个无法单独运行的微服务。

**监控 (Monitoring)**：可以想象成"事件监听器"。监控进程订阅被监控进程的"down"事件。如果目标进程死亡，监控进程会收到消息但不会被迫死亡。这是一种单向的感知关系。

```erlang
% 链接进程（崩溃传播）
Parent = self(),
Child = spawn_link(fun() ->
    timer:sleep(1000),
    exit(crash)  % 崩溃
end),

process_flag(trap_exit, true),
receive
    {'EXIT', Child, Reason} ->
        io:format("Child crashed: ~p~n", [Reason])
end.

% 监控进程（单向）
Ref = monitor(process, Child),
receive
    {'DOWN', Ref, process, Child, Reason} ->
        io:format("Child down: ~p~n", [Reason])
end.
```

### 5.3 gen_server

gen_server 是 OTP（Open Telecom Platform）中的核心组件，用于构建标准化的、有状态的服务器进程。它封装了消息循环、错误处理和通用行为，让开发者能更专注于业务逻辑，而不用重复编写底层并发代码。

```erlang
-module(counter).
-behaviour(gen_server).

% API
-export([start_link/0, increment/0, get/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []).

increment() ->
    gen_server:cast(?MODULE, increment).

get() ->
    gen_server:call(?MODULE, get).

% Callbacks
init(InitialCount) ->
    {ok, InitialCount}.

handle_call(get, _From, Count) ->
    {reply, Count, Count};

handle_cast(increment, Count) ->
    {noreply, Count + 1}.

terminate(_Reason, _State) ->
    ok.
```

### 5.4 Supervisor

```erlang
-module(my_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % 崩溃策略
        % one_for_one: 只重启崩溃的子进程 (常用)
        % one_for_all: 如果一个子进程崩溃，重启所有其他子进程 (用于组件间紧密耦合)
        % rest_for_one: 如果一个子进程崩溃，重启在它之后启动的所有兄弟进程
        intensity => 5,          % 在 `period` 秒内最大重启次数
        period => 60             % 时间窗口（秒）
    },

    ChildSpecs = [
        #{
            id => counter,
            start => {counter, start_link, []},
            restart => permanent,    % 总是重启
            shutdown => 5000,
            type => worker
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

### 5.5 Application

```erlang
-module(my_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    my_supervisor:start_link().

stop(_State) ->
    ok.

% my_app.app 配置文件
{application, my_app, [
    {description, "My Application"},
    {vsn, "1.0.0"},
    {modules, [my_app, my_supervisor, counter]},
    {registered, [my_supervisor, counter]},
    {applications, [kernel, stdlib]},
    {mod, {my_app, []}}
]}.
```

## Day 6: 标准库（Standard Library）

### 6.1 错误处理 (try/catch/after) - 解构 Erlang 的异常模型

对于 Java 开发者来说，Erlang 的错误处理机制初看起来可能有些奇特，但它与 OTP 的“任其崩溃 (Let it Crash)”哲学紧密相连。让我们先从您最困惑的 `try...catch` 语法开始，彻底拆解它。

#### 深入解析 `catch` 子句：`Class:Reason`

在 Java 中，`catch` 块捕获的是一个 `Exception` 对象，例如 `catch (IOException e)`。在 Erlang 中，`catch` 块使用的是**模式匹配**来捕获一个结构化的异常“信号”。

您看到的 `error:Reason` 就是一个模式匹配表达式，其语法是 `Class:Reason`。

-   **`Class` (类别)**：这是一个**原子**，用来标识异常的“类型”。它主要有三种：`error`、`exit` 和 `throw`。这有点像 Java 中 `Throwable` 的三个主要子类：`Error`、`Exception` 和 `RuntimeException`，但用途和语义有很大不同。
-   **`Reason` (原因)**：这是一个**变量**，用来绑定异常的“原因”或“负载”。它可以是任何 Erlang 的数据类型（一个原子、一个元组、一个字符串等）。这相当于 Java 异常对象中的 `message` 或其他字段。
-   **`:` (冒号)**：这是分隔 `Class` 和 `Reason` 的语法。

所以，`catch error:Reason -> ...` 的意思是：
> “捕获一个**类别**为 `error` 的异常，并将其**原因**绑定到变量 `Reason` 上，然后执行 `->` 后面的代码。”

**一个完整的 `try...catch` 块如下：**

```erlang
try
    % 这里是可能会“爆炸”的代码
    1 / 0
catch
    % Class:Reason -> Body;
    error:badarith ->
        % 匹配一个类别为 error，原因为 badarith 的异常
        io:format("算术错误发生了!~n");

    error:Reason ->
        % 匹配所有其他类别为 error 的异常，并将原因绑定到 Reason
        io:format("捕获到一个运行时错误: ~p~n", [Reason]);

    throw:ThrownValue ->
        % 匹配一个类别为 throw 的异常，并将抛出的值绑定到 ThrownValue
        io:format("捕获到一个抛出值: ~p~n", [ThrownValue]);

    exit:ExitSignal ->
        % 匹配一个类别为 exit 的异常，并将退出信号绑定到 ExitSignal
        io:format("捕获到一个退出信号: ~p~n", [ExitSignal])
after
    % 这里的代码总会执行，无论是否发生异常
    % 类似于 Java 的 finally 块
    io:format("清理工作完成。~n")
end.
```

#### Erlang 异常三巨头 vs. Java 的 `Throwable`

为了更好地理解，我们将 Erlang 的三种异常类型与 Java 的异常体系进行类比：

| Erlang 异常 | 触发方式 | Java 类比 | 用途和哲学 |
| :--- | :--- | :--- | :--- |
| **`error`** | 自动触发 (如 `1/0`) 或手动 `erlang:error(Reason)` | **`java.lang.RuntimeException`** (如 `NullPointerException`, `ArithmeticException`) | **代表程序缺陷 (Bug)**。你不应该试图在本地捕获它们。正确的做法是“任其崩溃”，让 Supervisor 来处理。看到 `error`，就意味着你需要修复代码。 |
| **`exit`** | `exit(Reason)` | **`System.exit()` / `Thread.interrupt()` / 进程间信号** | **代表进程生命周期事件**。它不是一个传统意义上的“错误”，而是一个进程通知其他（通常是链接的）进程它要“死亡”的信号。`exit(normal)` 是正常退出。`Supervisor` 正是依赖捕获子进程的 `exit` 信号来决定是否重启它们。 |
| **`throw`** | `throw(Value)` | **`CheckedException` / `break` / `return`** | **代表非本地返回 (Non-local Return)**。当你需要从一个深度嵌套的函数调用中“跳出”并返回一个值时使用。它用于处理**可预期的、非错误的**控制流变更，而不是意外的失败。 |

#### 代码示例：触发与捕获

**1. `error` - 程序缺陷**

```erlang
% 触发一个 error
trigger_error() ->
    % 隐式触发
    {ok, _Val} = {error, "oops"}, % badmatch 错误
    % 显式触发
    erlang:error({my_custom_error, "Something is very wrong"}).

% 不推荐的捕获方式（仅用于演示）
catch_error() ->
    try trigger_error()
    catch
        error:Reason -> {caught_error, Reason}
    end.

% 调用
% catch_error() 会返回 {caught_error, {badmatch, {error, "oops"}}}
```

**2. `throw` - 可预期的非本地返回**

想象一下，你要在一个深度嵌套的列表中寻找一个特定的值，找到后就想立即返回，而不是继续递归。

```erlang
-module(search).
-export([find/2]).

find(List, Value) ->
    try
        do_find(List, Value),
        {error, not_found} % 如果 do_find 正常结束（没抛出），说明没找到
    catch
        throw:found -> {ok, Value}
    end.

do_find([Value | _], Value) ->
    throw(found); % 找到了，立即“跳出”
do_find([H | T], Value) when is_list(H) ->
    do_find(H, Value), % 先在子列表中找
    do_find(T, Value); % 再在列表剩余部分找
do_find([_ | T], Value) ->
    do_find(T, Value);
do_find([], _Value) ->
    ok. % 什么也不做，让递归自然结束
```

**3. `exit` - 进程生命周期信号**

`exit` 信号通常在多进程场景下才有意义，它与 `spawn_link` 和 `Supervisor` 息息相关。

```erlang
% 触发一个 exit
main() ->
    % 链接一个子进程
    Parent = self(),
    spawn_link(fun() ->
        timer:sleep(500),
        % 子进程发送一个退出信号
        exit({i_am_done, "finished my job"})
    end),

    % 父进程需要"捕获"这个信号，否则它也会被默认行为杀死
    process_flag(trap_exit, true),

    % 等待并处理退出信号
    receive
        {'EXIT', FromPid, Reason} ->
            io:format("父进程捕获到来自 ~p 的退出信号, 原因: ~p~n", [FromPid, Reason])
    end.

% 调用 main().
% 输出：父进程捕获到来自 <0.123.0> 的退出信号, 原因: {i_am_done,"finished my job"}
```

#### 总结：Java 开发者如何适应？

1.  **函数式错误处理优先**：对于可预期的失败（如文件未找到、网络超时），**不要使用 `try...catch`**。而是让函数返回 `{ok, Value}` 或 `{error, Reason}` 元组。这是最地道、最常见的 Erlang 风格。
    ```erlang
    % ✅ 推荐的风格
    safe_divide(A, B) when B =/= 0 ->
        {ok, A / B};
    safe_divide(_, 0) ->
        {error, division_by_zero}.
    ```
2.  **将 `try...catch` 视为最后的防线**：仅在与不遵循上述风格的旧库交互，或需要处理 `throw`（非本地返回）时才使用 `try...catch`。
3.  **拥抱“任其崩溃”**：忘记在每个函数里都写上防御性代码。把精力放在设计好 Supervisor 树上，让它来处理那些你没预料到的 `error`，这才是 Erlang 系统健壮性的来源。

### 6.2 二进制操作

```erlang
% 分割
binary:split(<<"a,b,c">>, <<",">>),        % [<<"a">>, <<"b,c">>]
binary:split(<<"a,b,c">>, <<",">>, [global]), % [<<"a">>, <<"b">>, <<"c">>]

% 替换
binary:replace(<<"hello">>, <<"l">>, <<"L">>, [global]), % <<"heLLo">>

% 编码
binary:encode_hex(<<1, 255>>), % <<"01ff">>
binary:decode_hex(<<"01ff">>), % <<1, 255>>
```

### 6.3 加密

```erlang
% 哈希
crypto:hash(sha256, <<"data">>),  % 32字节哈希
crypto:hash(sha512, <<"data">>),  % 64字节哈希

% HMAC
Key = <<"secret">>,
crypto:mac(hmac, sha256, Key, <<"data">>),

% 随机数
crypto:strong_rand_bytes(32).    % 安全随机字节
```

### 6.4 列表操作

```erlang
lists:reverse([1,2,3]),        % [3,2,1]
lists:sort([3,1,2]),           % [1,2,3]
lists:member(2, [1,2,3]),      % true
lists:nth(2, [a,b,c]),         % b (1索引！)

lists:map(fun(X) -> X*2 end, [1,2,3]),    % [2,4,6]
lists:filter(fun(X) -> X>2 end, [1,2,3,4]), % [3,4]
lists:foldl(fun(X,Acc) -> X+Acc end, 0, [1,2,3]), % 6
```

### 6.5 映射操作

```erlang
maps:get(key, Map),                    % 值或异常
maps:get(key, Map, Default),           % 值或默认值
maps:put(key, value, Map),             % 新映射
maps:update(key, value, Map),          % 更新（必须存在）
maps:remove(key, Map),                 % 删除

maps:keys(Map), maps:values(Map),      % 键值列表
maps:merge(M1, M2),                    % 合并
maps:map(fun(K,V) -> V*2 end, Map).    % 变换
```

### 6.6 字符串操作

```erlang
string:uppercase(<<"hello">>),         % <<"HELLO">>
string:lowercase(<<"HELLO">>),         % <<"hello">>
string:trim(<<" hello ">>),            % <<"hello">>

string:split(<<"a,b,c">>, <<",">>),    % [<<"a">>, <<"b,c">>]
string:find(<<"hello world">>, <<"world">>), % <<"world">>
```

### 6.7 文件 I/O

```erlang
% 读取
{ok, Data} = file:read_file("file.txt"),
{ok, Fd} = file:open("file.txt", [read]),
{ok, Line} = file:read_line(Fd),
file:close(Fd),

% 写入
file:write_file("out.txt", <<"data">>),
{ok, Fd} = file:open("out.txt", [write]),
file:write(Fd, <<"line\n">>),
file:close(Fd),

% 信息
{ok, Info} = file:read_file_info("file.txt"),
Info#file_info.size,  % 文件大小
file:list_dir(".").   % 目录内容
```

### 6.8 正则表达式

```erlang
% 匹配
re:run(<<"abc123">>, "\\d+"),          % {match, ...}
{match, [Match]} = re:run(<<"abc123">>, "\\d+", [{capture, all, binary}]),
Match,  % <<"123">>

% 分割和替换
re:split(<<"a,b,c">>, ","),            % [<<"a">>, <<"b">>, <<"c">>]
re:replace(<<"hello">>, "l", "L", [global]), % <<"heLLo">>
```

## Day 7: 高级特性（Advanced Topics）

### 7.1 ETS - Erlang 内存数据库

```erlang
% 创建表
Table = ets:new(my_table, [set, public, named_table]),

% 插入
ets:insert(my_table, {key1, <<"value1">>}),
ets:insert(my_table, [{k1, v1}, {k2, v2}]),

% 查询
ets:lookup(my_table, key1),    % [{key1,"value1"}]
ets:member(my_table, key1),    % true

% 删除
ets:delete(my_table, key1),
ets:delete(my_table).          % 删除表
```

### 7.2 定时器

```erlang
% 发送延迟消息
timer:send_after(1000, self(), timeout),

% 定期消息
{ok, Ref} = timer:send_interval(1000, self(), tick),
timer:cancel(Ref),

% 测量执行时间
{Time, Result} = timer:tc(fun() -> expensive() end),
Time.  % 微秒
```

### 7.3 队列

```erlang
Q0 = queue:new(),
Q1 = queue:in(item1, Q0),
Q2 = queue:in(item2, Q1),
{{value, Item}, Q3} = queue:out(Q2),  % Item=item1

queue:len(Q3),      % 1
queue:is_empty(Q3), % false
queue:to_list(Q3).  % [item2]
```

### 7.4 端口（外部程序）

```erlang
% 启动外部程序
Port = open_port({spawn, "python script.py"}, [
    binary,
    {packet, 4},
    exit_status
]),

% 发送数据
Port ! {self(), {command, <<"input">>}},

% 接收输出
receive
    {Port, {data, Output}} ->
        handle_output(Output);
    {Port, {exit_status, Status}} ->
        done
end,

port_close(Port).
```

### 7.5 引用 (References)

```erlang
% 创建唯一标识符
Ref = make_ref(),

% 请求-响应模式
request(Pid, Msg) ->
    Ref = make_ref(),
    Pid ! {request, Ref, self(), Msg},
    receive
        {response, Ref, Result} -> {ok, Result}
    after 5000 -> timeout
    end.
```

> **Pro Tip**: `make_ref()` 返回的引用在整个 Erlang 运行时系统（节点）中都是唯一的，而不仅仅是在单个进程内。这使得它非常适合在分布式系统中标记请求而不用担心冲突。

### 7.6 属性列表 (Proplists)

```erlang
% 创建
Props = [{name, <<"Alice">>}, {age, 25}],

% 访问
proplists:get_value(name, Props),      % <<"Alice">>
proplists:get_value(missing, Props, default), % default

% 布尔标志
Props2 = [verbose, {debug, false}],
proplists:get_bool(verbose, Props2).  % true
```

### 7.7 位运算

```erlang
% 位运算
16#FF band 16#0F,  % AND: 15
16#F0 bor 16#0F,   % OR: 255
16#FF bxor 16#0F,  % XOR: 240
bnot 16#FF,        % NOT

% 移位
1 bsl 3,   % 左移: 8
8 bsr 1,   % 右移: 4

% 位操作
set_bit(Value, Position) -> Value bor (1 bsl Position).
clear_bit(Value, Position) -> Value band bnot (1 bsl Position).
check_bit(Value, Position) -> (Value band (1 bsl Position)) =/= 0.
```

### 7.8 系统自省

```erlang
% 进程信息
process_info(self()),              % 所有信息
process_info(self(), memory),      % 内存使用
process_info(self(), message_queue_len), % 消息队列长度

% 系统信息
erlang:system_info(process_count), % 进程数量
erlang:system_info(schedulers),    % 调度器数量
erlang:memory(),                   % 内存统计

% 进程列表
erlang:processes(),                % 所有进程
erlang:registered().               % 注册的进程名

% 进程字典
put(key, value),                   % 设置
get(key),                          % 获取
erase(key).                        % 删除

% **⚠️ 强烈建议：** 避免使用进程字典。它类似于一个进程内的"全局变量"，会破坏函数式编程的纯粹性，并使代码难以维护。状态应始终通过函数参数或 gen_server 的循环状态来显式传递。
```

## Day 8: 模块系统与 NIFs

### 8.1 模块基础

```erlang
% 模块定义
-module(my_module).                    % 文件名必须是 my_module.erl
-export([public_func/1]).              % 导出的函数
-import(lists, [map/2, filter/2]).     % 导入函数

% 宏定义
-define(TIMEOUT, 5000).
-define(PI, 3.14159).

% 记录定义
-record(user, {id, name, age = 0}).

% 公共函数
public_func(X) ->
    private_func(X) * 2.

% 私有函数
private_func(X) ->
    X + 1.
```

### 8.2 编译指令

```erlang
% 导出所有函数（调试用）
-compile(export_all).

% 内联优化
-compile({inline, [fast_func/1]}).

% 条件编译
-ifdef(DEBUG).
debug_log(Msg) -> io:format("DEBUG: ~p~n", [Msg]).
-else.
debug_log(_) -> ok.
-endif.
```

### 8.3 类型规范

```erlang
% 函数类型规范
-spec add(integer(), integer()) -> integer().
add(A, B) ->
    A + B.

% 自定义类型
-type user_id() :: pos_integer().
-type user() :: #{id => user_id(), name => binary()}.

-spec find_user(user_id()) -> {ok, user()} | {error, not_found}.
```

### 8.4 NIF 基础

```erlang
% Erlang 模块
-module(my_nif).
-export([sha256/1]).
-on_load(init/0).

init() ->
    SoName = filename:join(code:priv_dir(my_app), "my_nif"),
    ok = erlang:load_nif(SoName, 0).

sha256(_Data) ->
    erlang:nif_error({not_loaded, ?MODULE}).

%% 使用
Hash = my_nif:sha256(<<"hello">>).
```

```c
// C NIF 实现
#include "erl_nif.h"

static ERL_NIF_TERM sha256_nif(ErlNifEnv* env, int argc,
                                const ERL_NIF_TERM argv[]) {
    ErlNifBinary input;
    if (!enif_inspect_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    unsigned char output[32];
    sha256(input.data, input.size, output);

    ERL_NIF_TERM result;
    unsigned char* result_data = enif_make_new_binary(env, 32, &result);
    memcpy(result_data, output, 32);

    return result;
}

static ErlNifFunc nif_funcs[] = {
    {"sha256", 1, sha256_nif}
};

ERL_NIF_INIT(my_nif, nif_funcs, NULL, NULL, NULL, NULL)
```

**重要提示**：NIFs 提供了在 Erlang 中执行高性能原生代码的能力。然而，NIF 中的任何崩溃都可能导致整个 Erlang 虚拟机（BEAM VM）崩溃，这与 Erlang 进程的"let it crash"哲学形成对比。因此，NIFs 的开发需要格外小心。

### 8.6 NIF 脏调度器 (Dirty Schedulers)

**问题**：NIF 函数必须快速完成（< 1ms），否则会阻塞整个 Erlang 调度器。

**解决方案**：使用脏调度器处理长时间运行的操作。

```erlang
% Erlang 模块声明脏 NIF
-on_load(init/0).

long_operation(_Data) ->
    erlang:nif_error({not_loaded, ?MODULE}).

init() ->
    SoName = filename:join(code:priv_dir(my_app), "my_nif"),
    ok = erlang:load_nif(SoName, 0).
```

```c
// C NIF 实现使用脏调度器
static ERL_NIF_TERM long_operation_nif(ErlNifEnv* env, int argc,
                                       const ERL_NIF_TERM argv[]) {
    // 这个函数会在脏调度器上运行
    // 不会阻塞 Erlang 的主调度器
    heavy_computation();
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    // 第四个参数指定调度器类型
    {"long_operation", 0, long_operation_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND}
};
```

**脏调度器类型**：
- `ERL_NIF_DIRTY_JOB_CPU_BOUND`: CPU 密集型操作
- `ERL_NIF_DIRTY_JOB_IO_BOUND`: I/O 密集型操作

**何时使用**：
- ✅ CPU 密集计算（加密、压缩）
- ✅ I/O 操作（文件、网络）
- ✅ 任何可能 > 1ms 的操作

**何时不用**：
- ❌ 快速操作（< 1ms）
- ❌ 简单的数据转换

### 8.5 Rustler NIFs

```rust
use rustler::{Binary, Encoder, Env, NifResult, Term};

#[rustler::nif]
fn sha256<'a>(env: Env<'a>, data: Binary) -> NifResult<Term<'a>> {
    let hash = compute_sha256(data.as_slice());
    Ok(hash.encode(env))
}

rustler::init!("my_nif");
```

---

## 🎉 现在你可以完全看懂 HyperBEAM 教程了！

这个教程涵盖了 https://hbdocs.vercel.app/hyperbeam/erlang 中的**所有基础知识**：

### ✅ 完全覆盖的概念：

**基础数据类型：**
- Numbers, atoms, binaries, strings
- Variables & pattern matching

**数据结构：**
- Lists, tuples, maps, records

**函数系统：**
- Functions, guards, pattern matching in functions

**控制流：**
- case/if expressions, recursion, higher-order functions

**并发编程：**
- Processes & message passing
- Process links & monitors
- gen_server, supervisor, application

**标准库：**
- try/catch/after (error handling)
- Binary, crypto, lists, maps, string modules
- File I/O, regular expressions

**高级特性：**
- ETS (in-memory database)
- Timer functions, queues, ports
- References, proplists, bitwise operations
- System introspection

**模块系统：**
- Module basics, compile directives, type specifications

**NIFs：**
- C NIFs basics, working with binaries
- Rustler NIFs, best practices

### 🚀 你现在具备了理解 HyperBEAM 源代码的完整基础知识！

---

## 📚 进阶学习资源

- [Erlang 官方文档](https://www.erlang.org/docs)
- [HyperBEAM Erlang 教程](https://hbdocs.vercel.app/hyperbeam/erlang)
- [Programming Erlang](https://pragprog.com/titles/jaerlang2/programming-erlang-2nd-edition/)
- [Erlang in Anger](https://www.erlang-in-anger.com/) - 生产环境最佳实践

---

## 🎯 Java 开发者学习 Erlang 的关键思维转变

### 核心概念对比

| Java 思维 | Erlang 思维 | 为什么 Erlang 更好 |
|-----------|-------------|-------------------|
| **面向对象**<br>`class User {}` | **函数式**<br>`-module(user).` | 更简单，数据与行为分离 |
| **可变状态**<br>`x = 10; x = 20;` | **不可变数据**<br>`X = 10, Y = X + 10.` | 线程安全，无副作用 |
| **异常抛出**<br>`throw new Exception()` | **返回值模式**<br>`{error, reason}` | 明确错误处理，函数式友好 |
| **线程同步**<br>`synchronized(obj)` | **消息传递**<br>`Pid ! message` | 避免了共享内存导致的死锁和竞争条件 |
| **继承**<br>`extends Parent` | **行为(Behaviour)**<br>`-behaviour(gen_server).` | 组合优于继承，更灵活 |
| **垃圾回收**<br>自动 | **垃圾回收**<br>响应性极佳 | 无『暂停世界』的垃圾回收，响应性极佳 |

### 学习曲线与建议

#### 📈 **学习阶段**
1. **Day 1-2**: 语法基础 - 像学习新语言语法
2. **Day 3-4**: 函数式思维 - 最难的思维转变
3. **Day 5-6**: 并发编程 - Erlang 的核心优势
4. **Day 7-8**: 实践应用 - 融会贯通

#### 💡 **学习技巧**
- **多写递归**：忘记循环，用递归思考
- **使用模式匹配**：参数解构是常态
- **函数式错误处理**：返回值而非异常
- **消息传递**：进程间通信而非共享状态

#### ⚠️ **常见陷阱**
- 忘记变量不可变，试图重新赋值
- 用 Java 异常处理方式写 Erlang 代码
- 试图用线程思维理解 Erlang 进程
- 忘记原子（atom）常用于状态和标签
- 在需要高性能处理的地方误用字符串（字符列表），而不是高效的二进制

### Erlang 开发最佳实践

#### 🏗️ **代码组织**
- 一个模块一个职责
- 导出最小化接口
- 使用记录定义数据结构
- 添加类型规范

#### 🔧 **错误处理**
- 让进程崩溃（"Let it crash"）
- 使用 supervisor 重启失败进程
- 函数式错误处理（返回值）
- 日志记录重要错误

#### 🚀 **性能优化**
- 使用尾递归
- 利用二进制（binaries）处理大数据
- ETS 存储频繁访问数据
- NIF 处理性能关键代码

### 下一步学习路径

1. **深入 OTP**：supervisor 树、应用生命周期
2. **分布式 Erlang**：节点通信、集群管理
3. **实际项目**：构建聊天服务器、缓存系统
4. **性能调优**：VM 参数、代码优化
5. **生产部署**：发布、监控、维护

**记住：Erlang 不是 Java 的替代品，而是并发编程、分布式系统的首选！** 🚀

---

## 📚 完整学习资源

### 官方资源
- [Erlang 官方文档](https://www.erlang.org/docs) - 最权威的参考
- [HyperBEAM Erlang 教程](https://hbdocs.vercel.app/hyperbeam/erlang) - 现在你可以完全看懂了！

### 书籍推荐
- [Programming Erlang](https://pragprog.com/titles/jaerlang2/programming-erlang-2nd-edition/) - 入门经典
- [Erlang in Anger](https://www.erlang-in-anger.com/) - 生产环境最佳实践
- [Learn You Some Erlang](https://learnyousomeerlang.com/) - 免费在线教程

### 项目实践
- [HyperBEAM 项目](https://github.com/weavedb/hyperbeam) - 学习真实项目
- [RabbitMQ](https://github.com/rabbitmq/rabbitmq-server) - Erlang 明星项目
- [ejabberd](https://github.com/processone/ejabberd) - XMPP 服务器

### 社区资源
- [Erlang Forums](https://erlangforums.com/) - 官方论坛
- [Reddit r/erlang](https://reddit.com/r/erlang) - 社区讨论
- [Erlang Slack](https://erlang-slack.herokuapp.com/) - 实时交流

**祝你在 Erlang 的并发世界中编程愉快！** 🎉

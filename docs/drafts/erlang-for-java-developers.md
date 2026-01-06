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

## Day 0: Erlang 语法基础 - 从 Java 思维转变的开始

> **重要提醒**：对于 Java 开发者，学习 Erlang 的第一个挑战不是数据类型，而是要彻底改变对“语句”和“表达式”的看法。请忘掉 Java 的分号和花括号，让我们建立新的心智模型。

### 0.1 核心区别：表达式 vs 语句

这是从 Java 到 Erlang 最根本的思维转变。

- **Java 的世界是“语句”驱动的**：你执行一条条指令（`x = 5;`, `System.out.println("Hello");`），某些语句（如 `return ...;`）可以返回值。
- **Erlang 的世界是“表达式”驱动的**：**一切都是表达式，一切都有返回值**。`X = 5` 这个绑定操作本身也是一个表达式，它的值是 `5`。函数体由一系列表达式组成，最后一个表达式的值就是整个函数的返回值。

因为这个核心区别，Erlang 不需要 `return` 关键字。

```erlang
% Java 风格（语句）
public int add(int a, int b) {
    int temp = a + 10;  // 这是一个语句
    return temp + b;    // 必须用 return 显式返回值
}

% Erlang 风格（表达式）
add(A, B) ->
    Temp = A + 10,      % 这是一个表达式，其值为 A + 10
    Temp + B.           % 这是最后一个表达式，其值自动成为函数的返回值
```

### 0.2 分隔符：逗号 (,) vs 句点 (.) - Java 开发者的关键类比

忘掉分号和花括号，记住这个关键类比：

- **逗号 `,`**  →  类似于 Java 方法体**内部的分号 `;`**。
  - **作用**：分隔**同一个函数定义中**的一系列连续表达式。它告诉编译器：“这个表达式结束了，下一个表达式马上开始，但我们还在同一个函数里。”

- **句点 `.`** → 类似于 Java 方法的**最后一个右花括号 `}`**。
  - **作用**：**终结一个完整的逻辑单元**，最常见的就是一个函数定义（或一个函数子句）。它告诉编译器：“这个函数到此彻底结束了，后面不会再有属于它的代码了。”

让我们用代码来固化这个理解：

```java
// Java: 完整的 "逻辑单元" 是整个方法
public int example(int x) {
    int y = x + 1; // 内部用分号
    int z = y * 2; // 内部用分号
    return z;
} // 用 } 终结整个逻辑单元
```

```erlang
% Erlang: 完整的 "逻辑单元" 是整个函数定义
example(X) ->
    Y = X + 1,  % 内部用逗号
    Z = Y * 2,  % 内部用逗号
    Z.          % 用 . 终结整个逻辑单元
```

**这个类比能帮你理解为什么下面的代码是错误的：**

```erlang
% ❌ 绝对错误的写法
bad_function(X) ->
    A = X + 1.  % 错误！句点在这里相当于 Java 的 "}"
    B = A * 2.  % 编译失败！编译器认为函数在上一行已经结束了
                % 这行代码就像是写在 Java 方法的 "}" 之后，属于无主代码
```
**编译错误会是**: `syntax error before: 'B'`，因为编译器认为 `bad_function` 在 `A = X + 1.` 之后已经结束，`B` 的出现不合法。

### 0.3 代码块也是表达式

在 Java 中，`if`、`switch`、`try-catch` 都是语句块。但在 Erlang 中，`if`、`case`、`receive` 块本身就是**表达式**，它们一定会返回一个值。

这就像 Java 中的三元运算符 `(condition ? value1 : value2)`，它本身是有值的。

```erlang
% ✅ case 块本身是一个表达式，其结果被绑定到变量 Result
Result = case X of
    1 -> "one";
    2 -> "two";
    _ -> "other"
end, % case 表达式结束，用逗号连接后续表达式
io:format("Result: ~p~n", [Result]).

% ✅ if 块也是一个表达式
Value = if
    X > 10 -> "big";
    X < 0 -> "negative";
    true -> "normal"  % 必须有 true 分支，保证表达式总有返回值
end,
io:format("Value: ~p~n", [Value]).

% ✅ 当块表达式是函数的最后一步时，其后跟句点
func_returning_case(X) ->
    case X of
        1 -> "one";
        _ -> "other"
    end. % case 表达式的结果就是函数的返回值，用句点结束整个函数
```

**`if` 为什么必须有 `true` 子句？**
因为它是一个必须返回值的表达式。如果没有 `true` 这样的“全匹配”子句，当所有条件都不满足时，表达式就没法返回值，这在逻辑上是矛盾的，所以 Erlang 会直接让程序崩溃并抛出 `if_clause` 异常。这是一种强制性的安全措施。

### 0.4 注释

```erlang
% 单行注释：以百分号开头，直到行尾。

%% 多行注释的惯例：
%% 虽然每一行都是一个独立的单行注释，
%% 但使用两个百分号是社区推荐的、用于文档块的风格。
```
Erlang 没有 `/* ... */` 这样的块注释语法。

### 0.5 模块与可见性 (Module & Visibility) - 类比 Java 的 public/private

一个 Erlang 文件就是一个模块（Module）。模块是代码组织的基本单元。

```erlang
% 文件名: user_service.erl

-module(user_service). % 类似于 Java 的 `public class user_service`

% 导出函数，类似于 Java 的 `public` 方法
-export([create_user/2, find_user/1]).

% Arity (元数) 的概念：
% `create_user/2` 指的是名为 `create_user` 且有 2 个参数的函数。
% `find_user/1`   指的是名为 `find_user`   且有 1 个参数的函数。
% Erlang 通过 "函数名/元数" 来唯一确定一个函数，这是其函数重载的方式，
% 不同于 Java 通过参数类型来重载。

%% ===================================================================
%% Public API Functions
%% ===================================================================

% @doc 创建一个用户
% 这是导出的 "public" 函数
create_user(Name, Age) ->
    Id = generate_id(), % 调用一个内部的 "private" 函数
    {ok, #{id => Id, name => Name, age => Age}}.

% @doc 查找用户
% 这是另一个 "public" 函数
find_user(Id) ->
    % ... 查找逻辑 ...
    {ok, #{id => Id, name => <<"Dummy">>, age => 99}}.

%% ===================================================================
%% Private Helper Functions
%% ===================================================================

% 这个函数没有在 -export列表中，所以是 "private" 的
% 只能在 user_service 模块内部被调用
generate_id() ->
    % ... 复杂的ID生成逻辑 ...
    12345.
```

**与 Java 的类比总结：**

| Erlang 概念 | Java 类比 | 解释 |
| :--- | :--- | :--- |
| `-module(my_module).` | `public class my_module { ... }` | 定义了一个代码单元。 |
| `-export([func/1]).` | `public Result func(Arg1) { ... }` | 声明了模块的公共接口，允许外部调用。 |
| 未导出的函数 | `private Result helper() { ... }` | 默认是私有的，只能在模块内部使用。 |
| `func/1`, `func/2` | 方法重载 (Method Overloading) | Erlang 根据参数**数量**（元数）区分函数，Java 根据参数**类型和数量**。|

现在，你已经掌握了 Erlang 的骨架。接下来，我们开始填充血肉——数据类型。

## Day 1: 基础数据类型 (Primitives) - 新的思维模式

Erlang 的基础类型看起来很简单，但其背后的哲学与 Java 大相径庭。理解这些差异是学习 Erlang 的关键。

### 1.1 数字 (Numbers) - 再见，`BigInteger`！

Java 中有 `int`, `long`, `float`, `double`，当 `long` 不够用时，你必须显式地换用 `BigInteger` 类，这很繁琐。

Erlang 则优雅得多：

```erlang
% Erlang 的数字：简单、强大
Age = 25,                    % 整数 (Integer)
Population = 7800000000,     % 大整数 (Bignum)，自动处理，无需关心
Pi = 3.14159,                % 浮点数 (Float)，通常是64位双精度

1_000_000.    % 可以用下划线增加可读性
```

**核心优势**：你**永远不需要担心整数溢出**。Erlang 会在运行时根据数字的大小在小整数（machine word）和大整数（`BigInteger`的等价物）之间自动切换，对开发者完全透明。

#### **一个重要的“陷阱”：`==` vs `=:=`**

这对于 Java 开发者是一个常见的坑。

- **`==`：值相等比较 (Value Equality)**
  - **类比 Java 的 `.equals()`**
  - 它会自动进行类型转换，只要**数值**相等，就返回 `true`。
  - `5 == 5.0` → `true`。因为从值的角度看，`5` 和 `5.0` 是相等的。

- **`=:=`：精确相等比较 (Exact Equality)**
  - **类比 Java 对基本类型的 `==`，或者更像是类型和值都严格的比较**
  - 它**从不**进行类型转换。只有当类型和值都完全相同时，才返回 `true`。
  - `5 =:= 5.0` → `false`。因为一个是整数类型，一个是浮点数类型。

**经验法则**：在你不确定或不希望进行自动类型转换时，**总是优先使用 `=:=`**。这能让你的代码更严谨，避免隐蔽的 bug。对应的“不精确等于”是 `=/=`。

### 1.2 原子 (Atoms) - 超级`enum`和高性能`String`

原子是 Erlang 的一个特色。对于 Java 开发者，可以从以下几个方面来理解它：

-   **它是 `enum`**：用于表示一组固定的、命名的状态。例如 `ok`, `error`, `pending`。
-   **它是 `public static final String`**：用于消息的标签或 `Map` 的键。
-   **它是 `Boolean`**: `true` 和 `false` 在 Erlang 中**就是原子**！

```erlang
% 原子就是它本身，以小写字母开头
Status = ok,
Result = {ok, <<"Data">>},
IsAdmin = true. % true 就是一个原子！

% 在 case 语句中，原子是完美的匹配对象
case Status of
    ok -> "成功";
    error -> "失败";
    _ -> "未知"
end.
```

**性能的秘密：指针比较**
为什么不用字符串 `"ok"` 而用原子 `ok`？

-   当你在代码里写下一个原子 `ok`，Erlang 编译器会把它添加到一个全局的“原子表”中，并给它一个唯一的整数索引。
-   在运行时，代码中所有出现的 `ok` 实际上都是那个小小的整数索引。
-   所以，比较 `Status == ok` 就变成了**比较两个整数**，这比 Java 中 `status.equals("ok")` 逐字符比较要快几个数量级。

**⚠️ 终极警告：动态创建原子的危险**
原子一旦被创建，**永远不会被垃圾回收**。

-   **类比**：想象一下 Java 中有一个**全局的、永不清理的 `HashSet<String>`**。你每调用一次 `string.to_atom()`，就等于向这个 `HashSet` 中添加一个元素。
-   **后果**：如果你的代码允许从外部输入（如用户 HTTP 请求的参数）动态创建原子，攻击者就可以通过发送大量不同的字符串来耗尽你虚拟机的内存（因为原子表是有上限的，默认为 `1_048_576`），导致整个系统崩溃。
-   **铁律**：**永远不要将不受信任的、动态的外部输入转换为原子。**

### 1.3 字符串：一个历史遗留的陷阱

这是 Erlang 新手（尤其是来自其他语言的开发者）最容易掉进去的坑。

**请牢记这个事实：**

-   `<<"我是字符串">>` (Binary - 二进制) → 这**才是** Erlang 中现代、高效、正确的字符串。它类似于 Java 的 `String` 类。
-   `"我也是字符串"` (List - 列表) → 这**不是**真正的字符串！它是一个由字符编码（整数）组成的**链表**。它只是语法糖，实际上等同于 `[25105, 20063, 26159, 23383, 31526]`。

| Erlang 类型 | 语法 | 实际内容 | Java 类比 | 性能/内存 | 推荐用法 |
| :--- | :--- | :--- | :--- | :--- | :--- |
| **二进制** | `<<"text">>` | 连续的字节块 | `String` / `byte[]` | **高效** | **所有文本处理的首选** |
| **字符列表**| `"text"` | `[116, 101, 120, 116]` | `LinkedList<Integer>` | **极度低效** | **仅用于兼容旧的库函数** |

**为什么会这样？**
这是 Erlang 的历史遗留问题。在早期，双引号被设计为字符列表。为了向后兼容，这个设计被保留了下来。现代 Erlang 编程中，你几乎总应该使用 `<<"...">>` 形式的二进制字符串。

**二进制的强大之处：模式匹配**
二进制类型是 Erlang 的一大法宝，尤其适合处理网络协议、解析二进制文件等。这是 Java 中需要用 `ByteBuffer` 等工具繁琐处理的场景。

```erlang
% 从 HTTP 头中解析内容长度
Header = <<"Content-Length: 1024\r\n">>,
<< "Content-Length: ", LengthStr/binary >> = Header, % 匹配前缀
Length = binary_to_integer(LengthStr). % Length = 1024
```

### 1.4 变量与模式匹配 - `=` 不是赋值，是断言！

Java 开发者看到 `=` 会下意识地认为是“赋值”。在 Erlang 中，请彻底抛弃这个想法。

**Erlang 的 `=` 是“匹配运算符”，它更像一个“断言 (Assertion)”**。

-   **单次绑定**：所有 Erlang 变量都**只能被绑定一次**。它们就像 Java 中的 `final` 变量。
-   **`=` 的工作原理**：
    1.  当 `=` 左边的变量**未被绑定**时，Erlang 会尝试通过绑定该变量，来使得 `=` 两边相等。
    2.  当 `=` 左边的变量**已被绑定**时，`=` 就变成一个**断言**，它会判断两边的值是否相等。如果相等，表达式成功；如果不等，**立即崩溃**并抛出 `badmatch` 错误。

```erlang
% 1. 左边变量 X 未绑定
X = 5.
% 这不是 "把 5 赋给 X"，而是 "我声称 X 等于 5"。
% 为了让这个声称成立，Erlang 把 X 绑定到 5。

% 2. 左边变量 Y 已绑定
Y = 10,
Y = 10. % 正确！声称 "10 等于 10"，断言成功。

% 3. 断言失败 -> 崩溃！
Z = 15,
Z = 20. % ❌ badmatch 错误！声称 "15 等于 20"，断言失败。
```

**模式匹配：强大的解构能力**
这个“断言”机制延伸到复杂的数据结构上，就形成了极其强大的模式匹配（或称“解构”）。

```erlang
% 解构元组
{ok, Value} = {ok, "some data"}.
% 声称左边模式匹配右边值。
% 为使声称成立，Erlang 将 Value 绑定到 "some data"。

% 如果 API 返回了错误
{ok, OtherValue} = {error, "not found"}.
% ❌ badmatch 错误！因为原子 ok 不等于 error。
% 这让你的代码在处理非预期返回值时会立即失败，而不是把错误带到更深的地方。

% 解构列表
[Head | Tail] = [1, 2, 3].
% Head 会被绑定到 1
% Tail 会被绑定到 [2, 3]
```

这个机制是 Erlang 编程的基石，它让代码极其简洁并强制你处理所有可能的情况。

## Day 2: 数据结构 (Data Structures) - 不可变性的力量

Erlang 的核心数据结构非常少，但极其强大。它们的设计完全服务于函数式编程和不可变性的哲学。

### 2.1 列表 (Lists) - 精准类比：Java 的 `LinkedList`

忘掉 `ArrayList`，Erlang 的列表是**纯粹的、不可变的单向链表**。

```java
// Java 中，这更像是...
LinkedList<Object> names = new LinkedList<>();
names.add("Alice");
names.add("Bob");
```

```erlang
% Erlang 的列表
Names = ["Alice", "Bob", "Charlie"]. % 语法糖，实际上是 [ "Alice" | ["Bob" | ["Charlie" | []]]]

% 构造与解构 (模式匹配是核心)
[H | T] = Names. % H = "Alice", T = ["Bob", "Charlie"]
```

**性能影响与不可变性**
因为是链表且不可变，所以操作性能与 Java 的 `LinkedList` 非常相似：

-   **头部添加 (`[New | List]`)：O(1)，极快。**
    这并**不会修改**原列表。它会创建一个新的列表元素，该元素的“头”是你的新元素，而“尾”则是一个指向原列表的**指针**。原列表 `Names` 保持不变。
    ```erlang
    Names = ["Alice", "Bob"].
    NewNames = ["Zero" | Names]. % NewNames = ["Zero", "Alice", "Bob"]
    % Names 依然是 ["Alice", "Bob"]
    ```

-   **尾部添加 (`List ++ [New]`)：O(n)，非常慢！**
    操作符 `++` 是列表连接。为了在尾部添加一个元素，Erlang 必须**完整地复制** `++` 操作符左边的整个列表，然后将新元素链接上去。在循环或递归中频繁这样做会导致严重的性能问题。

-   **读取第 N 个元素 (`lists:nth(N, List)`)：O(n)，慢。**
    需要从头开始遍历 N 个元素。

**结论**：Erlang 的列表是为递归而生的。处理列表的标准方式是：处理头部元素，然后将尾部列表传入下一次递归调用。

### 2.2 元组 (Tuples) - 轻量级的不可变对象

忘掉“固定数组”这个简单的类比，一个更精确的 Java 类比是“**一个匿名的、`final`的、只有 `public final` 字段的轻量级对象**”。

-   **用途**：用于存放固定数量、类型可能不同、但逻辑上相关的一组数据。
-   **性能**：元组在内存中是**连续存储**的。因此，访问其元素 (`element(N, Tuple)`) 的速度是 **O(1)**，非常快。
-   **惯用法**：最重要的惯用法是作为函数返回值的标准格式，即“标签元组”(Tagged Tuple)。
    -   `{ok, Value}`：表示操作成功，并携带返回值。
    -   `{error, Reason}`：表示操作失败，并携带失败原因。

```erlang
% Java 中，你可能会创建一个Result类
class Result {
    final boolean isSuccess;
    final Object value;
    final String error;
    // ... 构造函数 ...
}

% Erlang 中，你只需要一个元组
case file:read_file("my_file.txt") of
    {ok, BinaryData} -> % 成功时，模式匹配出数据
        handle_data(BinaryData);
    {error, enoent} -> % 失败时，模式匹配出原因（enoent = a file not found error）
        log_error("文件未找到")
end.
```
这种方式强制开发者在调用处处理成功和失败两种情况，比 Java 的 `try-catch` 更具函数式风格。

### 2.3 映射 (Maps) - 就是 Java 的 `HashMap`

这可能是 Java 开发者最熟悉的数据结构。Erlang 的 `Map` 提供了键值对存储，并且是现代 Erlang 代码中处理动态数据的首选。

```erlang
% 键可以是原子或二进制字符串
User = #{
    name => <<"Alice">>,
    age => 25,
    status => active % 使用原子作为键
}.

% 访问
maps:get(name, User). % <<"Alice">>
```

**更新语法的关键区别：`=>` vs `:=`**

-   **`=>`：添加或更新 (Upsert)**
    `NewUser = User#{age => 26, email => <<"a@b.com">>}.`
    -   如果键 `age` 存在，则更新其值。
    -   如果键 `email` 不存在，则添加该键值对。
    -   **类比 Java 的 `map.put("age", 26)`**。

-   **`:=`：仅更新 (Update-only)**
    `StrictUpdate = User#{age := 27}.`
    -   如果键 `age` 存在，则更新其值。
    -   如果键 `age` **不存在**，**程序立即崩溃**并抛出 `badkey` 错误。
    -   这在你想**断言**某个键必须存在时非常有用，可以避免因拼写错误等原因意外引入新键，是一种防御性编程。Java 的 `HashMap` 没有直接对应的单方法操作。

### 2.4 记录 (Records) - 编译时的“语法糖”

记录是 Java 开发者最容易误解的概念之一，因为它看起来像个对象，但又不是。

**核心秘密**：记录是**编译时**的概念。在编译后，它**会变成一个元组**。

-   **类比**：一个有严格字段检查的、不可变的 DTO (Data Transfer Object)。

```erlang
% 1. 在模块顶部定义记录，像定义一个类的结构
-record(user, {name, age = 0, active = true}).

test() ->
    % 2. 创建一个记录实例
    User = #user{name = <<"Alice">>, age = 25},

    % 3. 访问字段
    UserName = User#user.name, % <<"Alice">>

    % 在编译时，上面的代码会被转换成类似下面的样子：
    % UserTuple = {user, <<"Alice">>, 25, true}.
    % UserName = element(2, UserTuple).
    ok.
```
**为什么要用记录？**
如果记录本质上是元组，为什么不直接用元组？
**为了编译时安全和代码可读性！**
-   **可读性**：`User#user.name` 远比 `element(2, UserTuple)` 清晰。
-   **安全性**：如果你写错了字段名 `User#user.nme`，**编译器会报错**。而如果你写错了元组索引 `element(99, UserTuple)`，只会在**运行时崩溃**。

#### Maps vs Records：如何选择？

我将用一个更详尽的表格来帮你决策：

| 特性 | 映射 (Maps) | 记录 (Records) |
| :--- | :--- | :--- |
| **Java 类比** | `HashMap<String, Object>` | 编译时检查的不可变 DTO |
| **字段检查** | **运行时** (键可能不存在) | **编译时** (字段名拼写错误会被捕获) |
| **键** | 动态，任意类型 | 预定义的原子，在编译后消失 |
| **定义** | 无需预定义 | 必须用 `-record(...)` 在模块内预定义 |
| **底层实现** | 哈希数组映射树 (HAMT) | **元组 (Tuple)** |
| **性能** | 通用，高效 | 访问速度极快（元组的 O(1) 访问）|
| **适用场景** | 处理动态、非结构化数据 (如 JSON) | 定义模块内部结构化的、字段固定的状态 |
| **结论** | **灵活性**：用于与外部世界打交道 | **安全性与性能**：用于系统内部核心数据 |

## Day 3: 函数基础 (Functions) - Erlang 的“智能方法”

在 Erlang 中，函数是核心。但与 Java 的方法相比，Erlang 的函数因“多子句”和“模式匹配”而变得异常强大和富有表现力。

### 3.1 函数与多子句 - 内置在方法签名中的 `switch`

在 Java 中，一个方法只有一个入口和一个方法体。方法内部的逻辑分支由 `if-else` 或 `switch` 控制。

在 Erlang 中，**一个函数可以由多个、同名的“子句 (Clauses)”组成**，只要它们的参数数量（元数）相同。这些子句用分号 `;` 分隔，最后一个用句点 `.` 结束。

**核心类比**：将一组同名/同元数的函数子句，看作是**一个** Java 方法，但这个方法拥有一个内置的、基于参数模式的强大 `switch` 语句。

当你调用这个函数时，Erlang VM 会从上到下，依次用你传入的参数去匹配每个子句的头部。一旦找到第一个完全匹配的子句，就会执行该子句的函数体，并且**不再继续尝试匹配后续子句**。

**示例：阶乘函数**

```java
// Java 版本，使用 if-else
public int factorial(int n) {
    if (n == 0) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}
```

```erlang
% Erlang 版本，使用多子句函数
% 子句 1: 专门处理 N 为 0 的情况
factorial(0) ->
    1; % 分号，表示“如果上面的不匹配，请尝试下一个子句”

% 子句 2: 处理所有其他 N 的情况
factorial(N) ->
    N * factorial(N - 1). % 句点，表示 factorial/1 函数的定义到此结束
```

这里的两个 `factorial/1` 子句共同定义了**一个**函数。当你调用 `factorial(0)` 时，它会匹配第一个子句。当你调用 `factorial(5)` 时，`5` 无法匹配 `0`，于是 VM 尝试下一个子句，成功匹配了 `factorial(N)`，并把 `N` 绑定为 `5`。

这种方式比 `case` 语句更简洁，是 Erlang 代码中最常见的风格。

### 3.2 守卫 (Guards) - 为模式匹配加上 `if` 条件

有时，仅靠模式匹配还不够。你可能想在模式匹配成功的基础上，再增加一些条件判断。这就是“守卫”的用武之地。

**类比**：守卫就像是写在 Java `case` 语句后面的 `if` 条件，或者 `if-else-if` 中的 `&&` 逻辑。

-   守卫跟在函数头参数列表之后，由 `when` 关键字引导。
-   多个守卫条件用逗号 `,` 分隔，代表逻辑 “与” (`andalso`)。
-   守卫表达式必须是**绝对纯净**的，即不能有任何副作用。你只能在其中使用一组受限的内置函数（BIFs），如类型检查 (`is_integer/1`)、比较操作等。

**为什么守卫的功能如此受限？**
这是 Erlang 设计者有意为之。通过限制守卫的能力，保证了函数匹配过程的**纯粹性和高效性**。因为守卫没有副作用，编译器可以对其进行大量优化，并且 VM 在进行子句选择时，可以安全地执行守卫代码而不用担心它会改变任何系统状态。这是一个为速度和安全而做的明智权衡。

```erlang
% 之前的 factorial/1 存在一个 bug：如果输入是负数，它会无限递归
% 让我们用守卫来修复它

% 子句 1: 处理 N 为 0
factorial(0) ->
    1;

% 子句 2: 匹配任意 N，但增加了守卫条件
factorial(N) when N > 0 -> % 只有当 N 是正数时，这个子句才会被选中
    N * factorial(N - 1).

% 如果调用 factorial(-1)，前两个子句都无法匹配，程序会因 function_clause 错误而崩溃
% 这通常是期望的行为（任其崩溃），因为负数的阶乘是无定义的。
```

### 3.3 终极武器：函数头中的模式匹配 + 守卫

将多子句、模式匹配和守卫结合起来，你将得到一种极具表现力的编程范式。

**实际场景：HTTP 请求路由**

想象你在用 Java 写一个简单的 Web 服务器，你需要根据方法和路径来路由请求。

```java
// Java 风格的路由
void handle(HttpRequest request) {
    String method = request.getMethod();
    String path = request.getPath();
    
    if (method.equals("GET") && path.equals("/users")) {
        handleGetUsers(request);
    } else if (method.equals("POST") && path.equals("/users")) {
        handleCreateUser(request);
    } else if (method.equals("GET") && path.startsWith("/users/")) {
        String id = path.substring(7);
        handleGetUserById(request, id);
    } else {
        sendNotFound(request);
    }
}
```

现在，看 Erlang 如何优雅地完成同样的工作：

```erlang
% 我们假设请求被表示为一个元组：{Method, Path, Body}

% 子句1: 匹配 GET /users
handle({get, "/users", _Body}) ->
    handle_get_users();

% 子句2: 匹配 POST /users
handle({post, "/users", Body}) ->
    handle_create_user(Body);

% 子句3: 匹配 GET /users/ID
% 注意这里是如何在模式匹配中“解构”二进制字符串的
handle({get, <<"/users/", Id/binary>>, _Body}) ->
    handle_get_user_by_id(Id);

% 子句4: 捕获所有其他情况的“兜底”子句
handle(_) ->
    send_not_found().
```
在这个 Erlang 版本中，没有 `if`，没有 `switch`，也没有临时变量。路由逻辑完全通过函数头的模式匹配来声明。代码即文档，清晰地描述了它能处理哪几种请求模式，这极大地增强了代码的可读性和可维护性。

### 3.4 匿名函数 (Anonymous Functions) - Java 的 Lambda

与 Java 8+ 的 Lambda 表达式一样，Erlang 也有匿名函数，使用 `fun ... end` 语法。它们在与高阶函数（如 `lists:map/2`）结合使用时非常有用。

```erlang
% Java
List<Integer> numbers = Arrays.asList(1, 2, 3);
List<Integer> doubled = numbers.stream().map(x -> x * 2).collect(Collectors.toList());
```

```erlang
% Erlang
Numbers = [1, 2, 3],
Doubled = lists:map(fun(X) -> X * 2 end, Numbers). % Doubled = [2, 4, 6]

% Erlang的匿名函数也可以捕获外部作用域的变量（闭包）
Factor = 3,
Triple = fun(X) -> X * Factor end,
Triple(5). % 返回 15
```

## Day 4: 控制流 (Control Flow) - 告别 `for` 循环

Erlang 没有 `for` 或 `while` 循环。其所有的流程控制和迭代都通过函数式编程的基石——`case` 表达式和递归来完成。

### 4.1 `case` 表达式 - Erlang 的瑞士军刀

`case` 是 Erlang 中最重要、最强大的控制流结构。

**类比**：它远比 Java 的 `switch` 强大。Java 的 `switch` 只能对简单的值进行分支，而 Erlang 的 `case` 是对**任意数据结构进行模式匹配**。

```erlang
handle_request(Request) ->
    case Request of
        % Java switch 无法做到对数据结构的匹配
        {get, Path, _Headers} ->
            handle_get(Path);

        {post, Path, Body} ->
            handle_post(Path, Body);

        % 带有守卫的匹配
        {delete, Path, _} when is_binary(Path) ->
            handle_delete(Path);

        % 通配符，类似于 Java 的 default
        _ ->
            handle_unknown_request()
    end.
```

### 4.2 `if` 表达式 - `case` 的“语法糖”

Erlang 的 `if` 表达式非常受限，通常不推荐使用。理解其本质的最佳方式是：**`if` 只是 `case` 语句的一种简写形式（语法糖）**。

一个 `if` 表达式：
```erlang
Result = if
    X > 10 -> "big";
    X < 0 -> "negative";
    true -> "normal"
end.
```

完全等价于下面这个 `case` 表达式：
```erlang
Result = case true of % 注意，case 的对象是 true
    _ when X > 10 -> "big";
    _ when X < 0 -> "negative";
    true -> "normal"
end.
```
这个“脱糖”过程解释了 `if` 的两个关键特性：
1.  **条件必须是守卫表达式**：因为它们实际上是 `case` 语句 `when` 子句中的内容。
2.  **必须有 `true` 分支**：因为它对应于 `case` 语句中的 `_` 或 `true` 通配符分支，以确保 `case` 表达式总能匹配成功并返回一个值。

**结论**：既然 `if` 只是一个受限的 `case`，那么在实践中，直接使用功能更全、更清晰的 `case` 表达式通常是更好的选择。

### 4.3 递归与尾递归优化 (TCO) - Erlang 的“循环”

Erlang 使用递归来完成所有迭代任务。对于 Java 开发者来说，这通常会立即引发对 `StackOverflowError` 的担忧。然而，Erlang 通过**尾递归优化 (Tail Call Optimization, TCO)** 解决了这个问题。

#### 什么是尾递归？

当一个函数的**最后一步**是**仅仅调用自身**时，它就是尾递归。这意味着在递归调用之后，没有其他任何操作（如加法、乘法等）。

**一个经典的对比：**

```erlang
% ❌ 普通递归 (非尾递归)
% 这个版本会耗尽栈空间，就像在Java里一样！
sum_bad([H|T]) ->
    H + sum_bad(T); % 递归调用后，还有一个 "+" 操作
sum_bad([]) ->
    0.
% 调用栈: sum_bad([1,2,3]) -> 1 + (sum_bad([2,3])) -> 1 + (2 + (sum_bad([3]))) -> ...
% 每一层递归都必须在栈中等待内部调用的返回结果，以便执行“+”操作。
```

```erlang
% ✅ 尾递归 (被优化成循环)
% 这个版本可以处理无限长的列表，绝不会栈溢出！
sum_ok(List) ->
    sum_ok(List, 0). % 对外暴露的API，调用真正的尾递归辅助函数

% 辅助函数，携带一个累加器 (Accumulator)
sum_ok([], Acc) -> % 当列表为空时，递归结束，返回累加器
    Acc;
sum_ok([H|T], Acc) ->
    % 最后一步是仅仅调用自身，没有其他操作
    sum_ok(T, H + Acc). % 计算在参数中完成，然后“跳转”
```

#### TCO 的 Java 类比：`while` 循环

理解 TCO 最好的方式，就是把它想象成一个 `while(true)` 循环。

`sum_ok/2` 函数的行为，可以类比为以下 Java 代码：

```java
public int sum_ok(List<Integer> list) {
    int acc = 0; // 累加器
    List<Integer> currentList = list;

    while (true) {
        if (currentList.isEmpty()) {
            return acc; // 循环结束
        } else {
            int h = currentList.get(0);
            List<Integer> t = currentList.subList(1, currentList.size());
            
            // 更新“循环变量”，然后继续下一次迭代
            acc = acc + h;
            currentList = t;
        }
    }
}
```
在尾递归中，下一次函数调用并不会创建新的栈帧，而是像 `goto` 语句一样，直接用新参数替换当前栈帧的内容并“跳转”回函数开头。因此，它的空间复杂度是 O(1)，与循环完全相同。

**核心思想**：将计算结果作为参数（累加器）向下传递，而不是在递归返回时组合它们。

### 4.4 列表推导式 (List Comprehensions) - `Stream` API 的语法糖

列表推导式是 Erlang 中一个非常方便的语法糖，用于从一个或多个列表中生成新的列表。

**类比**：它与 Java 8 的 `Stream` API 在思想上非常相似。

```java
// Java Stream API
List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);
List<Integer> result = numbers.stream()
                              .map(x -> x * 2)
                              .filter(x -> x > 5)
                              .collect(Collectors.toList()); // result: [6, 8, 10]
```

```erlang
% Erlang 列表推导式
Numbers = [1, 2, 3, 4, 5],
Result = [X * 2 || X <- Numbers, X * 2 > 5]. % Result: [6, 8, 10]

% 语法解释：
% [ NewElement || Pattern <- SourceList, Condition1, Condition2, ... ]
%  |             |             |                  |
%  |             |             |                  +-- 可选的过滤条件 (filter)
%  |             |             +-- 生成器，从列表中取出元素 (from)
%  |             +-- 映射表达式 (map)
%  +-- 生成新列表的语法
```

列表推导式甚至可以处理多个列表，生成笛卡尔积，这是 `Stream` API 不容易直接做到的：

```erlang
[{X, Y} || X <- [1, 2], Y <- [a, b]].
% 结果: [{1,a}, {1,b}, {2,a}, {2,b}]
```

## Day 5: 并发编程 (Concurrency) - Erlang 的灵魂

欢迎来到 Erlang 的核心领域。忘掉 Java 的 `Thread`, `synchronized`, `Lock`, `Future`。Erlang 的并发模型从根本上就完全不同，它简单、强大且极其可靠。

### 5.1 进程 (Processes) vs. Java 线程 (Threads)

这是你需要理解的最关键的区别。

| 特性 | Java `Thread` | Erlang `Process` | 核心差异与优势 |
| :--- | :--- | :--- | :--- |
| **本质** | 操作系统线程的薄封装 | VM管理的、极其轻量的执行单元 | **Erlang进程不是OS线程**。一个OS线程可以承载成千上万个Erlang进程。 |
| **创建开销** | **重** (MB级别内存, OS调用) | **极轻** (KB级别内存, VM内部操作) | 在一台普通服务器上可以轻松创建**数百万**个Erlang进程，但无法创建数百万个Java线程。 |
| **内存模型** | **共享内存** | **无共享内存 (Share Nothing)** | 这是Erlang可靠性的基石！没有共享内存，就**没有竞态条件、没有死锁**。你永远不需要使用 `synchronized`。 |
| **通信** | 方法调用、共享变量 | **异步消息传递** | 进程间通过发送不可变消息 `!` 进行通信，就像分布式系统中的微服务一样，但效率极高。 |
| **错误隔离** | 一个线程的未捕获异常可能导致整个JVM崩溃 | **完全隔离** | 一个进程的崩溃**绝不会**影响其他进程。 |
| **垃圾回收** | 全局GC暂停 (Stop-the-world) | **每个进程独立GC** | GC只发生在单个进程内部，不会暂停整个系统，保证了系统的低延迟和高响应性。 |

**一句话总结**：Erlang 的“进程”更像是 Java 中的一个带有邮箱的、极其轻量级的 `Object` 或 Actor，而不是一个 `Thread`。

### 5.2 消息传递 - 唯一的通信方式

因为进程间内存完全隔离，消息传递是它们沟通的唯一方式。

-   **发送消息 `!` (bang)**：`Pid ! Message.`
    -   这是一个**异步**操作。它把 `Message` 的一个**副本**放入目标进程 `Pid` 的“邮箱”中，然后立即返回，不会阻塞。
    -   因为发送的是副本，所以符合“无共享”原则，极其安全。
-   **接收消息 `receive`**：
    -   `receive` 块会**阻塞性地**检查当前进程的邮箱。
    -   它使用**模式匹配**来寻找第一个匹配的消息。
    -   找到后，执行对应子句，然后从邮箱中**删除该消息**。
    -   `after` 子句可以设置超时，如果在指定时间内没有收到任何匹配的消息，就会执行 `after` 块。

```erlang
ping_server(PongPid) ->
    PongPid ! {ping, self()}, % 向 Pong 进程发送消息
    receive
        pong -> % 等待一个内容为原子 pong 的消息
            io:format("Ping server received pong!~n");
        _AnyOtherMessage -> % 匹配任何其他消息
            io:format("Ping server received something unexpected.~n")
    after 5000 -> % 如果5秒内没收到任何消息
        io:format("Ping server timed out.~n")
    end.
```

### 5.3 链接与监控 - 管理进程间的生死契约

**链接 (Linking) - “命运共同体”**
-   **做什么**：用 `spawn_link` 创建一个与父进程双向链接的子进程。
-   **行为**：如果其中一个进程因**非正常原因**（即 `Reason` 不是 `normal`）死亡，它会发送一个 `exit` 信号给所有链接到的进程。接收方收到信号后的默认行为是**也立即死亡**。
-   **类比**：两个部署在同一个 K8s Pod 中的容器。它们构成一个原子单元，一个挂了，另一个也应该被销毁重建。
-   **“陷阱”模式 `trap_exit`**：通过调用 `process_flag(trap_exit, true)`，一个进程可以“免疫”连锁死亡。它不再默认死亡，而是将收到的 `exit` 信号转换为一条普通的 `{'EXIT', From, Reason}` 消息放入邮箱。这正是 Supervisor 的工作原理——它通过捕获子进程的死亡信号来管理它们。

**监控 (Monitoring) - “事件订阅者”**
-   **做什么**：用 `monitor` 创建一个从你的进程到目标进程的**单向**监听。
-   **行为**：如果被监控的进程死亡，你的进程只会在邮箱里收到一条无害的 `{'DOWN', Ref, process, Pid, Reason}` 消息。你的进程**绝不会**因此而崩溃。
-   **类比**：你 `watch` 了一个 K8s deployment。当 deployment 发生变化时，你会收到通知，但你自己的程序不会受任何影响。

| 关系 | `spawn_link` (链接) | `monitor` (监控) |
| :--- | :--- | :--- |
| **方向** | 双向 | 单向 |
| **默认行为** | 连锁死亡 | 接收`'DOWN'`消息 |
| **用途** | 构建不可分割的、必须共存亡的组件 | 观察其他进程的状态，但自身不受其影响 |

### 5.4 `gen_server` - 你的第一个 OTP “服务器”

`gen_server` 是 OTP 中最常用的**行为模式 (Behaviour)**。

**类比**：它就像是实现了 `javax.servlet.Servlet` 接口的 `GenericServlet`。你不需要自己编写处理 HTTP 请求、管理生命周期的底层代码，只需要填充 `doGet()`, `doPost()` 等方法。

同样，`gen_server` 为你提供了一个健壮的、标准的服务器进程循环。你不需要自己写 `receive` 循环和状态管理，只需要实现一组标准的回调函数。

```erlang
-module(counter).
-behaviour(gen_server). % 声明“实现”了 gen_server 接口

% --- Client API (公共接口) ---
-export([start_link/0, increment/0, get/0]).

% --- gen_server Callbacks (需要你填充的方法) ---
-export([init/1, handle_call/3, handle_cast/2]).

% 启动服务器
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []).

% --- API 实现 ---

% 同步调用: 需要等待返回值
get() -> gen_server:call(?MODULE, get).

% 异步调用: "发后不理"
increment() -> gen_server:cast(?MODULE, increment).

% --- 回调函数实现 ---

% 1. 初始化服务器状态
init(InitialCount) ->
    {ok, InitialCount}.

% 2. 处理同步调用 (handle_call)
% 对应 gen_server:call
handle_call(get, _From, Count) -> % _From 是调用者的Pid
    {reply, Count, Count}. % {reply, 返回给客户端的值, 服务器的新状态}

% 3. 处理异步调用 (handle_cast)
% 对应 gen_server:cast
handle_cast(increment, Count) ->
    {noreply, Count + 1}. % {noreply, 服务器的新状态}
```

-   **`handle_call`** → **同步** → 类比需要返回值的 service 方法。
-   **`handle_cast`** → **异步** → 类比往 `BlockingQueue` 中 `put` 一个事件。

### 5.5 Supervisor - “任其崩溃”的守护神

Supervisor 是一个特殊的进程，它的唯一工作就是**监控**它的子进程，并在子进程死亡时根据预设的策略将其重启。

**类比**：一个 Java 应用的运维团队。如果一个服务实例挂了，他们不会试图 `ssh` 上去调试，而是直接销毁实例并根据 K8s 的 `deployment` 配置启动一个新的。

```erlang
-module(my_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % 重启策略
    SupFlags = #{
        strategy => one_for_one, % 最常用的策略
        intensity => 5,  % 60秒内最多重启5次
        period => 60
    },

    % 子进程定义
    ChildSpecs = [
        % 定义一个名为 counter 的子进程
        #{
            id => counter,
            start => {counter, start_link, []}, % 如何启动它
            restart => permanent, % 总是重启
            type => worker % 这是一个工作进程
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```
**重启策略 (Restart Strategy)**：
-   **`one_for_one`**：子进程A死了，只重启A。**最常用**。
-   **`one_for_all`**：子进程A死了，**杀死并重启所有其他子进程**。用于子进程间有强依赖的场景。
-   **`rest_for_one`**：子进程A死了，杀死并重启**在A之后启动的**所有兄弟进程。

### 5.6 Application - 打包你的应用

**类比**：OTP Application 就像是一个可部署的 **`.jar` 或 `.war` 文件**。它将你的所有模块、Supervisor 和配置打包成一个独立的、可启动、可停止的单元。

-   **`.app` 文件**：一个元数据文件，描述了应用的名称、版本、包含的模块，以及它依赖的其他应用（如 `kernel`, `stdlib`）。
-   **Application Behaviour**：一个模块，实现 `application` 行为，定义了 `start/2` 和 `stop/1` 回调。
    -   `start/2` 的核心职责就是**启动你应用的顶层 Supervisor**。

```erlang
% my_app.erl
-module(my_app).
-behaviour(application).

-export([start/2, stop/1]).

% 应用启动时，启动我们的顶层 Supervisor
start(_Type, _Args) ->
    my_supervisor:start_link().

stop(_State) ->
    ok.
```
通过这套 OTP 机制，多个独立的“应用”可以组合在一起，形成一个健壮、可维护、容错的大型系统。

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

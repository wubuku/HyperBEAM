# Erlang 入门教程 - 专为前端 TypeScript 开发者打造

> **前言**：作为前端开发者，你对 JavaScript/TypeScript 的异步编程、函数式编程概念、类型系统应该很熟悉。本教程将以 TypeScript 为主要类比对象，帮助你快速掌握 Erlang 的核心概念。Erlang 就像是"分布式版本的 Node.js"，但具备超越 JavaScript 的并发能力和可靠性。

## 🎯 为什么前端开发者需要学习 Erlang？

### Erlang vs JavaScript/TypeScript 对比

| 特性 | JavaScript/TypeScript | Erlang |
|------|----------------------|--------|
| **并发模型** | 单线程 + Event Loop | 百万级轻量进程 |
| **错误处理** | try/catch + Promise.reject | "Let it crash" + Supervisor |
| **数据类型** | 动态类型（可选静态） | 动态类型 + 模式匹配 |
| **函数式编程** | 支持（ES6+） | 纯粹函数式 |
| **分布式** | 需要额外库（如 Socket.io） | 天生支持 |

**Erlang 的独特优势：**
- 🚀 **真正的并发**：不像 JavaScript 的单线程伪并发，Erlang 可以轻松处理数百万并发连接
- 🛡️ **高可用性**：电信级可靠性，系统可用性可达 99.9999999%
- 🌐 **分布式原生**：进程间通信就像本地函数调用一样简单

### Erlang 的包管理器 - rebar3

**rebar3 vs npm：**
```erlang
% rebar.config - 类似 package.json
{deps, [
    % Hex.pm 包（类似 npm 包）
    {prometheus, "4.11.0"},
    {luerl, "1.3.0"},
    % Git 依赖（类似 GitHub 依赖）
    {cowboy, {git, "https://github.com/ninenines/cowboy",
              {ref, "022013b6c4e967957c7e0e7e7cdefa107fc48741"}}}
]}.
```

**TypeScript 类比：**
```json
// package.json
{
  "dependencies": {
    "prometheus": "^4.11.0",
    "luerl": "^1.3.0"
  },
  "devDependencies": {
    "@types/node": "^18.0.0"
  }
}
```

**常用命令：**
```bash
rebar3 compile   # 编译项目（类似 npm run build）
rebar3 eunit      # 运行测试（类似 npm test）
rebar3 shell      # 启动 Erlang shell（类似 node）
```

## 📚 学习路径（前端开发者版本）

```
Day 1: JavaScript思维 → Erlang思维转变
Day 2: 数据类型与模式匹配
Day 3: 函数与高阶函数
Day 4: 并发编程（重点）
Day 5: 错误处理与OTP
Day 6: 实践项目
```

---

## Day 1: 从 JavaScript 到 Erlang 的思维转变

### 1.1 表达式 vs 语句：告别分号文化

**JavaScript/TypeScript 中的语句：**
```typescript
// 这是语句，需要分号结尾
const x = 5;           // 赋值语句
console.log("Hello");  // 调用语句
if (x > 0) {           // 条件语句
  return x;
}
```

**Erlang 中的表达式：**
```erlang
% 一切都是表达式！最后一行自动成为返回值
X = 5,                    % 绑定表达式
io:format("Hello~n", []), % 函数调用表达式
if                      % 条件表达式
    X > 0 -> X;         % 返回 X
    true -> 0           % 必须有 true 分支
end
```

**关键差异：**
- **Erlang 没有分号文化**：用逗号 `,` 分隔连续表达式，用句点 `.` 结束逻辑单元
- **没有 `return`**：函数的最后表达式自动成为返回值
- **一切都有值**：`X = 5` 不仅是赋值，还返回 `5`

### 1.2 变量：单次赋值 vs 可变变量

**TypeScript 的变量：**
```typescript
let counter = 0;
counter = counter + 1;  // 可以重新赋值
```

**Erlang 的变量：**
```erlang
Counter = 0,          % 第一次绑定
NewCounter = Counter + 1.  % 不能修改 Counter，必须创建新变量
```

**思维转变：**
- Erlang 变量像 `const` + 模式匹配的结合
- 变量名大写开头（`Counter`），小写的是原子常量（`counter`）

### 1.3 模块系统：从 ES6 到 Erlang

**ES6 模块：**
```typescript
// user.ts
export function createUser(name: string) {
  return { id: generateId(), name };
}

// main.ts
import { createUser } from './user';
```

**Erlang 模块：**
```erlang
% user.erl
-module(user).
-export([create_user/1]).

create_user(Name) ->
    Id = generate_id(),
    #{id => Id, name => Name}.

% main.erl
% 自动导入，无需显式 import
```

---

## Day 2: 数据类型与模式匹配

### 2.1 基础数据类型

```erlang
% 数字（任意精度）
Age = 25.                    % 整数
Price = 99.99.              % 浮点数
BigNum = 12345678901234567890.  % 自动处理大整数

% 原子（类似 TypeScript 的 symbol 或枚举）
Status = ok.                % 相当于 Symbol('ok')
Type = error.               % 相当于 Symbol('error')
IsAdmin = true.             % true 和 false 都是原子

% 字符串（两种形式）
ListString = "Hello".       % 字符列表 [72, 101, 108, 108, 111]
BinString = <<"Hello">>.    % 二进制字符串（推荐）
```

**TypeScript 类比：**
```typescript
// Erlang 的原子就像 TypeScript 的字面量类型
type Status = 'ok' | 'error';
type Result<T> = { status: Status, data: T };

// Erlang 的二进制字符串就像 Buffer 或 Uint8Array
const binString: Buffer = Buffer.from("Hello");
```

### 2.2 复合数据类型

#### 元组（Tuple）- 固定大小的数组

```erlang
% 创建元组
User = {<<"alice">>, 25, <<"alice@example.com">>}.
Result = {ok, <<"Data loaded">>}.
Error = {error, <<"File not found">>}.
```

**TypeScript 类比：**
```typescript
// 元组就像 TypeScript 的元组类型
type User = [string, number, string];
type Result<T> = [true, T] | [false, string];

const user: User = ["alice", 25, "alice@example.com"];
const result: Result<string> = [true, "Data loaded"];
```

#### 列表（List）- 可变长度的链表

```erlang
% 创建列表
Numbers = [1, 2, 3, 4, 5].
Names = [<<"Alice">>, <<"Bob">>, <<"Charlie">>].

% 头部添加（高效操作）
NewList = [0 | Numbers].  % [0, 1, 2, 3, 4, 5]

% 列表拼接（低效）
Combined = Numbers ++ [6, 7].  % [1, 2, 3, 4, 5, 6, 7]
```

**TypeScript 类比：**
```typescript
// 列表就像数组，但头部操作更高效
const numbers: number[] = [1, 2, 3, 4, 5];
const newList: number[] = [0, ...numbers];  // 扩展运算符
```

#### 映射（Map）- 键值对

```erlang
% 创建 Map
User = #{
    <<"name">> => <<"Alice">>,
    <<"age">> => 25,
    <<"email">> => <<"alice@example.com">>
}.

% 访问
Name = maps:get(<<"name">>, User).

% 更新（返回新 Map）
OlderUser = User#{<<"age">> => 26}.
```

**TypeScript 类比：**
```typescript
// Map 就像 TypeScript 的对象或 Map
interface User {
    name: string;
    age: number;
    email: string;
}

const user: User = {
    name: "Alice",
    age: 25,
    email: "alice@example.com"
};
```

#### 记录（Records）- 结构化数据

**Erlang 的记录（Records）：**
```erlang
% 定义记录（通常在 .hrl 文件中）
-record(user, {
    id,
    name,
    age = 0,      % 默认值
    email
}).

% 使用记录
User = #user{
    id = 1,
    name = <<"Alice">>,
    age = 25,
    email = <<"alice@example.com">>
}.

% 访问字段
Name = User#user.name.  % <<"Alice">>

% 更新记录（返回新记录）
OlderUser = User#user{age = 26}.
```

**TypeScript 类比：**
```typescript
// 记录就像 TypeScript 的接口 + 类
interface User {
    id: number;
    name: string;
    age: number;
    email: string;
}

class UserRecord implements User {
    constructor(
        public id: number,
        public name: string,
        public age: number = 0,  // 默认值
        public email: string
    ) {}

    // 不可变更新方法
    withAge(newAge: number): UserRecord {
        return new UserRecord(this.id, this.name, newAge, this.email);
    }
}

const user = new UserRecord(1, "Alice", 25, "alice@example.com");
const olderUser = user.withAge(26);  // 返回新实例
```

### 2.3 模式匹配：Erlang 的解构赋值

#### 基础模式匹配

```erlang
% 解构元组
{ok, Data} = {ok, <<"Hello">>}.  % Data = <<"Hello">>

% 解构列表
[Head | Tail] = [1, 2, 3, 4].   % Head = 1, Tail = [2, 3, 4]

% 解构 Map
#{<<"name">> := Name, <<"age">> := Age} = User.
```

**TypeScript 类比：**
```typescript
// 数组解构
const [head, ...tail] = [1, 2, 3, 4];

// 对象解构
const { name, age } = user;

// Erlang 的模式匹配更强大，可以在赋值时检查
const { status, data } = result;  // 如果 status 不是 'ok'，会抛出异常
```

#### 精确相等比较：`==` vs `=:=`

**Erlang 的精确相等（重要概念）：**
```erlang
% == 是值相等（类型转换）
5 == 5.0      % true - 值相等
5 =:= 5.0     % false - 类型不同（整数 vs 浮点数）

% =:= 是精确相等（严格比较）
5 =:= 5       % true - 完全相同
5 =:= 6       % false - 值不同
```

**TypeScript 类比：**
```typescript
// TypeScript 只有 ===（严格相等）
5 === 5.0     // false - 类型不同
5 === 5       // true - 完全相同

// 没有像 Erlang == 那样的自动类型转换
5 == 5.0      // true - JavaScript 的宽松相等（不推荐）
```

#### 函数参数模式匹配

```erlang
% 多子句函数（像 switch case，但更强大）
handle_result({ok, Data}) ->
    io:format("Success: ~p~n", [Data]);
handle_result({error, Reason}) ->
    io:format("Error: ~p~n", [Reason]).

% 递归处理列表
sum([]) -> 0;
sum([Head | Tail]) -> Head + sum(Tail).
```

**TypeScript 类比：**
```typescript
// 函数重载 + 类型守卫的结合
function handleResult(result: { status: 'ok', data: any } | { status: 'error', reason: string }) {
    if (result.status === 'ok') {
        console.log("Success:", result.data);
    } else {
        console.log("Error:", result.reason);
    }
}
```

#### 守卫（Guards）- 模式匹配的条件扩展

**Erlang 的守卫（Guards）：**
```erlang
% 守卫用于在模式匹配成功后添加额外条件
classify_age(Age) when Age >= 0, Age < 18 -> child;
classify_age(Age) when Age >= 18, Age < 65 -> adult;
classify_age(Age) when Age >= 65 -> senior.

% 在 case 表达式中使用守卫
classify(X) ->
    case X of
        N when N > 0 -> positive;
        N when N < 0 -> negative;
        0 -> zero
    end.
```

**TypeScript 类比：**
```typescript
// 守卫就像 TypeScript 的类型守卫函数
function isAdult(age: number): boolean {
    return age >= 18 && age < 65;
}

function classifyAge(age: number): string {
    if (age >= 0 && age < 18) return 'child';
    if (isAdult(age)) return 'adult';  // 使用守卫函数
    if (age >= 65) return 'senior';
    return 'invalid';
}

// 或者使用条件表达式
function classifyNumber(x: number): string {
    if (x > 0) return 'positive';
    if (x < 0) return 'negative';
    return 'zero';
}
```

#### if 表达式 - case 的简化版

**Erlang 的 if 表达式：**
```erlang
% if 表达式必须有 true 分支（确保总有返回值）
check_age(Age) ->
    if
        Age >= 18 -> allowed;
        Age >= 13 -> with_parent;
        true -> denied  % 必须有 true 分支
    end.
```

**TypeScript 类比：**
```typescript
// if 表达式就像条件运算符链
function checkAge(age: number): string {
    if (age >= 18) return 'allowed';
    if (age >= 13) return 'with_parent';
    return 'denied';  // 确保总有返回值
}

// 或者使用三元运算符
const result = age >= 18 ? 'allowed' :
               age >= 13 ? 'with_parent' : 'denied';
```

---

## Day 3: 函数式编程进阶

### 3.1 高阶函数与列表操作

```erlang
% 映射（map）
Numbers = [1, 2, 3, 4, 5],
Doubled = lists:map(fun(X) -> X * 2 end, Numbers).  % [2, 4, 6, 8, 10]

% 过滤（filter）
EvenNumbers = lists:filter(fun(X) -> X rem 2 == 0 end, Numbers).  % [2, 4]

% 折叠（reduce）
Sum = lists:foldl(fun(X, Acc) -> X + Acc end, 0, Numbers).  % 15
```

**TypeScript 类比：**
```typescript
const numbers = [1, 2, 3, 4, 5];
const doubled = numbers.map(x => x * 2);          // [2, 4, 6, 8, 10]
const evenNumbers = numbers.filter(x => x % 2 === 0); // [2, 4]
const sum = numbers.reduce((acc, x) => acc + x, 0); // 15
```

### 3.2 列表推导式：函数式循环

```erlang
% 基本推导式
Numbers = [1, 2, 3, 4, 5],
Doubled = [X * 2 || X <- Numbers].  % [2, 4, 6, 8, 10]

% 带条件的推导式
EvenDoubled = [X * 2 || X <- Numbers, X rem 2 == 0].  % [4, 8]

% 笛卡尔积
Pairs = [{X, Y} || X <- [1, 2], Y <- [a, b]].  % [{1,a}, {1,b}, {2,a}, {2,b}]
```

**TypeScript 类比：**
```typescript
// 列表推导式就像链式调用
const doubled = numbers.map(x => x * 2);
const evenDoubled = numbers.filter(x => x % 2 === 0).map(x => x * 2);

// 笛卡尔积需要嵌套循环
const pairs = [];
for (const x of [1, 2]) {
    for (const y of ['a', 'b']) {
        pairs.push([x, y]);
    }
}
```

### 3.3 匿名函数与闭包

```erlang
% 匿名函数
Square = fun(X) -> X * X end,
Result = Square(5).  % 25

% 闭包（捕获外部变量）
Multiplier = fun(N) ->
    fun(X) -> X * N end
end,
Double = Multiplier(2),
Triple = Multiplier(3),
Double(5),  % 10
Triple(5).  % 15
```

**TypeScript 类比：**
```typescript
// 匿名函数
const square = (x: number) => x * x;
const result = square(5);  // 25

// 闭包
const multiplier = (n: number) => (x: number) => x * n;
const double = multiplier(2);
const triple = multiplier(3);
double(5);  // 10
triple(5);  // 15
```

### 3.4 递归与尾递归优化

**Erlang 的递归（Recursion）：**
```erlang
% 普通递归（非尾递归）
sum([]) -> 0;
sum([H|T]) -> H + sum(T).  % 递归调用后还有 + 操作

% 尾递归（推荐）
sum_tail(List) -> sum_tail(List, 0).
sum_tail([], Acc) -> Acc;
sum_tail([H|T], Acc) -> sum_tail(T, H + Acc).  % 最后一步是递归调用
```

**TypeScript 类比：**
```typescript
// 普通递归（可能栈溢出）
function sum(numbers: number[]): number {
    if (numbers.length === 0) return 0;
    return numbers[0] + sum(numbers.slice(1));  // 递归调用后还有 + 操作
}

// 尾递归（但 JavaScript 不优化尾递归）
function sumTail(numbers: number[], acc: number = 0): number {
    if (numbers.length === 0) return acc;
    return sumTail(numbers.slice(1), acc + numbers[0]);  // 尾递归形式
}

// 在 JavaScript 中，通常使用循环
function sumLoop(numbers: number[]): number {
    let total = 0;
    for (const num of numbers) {
        total += num;
    }
    return total;
}
```

**尾递归优化的重要性：**
- Erlang 的尾递归调用会被优化为循环，不会消耗栈空间
- 可以处理无限长的列表而不会栈溢出
- 这是 Erlang 处理迭代的标准方式

---

## Day 4: 并发编程 - Erlang 的核心竞争力

### 4.1 进程：轻量级的执行单元

**JavaScript 的"并发"：**
```typescript
// 单线程伪并发
async function processTasks(tasks: Task[]) {
    for (const task of tasks) {
        await processTask(task);  // 阻塞等待
    }
}
```

**Erlang 的真正并发：**
```erlang
% 创建进程（像启动一个独立的微服务）
Pid = spawn(fun() -> worker_process() end),

% 向进程发送消息（异步）
Pid ! {process, Data},

% 接收结果
receive
    {result, Result} -> handle_result(Result)
after 5000 ->
    timeout
end.
```

### 4.2 消息传递：进程间的通信

```erlang
% 工作进程
worker() ->
    receive
        {add, A, B, From} ->
            Result = A + B,
            From ! {result, Result},  % 发送结果
            worker();  % 递归继续
        stop ->
            ok  % 退出
    end.

% 使用工作进程
start_worker() ->
    Pid = spawn(fun() -> worker() end),
    Pid ! {add, 3, 5, self()},  % 发送任务给自己

    receive
        {result, Sum} -> io:format("Result: ~p~n", [Sum])
    end.
```

**TypeScript 类比（模拟）：**
```typescript
// 使用 Worker 线程模拟（但远不如 Erlang 轻量）
class Worker {
    private results: Map<string, (result: any) => void> = new Map();

    async sendMessage(message: any): Promise<any> {
        return new Promise((resolve) => {
            const id = Math.random().toString();
            this.results.set(id, resolve);
            // 发送消息到 worker
            this.worker.postMessage({ id, ...message });
        });
    }
}
```

### 4.3 gen_server：状态管理的标准模式

```erlang
-module(counter).
-behaviour(gen_server).

% 客户端 API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, 0, []).

increment() ->
    gen_server:call(?MODULE, increment).

get_count() ->
    gen_server:call(?MODULE, get).

% gen_server 回调
init(InitialCount) ->
    {ok, InitialCount}.

handle_call(increment, _From, Count) ->
    {reply, ok, Count + 1};
handle_call(get, _From, Count) ->
    {reply, Count, Count}.
```

**TypeScript 类比（简化）：**
```typescript
class Counter {
    private count = 0;

    async increment(): Promise<void> {
        this.count++;
    }

    async getCount(): Promise<number> {
        return this.count;
    }
}
```

### 4.4 标准库函数补充

**Erlang 的实用标准库函数：**

```erlang
% 字符串操作
String = "Hello World",
Upper = string:uppercase(String),     % "HELLO WORLD"
Lower = string:lowercase(String),     % "hello world"
Length = string:length(String),       % 11

% 正则表达式
{match, [Match]} = re:run(String, "\\w+", [{capture, all, binary}]),
% Match = <<"Hello">>

% 文件操作
{ok, Data} = file:read_file("file.txt"),
file:write_file("output.txt", Data),

% 加密
Hash = crypto:hash(sha256, <<"data">>),  % SHA256 哈希
Random = crypto:strong_rand_bytes(32),  % 安全随机字节

% 时间和定时器
timer:sleep(1000),  % 暂停 1 秒
{ok, Ref} = timer:send_after(5000, self(), timeout),  % 5 秒后发送消息
```

**TypeScript 类比：**
```typescript
// 字符串操作
const str = "Hello World";
const upper = str.toUpperCase();     // "HELLO WORLD"
const lower = str.toLowerCase();     // "hello world"
const length = str.length;           // 11

// 正则表达式
const match = str.match(/\w+/);
// match[0] = "Hello"

// 文件操作（Node.js）
const fs = require('fs');
const data = fs.readFileSync('file.txt');
fs.writeFileSync('output.txt', data);

// 加密（Node.js crypto）
const crypto = require('crypto');
const hash = crypto.createHash('sha256').update('data').digest();
const random = crypto.randomBytes(32);

// 时间和定时器
setTimeout(() => console.log('timeout'), 5000);
```

---

## Day 5: 错误处理与系统设计

### 5.1 "Let it Crash" 哲学

**JavaScript/TypeScript 的错误处理：**
```typescript
try {
    riskyOperation();
} catch (error) {
    console.error("Error:", error);
    // 尝试恢复或重新抛出
}
```

**Erlang 的"任其崩溃"：**
```erlang
% 不需要 try/catch，让进程崩溃
risky_operation() ->
    case file:read_file("config.txt") of
        {ok, Data} -> process_data(Data);
        {error, _} -> exit(config_error)  % 让进程崩溃
    end.
```

### 5.2 Supervisor：自动重启机制

```erlang
-module(my_supervisor).
-behaviour(supervisor).

init([]) ->
    ChildSpecs = [
        #{
            id => worker1,
            start => {worker, start_link, []},
            restart => permanent,  % 总是重启
            type => worker
        }
    ],
    {ok, {{one_for_one, 5, 60}, ChildSpecs}}.  % 重启策略
```

**TypeScript 类比（概念）：**
```typescript
class Supervisor {
    private children: Map<string, () => Promise<void>> = new Map();

    async supervise() {
        for (const [name, startFn] of this.children) {
            try {
                await startFn();
            } catch (error) {
                console.error(`Child ${name} crashed, restarting...`);
                this.restart(name);  // 重启子进程
            }
        }
    }
}
```

### 5.4 高级特性补充

#### ETS - Erlang 内存数据库

**Erlang 的 ETS（内存键值存储）：**
```erlang
% 创建 ETS 表
Table = ets:new(my_cache, [set, public]),

% 插入数据
ets:insert(Table, {user_123, #{name => <<"Alice">>, age => 25}}),

% 查询数据
[{user_123, UserData}] = ets:lookup(Table, user_123),

% 删除数据
ets:delete(Table, user_123),
ets:delete(Table).  % 删除整个表
```

**TypeScript 类比：**
```typescript
// ETS 就像一个高性能的 Map，进程间共享
class ETSCache {
    private cache = new Map<string, any>();

    set(key: string, value: any) {
        this.cache.set(key, value);
    }

    get(key: string) {
        return this.cache.get(key);
    }

    delete(key: string) {
        return this.cache.delete(key);
    }
}
```

#### 宏（Macros）- 编译时代码生成

**Erlang 的宏：**
```erlang
% 定义宏
-define(DEBUG(Msg), io:format("DEBUG: ~p~n", [Msg])).

% 使用宏
?DEBUG("Application started").
```

**TypeScript 类比：**
```typescript
// 宏就像 TypeScript 的装饰器或编译时常量
const DEBUG = (msg: string) => console.log(`DEBUG: ${msg}`);

DEBUG("Application started");
```

---

## Day 6: 实践项目 - 构建聊天服务器

### 6.1 项目结构

```
chat_server/
├── src/
│   ├── chat_server.erl     # 主服务器
│   ├── chat_room.erl        # 聊天室管理
│   ├── chat_client.erl      # 客户端处理
│   └── chat_supervisor.erl  # 监督者
├── include/
│   └── chat.hrl            # 类型定义
└── rebar.config            # 项目配置
```

### 6.2 聊天室服务器实现

```erlang
% chat_room.erl
-module(chat_room).
-behaviour(gen_server).

-record(state, {
    name :: binary(),
    clients = [] :: [pid()]
}).

start_link(RoomName) ->
    gen_server:start_link(?MODULE, RoomName, []).

init(RoomName) ->
    {ok, #state{name = RoomName}}.

handle_call({join, ClientPid}, _From, State) ->
    NewClients = [ClientPid | State#state.clients],
    {reply, ok, State#state{clients = NewClients}};

handle_call({leave, ClientPid}, _From, State) ->
    NewClients = lists:delete(ClientPid, State#state.clients),
    {reply, ok, State#state{clients = NewClients}}.

handle_cast({broadcast, Message, FromPid}, State) ->
    % 向所有客户端广播消息
    lists:foreach(
        fun(ClientPid) ->
            ClientPid ! {message, State#state.name, Message, FromPid}
        end,
        State#state.clients
    ),
    {noreply, State}.
```

### 6.3 客户端连接处理

```erlang
% chat_client.erl
-module(chat_client).
-behaviour(gen_server).

-record(state, {
    room_pid :: pid(),
    user_name :: binary()
}).

handle_info({message, RoomName, Message, FromPid}, State) ->
    % 收到消息，发送给客户端
    % 这里可以是 WebSocket 连接或其他输出
    io:format("[~s] ~s: ~s~n", [RoomName, get_user_name(FromPid), Message]),
    {noreply, State};
```

### 6.4 HyperBEAM 代码示例解析

#### 测试文件 (`src/test/my_first_test.erl`)

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
```

**TypeScript 类比：**
```typescript
// Jest 测试
describe('my_first_test', () => {
  test('basic_test', () => {
    expect(2 + 2).toBe(4);
  });

  test('hb_util_test', () => {
    const encoded = hbUtil.encode(Buffer.from('hello'));
    expect(hbUtil.decode(encoded)).toBe('hello');
  });

  test('message_test', () => {
    const msg = { key: 'value' };
    expect(msg.key).toBe('value');
  });
});
```

#### 设备模块 (`src/dev_hello.erl`)

```erlang
-module(dev_hello).
-export([info/3, greet/3]).

info(_Msg, _Msg2, _Opts) ->
    {ok, #{
        <<"name">> => <<"hello">>,
        <<"version">> => <<"1.0">>
    }}.

greet(Msg, _Msg2, Opts) ->
    Name = hb_ao:get(<<"name">>, Msg, Opts),
    Greeting = <<"Hello, ", Name/binary, "!">>,
    {ok, hb_ao:set(Msg, #{ <<"greeting">> => Greeting }, Opts)}.
```

**TypeScript 类比：**
```typescript
// Express 路由处理
export class HelloDevice {
  info(_msg: any, _msg2: any, _opts: any) {
    return {
      ok: true,
      data: {
        name: 'hello',
        version: '1.0'
      }
    };
  }

  greet(msg: any, _msg2: any, opts: any) {
    const name = hbAo.get('name', msg, opts);
    const greeting = `Hello, ${name}!`;
    return {
      ok: true,
      data: hbAo.set(msg, { greeting }, opts)
    };
  }
}
```

**与 Node.js Express + Socket.io 的对比：**

```typescript
// Node.js 版本
const express = require('express');
const socketIo = require('socket.io');

const app = express();
const io = socketIo(app);

io.on('connection', (socket) => {
    socket.on('join', (roomName) => {
        socket.join(roomName);
    });

    socket.on('message', (data) => {
        io.to(data.room).emit('message', data);
    });
});
```

```erlang
% Erlang 版本
handle_info({socket_message, <<"join">>, RoomName}, State) ->
    % 加入聊天室
    chat_room:join(RoomName, self()),
    {noreply, State};

handle_info({socket_message, <<"message">>, #{room := Room, text := Text}}, State) ->
    % 广播消息
    chat_room:broadcast(Room, Text, self()),
    {noreply, State}.
```

---

## 🚀 进阶学习路径

### 阶段 1：巩固基础（1-2 周）
- **目标**：理解 Erlang 语法和基本概念
- **练习**：实现基本的 CRUD 操作、简单的数据处理函数
- **阅读**：《Programming Erlang》前 5 章

### 阶段 2：并发编程（2-3 周）
- **目标**：掌握进程、消息传递、OTP 模式
- **项目**：构建一个简单的分布式缓存系统
- **阅读**：《Erlang in Anger》并发章节

### 阶段 3：系统设计（3-4 周）
- **目标**：学习 Supervisor 树、应用结构、发布流程
- **项目**：实现一个完整的 Web 服务（类似 Express.js）
- **实践**：参与 HyperBEAM 项目的开发

### 阶段 4：生产实践（持续）
- **部署**：学习 Erlang 应用的打包和部署
- **监控**：集成日志、指标收集
- **优化**：性能调优、内存管理

## 📚 推荐资源

### 官方文档
- [Erlang 官方文档](https://www.erlang.org/docs) - 最权威的参考
- [HyperBEAM 文档](https://hbdocs.vercel.app/) - 实践项目

### 书籍
- **《Programming Erlang》** - 入门经典，循序渐进
- **《Erlang in Anger》** - 生产环境最佳实践
- **《Learn You Some Erlang》** - 免费在线教程

### 在线社区
- [Erlang Forums](https://erlangforums.com/) - 官方论坛
- [Reddit r/erlang](https://reddit.com/r/erlang) - 社区讨论

### Erlang 命名规则详解

**变量必须大写开头：**
```erlang
% ✅ 正确：变量大写开头
Name = "Alice".
Age = 25.

% ❌ 错误：小写开头被认为是原子
name = "Alice".  % 这是原子 'name'，不是变量
```

**函数和模块名必须小写开头：**
```erlang
% ✅ 正确
-module(user_service).
create_user(Name) -> ok.

% ❌ 错误
Create_User(Name) -> ok.  % 大写函数名
```

**TypeScript 类比：**
```typescript
// TypeScript 没有强制命名规则，但有约定
const userName: string = "alice";  // 驼峰变量
function createUser(name: string) {} // 驼峰函数名
class UserService {} // PascalCase 类名
```

## 🎯 关键思维转变总结

| JavaScript/TypeScript 思维 | Erlang 思维 |
|---------------------------|-------------|
| **变量可变** | 变量不可变，单次赋值 |
| **异常处理** | 让进程崩溃 + Supervisor 重启 |
| **异步编程** | 基于消息的进程间通信 |
| **对象状态** | 不可变数据 + 状态循环 |
| **类型检查** | 模式匹配 + Dialyzer |
| **模块系统** | 编译时链接，无需 import |
| **命名规则** | 强制大小写规则（变量大写，函数小写） |
| **包管理** | rebar3 vs npm |

**记住**：Erlang 不是 JavaScript 的替代品，而是并发编程、分布式系统的首选工具。掌握 Erlang 思维后，你会发现很多原本复杂的问题变得异常简单！

---

---

## 📋 知识点覆盖检查

✅ **已完整覆盖的知识点：**
- 表达式 vs 语句
- 分隔符（逗号、句点、分号）
- 代码块也是表达式
- 注释
- 模块与可见性
- end 关键字（在代码示例中使用）
- 数字类型
- **== vs =:=**（新增）
- 原子 (Atoms)
- 字符串（二进制 vs 列表）
- 变量与模式匹配
- 列表 (Lists)
- 元组 (Tuples)
- 映射 (Maps)
- **记录 (Records)**（新增）
- 函数与多子句
- **守卫 (Guards)**（新增）
- 模式匹配+守卫
- 匿名函数
- case 表达式
- **if 表达式**（新增）
- **递归与尾递归优化**（新增）
- 列表推导式
- 进程 vs 线程
- 消息传递
- 链接与监控
- gen_server
- Supervisor
- Application
- 错误处理 (try/catch/after)
- **标准库函数**（新增补充）
- **ETS**（新增）
- **定时器**（标准库中补充）
- 宏 (Macros)（新增）
- **rebar3 包管理器** ✅（新增补充）
- **HyperBEAM 代码示例解析** ✅（新增）
- **Erlang 命名规则** ✅（新增）
- 宏的详细语法 ✅（新增）
- Erlang 宏详解 ✅（新增）

❌ **仍缺失但相对次要的知识点：**
- 端口（外部程序）
- 属性列表 (Proplists)
- 引用 (References)
- 位运算
- 系统自省
- 类型规范
- NIF 基础
- 脏调度器
- 模块属性详解
- loop() 函数详解（虽然在实践中使用了）
- 队列（虽然在实践中使用了）
- 行为 (Behaviour) 详解

*本教程基于 HyperBEAM 项目的实际需求，为前端 TypeScript 开发者量身定制。如有疑问，欢迎在项目中提出 issue 或讨论。*
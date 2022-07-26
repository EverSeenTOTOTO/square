# square

一个玩具语言，语法模仿lisp，使用js编写，兼具趣味性和表现力。

[English](./README.md)

## Q & A

1. 这是什么？

    **square**是一个使用js编写的小型语言，语法与lisp类似。语言设计上尽可能避免`<Ctrl>`、`<Shift>`控制键的使用，因此除了运算符之外语言主要使用`.`、`[]`、`;`和`/`结构化代码，同时借鉴了`...`数组展开赋值、`..`连接运算等精简优雅的语法，提高生产力。

    **square**实现上采用普通的递归下降方法，总代码量不超过1000行，具有相当的可读性和拓展性。语言内置了JS的运行环境，可以提供70~80%的JS功能。

2. 为什么叫做**square**？

    **square**使用`[]`代替了lisp方言常见的`()`，因此它看起来真得很“方”。

## 变量

```lisp
[= a 'foo']
[= b 2]
[= c [.. 1 10]]
[= e 0b11.11]
[= f 0xff.ff]
[= [x] [1 2]]
[= [. x] [vec 1 2 3]] ; x = 2
[= [... x] [vec 1 2 3]] ; x = 3
[= [. [x] ... y] [vec 1 [vec 2] 3 4]] ; x = 2, y = 4
[= [. x . ... [y] ... [. z .]] [vec 1 2 3 [vec 4] [vec 5 6 7]]] ; x = 2, y = 4, z = 6
```

## 控制流

```lisp
[match a
  [[and [> q a] [< p a]] foo]
  [[[regex '[a-z]+' 'gi'].test a] [bar]]]

[if true true]
[if true true false]

[begin
  [= i 0]
  [while [< i 10]
    [console.log i]
    [+= i 1]]]
```

## 函数

```lisp
[= foo /[] 2]

[foo]

[= foo /[. z] [console.log [.. z 4]]

[foo 'ignored' 0]

[= fib /[n] [begin
  [console.log n]
  [match n
    [[< n 0] 0]
    [[< n 2] 1]
    [+
      [fib [- n 1]]
      [fib [- n 2]]]]]]

[console.log [[.. 1 10].map /[x] [fib x]]]

[= cons /[x g] /[f] [f x g]]
[= car /[pair] [pair /[x .] x]]
[= cdr /[pair] [pair /[. g] [g]]]

[= ones [cons 1 /[] ones]]

[console.log [car ones]]
[console.log [car [cdr [cdr ones]]]]
```

## 延续

```lisp
[= cc /[] [callcc /[cc] [cc cc]]]

[begin
  [= start [cc]]
  [console.log 'loop']
  [start start]]

[= gen /[yield] [begin
  [[.. 1 10].forEach /[x] [callcc /[cc] [yield [x cc]]]]]]

[begin
  [= p [callcc /[outcc] [gen outcc]]]
  [if [Array.isArray p]
    [begin
      [= [value incc] p]
      [console.log value]
      [incc]]]]
```

## 数据结构

```lisp
[= stack /[vec] [begin
  [= this [obj]]
  [= this.vec [vec.slice 0]]
  [= this.clear /[] [= this.vec []]]
  [= this.push /[x] [begin
    [= this.vec [.. this.vec [x]]]]]
  [= this.pop /[] [begin
    [= [... x] this.vec]
    [this.vec.splice [- this.vec.length 1] 1]
    x]]
  this]]

[= v [vec 1 2 3]]
[= s [stack v]]
[= x [s.pop]]
[s.clear]
[s.push 1]
[s.push 0]
[= y [s.pop]]

[x y v] ; [3 0 [1 2 3]]
```

## 注释

```lisp
; 单行注释
; 行内注释 ;
```

## 模块

```lisp
[= http [import 'http']] ; actually require('http')
[= demo [import 'module.sq']]

[[importDyn 'path'].then /[path] [path.resolve '.']] ; dynamic import()

[export add /[a b] [+ a b]]

[= sub /[a b] [- a b]]

[export sub]
```

## 示例

```lisp
[= http [import'http']]
[= path [import 'path']]
[= fs [import 'fs']]

[= self [path.resolve 'examples/2.sq']]
[= stream [fs.createReadStream self]]

; line comment
; line comment

[= app /[req res] [begin
    [console.log ; inline comment ; req.url]
    [stream.on 'end' /[] [res.end]]
    [stream.pipe res]]]

[= server [http.createServer app]]

[setTimeout /[] [begin
  [console.warn '---- NOW TURNING OFF SERVER ----']
  [server.close]]
  4000]

[server.listen 8080 /[] [console.log '---- SERVER LISTENING ON 8080 ----']]
```

## BNF范式

```bnf
<lit> ::= <num> | <str> | <bool>
<unOp> ::= '!' | '-'
<binOp> ::= '-' | '..' | '/' | '+' | '*' | '>' | '<' | '%' | '^' | '==' | '!=' | ...
<dot> ::= '.' <id> <dot>

<expandItem> ::= '.' | '...' | <id> | <expand>
<expand> ::= '[' <expandItem>+ ']'

<func> ::= '/' (<expand> | '['']') <expr>

<unOpExpr> ::= '[' <unOp> <expr>  ']'

<assign> ::= '[' '=' (<id> <dot>* | <expand>) <expr> ']'

<binOpExpr> ::= '[' <binOp> <expr> <expr> ']'
<call> ::= '[' <expr>* ']' 

<expr> ::= (<id> | <lit> | <func> | <assign> | <binOpExpr> | <unOpExpr> | <call>) <dot>*
```

## 中间代码

square 使用一个简化的三地址码作为中间代码格式，可用的指令有：

| IR 指令 | 描述 | 示例 |
| ---- | ---- | ---- |
| `x = y op z` | 二元操作 | `t0 = a + b` |
| `x = op y` | 一元操作 | `t1 = !t0` |
| `x = y` | 赋值 | `x = y` |
| `jump L` | 无条件转移 | `jump 'L1'` |
| `test x L` | 条件转移 | `test x 'L2'` |
| `param x` | 参数传递 | `param y1` |
| `call p, n` | 使用`n`个参数调用过程`p` | `call p, 2` |
| `ret` | 返回 | `ret r0` |

## 指令集架构

Square 首先被翻译为IR然后再被翻译为汇编格式。其汇编代码将被直接求值而非是转译为某种二进制格式，因此所谓的汇编部分并非真实的指令集，仅仅是一种模拟。

其“指令集”一共只有7种指令：

| 指令 | 格式 | 描述 | 示例 |
| ---- | ---- | ---- | ---- |
| `move` | `move <$1> <$2>` | 将 `$2` 的内容赋给 `$1`, `$1` 必须是寄存器而 `$2` 可以是寄存器或立即数 | `move t0 ra` |
| `save` | `save <reg>` | 将 `reg` 内容入栈 | `save t0` |
| `load` | `load <reg>` | 将栈顶内容弹入 `reg` | `load t0` |
| `label` | `label <string>` | 创建一个名为 `<string>` 的`label` | `label 'loop'` |
| `test` | `test <$1> <$2> <label>` |  如果 `$2` 等于 `$1`，跳转至 `label`， `$1` 和 `$2` 既可以是寄存器名也可以是立即数 | `test t0 t1 'loop'`
| `jump` | `jump <label>` | 跳转至 `label` | `jump 'done'` |
| `perform` | `perform <op> ...` | 调用外部函数`<op>`，这允许我们直接利用js运行时而非实现完整的模拟器 | `perform t1 t2` |

> 如果你看过《SICP》的话，也许会对这些指令感到熟悉。

一共有10个可见寄存器和1个不可见寄存器`pc`：

+ `pc`: 程序计数器
+ `fp`: 帧指针
+ `ra`: 返回地址，也用于存放返回值
+ `a0~a1`: 函数参数
+ `t0~t2`: 临时寄存器
+ `s0-s2`: 保存寄存器

在子过程中始终优先考虑临时寄存器，如果不够用，则会使用保存寄存器来存放局部变量。但在那之前，子过程会将保存寄存器之前的值暂存到栈里，并在过程调用结束时恢复。如果父过程此前使用了临时寄存器，则在调用子过程之前保存临时寄存器的状态是父过程的责任。（和MIPS类似）

寄存器的数量很少，因为square的目的不在于多高效，而在于学习实践，因此制造一个寄存器溢出的典型场景也许更有意义。


# square

一个玩具语言，语法模仿lisp，使用js编写，兼具趣味性和生产力。

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
[= [. x] [1 2 3]] ; x = 2
[= [... x] [1 2 3]] ; x = 3
[= [. [x] ... y] [1 [2] 3 4]] ; x = 2, y = 4
[= [. x . ... [y] ... [. z .]] [1, 2, 3, [4], [5, 6, 7]]] ; x = 2, y = 4, z = 6

```

## 控制流

```lisp
[match a
  [[and [> q a] [< p a]] foo]
  [[[regex '[a-z]+' 'g'].test a] [bar]]]

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
  [= this [Object]]
  [= this.vec [vec.slice 0]]
  [= this.clear /[] [= this.vec []]]
  [= this.push /[x] [begin 
    [= this.vec [.. this.vec [x]]]]]
  [= this.pop /[] [begin
    [= [... x] this.vec]
    [this.vec.splice [- this.vec.length 1] 1]
    x]]
  this]]

[= v [1 2 3]]
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
[= squareModule [import 'module.sq']]

[[importDyn 'path'].then /[path] [path.resolve '.']] ; dynamic import

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

## BNF

```bnf
<lit> ::= <num> | <str> | <bool>
<unOp> ::= '!' | '-'
<binOp> ::= '-' | '..' | '/' | '+' | '*' | '>' | '<' | '%' | '^' | '==' | '!=' | ...
<dot> ::= '.' <id> <dot>

<expandItem> ::= '.' | '...' | <id> | <expand>
<expand> ::= '[' <expandItem>+ ']'

<func> ::= '/' (<expand> | '['']') <expr>

<unOpExpr> ::= <unOp> <expr> 

<assign> ::= '[' '=' (<id> <dot>* | <expand>) <expr> ']'

<binOpExpr> ::= '[' <binOp> <expr> <expr> ']'
<call> ::= '[' <expr>* ']' 

<expr> ::= (<id> | <lit> | <func> | <assign> | <binOpExpr> | <unOpExpr> | <call>) <dot>*
```

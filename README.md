# square

A toy lisp-like language written in js, aims to be both fun and expressive.

[中文](./README-CN.md)

## Q & A

1. What's this?

    **Square** is a small language written in JS with a lisp-like grammer. The language's design goal is to use as few control keys like `<Ctrl>`, `<Shift>` as possible, so the code structure is determined by `.`, `[]`, `;` and `/`. In addition, there are builtin supports for expanding vectors using `...`, concat operator `..` and other elegant simple syntax, which tends to be more productive.

    **Square** is parsed by a recursive descent algorithm, the source code is less than 1000 lines so it's easy to read and extend. Square also has an embedded JS runtime that provides 70~80% JS functionality.

2. Why called **square**?

    **Square** use `[]` rather than `()` which appears in most lisp languages, so it looks really "square".

## Variable

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

## Control flow

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

## Function

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

## Continuation

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

## Structure

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

## Comment

```lisp
; 单行注释
; 行内注释 ;
```

## Module

```lisp
[= http [import 'http']] ; actually require('http')
[= demo [import 'module.sq']]

[[importDyn 'path'].then /[path] [path.resolve '.']] ; dynamic import()

[export add /[a b] [+ a b]]

[= sub /[a b] [- a b]]

[export sub]
```

## Example

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

## ISA

Square is first compiled to a custom assemble code before it is executed. The assemble code is than directly evaluated rather than got translate to a binary format, so the assemble format is actually not a real instruction set but a simulation.

The "ISA" has only 7 instructions:

| instructions | format | description | example |
| ---- | ---- | ---- | ---- |
| `reg` | `reg <reg>` | get the value at `[<reg>]` in stack or continuation chain, only available for `sp`, `fp` and `cp` | `reg sp` |
| `move` | `move <$1> <$2>` | move the value of `$2` to `$1`, `$1` must be a register while `$2` can be a register or an immediate value | `move t0 ra` |
| `save` | `save <reg>` | push the content of `reg` into stack | `save t0` |
| `load` | `load <reg>` | pop stack and save the value into `reg` | `load t0` |
| `test` | `test <$1> <$2> <label>` |  if `$2` is not equal to `$1`, jump to `label`, `$1` and `$2` can be either a register or an immediate value | `test t0 t1 'loop'`
| `jump` | `jump <label>` | jump to `label` | `jump 'done'` |
| `perform` | `perform ...` | perform an external call, this allows us to take the advantage of js runtime without implementing a full emulator | `perform t1 t2` |

> If you have read _Structure and Implemention of Computer Programs_, you may find these instructions familiar.

There are 10 visible registers and one invisible register `pc`:

+ `pc`: Program Counter
+ `sp`: Stack pointer, in our simulated machine, the stack is a tuple, so `sp` is an array index instead of a memory address
+ `fp`: Frame pointer, also an array index
+ `cp`: Continuation chain pointer, mainly used for `callcc`
+ `ra`: Return address, also used to save return value
+ `a0~a1`: Function arguments, used in order, as well as `t0~t1`, `s0-s1`
+ `t0~t1`: Temporaries
+ `s0-s1`: Save register

Temporary registers are always first considered in callee procedure, if not enough, save registers will be used to save temporary values. But before that, the **callee** procedure must save the previous value of a save register in stack, and restore them when it is done. If the caller used temporary registers before calling the callee, then it is the caller's own responsibility to save temporary registers state before that call.

The number of registers is very small, because square's aim is not to be efficient, but to be educational, so an overflow of registers might be more typical.

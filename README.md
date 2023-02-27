# square

A toy lisp-like language written in Rust, aims to be both fun and expressive.

## Q & A

1. What's this?

    **Square** is a small language written in Rust with a lisp-like grammer. The language's design goal is to use as few control keys like `<Ctrl>`, `<Shift>` as possible, so the code structure is determined by `.`, `[]`, `;` and `/`. In addition, there are builtin supports for expanding vectors using `...`, concat operator `..` and other elegant simple syntax, which tends to be more productive.

    **Square** is parsed by a recursive descent algorithm, the source code is less than 1000 lines so it's easy to read and extend.
  
2. Why called **square**?

    **Square** use `[]` rather than `()` which appears in most lisp languages, so it looks really "square".

## Variable

```lisp
[= x 2]
[= x [.. 1 10]]
[= [x y] [1 2]] ; x = 1, y = 2
[= [. x] [1 2 3]] ; x = 2
[= [... x] [1 2 3]] ; x = 3
[= [. [x] ... y] [1 [2] 3 4 5]] ; x = 2, y = 5
```

## Control flow

```lisp
[match expr
  [[and [> q .] [< p .]] foo]
  [[[regex '[a-z]+' 'gi'].test .] bar]]

[if true true]
[if true true false]

[begin 
  [= i 0]
  [while [< i 10]
    [print i]
    [+= i 1]]]
```

## Function

```lisp
[= foo /[] 2]

[foo]

[= foo /[. z] [print [.. z 4]]

[foo 'ignored' 0]

[= fib /[n] [begin
  [print n]
  [match n
    [[< . 2] 1]
    [+
      [fib [- n 1]] 
      [fib [- n 2]]]]]]

[print [[.. 1 10].map /[x] [fib x]]]
```

## Coroutine

```lisp
[= genFib /[n]
  [co.wrap /[]
    [= [a b] [1 1]]
    [while [<= a n]
      [co.yield a]
      [= [a b] [b [+ a b]]]]]]

[[genFib 100].forEach print]
```

## Structure

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
; comment
; inline ;
```

## BNF

```bnf
<lit> ::= <num> | <str> | <bool>
<unOp> ::= '!' | '-'
<binOp> ::= '-' | '..' | '/' | '+' | '*' | '>' | '<' | '%' | '^' | '==' | '!=' | ...
<dot> ::= '.' <id> <dot>

<expandItem> ::= '.' | '...' | <id> | <expand>
<expand> ::= '[' <expandItem>* ']'

<func> ::= '/' <expand> <expr>

<unOpExpr> ::= <unOp> <expr> 

<assign> ::= '[' '=' (<id> <dot>* | <expand>) <expr> ']'

<binOpExpr> ::= '[' <binOp> <expr> <expr> ']'

<expr> ::= (<id> | <lit> | <func> | <assign> | <binOpExpr> | <unOpExpr> | <call>) <dot>*
<call> ::= '[' <expr>* ']' 
```

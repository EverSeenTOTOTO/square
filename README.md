# square

A toy Lisp-like language written in Rust, aims to be both fun and expressive.

## Q & A

1.  What's this?

    **Square** is a neat Lisp-inspired language, crafted with Rust. It's all about simplicity and cleanliness, aiming to reduce the reliance on control keys like `<Ctrl>` and `<Shift>`. That's why it employs symbols like `.`, `[]`, `;`, and `/` to structure the code.

    **Square** is parsed by a recursive descent algorithm, compiled to a custom instruction set, and operates on a stack-based virtual machine. The source code, written with a Rust `no_std` environment, spans less than 3000 lines and can be compiled to a incredibly lightweight wasm file.

2.  Why called **square**?

    **Square** use `[]` rather than `()` which is adopted in most lisp languages, giving it a truly "square" aesthetic that matches its name.

## Variable

```lisp
; basic
[= x 2] ; x = 2
[= x [.. 1 4]] ; x = [1 2 3 4], `..` can concat strings, ranges and vectors

; expansion
[= [x y] [1 2]] ; x = 1, y = 2
[= [. x] [1 2 3]] ; x = 2, `.` is a placehoder that must occupy one position

; convenient placehoders in expansion
[= [... x] [.. 1 10]] ; x = 10, `...` is a placehoder that can occupy zero or as many positions as possible
[= [x ... y] [1]] ; x = 1, y = 1
[= [. [x] ... y] [1 [2] 3 4 5]] ; x = 2, y = 5
```

## Control flow

```lisp
; match
[match x
  [[> 42 x] foo]
  [[[regex '[a-z]+' 'gi'].test x] bar]]

; branch
[if true true]
[if true true false]

; block
[begin 
  [= i 0]
  [while [< i 10]
    [print i]
    [+= i 1]]]
```

## Function

```lisp
; function starts with /[ as it looks like λ
[= foo /[] 2]

[foo]

; expansion in parameter
[= foo /[. z] [print [.. z 4]]]

[foo 'ignored' 0]

[= fib /[n] [begin
  [print n]
  [match n
    [[< n 2] 1]
    [+
      [fib [- n 1]] 
      [fib [- n 2]]]]]]

[print [[.. 1 10].map /[x] [fib x]]]
```

## Coroutine

```lisp
; similar to Lua coroutine
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
; use built-in function `obj` to create an object
[= stack /[vec] [begin 
  [= this [obj]] ; `this` is just a variable name

  [= this.vec vec]

  [= this.clear /[] [= this.vec []]]

  [= this.push /[x] [begin 
    [= this.vec [.. this.vec [x]]]]]

  [= this.pop /[] [begin
    [= [... x] this.vec]
    [= this.vec [this.vec.slice 0 -1]]
    x]]

  this]]

[= v [1 2 3]]
[= s [stack v]]
[= x [s.pop]] ; x = 3
[s.clear]
[s.push 42]
[= y [s.pop]] ; y = 42
```

## Comment

```lisp
; comment
; inline ;
```

## BNF

    expand -> '[' ('.' | '..' | '...' | id | expand)+ ']'
    fn -> '/' expand expr

    prop ->  . id
    assign -> '=' (id prop* | expand) expr

    op -> op expr expr*

    call -> '[' assign | op | expr* ']'

    dot -> (id | call) prop*

    expr -> fn | num | str | dot

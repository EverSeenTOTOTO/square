# square

A toy Lisp-like language written in Rust, aims to be both fun and expressive.

## Q & A

1.  What's this?

    **Square** is a neat Lisp-inspired language, crafted with Rust. It's all about simplicity and cleanliness, aiming to reduce the reliance on control keys like `<Ctrl>` and `<Shift>`. That's why it employs symbols like `.`, `[]`, `;`, and `/` to structure the code.

    **Square** is parsed by a recursive descent algorithm, compiled to a custom instruction set, and operates on a stack-based virtual machine. The source code, written with a Rust `no_std` environment and can be compiled to a incredibly lightweight wasm file.

2.  Why called **square**?

    **Square** use `[]` rather than `()` which is adopted in most lisp languages, giving it a truly "square" aesthetic that matches its name.

## Variable

```lisp
; basic
[= x 2] ; x = 2

[= x [.. 1 4]] ; x = [1 2 3 4], `..` can concat strings, ranges and vectors

; expansion
[= [x y] [vec 1 2]] ; x = 1, y = 2

; convenient placehoders in expansion
[= [. x] [vec 1 2 3]] ; x = 2, `.` is a placehoder that MUST occupy one position
[= [... x] [.. 1 10]] ; x = 10, `...` is a placehoder that can occupy zero or as many positions as possible
[= [. [x] ... y] [vec 1 [vec 2] 3 4 5]] ; x = 2, y = 5
```

## Control flow

```lisp
; match
[match x
  [[> -2.3e-2 x] foo]
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

## Function(first class)

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

## Continuation(first class)

```lisp
[= gen /[yield]
    [begin
        [= i 0]
        [while [< i 10]
            [callcc /[cc]
                [yield [vec i cc]]]]]]

[= innerCc nil]

[= next /[g]
    [if [== [typeof innerCc] 'fn']
        [innerCc]
        [begin
            [= p [callcc /[cc] [g cc]]]
            [if [== [typeof p] 'vec']
                [begin
                    [= [i innerCc] p]
                    [print i]]]]]]
                    
[next gen]
[next gen]
[next gen]
[next gen]
[next gen]
```

## Structure

```lisp
[= stack [obj
    ['new' /[initial] [= this.vec initial]]
    ['clear' /[] [= this.vec [vec]]]
    ['push' /[x] [= this.vec [.. this.vec [x]]]]
    ['pop' /[] [begin
        [= [... last] this.vec]
        [= this.vec [this.vec.slice 0 -1]]
        last]]]]

[= s [stack.new [vec 1 2 3]]]
[= x [s.pop]] ; x = 3
[s.push 42] ; s = [1 2 42]
```

## Comment

```lisp
; comment
; inline ;
```

## BNF

    hex -> [0-9a-fA-F]
    ascii_escape -> x hex hex
    unicode_escape -> u hex{4}
    single_escape -> ['\\bfnrtv]

    escape -> \\ ascii_escape | unicode_escape | single_escape

    str -> ' ([^'\\\r\n] | escape )* '
    num -> -?[0-9]+(\.[0-9]+)?(e|E-?[0-9]+)?

    expand -> [ (. | ... | id | expand)+ ]
    fn -> / expand expr

    prop ->  . id
    assign -> = (expand | id prop*) expr

    op -> (binary_op expr expr*) | (binary_assign_op dot expr*)

    call -> [ assign | op | expr* ]

    dot -> (id | call) prop*

    expr -> fn | str | -?(num | dot)

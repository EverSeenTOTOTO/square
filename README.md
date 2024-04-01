# square

A toy Lisp-style language written in Rust, supports first-class function and continuation, aims to be both fun and expressive.

[Playground](https://www.everseenflash.com/CS/Square.html#playground)

## Q & A

1.  What's this?

    **Square** is a neat Lisp-inspired language, crafted with Rust. It's all about simplicity and cleanliness, aiming to reduce the reliance on control keys like `<Ctrl>` and `<Shift>`. That's why it only uses symbols like `.`, `[]`, `;`, and `/` to structure the code.

    **Square** is parsed by a recursive descent algorithm, compiled to a custom (high level) instruction set, and operates on a stack-based virtual machine.

2.  Why called **square**?

    **Square** use `[]` rather than `()` which is adopted in most Scheme dialects, giving it a truly "square" shape that matches its name.

## Variable

```lisp
; basic
[let x 2] ; x = 2

; expansion
[let [x y] [vec 1 2]] ; x = 1, y = 2

; convenient placehoders in expansion
[let [. x] [vec 1 2 3]] ; x = 2, `.` is a placehoder that MUST occupy one position
[let [... x] [1 2 3 4]] ; x = 4, `...` is a placehoder that can occupy zero or as many positions as possible
[let [. [x] ... y] [vec 1 [vec 2] 3 4 5]] ; x = 2, y = 5
```

## Control flow

```lisp
; match
[cond
  [[> -2.3e-2 x] foo]
  [[[regex '[a-z]+' 'gi'].test x] bar]]

; branch
[if true true]
[if true true false]

; block
[begin 
  [let i 0]
  [while [< i 10]
    [print i]
    [+= i 1]]]
```

## Function(first class)

```lisp
; function starts with /[ as it looks like λ
[let foo /[] 2]

[foo]

; expansion in parameter
[let foo /[[.] z] [print z]]

[foo [vec 'ignored'] 0]

[let fib /[n] 
  [if [<= n 2] 
    1
    [+ [fib [- n 1]] [fib [- n 2]]]]]

[println [fib 30]]
```

## Continuation(first class)

```lisp
[let gen /[yield]
                [begin 
                    [let i 0]
                    [while [< i 4]
                        [callcc /[cc] [yield [vec i cc]]]
                        [+= i 1]]]]
[let iter_k nil]
[let next /[g]
            [begin 
                [if iter_k
                    [iter_k]
                    [begin
                        [let value [callcc /[cc] [g cc]]]
                        [if [== [typeof value] 'vec']
                          [begin 
                            [let i nil]
                            [= [i iter_k] value]
                            [println i]]
                          nil]]]]]

[next gen]
[next gen]
[next gen]
[next gen]
[next gen]
[next gen]
```

## Structure

You can customize the behavior of getters and setters by setting the `__get__/__set__` properties,
unlocking the ability to implement features such as proxies and inheritance.

```lisp
[let o [obj]]

[let observer println]

[= o.__set__ /[k v] [begin
	[if observer [observer k v]]
	[set this k v]
]]

[= o.x 42] ; print x 42
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
    assign -> let (expand | id) expr | = (expand | id prop*) expr

    op -> (operator expr expr*) | (assign_operator id prop* expr*)

    call -> [ assign | op | expr* ]

    dot -> (str | id | call) prop*

    expr -> fn | -?(num | dot)

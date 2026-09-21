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

; convenient placeholders in expansion
[let [. x] [vec 1 2 3]] ; x = 2, `.` is a placeholder that MUST occupy one position
[let [... x] [1 2 3 4]] ; x = 4, `...` is a placeholder that can occupy zero or as many positions as possible
[let [. [x] ... y] [vec 1 [vec 2] 3 4 5]] ; x = 2, y = 5

; variadic: `/[...]` makes no assumption about arity; the whole argument
; pack is bound under the name `__args` (prelude's `vec` is exactly this)
[= myvec /[...] __args]
[= mylist /[a ...] [vec a __args]] ; leading fixed params + the rest as a pack
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

You can customize the behavior of getters and setters by using `proxy` syscall,
unlocking the ability to implement features such as proxies and inheritance.

```lisp
[let o [obj]]

[let observer println]

[let p [proxy o 'set' /[t k v] [begin
  [if observer [observer k v]]
  [set t k v]]]]

[= p.x 42]  ; print x 42
[print o.x] ; 42
```

## Error handling

```lisp
; try 表达式：出错时 handler 闭包收到错误消息（Str），返回其值；
; 正常时值为 PROTECTED 本身。未捕获错误经输出通道打印，实例存活可复用
[println [try [put [vec 1] 5 0] /[e] 'caught!']]
```

## JS interop (FFI)

```lisp
; [js 'dotted.path' [vec args...]] —— 调用宿主（globalThis）函数/读属性，
; 参数与结果经 JSON 桥往返（number/string/bool/nil/array/object）
[println [js 'Math.max' [vec 1 2 3]]]              ; 3
[println [js 'JSON.parse' [vec '{"a":42}']].a]     ; 42
```

## Prelude & builtins

```lisp
; 预置库（编译期拼接，可用 let 遮蔽）：not vec slice str map filter fold
; range reverse append contains sum join
[println [sum [map /[x] [* x x] [range 5]]]]       ; 30

; 内建只剩原语：print println at put len splice typeof keys obj set get
; proxy callcc js await；数学 floor ceil round abs sqrt pow min max；
; substr。len/at 兼字符串；+ 对两字符串为拼接
```

## Async

全部建立在两个原语上——没有内建的 sleep/defer/spawn，调度就是宿主的事件循环：

```lisp
; await：调用宿主函数并 park 等待——Promise 则 .then/.catch，同步值立即回调；
; 拒绝/宿主异常经 {"__sq_err"} 回传，try 可直接捕获
[println [await 'Math.max' [vec 1 7 3]]]              ; 7
[println [try [await 'Promise.reject' [vec 'boom']] /[e] e]]  ; boom

; sleep/defer 只是原语上的普通函数（宿主提供 Promise 化 __square_sleep），
; 程序自带定义即可：
[= sleep /[ms] [await '__square_sleep' [vec ms]]]
[= defer /[f] [js 'queueMicrotask' [vec f]]]
[sleep 500]
[defer /[] [println 'later']]

; 闭包跨界：作为实参传给 JS 的 square 闭包自动变回调句柄，
; 宿主调用它 = call_cb 唤醒该任务（事件/Promise 型 API 天然契合；
; 多实参以 vec 进首参；回调为异步触发，返回值不同步回流）
[js 'someEventSource.on' [vec 'click' /[ev] [println ev.type]]]
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

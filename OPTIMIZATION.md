# square 性能优化记录

记录 `optimize` 分支上的解释器优化工作：思路、方法论、每轮进展、失败实验与决策。
基线时间：2026-09-20。所有倍率均相对 **Lua 5.1**（同机、同基准、三方正确性校验）。

## 一、目标与参照系

性能目标是"接近生产级解释器"。选择 Lua 5.1 作为参照系而非 V8：Lua 和 square
一样是纯字节码解释器（无 JIT），V8 那一列只用来丈量与天花板的天真距离
（Lua 自己比 V8 慢 6-28 倍属正常水位）。

## 二、方法论（先于一切优化的投资）

1. **基准套件**（`bench/suite.mjs`，commit `a663336`）：6 基准 × square/Lua/Node
   三方实现同一算法，**正确性校验先行**（三方输出不一致直接失败退出），中位数计时。
   覆盖六个正交维度：递归调用（fib）、深递归（tak）、派发与名字解析（loop）、
   分配与内建调用（vec）、闭包捕获（closure）、哈希读写（obj）。
2. **rdtsc 逐指令剖析器**（commit `4d2196c`）：perf 被内核 `perf_event_paranoid=4`
   禁用；改为 VM 内建周期计数（`square -p file.sq`，release 可用，非剖析时仅一个
   可预测分支）。逐操作码归因是解释器真正需要的粒度，perf 反而给不了。
3. **提交纪律**：每项优化独立提交，进提交前必须全绿——146 native 测试
   （`cargo +nightly test --target=x86_64-unknown-linux-gnu`）+ 10 个 Node 异步
   测试（`make test-js`）+ 基准三方校验 + 7 个示例程序冒烟（含 callcc 生成器、
   prototype 继承、教堂数惰性链表——后者曾抓到 TCO+展开参数的真实 bug）。
4. **噪声意识**：机器热状态噪声后期已与单项增益同量级（Lua 自身波动 ±30%），
   重要结论需多轮取稳定值。

## 三、优化分层模型

- **第一层：表示与约定**（本文档主体）——Value 装箱、调用约定、帧布局、超指令、
  快查结构。特征：已知套路、逐项可测、每项 5-30%。
- **第二层：反馈驱动的特化**——运行时类型反馈 + 特化指令（与现有超指令框架
  天然衔接），CPython 3.11 adaptive opcode / Lua 5.4 的路线。
- **第三层：深水区**——JIT（投机、去优化，与 callcc/续延语义存在根本张力）、
  分代 GC。
- **逃生通道**：wasm 目标下直接编译到 JS/wasm 后端，把优化交给 V8
  （Gambit/ClojureScript 路线），可与解释器并存。

## 四、进展总表（对 Lua 5.1 倍率）

| 基准 | 起点 | 槽位化后 | 第一层收官 | 累计提速 |
|---|---|---|---|---|
| fib | 41.9x（2827ms） | 10.9x | **6.5x（425ms）** | **6.6×** |
| tak | 80.1x（1238ms） | 12.9x | **9.5x** | **8.4×** |
| loop | 29.3x | 20.4x | **11.0x** | 2.7× |
| vec | 15.8x | 12.5x | **9.2x** | 1.7× |
| closure | 17.1x | 6.9x | **4.7x** | 3.6× |
| obj | 40.5x（395ms） | 11.6x | **8.8x** | 4.6× |

对照：CPython 通常比 Lua 慢 2-4 倍。square 从"落后同类 40-80 倍"到
"最差 11 倍、最好 4.7 倍"。

## 五、每轮改动明细

### 第 0 轮：可见性（a663336）
基准套件建立。**正确性门禁立刻抓到两个自 bug**（tak 条件写反、JS 引用适配），
证明"先有度量再有优化"。

### 第 1 轮：外科手术（76c6f67, 0d1fbf5, 652ae64）
- 恢复 GET/SET 为快速操作码（曾为统一 proxy 拦截被移除；现 Obj 直访 +
  proxy 回退内建，语义零变化）：obj 40.5x→15.7x
- PEEK 参数绑定不再整包 clone；CALL 臂合并重复借用
- LOAD 局部优先于 builtin（顺带修正"局部无法遮蔽内建名"的语义）
- 新增 `test_profile_fib` 逐指令剖析，暴露根因：**CALL 26% / LOAD 21% /
  PUSH_CLOSURE 19%——if/while/begin/cond 全部编译为"闭包 thunk + 零参立即调用"**，
  分支作用域靠独立调用帧实现，是运行时按名解析的架构性代价

### 第 2 轮：静态槽位化（632d8c2，本工程最大单项）
- EmitContext 改 FnCtx 栈：名字使用处编译期 resolve 到
  `Local(slot)/Upvalue(idx)/Global`，传递捕获沿外层函数链逐层注册（Lua 式）
- CallFrame = slots/ups/names 三字段；槽位静态布局，捕获槽位 UpValue 单元
- CALL(n) 参数从调用方栈直拷（不再 PACK 打包分配）；TCO 帧复用保留
- 闭包 = 共享 ClosureInfo + 每实例 upvalue 单元表
- **if/while/begin/cond 全部去 thunk 化**为纯跳转
- 语义决策：前向引用走全局表 + 顶层 let 双写（保住旧惰性捕获语义，块作用域
  不泄漏）；`this` 改编译期专用捕获源 `CaptureSrc::This`；姊妹作用域前向引用
  从静默 Nil 变为明确报错（完成旧代码 FIXME）
- 修复顺带发现的 TCO+展开参数序反转 bug（`test_exec_pack_args_tco` 回归）
- 效果：fib 39.6→11.1x，tak 74→15x，closure 15→6.1x，obj→14.2x

### 第 3 轮：调用路径去分配（a2e928b, eff6d20, cec74c7 部分）
- upvalue 表 Rc<Vec> 共享：CALL 从 Vec 堆分配变引用自增；无捕获闭包共享
  VM 级空表（零分配）
- VM.cur 当前帧缓存（push/pop/restore/reset 同步）
- SET 热路径免 String 分配；RET 返回值经 cur 压栈

### 第 4 轮：rdtsc 剖析器 + 原生 CLI（4d2196c）
`square [-p] file.sq`。首个 release 数据：派发底价 ~34 cyc（JMP）、
CALL 176、binop ~70、PUSH/LOAD ~50。

### 第 5 轮：超指令融合（cec74c7, c4ca619, 1b6896f, 7037422）
emit 出口 peephole（`emit_multi_node` 不做，字节码断言测试零改动）；
融合改变下标，所有相对偏移（含 PUSH_CLOSURE meta）经 old→new 映射重定位。
同时帧池改存 Rc 壳（CALL 全程零堆分配）、CALL 错误路径免贪婪克隆。

超指令全表（见 `fuse_superinsts`）：

| 模式 | 融合为 | 动机 |
|---|---|---|
| CMP + JNE | CMP_JNE | 免中间 Bool 压弹栈与一次派发 |
| LOAD_LOCAL ×2 | LOAD2_LOCAL | 合一次借用 |
| LOAD_LOCAL + PUSH | LOADP_LOCAL | 合一次借用 |
| PUSH + 算术 | BINOP_IMM | 立即数为右操作数，原地替换 |
| LOAD+PUSH+CMP+JNE | LOADC_JNE | **整条循环条件一条指令** |
| LOAD+PUSH+算术 | LOAD_ARITH_IMM | `[- n 1]` 一条指令 |
| LOAD×2+算术 | LOAD2_ARITH | `[* i i]` 一条指令 |
| LOAD_LOCAL + GET | LOADGET_LOCAL | `o.y` 一条指令（get_field 共享路径） |
| LOAD_UP+PUSH / +算术 | LOADP_UP / LOADU_ARITH | 捕获变量 += 类模式 |

配套：REM 整数快速路径（i64 往返判定——**no_std 无 fract()**，勿用——
绕开软件 fmod 283 cyc）。

### 第 6 轮：表示瘦身（fd06c0a, f45edb0）
- `Value::Str(String)` → `Rc<str>`；Object 键同步 `HashMap<Rc<str>, _>`
  （字面量/as_str/键插入全部免深拷贝——生态前置债）
- Proxy 载荷装箱：Value enum 32B → 24B

### 第 7 轮：第一层收官（7308b62）
- **FxHash**：Object/globals/builtin 表换 FxHasher（短键从 SipHash 数十周期
  降到数周期；no_std 下经 `BuildHasherDefault` 自组别名）
- **while 回边穿线**：JMP 目标为 LOADC_JNE 时，就地换成为其引入的为真跳转
  孪生 `LOADC_JNZ`——省一次跳转派发

## 六、失败与放弃的实验（与成功同等重要）

1. **派发收紧（exec 帧参数 + run 脏标记）**：146 测试全绿、debug 剖析甚至
   显示多数指令变快，但 wasm release 下调用路径实测回退 ~50%（fib 730→1159ms）。
   诊断：额外 `&Rc` 间接层疑破坏 CALL 热路径的内联/寄存器分配。**教训：
   debug 剖析与 wasm release 行为会背离，度量必须落在目标产物上**（rdtsc
   剖析器因此而生）。已回滚（eff6d20 提交信息有记录）。
2. **syscall 切片 ABI**：参数留在帧栈、syscall 内部需可变借同帧，安全实现
   绕不开拷贝，收益仅 vec ~5%。放弃。
3. **NaN-boxing（Value 24B→8B）**：评估后推迟。理由：(a) 实测收益 ~10%
   （派发与 CALL 占比远大于拷贝宽度）；(b) 需 unsafe Rc 手术（clone/drop 全
   部指针操作）+ Str 改 `Rc<Box<str>>` 双重解引用 + `Borrow` 边界重构
   （~100 站点）；(c) 与第二层类型特化（ADD_NUM 等）收益重叠。归入 Value 2.0，
   与字符串驻留/GC 决策一起定案。
4. **穿线取反陷阱**：回边穿线的第一版把 JMP 换成 LOADC_JNE 并跳向其目标——
   `test_exec_while` 立刻失败（循环一次即退出）。根因：比较谓词**不可取反**
   （NaN/混合类型下 `NOT(a<b) ≠ a>=b`），必须用极性相反的孪生指令。
   正确性门禁再一次证明了价值。

## 七、语义决策记录（语言设计层面，需长期保持）

| 决策 | 内容 | 动因 |
|---|---|---|
| 前向引用 | 走全局表 + 顶层 let 双写接住 | 保住旧"Nil 单元 + 后续 let 写穿"惰性捕获；块作用域 let 不双写（保持封闭） |
| `this` | 编译期专用捕获源 `CaptureSrc::This`，set/obj 回填 | 替代旧的运行时往闭包塞名字的 hack；inherit.sq 验证 |
| 内建遮蔽 | 局部变量可遮蔽内建名 | LOAD 顺序反转的副产品，与主流语言一致 |
| 姊妹作用域前向引用 | 明确报 undefined（旧为静默 Nil） | 完成旧代码 FIXME |
| 作用域遮蔽 | fn 体内 `[let x 24]` 正确遮蔽（旧误报 undefined） | 槽位化自然修正 |
| `=` 动态定义 | 未声明名字经 STORE_GLOBAL 入 VM.globals | 保持旧"`=` 即定义"语义 |

## 八、现状与下一步

**现状**（第一层收官，7308b62）：fib 6.5x / tak 9.5x / loop 11.0x /
vec 9.2x / closure 4.7x / obj 8.8x。剖析指出的残余：派发底价 ~34 cyc ×
指令数（紧凑字节码或更多融合可压）、CALL ~176 cyc（被调者提取的 Rc 自增链）。

**下一步**（按序）：
1. **第二层：运行时类型反馈 + 特化指令**——CALL site 记录实参类型，N 次后
   自动换成 `CALL_NUM`/`ADD_NUM` 等特化超指令（与现有融合框架同构，只是
   触发器从静态模式变为运行时反馈）。
2. **阶段 2 生态**（性能边际收益递减后转向）：try 错误处理（消灭 wasm panic
   路径，生产级必修）→ JS FFI（`js.call` 打通 npm 生态）→ 模块系统 + prelude。
3. **Value 2.0**（与 GC/驻留一起）：NaN-boxing 或句柄堆，评估文档见第六节。

## 九、阶段 2 补记：异步原语统一（7e37b0f）

生态工作（try / js FFI / prelude）见各提交。异步整合的设计与教训：

**设计**：原有 sleep/defer/spawn 三个 bespoke syscall + js_sleep/
js_queue_microtask 两个导入，统一为两个原语——`[await 'fn' [vec args]]`
（park + 宿主按结果回调，Promise then/catch、同步值立即投递）与
**闭包跨界**（[js ...] 实参中的闭包变回调句柄，宿主调用即 call_cb
唤醒）。sleep/defer/spawn 降为 prelude 两行糖。唤醒统一为
`call_cb(id, ptr, len)`（零参即裸唤醒）。

**教训**（调试此功能挖出三个 bug，全部有普遍价值）：

1. **哨兵 ra 覆盖**：`run()` 开头的 `current_frame().ra = insts.len()`
   （程序结束哨兵）会覆盖跨帧 park 恢复中闭包帧的**活返回地址**——
   任何在闭包内 park 的功能（await 在函数里）都会吞掉返回。修复：
   仅根帧设置。教训：看似无害的防御性初始化在并发/恢复路径上是
   定时炸弹。
2. **闭包任务 ra=0 陷阱**：任务帧的 ra 默认 0，闭包 body RET 后
   回落哨兵帧 → 从程序头重跑 → 每轮重跑注册新闭包 → microtask
   级联 → 无限循环。修复：闭包帧 ra 设程序外大值。教训：默认值
   在"程序外入口"语义下是"从头再来"而非"就此结束"。
3. **microtask 化投递**：同步结果若同步回调，任务尚未 park 完成，
   tick 会把未 park 的任务当"已完成"丢弃——随后 park 一个已出队
   的任务，永久静默。修复：宿主所有立即投递走 queueMicrotask。
   教训：跨同步边界的唤醒协议必须明确"谁保证 park 先于 wake"。

**契约**（v1 限制，README 已记）：闭包回调为异步触发，返回值不
同步回流宿主（事件/Promise 型 API 天然契合，arr.map 式同步取值不
适合）；多实参回调以 vec 进首参。

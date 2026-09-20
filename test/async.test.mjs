// square 异步运行时的 Node 单测（node:test）。
// 宿主导入与驱动逻辑复用 host.mjs。每个 test 一个独立 wasm 实例（运行时是客机内全局单例）。
//   node --test test/

import { test } from "node:test";
import assert from "node:assert/strict";
import { loadSquare, flush } from "../host.mjs";

const sq = () => loadSquare(); // 每个 case 独立实例

// ── run 模式：sleep / defer / spawn ──────────────────────────────

test("sleep: park 期间无输出，wake 后才打印", async () => {
  const s = await sq();
  s.program("[sleep 50]\n[println 'after']\n").run();
  assert.equal(s.stdout(), ""); // run 返回时仍在 sleep
  await flush(120);
  assert.equal(s.stdout(), "after\n");
});

test("defer: 在主同步栈清空后才运行", async () => {
  const s = await sq();
  s.program("[println 'main']\n[defer /[] [println 'deferred']]\n").run();
  assert.equal(s.stdout(), "main\n"); // 主程序已跑完，defer 还没
  await flush(10);
  assert.equal(s.stdout(), "main\ndeferred\n");
});

test("嵌套 defer: first / later / latest 顺序", async () => {
  const s = await sq();
  s.program(`[defer /[] [begin
  [defer /[] [begin [println 'later'] [defer /[] [println 'latest']]]]
  [println 'first']]]`).run();
  await flush(20);
  assert.equal(s.stdout(), "first\nlater\nlatest\n");
});

test("spawn: 并发 task 按睡眠时长先后完成", async () => {
  const s = await sq();
  s.program(`[spawn /[] [begin [sleep 40] [println 'A']]]
[spawn /[] [begin [sleep 80] [println 'B']]]
[spawn /[] [begin [sleep 120] [println 'C']]]`).run();
  assert.equal(s.stdout(), ""); // 都在 sleep
  await flush(250);
  assert.equal(s.stdout(), "A\nB\nC\n");
});

test("嵌套 defer + sleep: 内层 deferred task 自己 sleep，仍按序完成", async () => {
  const s = await sq();
  s.program(`[defer /[] [begin
  [defer /[] [begin [sleep 30] [println 'inner-late']]]
  [println 'outer']]]
[println 'main']`).run();
  // microtask 排空后：main + outer 已打印；inner 已进入 sleep（park），还没输出。
  await flush(10);
  assert.equal(s.stdout(), "main\nouter\n");
  // inner 的 sleep 到期，wake 后续跑打印。
  await flush(50);
  assert.equal(s.stdout(), "main\nouter\ninner-late\n");
});

// ── 错误处理：坏参 / 缺参都该 trap 并报因 ────────────────────────

test("await: 拒绝 → try 捕获原始消息", async () => {
  const s = await sq();
  assert.equal(
    s.program("[println [try [await 'Promise.reject' [vec 'boom']] /[e] e]]\n").run(),
    0,
  );
  await flush(30);
  assert.equal(s.stdout().trim(), "boom");
});

test("await: 同步值立即回调", async () => {
  const s = await sq();
  assert.equal(s.program("[println [await 'Math.max' [vec 1 7 3]]]\n").run(), 0);
  await flush(30);
  assert.equal(s.stdout().trim(), "7");
});

test("defer: 非闭包 → js 序列化错误（状态返回）", async () => {
  const s = await sq();
  assert.equal(s.program("[defer 5]\n").run(), 1);
  // 宿主 queueMicrotask(5) 抛 TypeError → 回传为语言级错误（实例存活）
  assert.match(s.stdout(), /ERR_INVALID_ARG_TYPE|callback/);
});

test("回调跨界：square 闭包作为 JS 事件回调（异步触发）", async () => {
  const s = await sq();
  globalThis.__sq_test_emit = (cb) => setTimeout(() => cb(42), 10);
  assert.equal(
    s.program("[js '__sq_test_emit' [vec /[v] [println [+ 'got ' v]]]]\n").run(),
    0,
  );
  await flush(50);
  assert.equal(s.stdout().trim(), "got 42");
});

test("闭包跨界多实参 → 首参收 vec", async () => {
  const s = await sq();
  globalThis.__sq_test_pair = (cb) => setTimeout(() => cb(7, 8), 10);
  assert.equal(
    s
      .program(
        "[js '__sq_test_pair' [vec /[xs] [println [str [at xs 0]]]]]\n",
      )
      .run(),
    0,
  );
  await flush(50);
  assert.equal(s.stdout().trim(), "7");
});

// ── step 模式：逐指令粒度 ────────────────────────────────────────

test("step: 单次 step 至多打印一行（defer 不被一次性 tick 跑完）", async () => {
  const s = await sq();
  const prog = s.program(`[defer /[] [begin [println 'a'] [println 'b']]]`);
  let maxPerStep = 0;
  for (let i = 0; i < 40; i++) {
    const before = s.stdout().length;
    prog.step();
    await flush(0); // 排空本轮 microtask
    maxPerStep = Math.max(maxPerStep, s.stdout().length - before);
  }
  // println 'a' / println 'b' 各占一行（含换行），不会有一条 step 把两行都打出来。
  assert.ok(
    maxPerStep <= "b\n".length,
    `某次 step 产出 ${maxPerStep} 字节，超过单行`,
  );
});

test("step: sleep 期间 step 空过，wake 后续跑", async () => {
  const s = await sq();
  const prog = s.program("[sleep 40]\n[println 'after']\n");
  // 逐指令推进到 sleep（sleep 会 park，之后 step 空过）。
  // prelude 拼接后语句数过百，步数上限放宽
  for (let i = 0; i < 500; i++) prog.step();
  assert.equal(s.stdout(), ""); // 还没 wake，无输出
  await flush(80); // timer 触发 wake
  for (let i = 0; i < 20 && !s.stdout(); i++) prog.step();
  assert.equal(s.stdout(), "after\n");
});

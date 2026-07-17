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

test("错误: sleep 非数字 → trap", async () => {
  const s = await sq();
  assert.throws(() => s.program("[sleep 'x']\n").run());
  assert.match(s.stdout(), /sleep expect a number/);
});

test("错误: defer 非函数 → trap", async () => {
  const s = await sq();
  assert.throws(() => s.program("[defer 5]\n").run());
  assert.match(s.stdout(), /defer expect a function/);
});

test("错误: 缺参数 → trap（不该 index 越界 panic）", async () => {
  const a = await sq();
  assert.throws(() => a.program("[sleep]\n").run());
  assert.match(a.stdout(), /sleep expect a number/);
  const b = await sq();
  assert.throws(() => b.program("[defer]\n").run());
  assert.match(b.stdout(), /defer expect a function/);
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
  for (let i = 0; i < 8; i++) prog.step();
  assert.equal(s.stdout(), ""); // 还没 wake，无输出
  await flush(80); // timer 触发 wake
  for (let i = 0; i < 20 && !s.stdout(); i++) prog.step();
  assert.equal(s.stdout(), "after\n");
});

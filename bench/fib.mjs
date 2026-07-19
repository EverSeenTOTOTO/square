// fib 基准：square（wasm 字节码解释器）vs 原生 JS，同一套朴素递归 fib。
//   node bench/fib.mjs
//
// 时间只计 `run()`（执行），不含编译；每个 n 先 warmup 让 V8 把 wasm 解释循环与
// JS 的 fib JIT 升级，再取多次测量的中位数。square 复用同一 wasm 实例，靠 reset()
// 重跑同一份编译产物。
import { performance } from "node:perf_hooks";
import { loadSquare } from "../host.mjs";

const NS = [10, 20, 30];
const WARMUP = 2;
const RUNS = 7;

const fibSrc = `[let fib /[n] [if [<= n 2] 1 [+ [fib [- n 1]] [fib [- n 2]]]]]`;
const fibJs = (n) => (n <= 2 ? 1 : fibJs(n - 1) + fibJs(n - 2));

const median = (xs) => {
  const s = [...xs].sort((a, b) => a - b);
  const m = s.length >> 1;
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
};

const squareSrc = (n) => `${fibSrc}\n[fib ${n}]\n`; // 顶层算 fib(n)，结果留在栈上（不计 println 开销）

async function timeSquare(s, n) {
  const prog = s.program(squareSrc(n)); // 编译一次，不计时
  for (let i = 0; i < WARMUP; i++) {
    s.reset();
    prog.run();
  }
  const ts = [];
  for (let i = 0; i < RUNS; i++) {
    s.reset();
    const t0 = performance.now();
    prog.run();
    ts.push(performance.now() - t0);
  }
  return median(ts);
}

function timeJs(n) {
  for (let i = 0; i < WARMUP; i++) fibJs(n);
  const ts = [];
  for (let i = 0; i < RUNS; i++) {
    const t0 = performance.now();
    fibJs(n);
    ts.push(performance.now() - t0);
  }
  return median(ts);
}

const s = await loadSquare();

// 正确性：square 的 fib(n) 必须与 JS 一致
for (const n of NS) {
  const before = s.stdout().length; // host 的 output 在实例内累积，截取本次增量
  s.reset();
  s.program(`${fibSrc}\n[println [fib ${n}]]\n`).run();
  const got = Number(s.stdout().slice(before).trim());
  const want = fibJs(n);
  if (got !== want) {
    console.error(`✗ fib(${n}): square=${got} js=${want}`);
    process.exit(1);
  }
}
console.log("correctness: square == JS  (fib 10/20/30)\n");

console.log("  n  |  square (ms) |   JS (ms)   |  slowdown");
console.log("-----+--------------+-------------+----------");
for (const n of NS) {
  const sq = await timeSquare(s, n);
  const js = timeJs(n);
  console.log(
    `${String(n).padStart(4)} | ${sq.toFixed(2).padStart(12)} | ${js
      .toFixed(3)
      .padStart(11)} | ${(sq / js).toFixed(1).padStart(7)}x`,
  );
}

// JS FFI 测试：[js 'dotted.path' [vec args]] 打通宿主全局。
import test from "node:test";
import assert from "node:assert";
import { loadSquare } from "../host.mjs";

const sq = () => loadSquare();

test("js: Math.max 数值往返", async () => {
  const s = await sq();
  s.program("[println [js 'Math.max' [vec 1 2 3]]]\n").run();
  assert.equal(s.stdout().trim(), "3");
});

test("js: 字符串与转义往返", async () => {
  const s = await sq();
  s.program("[println [js 'encodeURIComponent' [vec 'a b&c']]]\n").run();
  assert.equal(s.stdout().trim(), "a%20b%26c");
});

test("js: 数组结果 → vec", async () => {
  const s = await sq();
  s.program("[println [len [js 'Object.keys' [vec [obj 'x' 1 'y' 2]]]]]\n").run();
  assert.equal(s.stdout().trim(), "2");
});

test("js: undefined / 不存在的函数 → nil", async () => {
  const s = await sq();
  s.program(
    "[println [== [js 'no.such.fn' [vec]] nil]] [println [== [js 'Date.now' [vec]] nil]]\n",
  ).run();
  const lines = s.stdout().trim().split("\n");
  assert.equal(lines[0], "true"); // 不存在 → nil
  assert.equal(lines[1], "false"); // Date.now 返回数值
});

test("js: NaN 参数不炸（序列化为 null）", async () => {
  const s = await sq();
  s.program(
    "[println [js 'Math.max' [vec [+ 0.0 [* 0.0 1e400]] 5]]]\n",
  ).run();
  // NaN → null → Math.max(null, 5) = 5
  assert.equal(s.stdout().trim(), "5");
});

test("js: try 捕获不可序列化参数", async () => {
  const s = await sq();
  s.program("[println [try [js 'Math.max' [vec print]] /[e] 'caught']]\n").run();
  assert.equal(s.stdout().trim(), "caught");
});

test("js: 结果对象 → obj 字段访问", async () => {
  const s = await sq();
  s.program("[println [js 'JSON.parse' [vec '{\"a\":42}']].a]\n").run();
  assert.equal(s.stdout().trim(), "42");
});

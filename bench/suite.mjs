// 基准套件：square（wasm 字节码 VM）vs Lua 5.1 vs 原生 JS，覆盖不同 VM 瓶颈：
//   fib     朴素递归        —— 调用/RET、算术
//   tak     深递归三参      —— 调用栈、算术（无分配）
//   loop    while 纯算术    —— 指令派发、LOAD/STORE 名字解析
//   vec     建表/读/原地写  —— 分配（Rc/RefCell）、内建调用
//   closure 造闭包+调用     —— PUSH_CLOSURE、upvalue 捕获与写穿
//   obj     三键哈希读写    —— get/set 内建、HashMap 键访问
//
//   node bench/suite.mjs [name ...]   （需先 make build 产出 square.wasm；可只跑指定基准）
//
// 正确性先行：三个实现在 square 的 N 下必须产出相同值，否则直接失败退出。
// 计时取 warmup 后多次运行的中位数，不含编译。fib/tak 三方同 N 直接比；
// 线性基准（loop/vec/closure/obj）JS/Lua 用大 N、square 用小 N，比值按
// "每单位工作量耗时"归一（work(n) = n）。
import { performance } from "node:perf_hooks";
import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { loadSquare } from "../host.mjs";

const WARMUP = 2;
const RUNS = 5; // lua / square
const JS_RUNS = 7; // JS 单次更快，多跑几次压噪声

const median = (xs) => {
  const s = [...xs].sort((a, b) => a - b);
  const m = s.length >> 1;
  return s.length % 2 ? s[m] : (s[m - 1] + s[m]) / 2;
};

// 中日韩字符占 2 列，按显示宽度右补空格
const pad = (s, w) => {
  const v = [...s].reduce((a, c) => a + (c.charCodeAt(0) > 0xff ? 2 : 1), 0);
  return s + " ".repeat(Math.max(0, w - v));
};

function fibJs(n) {
  return n <= 2 ? 1 : fibJs(n - 1) + fibJs(n - 2);
}
function takJs(x, y, z) {
  return y >= x ? z : takJs(takJs(x - 1, y, z), takJs(y - 1, z, x), takJs(z - 1, x, y));
}
function loopJs(n) {
  let i = 0;
  let acc = 0;
  while (i < n) {
    acc += (i * i) % 97;
    i += 1;
  }
  return acc;
}
function vecJs(n) {
  const v = [];
  let i = 0;
  while (i < n) {
    v.push(i);
    i += 1;
  }
  let s = 0;
  let j = 0;
  while (j < n) {
    s += v[j];
    j += 1;
  }
  let k = 0;
  while (k < n) {
    v[k] = v[k] + 1;
    k += 1;
  }
  return s + v[0];
}
function closureJs(n) {
  const make = (n) => () => {
    n += 1;
    return n;
  };
  let total = 0;
  let i = 0;
  while (i < n) {
    const c = make(i);
    total += c();
    total += c();
    i += 1;
  }
  return total;
}
function objJs(n) {
  const o = { x: 1, y: 2, z: 3 };
  let i = 0;
  while (i < n) {
    o.x = o.y + 1;
    o.y = o.z + 1;
    o.z = o.x + 1;
    i += 1;
  }
  return o.x + o.y + o.z;
}

// square 源码拆成 head（定义）+ tail（末尾表达式，计时版裸执行，校验版包进 println）
const BENCHES = [
  {
    name: "fib",
    dims: "递归调用",
    n: 30,
    nSq: 30,
    linear: false,
    head: "[let fib /[n] [if [<= n 2] 1 [+ [fib [- n 1]] [fib [- n 2]]]]]",
    tail: (n) => `[fib ${n}]`,
    js: fibJs,
    lua: "local function fib(n) if n <= 2 then return 1 end return fib(n - 1) + fib(n - 2) end return fib(N)",
  },
  {
    name: "tak",
    dims: "深递归",
    n: 21, // tak(21, 14, 7)
    nSq: 21,
    linear: false,
    head: "[let tak /[x y z] [if [<= x y] z [tak [tak [- x 1] y z] [tak [- y 1] z x] [tak [- z 1] x y]]]]",
    tail: (n) => `[tak ${n} ${n - 7} ${n - 14}]`,
    js: (n) => takJs(n, n - 7, n - 14),
    lua: "local function tak(x, y, z) if x <= y then return z end return tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y)) end return tak(N, N - 7, N - 14)",
  },
  {
    name: "loop",
    dims: "派发/名字解析",
    n: 20_000_000,
    nSq: 300_000,
    linear: true,
    head: "",
    tail: () => "acc",
    js: loopJs,
    lua: "local i, acc = 0, 0 while i < N do acc = acc + (i * i) % 97 i = i + 1 end return acc",
    squareExtra: (n) =>
      `[let i 0]\n[let acc 0]\n[while [< i ${n}]\n  [+= acc [% [* i i] 97]]\n  [+= i 1]]`,
  },
  {
    name: "vec",
    dims: "分配/内建调用",
    n: 1_000_000,
    nSq: 100_000,
    linear: true,
    head: "",
    tail: () => "[+ s [at v 0]]",
    js: vecJs,
    lua: `local t = {}
local i = 0
while i < N do t[#t + 1] = i i = i + 1 end
local s, j = 0, 0
while j < N do s = s + t[j + 1] j = j + 1 end
local k = 0
while k < N do t[k + 1] = t[k + 1] + 1 k = k + 1 end
return s + t[1]`,
    squareExtra: (n) =>
      `[let v [vec]]\n[let i 0]\n[while [< i ${n}]\n  [splice v [len v] 0 [vec i]]\n  [+= i 1]]\n[let s 0]\n[let j 0]\n[while [< j ${n}]\n  [+= s [at v j]]\n  [+= j 1]]\n[let k 0]\n[while [< k ${n}]\n  [put v k [+ [at v k] 1]]\n  [+= k 1]]`,
  },
  {
    name: "closure",
    dims: "闭包捕获",
    n: 2_000_000,
    nSq: 100_000,
    linear: true,
    head: "[let make /[n] /[] [begin [= n [+ n 1]] n]]",
    tail: () => "total",
    js: closureJs,
    lua: `local function make(n) return function() n = n + 1 return n end end
local total, i = 0, 0
while i < N do
  local c = make(i)
  total = total + c()
  total = total + c()
  i = i + 1
end
return total`,
    squareExtra: (n) =>
      `[let total 0]\n[let i 0]\n[while [< i ${n}]\n  [let c [make i]]\n  [+= total [c]]\n  [+= total [c]]\n  [+= i 1]]`,
  },
  {
    name: "obj",
    dims: "哈希读写",
    n: 5_000_000,
    nSq: 100_000,
    linear: true,
    head: "[let o [obj 'x' 1 'y' 2 'z' 3]]",
    tail: () => "[+ o.x [+ o.y o.z]]",
    js: objJs,
    lua: `local o = { x = 1, y = 2, z = 3 }
local i = 0
while i < N do
  o.x = o.y + 1
  o.y = o.z + 1
  o.z = o.x + 1
  i = i + 1
end
return o.x + o.y + o.z`,
    squareExtra: (n) =>
      `[let i 0]\n[while [< i ${n}]\n  [= o.x [+ o.y 1]]\n  [= o.y [+ o.z 1]]\n  [= o.z [+ o.x 1]]\n  [+= i 1]]`,
  },
];

// ── 各运行时驱动 ─────────────────────────────────────────────────────────────

// head（定义）+ extra（主体）+ tail（末尾表达式，计时版裸执行，校验版包进 println）
const squareParts = (b, n) => [b.head, b.squareExtra?.(n)].filter(Boolean);
const squareSrc = (b, n) => [...squareParts(b, n), b.tail(n)].join("\n");

function squareCheck(s, b, n) {
  const before = s.stdout().length;
  s.reset();
  s.program([...squareParts(b, n), `[println ${b.tail(n)}]`].join("\n")).run();
  return s.stdout().slice(before).trim();
}

async function timeSquare(s, b, n) {
  const prog = s.program(squareSrc(b, n)); // 编译一次，不计时
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

function timeJs(fn, n) {
  for (let i = 0; i < WARMUP; i++) fn(n);
  const ts = [];
  for (let i = 0; i < JS_RUNS; i++) {
    const t0 = performance.now();
    fn(n);
    ts.push(performance.now() - t0);
  }
  return median(ts);
}

// lua 程序在进程内自计时（os.clock 为 CPU 时间），打印 VALUE/TIME 两行
function runLua(body, n, tmp) {
  const src = `local function run(N)
${body}
end
local v = run(${n})
for _ = 1, ${WARMUP} do run(${n}) end
local ts = {}
for _ = 1, ${RUNS} do
  local t0 = os.clock()
  run(${n})
  ts[#ts + 1] = (os.clock() - t0) * 1000
end
table.sort(ts)
print("VALUE " .. v)
print("TIME " .. ts[math.ceil(#ts / 2)])`;
  const file = join(tmp, `${Math.random().toString(36).slice(2)}.lua`);
  writeFileSync(file, src);
  const r = spawnSync("lua", [file], { encoding: "utf8" });
  if (r.status !== 0) throw new Error(`lua failed: ${r.stderr}`);
  const value = r.stdout.match(/^VALUE (.+)$/m)?.[1];
  const time = Number(r.stdout.match(/^TIME (.+)$/m)?.[1]);
  return { value, time };
}

// ── 主流程 ───────────────────────────────────────────────────────────────────

const args = process.argv.slice(2);
const bench = BENCHES.filter((b) => args.length === 0 || args.includes(b.name));
if (bench.length === 0) {
  console.error(`无匹配基准：${args.join(" ")}（可选：${BENCHES.map((b) => b.name).join(" ")}）`);
  process.exit(1);
}

const luaProbe = spawnSync("lua", ["-v"], { encoding: "utf8" });
const hasLua = luaProbe.status === 0;
const luaVer = hasLua ? /Lua [\d.]+/.exec(luaProbe.stderr)?.[0] : null;

const tmp = mkdtempSync(join(tmpdir(), "square-bench-"));
const s = await loadSquare();

console.log(
  `正确性校验 + 计时（中位数，warmup ${WARMUP}）：square(wasm) vs ${luaVer ?? "Lua(缺失)"} vs Node ${process.versions.node}\n`,
);

const rows = [];
try {
  for (const b of bench) {
    // 1) 正确性：三方在 nSq 下必须一致（JS 为参考实现）
    const want = String(b.js(b.nSq));
    let got = squareCheck(s, b, b.nSq);
    if (got !== want) {
      console.error(`✗ ${b.name}: square=${got} 期望=${want}`);
      process.exit(1);
    }
    if (hasLua) {
      got = runLua(b.lua, b.nSq, tmp).value;
      if (got !== want) {
        console.error(`✗ ${b.name}: lua=${got} 期望=${want}`);
        process.exit(1);
      }
    }

    // 2) 计时
    const sq = await timeSquare(s, b, b.nSq);
    const js = timeJs(b.js, b.n);
    const lua = hasLua ? runLua(b.lua, b.n, tmp).time : null;
    rows.push({ b, sq, js, lua });
    process.stderr.write(`.`);
  }
} finally {
  rmSync(tmp, { recursive: true, force: true });
}

const unit = (row) => (row.b.linear ? (ms, n) => ms / n : (ms) => ms);
console.log(`
bench      | 测量点          | N(lua/js)  | N(square)  | Lua(ms) | JS(ms) | square(ms) | sq/JS  | sq/Lua
-----------+-----------------+------------+------------+---------+--------+------------+--------+-------`);
for (const r of rows) {
  const u = unit(r);
  const f = (x) => (x == null ? "-" : x.toFixed(x >= 100 ? 0 : 1));
  const ratioJs = (u(r.sq, r.b.nSq) / u(r.js, r.b.n)).toFixed(1);
  const ratioLua = r.lua == null ? "-" : (u(r.sq, r.b.nSq) / u(r.lua, r.b.n)).toFixed(1);
  console.log(
    `${pad(r.b.name, 10)} | ${pad(r.b.dims, 14)} | ${String(r.b.n).padEnd(10)} | ${String(r.b.nSq).padEnd(10)} | ${f(r.lua).padStart(7)} | ${f(r.js).padStart(6)} | ${f(r.sq).padStart(10)} | ${ratioJs.padStart(6)}x | ${ratioLua === "-" ? "   -   " : ratioLua.padStart(6) + "x"}`,
  );
}
console.log(`
线性基准（loop/vec/closure/obj）square 与其他运行时 N 不同，比值按每单位工作量归一；
fib/tak 三方同 N 直接比。Lua 为 os.clock CPU 时间，其余为 wall 时间。`);

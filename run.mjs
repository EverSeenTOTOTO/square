// 宿主（Node）：加载 square.wasm、注入导入、调用导出的 `run()`，用来在命令行验证异步运行时。
//
// 与 docs/.vitepress/components/page-only/square/useSquare.ts 是同一套导入契约的 Node 版：
//   - `memory.write(ptr,len)`：客机打印时从线性内存读一段字节解码写 stdout。
//   - `host.js_sleep(id,ms)`：客机 park 时调用，我们排个 setTimeout，到点回调导出的
//     `wake_by_id(id)`——这就是「JS 事件 → 唤醒 Rust 任务 → 重新 poll」这条链的入口。
//   - `host.js_queue_microtask(id)`：同上，用 queueMicrotask 在当前同步栈清空后唤醒。
//
// 运行：先 `make build`（生成 square.wasm），再
//   node run.mjs [path/to/program.sq] [path/to/square.wasm]
// 不传参时默认跑内置 demo（并发 sleep + microtask）。

import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { performance } from "node:perf_hooks";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const wasmPath = process.argv[3] ?? join(here, "square.wasm");
const isFileArg = process.argv[2] && process.argv[2].endsWith(".sq");
const programFile = isFileArg ? process.argv[2] : null;
const source = readFileSync(programFile, "utf8");

const bytes = readFileSync(wasmPath);
const decoder = new TextDecoder();
const encoder = new TextEncoder();
const t0 = performance.now();

let memory;
let exports;
const { instance } = await WebAssembly.instantiate(bytes, {
  memory: {
    write: (ptr, len) =>
      process.stdout.write(
        decoder.decode(new Uint8Array(memory.buffer, ptr, len)),
      ),
  },
  host: {
    // 客机分配 id 后调 js_sleep(id, ms)；我们排 setTimeout，到点回调 wake_by_id(id)
    js_sleep: (id, ms) =>
      setTimeout(() => exports.wake_by_id(id), ms),
    // 客机调 js_queue_microtask(id)；用 queueMicrotask，宿主在当前同步栈清空后回调 wake_by_id(id)
    js_queue_microtask: (id) =>
      queueMicrotask(() => exports.wake_by_id(id)),
  },
});
exports = instance.exports;
memory = exports.memory;

// 把源码写进客机线性内存（alloc 借缓冲），编译成指令。
const enc = encoder.encode(source);
const addr = exports.alloc(enc.length);
new Uint8Array(memory.buffer, addr, enc.length).set(enc);
const insts = exports.compile(addr, enc.length);
exports.dealloc(addr, enc.length);

const vm = exports.init();
exports.run(vm, insts);
// console.log(`--- run returned @${Math.round(performance.now() - t0)}ms ---`);

// 等待异步回调把剩余任务跑完。并发 sleep 总耗时应 ≈ 单个 sleep 时长。
await new Promise((r) => setTimeout(r, 1500));
// console.log(`--- done @${Math.round(performance.now() - t0)}ms ---`);

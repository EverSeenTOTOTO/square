// 宿主（Node）侧的 square.wasm 驱动，抽出来供 run.mjs 和单测共用。
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const here = dirname(fileURLToPath(import.meta.url));
const defaultWasm = join(here, "square.wasm");

/**
 * 加载一个 square.wasm 实例，注入宿主导入，返回可编程 API。
 *
 * 程序输出（println）默认捕获到 `stdout()`；传 `onWrite(s)` 可同时转发（如写终端）。
 * 运行时是客机内的全局单例，每个 loadSquare 是独立实例——互不串状态。
 *
 * @param {{ wasmPath?: string, onWrite?: (s: string) => void }} [opts]
 */
export async function loadSquare({ wasmPath = defaultWasm, onWrite } = {}) {
  const bytes = readFileSync(wasmPath);
  const decoder = new TextDecoder();
  const encoder = new TextEncoder();
  const output = [];

  // 前向引用：host 回调里要用 exports.wake_by_id，而它要等 instantiate 完才就位。
  const ref = {};
  const write = (ptr, len) => {
    const s = decoder.decode(new Uint8Array(ref.memory.buffer, ptr, len));
    output.push(s);
    onWrite?.(s);
  };

  const { instance } = await WebAssembly.instantiate(bytes, {
    memory: { write },
    host: {
      js_sleep: (id, ms) => setTimeout(() => ref.exports.wake_by_id(id), ms),
      js_queue_microtask: (id) =>
        queueMicrotask(() => ref.exports.wake_by_id(id)),
    },
  });
  const exportsObj = instance.exports;
  ref.exports = exportsObj;
  ref.memory = exportsObj.memory;
  const vm = exportsObj.init();

  /** 编译源码成指令；返回绑定了 run/step 的 program 句柄。 */
  const program = (source) => {
    const enc = encoder.encode(source);
    const addr = exportsObj.alloc(enc.length);
    new Uint8Array(ref.memory.buffer, addr, enc.length).set(enc);
    const insts = exportsObj.compile(addr, enc.length);
    exportsObj.dealloc(addr, enc.length);
    return {
      insts,
      run: () => exportsObj.run(vm, insts),
      step: () => exportsObj.step(vm, insts),
    };
  };

  return {
    exports: exportsObj,
    memory: ref.memory,
    vm,
    program,
    reset: () => exportsObj.reset(vm),
    /** 迄今捕获的程序输出（含 panic 信息）。 */
    stdout: () => output.join(""),
  };
}

/** 推进 Node 事件循环：排空 microtask 并等 ms 毫秒，让异步 wake 触发。 */
export const flush = (ms = 0) => new Promise((r) => setTimeout(r, ms));

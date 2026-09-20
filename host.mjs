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

  /** square 闭包句柄 → JS 函数：实参经 call_cb 送回客机。
   *  投递一律 microtask 化：同步回调（如 forEach）会在 syscall 执行中途重入 VM，
   *  await 的同步结果会在 park 完成前唤醒任务（tick 会把未 park 的任务当已完成丢弃） */
  const squareCallback = (a) => {
    if (a && typeof a === "object" && !Array.isArray(a) && "__sq_cb" in a) {
      const id = a.__sq_cb;
      return (...as) => queueMicrotask(() => sendToSquare(id, as));
    }
    return a;
  };

  /** 宿主 → 客机唯一唤醒入口：实参数组序列化写入线性内存后 call_cb(id, ptr, len) */
  const sendToSquare = (id, args) => {
    const bytes = encoder.encode(JSON.stringify(args ?? []));
    const ptr = exportsObj.alloc(bytes.length);
    new Uint8Array(ref.memory.buffer, ptr, bytes.length).set(bytes);
    exportsObj.call_cb(id, ptr, bytes.length);
  };

  const packResult = (result) => {
    let json;
    try {
      json = JSON.stringify(result);
    } catch {
      json = "null";
    }
    const bytes = encoder.encode(json);
    const ptr = exportsObj.alloc(bytes.length);
    new Uint8Array(ref.memory.buffer, ptr, bytes.length).set(bytes);
    return (BigInt(ptr) << 32n) | BigInt(bytes.length);
  };

  // 宿主辅助：Promise 化 sleep（await 只走值/Promise 约定；回调风格 API 用闭包跨界）
  if (!globalThis.__square_sleep) {
    globalThis.__square_sleep = (ms) =>
      new Promise((resolve) => setTimeout(resolve, ms));
  }

  const { instance } = await WebAssembly.instantiate(bytes, {
    memory: { write },
    host: {
      // JS FFI：按点路径在 globalThis 解析、展开实参调用；结果 JSON 写回线性内存，
      // 返回 packed (ptr << 32 | len)。函数不存在/undefined → 0 句柄（客机置 nil）。
      // 参数中的 {"__sq_cb": id} 是 square 闭包句柄 → 换成 JS 函数（调用即 call_cb 唤醒）
      js_call: (name_ptr, name_len, args_ptr, args_len) => {
        const name = decoder.decode(
          new Uint8Array(ref.memory.buffer, name_ptr, name_len),
        );
        const args = JSON.parse(
          decoder.decode(new Uint8Array(ref.memory.buffer, args_ptr, args_len)),
        ).map(squareCallback);
        // 解析点路径同时保留父对象作接收者（Promise.reject 等需要 this）
        let parent = globalThis;
        let resolved = globalThis;
        for (const k of name.split(".")) {
          parent = resolved;
          resolved = resolved?.[k];
        }
        if (typeof resolved !== "function") {
          return packResult(resolved === undefined ? null : resolved);
        }
        let result;
        try {
          result = resolved.apply(parent, args);
        } catch (e) {
          // 宿主异常回传为错误对象：客机转 InstructionError（try 可捕获，实例存活）
          return packResult({ __sq_err: String(e) });
        }
        if (result === undefined) return 0n;
        return packResult(result);
      },
      // await 形态：调用后按结果回调 cb_id——Promise 则 .then(v)/.catch(e)，同步值立即回调
      js_await_call: (name_ptr, name_len, args_ptr, args_len, cb_id) => {
        const name = decoder.decode(
          new Uint8Array(ref.memory.buffer, name_ptr, name_len),
        );
        const args = JSON.parse(
          decoder.decode(new Uint8Array(ref.memory.buffer, args_ptr, args_len)),
        ).map(squareCallback);
        let parent = globalThis;
        let resolved = globalThis;
        for (const k of name.split(".")) {
          parent = resolved;
          resolved = resolved?.[k];
        }
        if (typeof resolved !== "function") {
          const v = resolved;
          queueMicrotask(() => sendToSquare(cb_id, v === undefined ? [] : [v]));
          return;
        }
        let result;
        try {
          result = resolved.apply(parent, args);
        } catch (e) {
          const err = String(e);
          queueMicrotask(() => sendToSquare(cb_id, [{ __sq_err: err }]));
          return;
        }
        if (result && typeof result.then === "function") {
          result.then(
            (v) => sendToSquare(cb_id, [v]),
            (e) => sendToSquare(cb_id, [{ __sq_err: String(e) }]),
          );
        } else {
          const v = result;
          queueMicrotask(() => sendToSquare(cb_id, v === undefined ? [] : [v]));
        }
      },
    },
  });
  const exportsObj = instance.exports;
  ref.exports = exportsObj;
  ref.memory = exportsObj.memory;
  const vm = exportsObj.init();

  /** 编译源码成指令；返回绑定了 run/step 的 program 句柄。
   * 编译失败（句柄为 0）抛 Error，错误文本已在实例输出里。 */
  const program = (source) => {
    const enc = encoder.encode(source);
    const addr = exportsObj.alloc(enc.length);
    new Uint8Array(ref.memory.buffer, addr, enc.length).set(enc);
    const insts = exportsObj.compile(addr, enc.length);
    exportsObj.dealloc(addr, enc.length);
    if (!insts) {
      throw new Error(output.join("").trim() || "compile failed");
    }
    return {
      insts,
      /** 返回 0 正常；1 运行错误（文本已写入输出，实例存活可复用） */
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

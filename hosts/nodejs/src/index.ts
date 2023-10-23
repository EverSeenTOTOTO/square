/* eslint-disable no-underscore-dangle */
import fs from 'fs';
import path from 'path';

const encoder = new TextEncoder();
const utf8Decoder = new TextDecoder('utf-8');

const readUtf8String = (memory: WebAssembly.Memory, offset: number, length: number) => {
  const array = new Uint8Array(memory.buffer, offset, length);
  return utf8Decoder.decode(array);
};

type WasmExports = {
  __data_end: WebAssembly.Global,
  __heap_base: WebAssembly.Global,
  memory: WebAssembly.Memory,

  main(): number,

  alloc(size: number): number,
  dealloc(ptr: number, size: number): void,

  execute(ptr: number, size: number): void,
};

(async () => {
  const wasm = await fs.promises.readFile(
    path.join(__dirname, '../../../sq.wasm'),
  );

  const { instance } = await WebAssembly.instantiate(wasm, {
    app: {
      execute() {
        const code = `
; similar to Lua coroutine
[= genFib /[n]
  [co.wrap /[]
    [= [a b] [1 1]]
    [while [<= a n]
      [co.yield a]
      [= [a b] [b [+ a b]]]]]]

[[genFib 100].forEach print]
`;

        const exports = instance.exports as WasmExports;
        const encodedString = encoder.encode(code);
        const addr = exports.alloc(encodedString.length);

        new Uint8Array(exports.memory.buffer, addr, encodedString.length).set(encodedString);

        exports.execute(addr, encodedString.length);
      },
    },
    memory: {
      write: (offset: number, length: number) => {
        const message = readUtf8String((instance.exports as WasmExports).memory, offset, length);

        process.stdout.write(message);
      },
    },
    wasm: {
      get_data_end() {
        return (instance.exports as WasmExports).__data_end.value;
      },
      get_heap_base() {
        return (instance.exports as WasmExports).__heap_base.value;
      },
      get_stack_base() {
        return (instance.exports as WasmExports).memory.buffer.byteLength;
      },
    },
  });

  const exports = instance.exports as WasmExports;

  const dataEnd = exports.__data_end.value;
  const heapBase = exports.__heap_base.value;
  const stackBase = exports.memory.buffer.byteLength;

  console.log(
    `Page count: ${exports.memory.buffer.byteLength / 64 / 1024}, data end: ${dataEnd}, heap base: ${heapBase}, stackBase: ${stackBase}`,
  );

  try {
    exports.main();
  } catch (e) {
    console.error(e);
  }
})();

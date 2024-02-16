/* eslint-disable no-underscore-dangle */
import fs from 'fs';
import path from 'path';

const utf8Decoder = new TextDecoder('utf-8');

const readUtf8String = (exports: WasmExports, offset: number, length: number) => {
  const array = new Uint8Array(exports.memory.buffer, offset, length);
  return utf8Decoder.decode(array);
};

const writeUtf8String = (exports: WasmExports, source: string) => {
  const encoder = new TextEncoder();
  const encodedString = encoder.encode(source);
  const sourceAddr = exports.alloc(encodedString.length);

  new Uint8Array(exports.memory.buffer, sourceAddr, encodedString.length).set(encodedString);

  return {
    sourceAddr,
    sourceLength: encodedString.length,
  };
};

type Square = {
  init_vm(): number,
  exec(vmAddr: number, sourceAddr: number, size: number): void,
};

type WasmExports = {
  __data_end: WebAssembly.Global,
  __heap_base: WebAssembly.Global,
  memory: WebAssembly.Memory,

  alloc(size: number): number,
  dealloc(ptr: number, size: number): void,
} & Square;

(async () => {
  const wasm = await fs.promises.readFile(
    path.join(__dirname, '../../../square.wasm'),
  );

  const { instance } = await WebAssembly.instantiate(wasm, {
    memory: {
      write: (offset: number, length: number) => {
        const message = readUtf8String(instance.exports as WasmExports, offset, length);

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

  const square = instance.exports as WasmExports;

  try {
    const vmAddr = square.init_vm();

    const code = `
[let fib /[n] [begin
  [if [<= n 0] 
      0
      [if [== n 1]
        1
        [+ [fib [- n 1]] [fib [- n 2]]]]]]]
[fib 16]
`;

    const { sourceAddr, sourceLength } = writeUtf8String(square, code);

    square.exec(vmAddr, sourceAddr, sourceLength);
    square.dealloc(sourceAddr, sourceLength);
  } catch (e) {
    console.error(e);
  }
})();

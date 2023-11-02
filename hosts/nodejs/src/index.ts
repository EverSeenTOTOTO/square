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
  parse_and_run(vmAddr: number, sourceAddr: number, size: number): void,
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
    path.join(__dirname, '../../../sq.wasm'),
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
; use built-in function \`obj\` to create an object
[= stack /[vec] [begin 
  [= this [obj]] ; \`this\` is just a variable name

  [= this.vec vec]

  [= this.clear /[] [= this.vec []]]

  [= this.push /[x] [begin 
    [= this.vec [.. this.vec [x]]]]]

  [= this.pop /[] [begin
    [= [... x] this.vec]
    [= this.vec [this.vec.slice 0 -1]]
    x]]

  this]]

[= v [1 2 3]]
[= s [stack v]]
[= x [s.pop]] ; x = 3
[s.clear]
[s.push 42]
[= y [s.pop]] ; y = 42
`;

    const { sourceAddr, sourceLength } = writeUtf8String(square, code);

    square.parse_and_run(vmAddr, sourceAddr, sourceLength);
    square.dealloc(sourceAddr, sourceLength);
  } catch (e) {
    console.error(e);
  }
})();

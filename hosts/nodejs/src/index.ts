/* eslint-disable no-underscore-dangle */
import fs from 'fs';
import path from 'path';

const encoder = new TextEncoder();
const utf8Decoder = new TextDecoder('utf-8');
const readUtf8String = (memory: WebAssembly.Memory, offset: number, length: number) => {
  const array = new Uint8Array(memory.buffer, offset, length);
  return utf8Decoder.decode(array);
};
const writeUtf8String = (memory: WebAssembly.Memory, offset: number, string: string) => {
  encoder.encodeInto(string, new Uint8Array(memory.buffer, offset, string.length));
};

(async () => {
  const wasm = await fs.promises.readFile(
    path.join(__dirname, '../../../sq.wasm'),
  );

  const { instance } = await WebAssembly.instantiate(wasm, {
    memory: {
      write: (offset: number, length: number) => {
        const message = readUtf8String(instance.exports.memory as WebAssembly.Memory, offset, length);

        process.stdout.write(message);
      },
    },
    wasm: {
      get_data_end() {
        return (instance.exports.__data_end as WebAssembly.Global).value;
      },
      get_heap_base() {
        return (instance.exports.__heap_base as WebAssembly.Global).value;
      },
      get_stack_base() {
        return (instance.exports.memory as WebAssembly.Memory).buffer.byteLength;
      },
    },
  });

  const exports = instance.exports as {
    __data_end: WebAssembly.Global,
    __heap_base: WebAssembly.Global,
    memory: WebAssembly.Memory,
    main(): number
  };

  const dataEnd = exports.__data_end.value;
  const heapBase = exports.__heap_base.value;
  const stackBase = exports.memory.buffer.byteLength;

  console.log(
    `Page count: ${exports.memory.buffer.byteLength / 64 / 1024}, data end: ${dataEnd}, heap base: ${heapBase}, stackBase: ${stackBase}`,
  );

  try {
    writeUtf8String(exports.memory, heapBase, 'Hello, world!');
    exports.main();
  } catch {
    // ...
  }

  const view = new DataView(exports.memory.buffer);

  console.log(view.getInt32(heapBase, true));
})();

/* eslint-disable no-underscore-dangle */
import fs from 'fs';
import path from 'path';

const utf8Decoder = new TextDecoder('utf-8');
const readUtf8String = (memory: WebAssembly.Memory, offset: number, length: number) => {
  const array = new Uint8Array(memory.buffer, offset, length);
  return utf8Decoder.decode(array);
};

(async () => {
  const wasm = await fs.promises.readFile(
    path.join(__dirname, '../../../sq.wasm'),
  );

  const { instance } = await WebAssembly.instantiate(wasm, {
    console: {
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
    },
  });

  const exports = instance.exports as {
    __data_end: WebAssembly.Global,
    __heap_base: WebAssembly.Global,
    memory: WebAssembly.Memory,
    main(): number
  };

  console.log(
    `Page size: ${exports.memory.buffer.byteLength / 64 / 1024}`,
  );

  exports.main();

  const view = new DataView(exports.memory.buffer, exports.__heap_base.value);

  console.log(view.getInt32(0, true));
})();

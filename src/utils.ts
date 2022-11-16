import { codeFrameColumns } from '@babel/code-frame';

export class Position {
  line: number;

  column: number;

  cursor: number;

  constructor(row = 0, column = 0, cursor = 0) {
    this.line = row;
    this.column = column;
    this.cursor = cursor;
  }

  clone(): Position {
    return new Position(this.line, this.column, this.cursor);
  }

  copy(another: Position) {
    this.line = another.line;
    this.column = another.column;
    this.cursor = another.cursor;
  }

  str() {
    return JSON.stringify(this);
  }
}

export const codeFrame = (input: string, message: string, start: Position, end?: Position): string => {
  return codeFrameColumns(
    input,
    {
      start: {
        line: start.line + 1, // @babel/code-frame starts from 1
        column: start.column + 1,
      },
      end: end
        ? {
          line: end.line + 1,
          column: end.column + 1,
        }
        : undefined,
    },
    {
      message,
    },
  );
};

export const repeat = <T>(x: T, length: number): T[] => {
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore
  // eslint-disable-next-line prefer-spread
  return Array.apply(null, { length }).map(() => x);
};

export const Constants = {
  EXPORTS: Symbol('exports'),
  IS_SQUARE_FUNC: Symbol('is_square_func'),
  RUNTIME_CONTINUATION: Symbol('runtime_continuation'),
  MAX_STACK_DEPTH: 200,
};

// 重用另一个对象的proto
export const reuseProto = <T extends object>(source: T, target: T) => {
  const proto = Object.getPrototypeOf(source);

  Object.setPrototypeOf(target, proto);
};

// 在proto上添加属性
export const setProtoProp = <T extends object, E extends object>(source: T, props: E) => {
  let proto = Object.getPrototypeOf(source);

  // avoid pollute builtin object
  if (proto === Function.prototype || proto === Object.prototype) {
    proto = Object.assign(Object.create(proto), proto);
  }

  Object.assign(proto, props);
  Object.setPrototypeOf(source, proto);
};

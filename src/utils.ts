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

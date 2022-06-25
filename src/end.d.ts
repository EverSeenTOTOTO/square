declare module '@babel/code-frame' {
  export function codeFrameColumns(
    input: string,
    loc: { start: { line: number, column: number }, end?: { line: number, column: number } },
    options: any
  ): string;
}

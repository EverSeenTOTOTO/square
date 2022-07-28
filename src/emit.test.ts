import * as emit from './emit';
import * as parse from './parse';

beforeEach(() => {
  emit.Temp.Index = 0;
  emit.Label.Index = 0;
});

const translateIR = (code: string) => {
  const expr = parse.parseExpr(code);
  const ir: emit.ThreeAddressCode[] = [];

  emit.emitExpr(expr, code, ir.push.bind(ir));

  return emit.stringifyIR(ir);
};

it('test op', () => {
  const ir = translateIR('[+ [* b -c] [* 4 -c]]');

  console.log(ir);

  expect(ir).toEqual([
    ['r0', '-', 'c'],
    ['r1', '*', 'b', 'r0'],
    ['r2', '-', 'c'],
    ['r3', '*', ['imm', 4], 'r2'],
    ['r4', '+', 'r1', 'r3'],
  ]);
});

it('test call', () => {
  const ir = translateIR('[foo bar 2]');

  console.log(ir);

  expect(ir).toEqual(
    [
      ['param', 'bar'],
      ['param', ['imm', 2]],
      ['r0', 'call', 'foo', 2],
    ],
  );
});

it('test dot', () => {
  const ir = translateIR('[foo 2].x.y');

  console.log(ir);

  expect(ir).toEqual([
    ['param', ['imm', 2]],
    ['r0', 'call', 'foo', 1],
    ['r1', 'perform', 'get', 'r0', '.x.y'],
  ]);
});

it('test func', () => {
  const ir = translateIR('/[] /[] [+ x 1]');

  console.log(ir);

  expect(ir).toEqual([
    ['f0', 'L0_Func'],
    ['L0_Func'],
    ['f1', 'L1_Func'],
    ['L1_Func'],
    ['r2', '+', 'x', ['imm', 1]],
    ['ret', 'r2'],
    ['ret', 'f1'],
  ]);
});

it('test if', () => {
  const ir = translateIR('[if foo bar]');

  console.log(ir);

  expect(ir).toEqual([
    ['test', 'foo', 'L0_IfElse'],
    ['t0', 'bar'],
    ['jump', 'L1_IfDone'],
    ['L0_IfElse'],
    ['L1_IfDone'],
  ]);
});

it('test ifelse', () => {
  const ir = translateIR('[if foo bar baz]');

  console.log(ir);

  expect(ir).toEqual([
    ['test', 'foo', 'L0_IfElse'],
    ['t0', 'bar'],
    ['jump', 'L1_IfDone'],
    ['L0_IfElse'],
    ['t0', 'baz'],
    ['L1_IfDone'],
  ]);
});

it('test begin', () => {
  const ir = translateIR('[begin [+ a 2] foo]');

  console.log(ir);

  expect(ir).toEqual([
    ['r0', '+', 'a', ['imm', 2]], ['r1', 'foo'],
  ]);
});

it('test while', () => {
  const ir = translateIR('[while [< i 0] [begin [-= i 1] [bar]]]');

  console.log(ir);

  expect(ir).toEqual([
    ['L0_WhileLoop'],
    ['r0', '<', 'i', ['imm', 0]],
    ['t1', '!', 'r0'],
    ['test', 't1', 'L1_WhileBreak'],
    ['i', '-', 'i', ['imm', 1]],
    ['r2', 'call', 'bar', 0],
    ['r3', 'r2'],
    ['jump', 'L0_WhileLoop'],
    ['L1_WhileBreak'],
    ['r4', ['imm', undefined]],
  ]);
});

it('test word', () => {
  const ir = translateIR('[in [regex a b] [typeof x]]');

  console.log(ir);

  expect(ir).toEqual([
    ['r0', 'perform', 'regex', 'a', 'b'],
    ['r1', 'perform', 'typeof', 'x', undefined],
    ['r2', 'perform', 'in', 'r0', 'r1'],
  ]);
});

it('test match', () => {
  const ir = translateIR(`
[match x
  [[< x 2] y]
  [[[regex '[a-z]+' 'gi'].test x] z]
  [default]]`);

  console.log(ir);

  expect(ir).toEqual([
    ['r0', ['imm', undefined]],
    ['r1', '<', 'x', ['imm', 2]],
    ['t2', '!', 'r1'],
    ['test', 't2', 'L1_Unmatched'],
    ['r0', 'y'],
    ['jump', 'L0_Matched'],
    ['L1_Unmatched'],
    ['r3', 'perform', 'regex', ['imm', '[a-z]+'], ['imm', 'gi']],
    ['r4', 'perform', 'get', 'r3', '.test'],
    ['param', 'x'],
    ['r5', 'call', 'r4', 1],
    ['t6', '!', 'r5'],
    ['test', 't6', 'L2_Unmatched'],
    ['r0', 'z'],
    ['jump', 'L0_Matched'],
    ['L2_Unmatched'],
    ['r0', 'default'],
    ['jump', 'L0_Matched'],
    ['L0_Matched'],
  ]);
});

it('test assign', () => {
  const ir = translateIR(`
[vec
  [= x 2]
  [= x.y 3]
  [= x [= y 4].z]]
`);

  console.log(ir);

  expect(ir).toEqual([
    ['x', ['imm', 2]],
    ['t0', 'perform', 'set', 'x', '.y', ['imm', 3]],
    ['y', ['imm', 4]],
    ['r1', 'perform', 'get', ['imm', 4], '.z'],
    ['x', 'r1'],
    ['r2', 'perform', 'vec', ['imm', 2], ['imm', 3], 'r1'],
  ]);
});

it('test expand assign', () => {
  const ir = translateIR('[ = [x . y ... [z .]] foo]');

  console.log(ir);

  expect(ir).toEqual([
    ['t0', ['imm', 0]],
    ['x', 'perform', 'at', 'foo', 't0'],
    ['t0', '+', 't0', ['imm', 1]],
    ['t0', '+', 't0', ['imm', 1]],
    ['y', 'perform', 'at', 'foo', 't0'],
    ['t0', '+', 't0', ['imm', 1]],
    ['t2', 'perform', 'len', 'foo'],
    ['t1', ['imm', 1]],
    ['t0', '-', 't2', 't1'],
    ['t3', 'perform', 'at', 'foo', 't0'],
    ['t4', ['imm', 0]],
    ['z', 'perform', 'at', 't3', 't4'],
    ['t4', '+', 't4', ['imm', 1]],
    ['t4', '+', 't4', ['imm', 1]],
    ['t0', '+', 't0', ['imm', 1]],
  ]);
});

it('test expand param', () => {
  const ir = translateIR('/[x . y ... [z .]] foo');

  console.log(ir);

  expect(ir).toEqual([
    ['f0', 'L0_Func'],
    ['L0_Func'],
    ['t1', 'param_tuple'],
    ['t2', ['imm', 0]],
    ['x', 'perform', 'at', 't1', 't2'],
    ['t2', '+', 't2', ['imm', 1]],
    ['t2', '+', 't2', ['imm', 1]],
    ['y', 'perform', 'at', 't1', 't2'],
    ['t2', '+', 't2', ['imm', 1]],
    ['t4', 'perform', 'len', 't1'],
    ['t3', ['imm', 1]],
    ['t2', '-', 't4', 't3'],
    ['t5', 'perform', 'at', 't1', 't2'],
    ['t6', ['imm', 0]],
    ['z', 'perform', 'at', 't5', 't6'],
    ['t6', '+', 't6', ['imm', 1]],
    ['t6', '+', 't6', ['imm', 1]],
    ['t2', '+', 't2', ['imm', 1]],
    ['ret', 'foo'],
  ]);
});

it('test mixed', () => {
  const ir = translateIR(`
[= fib /[n] [begin
  [console.log n]
  [match n
    [[< n 0] 0]
    [[< n 2] 1]
    [+
      [fib [- n 1]]
      [fib [- n 2]]]]]]
`);

  console.log(ir);

  expect(ir).toEqual([
    ['f0', 'L0_Func'],
    ['L0_Func'],
    ['t1', 'param_tuple'],
    ['t2', ['imm', 0]],
    ['n', 'perform', 'at', 't1', 't2'],
    ['t2', '+', 't2', ['imm', 1]],
    ['r3', 'perform', 'get', 'console', '.log'],
    ['param', 'n'],
    ['r4', 'call', 'r3', 1],
    ['r5', ['imm', undefined]],
    ['r6', '<', 'n', ['imm', 0]],
    ['t7', '!', 'r6'],
    ['test', 't7', 'L2_Unmatched'],
    ['r5', ['imm', 0]],
    ['jump', 'L1_Matched'],
    ['L2_Unmatched'],
    ['r8', '<', 'n', ['imm', 2]],
    ['t9', '!', 'r8'],
    ['test', 't9', 'L3_Unmatched'],
    ['r5', ['imm', 1]],
    ['jump', 'L1_Matched'],
    ['L3_Unmatched'],
    ['r10', '-', 'n', ['imm', 1]],
    ['param', 'r10'],
    ['r11', 'call', 'fib', 1],
    ['r12', '-', 'n', ['imm', 2]],
    ['param', 'r12'],
    ['r13', 'call', 'fib', 1],
    ['r14', '+', 'r11', 'r13'],
    ['r5', 'r14'],
    ['jump', 'L1_Matched'],
    ['L1_Matched'],
    ['r15', 'r5'],
    ['ret', 'r15'],
    ['fib', 'f0'],
  ]);
});

it('test mixed2', () => {
  const ir = translateIR(`
[= gen /[yield] [begin
  [[.. 1 10].forEach /[x] [callcc /[cc] [yield [vec x cc]]]]]]
`);

  console.log(ir);

  expect(ir).toEqual([
    ['f0', 'L0_Func'],
    ['L0_Func'],
    ['t1', 'param_tuple'],
    ['t2', ['imm', 0]],
    ['yield', 'perform', 'at', 't1', 't2'],
    ['t2', '+', 't2', ['imm', 1]],
    ['r3', '..', ['imm', 1], ['imm', 10]],
    ['r4', 'perform', 'get', 'r3', '.forEach'],
    ['f5', 'L1_Func'],
    ['L1_Func'],
    ['t6', 'param_tuple'],
    ['t7', ['imm', 0]],
    ['x', 'perform', 'at', 't6', 't7'],
    ['t7', '+', 't7', ['imm', 1]],
    ['f8', 'L2_Func'],
    ['L2_Func'],
    ['t9', 'param_tuple'],
    ['t10', ['imm', 0]],
    ['cc', 'perform', 'at', 't9', 't10'],
    ['t10', '+', 't10', ['imm', 1]],
    ['r11', 'perform', 'vec', 'x', 'cc'],
    ['param', 'r11'],
    ['r12', 'call', 'yield', 1],
    ['ret', 'r12'],
    ['param', 'f8'],
    ['r13', 'call', 'callcc', 1],
    ['ret', 'r13'],
    ['param', 'f5'],
    ['r14', 'call', 'r4', 1],
    ['r15', 'r14'],
    ['ret', 'r15'],
    ['gen', 'f0'],
  ]);
});

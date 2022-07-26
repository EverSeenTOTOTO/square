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
    ['r1', 'perform', 'dot', 'r0', '.x.y'],
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
    ['i', '-=', 'i', ['imm', 1]],
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
    ['r4', 'perform', 'dot', 'r3', '.test'],
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

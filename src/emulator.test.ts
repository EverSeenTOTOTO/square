import { Emulator } from './emulator';

it('test move', () => {
  const emulator = new Emulator();

  emulator.installInstructions([
    ['move', 't0', ['imm', 42]],
    ['move', 's0', ['reg', 't0']],
  ]);
  emulator.run();

  expect(emulator.getRegister('t0')).toBe(42);
  expect(emulator.getRegister('s0')).toBe(42);
});

it('test save', () => {
  const emulator = new Emulator();

  emulator.installInstructions([
    ['move', 't0', ['imm', 42]],
    ['save', 't0'],
  ]);
  emulator.run();

  expect(emulator.getRegister('t0')).toBe(42);
  expect(emulator.sp).toBe(0);
  expect(emulator.popStack()).toBe(42);
  expect(emulator.sp).toBe(-1);
});

it('test load', () => {
  const emulator = new Emulator();

  emulator.installInstructions([
    ['move', 't0', ['imm', 42]],
    ['save', 't0'],
    ['load', 's2'],
  ]);
  emulator.run();

  expect(emulator.getRegister('s2')).toBe(42);
});

it('test jump', () => {
  const emulator = new Emulator();

  emulator.installInstructions([
    ['jump', 'somewhere'],
    ['move', 't1', ['imm', 42]], // skip
    ['label', 'somewhere'],
  ]);
  emulator.run();

  expect(emulator.getRegister('t1')).toBeUndefined();
});

it('test test', () => {
  const emulator = new Emulator();

  emulator.installInstructions([
    ['move', 't0', ['imm', 42]],
    ['test', ['reg', 't0'], ['imm', 24], 'else'],
    ['move', 's0', ['imm', 0]],
    ['label', 'else'],
    ['move', 's1', ['imm', 1]],
  ]);
  emulator.run();

  expect(emulator.getRegister('s0')).toBeUndefined();
  expect(emulator.getRegister('s1')).toBe(1);
});

it('test perform', () => {
  const emulator = new Emulator();
  const foo: number[] = [];

  emulator.installInstructions([
    ['move', 't0', ['imm', foo.pop.bind(foo)]],
    ['perform', ['imm', foo.push.bind(foo)], ['imm', 42], ['imm', '42']],
    ['perform', ['reg', 't0']],
  ]);

  emulator.step();
  emulator.step();
  expect(foo).toEqual([42, '42']);

  emulator.step();
  expect(foo).toEqual([42]);
});

/* eslint-disable max-classes-per-file */

type PrimitiveValue = ['reg' | 'imm', unknown];

type MoveInstruction = ['move', string, PrimitiveValue];
type SaveInstruction = ['save', string];
type LoadInstruction = ['load', string];
type LabelInstruction = ['label', string];
type TestInstruction = ['test', PrimitiveValue, PrimitiveValue, string];
type JumpInstruction = ['jump', string];
type PerformInstruction = ['perform', PrimitiveValue, ...PrimitiveValue[]];

type Instructions =
  | MoveInstruction
  | SaveInstruction
  | LoadInstruction
  | LabelInstruction
  | TestInstruction
  | JumpInstruction
  | PerformInstruction;

type InstructionNames = Instructions[0];

type CompiledInstructions = [inst: InstructionNames, action: () => void][];

// 模拟立即数或者直接寻址
const fetchPrimitive = (prim: PrimitiveValue, emulator: Emulator) => {
  const [type, value] = prim;

  switch (type) {
    case 'reg':
      return emulator.getRegister(value as string);
    case 'imm':
      return value;
    default:
      throw new Error(`Unknown primitive type: ${type}`);
  }
};

const compileMove = (inst: MoveInstruction, emulator: Emulator) => () => {
  const [, $1, $2] = inst;
  const value = fetchPrimitive($2, emulator);

  emulator.setRegister($1, value);
};

const compileSave = (inst: SaveInstruction, emulator: Emulator) => () => {
  const [, reg] = inst;
  const value = emulator.getRegister(reg);

  emulator.pushStack(value);
};

const compileLoad = (inst: LoadInstruction, emulator: Emulator) => () => {
  const [, reg] = inst;
  const value = emulator.popStack();

  emulator.setRegister(reg, value);
};

const compileTest = (inst: TestInstruction, emulator: Emulator) => () => {
  const [, $1, $2, label] = inst;
  const lhs = fetchPrimitive($1, emulator);
  const rhs = fetchPrimitive($2, emulator);

  if (lhs !== rhs) { // goto label
    const position = emulator.getLabel(label);

    emulator.setRegister('pc', position);
  }
};

const compileJump = (inst: JumpInstruction, emulator: Emulator) => () => {
  const [, label] = inst;
  const position = emulator.getLabel(label);

  emulator.setRegister('pc', position);
};

const compilePerform = (inst: PerformInstruction, emulator: Emulator) => () => {
  const [, op, ...params] = inst;
  const func = fetchPrimitive(op, emulator);

  if (typeof func !== 'function') {
    throw new Error(`Unable to perform operation, op: ${op}`);
  }

  return func(...params.map((each) => fetchPrimitive(each, emulator)));
};

export class Register<T> {
  readonly name: string;

  value?: T;

  constructor(name: string, value?: T) {
    this.name = name;
    this.value = value;
  }

  get() {
    return this.value;
  }

  set(value: T) {
    this.value = value;
  }
}

export class Emulator {
  protected readonly pc: Register<number>;

  protected readonly fp: Register<number>;

  protected readonly ra: Register<number>;

  protected readonly registers: Map<string, Register<unknown>>;

  protected stack: Array<unknown> = [];

  protected labels: Map<string, number> = new Map<string, number>();

  protected instructions: CompiledInstructions = [];

  constructor() {
    this.pc = new Register<number>('pc', 0);
    this.fp = new Register<number>('fp', 0);
    this.ra = new Register<number>('ra');

    this.registers = new Map<string, Register<unknown>>([
      ['pc', this.pc],
      ['fp', this.fp],
      ['ra', this.ra],
      ['a0', new Register<unknown>('a0')],
      ['a1', new Register<unknown>('a1')],
      ['t0', new Register<unknown>('t0')],
      ['t1', new Register<unknown>('t1')],
      ['t2', new Register<unknown>('t2')],
      ['s0', new Register<unknown>('s0')],
      ['s1', new Register<unknown>('s1')],
      ['s2', new Register<unknown>('s2')],
    ]);
  }

  get sp() { // stack pointer register
    return this.stack.length - 1;
  }

  popStack() {
    return this.stack.pop();
  }

  pushStack(value: unknown) {
    return this.stack.push(value);
  }

  getRegister(name: string) {
    const reg = this.registers.get(name);

    if (!reg) {
      throw new Error(`No such register, register name: ${name}`);
    }

    return reg.get();
  }

  setRegister(name: string, value: unknown) {
    const reg = this.registers.get(name);

    if (!reg) {
      throw new Error(`No such register, register name: ${name}`);
    }

    return reg.set(value);
  }

  getLabel(label: string) {
    const position = this.labels.get(label);

    if (!position) {
      throw new Error(`Undefined label: ${label}`);
    }

    return position;
  }

  setLabel(label: string, position: number) {
    return this.labels.set(label, position);
  }

  get labelCount() {
    return [...this.labels.values()].length;
  }

  reset() {
    this.setRegister('pc', 0);
    this.setRegister('fp', 0);
    this.stack = [];
    this.labels.clear();
  }

  installInstructions(insts: Instructions[]) {
    for (const inst of insts) {
      switch (inst[0]) {
        case 'move':
          this.instructions.push([inst[0], compileMove(inst, this)]);
          break;
        case 'save':
          this.instructions.push([inst[0], compileSave(inst, this)]);
          break;
        case 'load':
          this.instructions.push([inst[0], compileLoad(inst, this)]);
          break;
        case 'test':
          this.instructions.push([inst[0], compileTest(inst, this)]);
          break;
        case 'jump':
          this.instructions.push([inst[0], compileJump(inst, this)]);
          break;
        case 'perform':
          this.instructions.push([inst[0], compilePerform(inst, this)]);
          break;
        case 'label':
          this.setLabel(inst[1], this.instructions.length);
          break;
        default:
          throw new Error(`Unknown instruction: ${inst[0]}`);
      }
    }
  }

  run() {
    while (this.step());
  }

  step() {
    const pos = this.pc.get();

    if (typeof pos === 'number' && pos < this.instructions.length) {
      const [, action] = this.instructions[pos];

      this.advancePC();
      action();

      return true;
    }

    return false;
  }

  protected advancePC() {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const pc = this.pc.get()!;

    this.pc.set(pc + 1);
  }
}

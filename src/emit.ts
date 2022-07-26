/* eslint-disable max-classes-per-file */
/* eslint-disable @typescript-eslint/no-use-before-define */
import * as parse from './parse';
import { Token } from './scan';
import { codeFrame, Position } from './utils';

export class Temp extends parse.Id {
  static Index = 0;

  constructor(prefix = 't') {
    super({
      type: 'id',
      source: `${prefix}${Temp.Index++}`,
      pos: new Position(), // useless
    });
  }
}

export class Label extends parse.Node {
  static Index = 0;

  readonly name: string;

  constructor(info: string) {
    super('Label', new Position());
    this.name = `L${Label.Index++}_${info}`;
  }
}

class Immediate extends parse.Node {
  readonly value: string | number | boolean | void | [];

  constructor(value: Immediate['value']) {
    super('Immediate', new Position());
    this.value = value;
  }

  val() {
    return ['imm', this.value];
  }
}

type WordOps = 'in' | 'and' | 'or' | 'regex' | 'vec' | 'obj' | 'instanceof' | 'typeof';
type ExternalOps = Token['type'] | WordOps;
type Operand = Immediate | Label | parse.Id;

type BinOpIR = [parse.Id, ExternalOps, Operand, Operand];
type UnOpIR = [parse.Id, ExternalOps, Operand];
type AssignIR = [parse.Id, Operand];
type JumpIR = ['jump', Label];
type TestIR = ['test', Operand, Label];
type ParamIR = ['param', Operand];
type CallIR = [parse.Id, 'call', Operand, number] | ['call', Operand, number];
type RetIR = ['ret', Operand];
type PerformIR = [parse.Id, 'perform', ...unknown[]] | ['perform', ...unknown[]]; // shortcut! perform JS external calls

export type ThreeAddressCode =
  | BinOpIR
  | UnOpIR
  | AssignIR
  | JumpIR
  | TestIR
  | ParamIR
  | CallIR
  | RetIR
  | PerformIR
  | [Label];

export const stringifyIR = (ir: ThreeAddressCode[]) => {
  return ir.map((c: unknown[]) => {
    return c.map((part: unknown) => {
      if (part instanceof parse.Id) {
        return part.name.source;
      }

      if (part instanceof Label) {
        return part.name;
      }

      if (part instanceof Immediate) {
        return part.val();
      }

      if (part instanceof parse.Dot) {
        return part.key();
      }

      return part;
    });
  });
};

type Emit = (tac: ThreeAddressCode) => void;

export function emitExpr(expr: parse.Expr, input: string, emit: Emit) {
  const cont = (temp: Operand) => {
    return expr.dot
      ? emitDot(expr.dot, input, emit)(temp)
      : temp;
  };

  switch (expr.master.type) {
    case 'BinOpExpr':
      return cont(emitBinOp(expr.master as parse.BinOpExpr, input, emit));
    case 'UnOpExpr':
      return cont(emitUnOp(expr.master as parse.UnOpExpr, input, emit));
    case 'Func':
      return cont(emitFunc(expr.master as parse.Func, input, emit));
    case 'Id':
      return cont(emitId(expr.master as parse.Id, input, emit));
    case 'Lit':
      return cont(emitLit(expr.master as parse.Lit, input, emit));
    case 'Call':
      return cont(emitCall(expr.master as parse.Call, input, emit));
    default:
      throw new Error(codeFrame(input, `Emit error, expect <expr>, got ${expr.type}`, expr.pos));
  }
}

function emitDot(expr: parse.Dot, _input: string, emit: Emit) {
  return (x: Operand) => {
    const result = new Temp('r');

    emit([result, 'perform', 'dot', x, expr]);

    return result;
  };
}

export function emitCall(expr: parse.Call, input: string, emit: Emit): Operand {
  if (expr.isEmpty()) return new Immediate([]);

  const callerExpr = expr.children[0] as parse.Expr;
  const { master } = callerExpr;

  // aims to be plugable
  if (master.type === 'Id') {
    switch ((master as parse.Id).name.source) {
      case 'begin':
        return emitBegin(expr, input, emit);
      case 'if':
        return emitIf(expr, input, emit);
      case 'match':
        return emitMatch(expr, input, emit);
      case 'while':
        return emitWhile(expr, input, emit);
      case 'regex':
      case 'vec':
      case 'obj':
      case 'and':
      case 'or':
      case 'in':
      case 'instanceof':
      case 'typeof':
        return emitWordOp(expr, input, emit);
      default:
        break;
    }
  }

  const caller = emitExpr(callerExpr, input, emit);

  // t0 = + x y
  if (['BinOpExpr', 'Assign'].indexOf(master.type) !== -1) {
    return caller;
  }

  const rest = expr.children.slice(1).map((e) => emitExpr(e as parse.Expr, input, emit));
  const result = new Temp('r');

  // caller(...rest)
  rest.forEach((temp) => emit(['param', temp]));
  emit([result, 'call', caller, rest.length]);

  return result;
}

export function emitExpand(expr: parse.Expand, input: string, emit: Emit) {
  // TODO
}

export function emitFunc(expr: parse.Func, input: string, emit: Emit) {
  const label = new Label('Func');
  const result = new Temp('f');

  emit([result, label]);
  emit([label]);

  if (expr.param.type === 'Expand') {
    emitExpand(expr.param as parse.Expand, input, emit);
  }

  emit(['ret', emitExpr(expr.body, input, emit)]);

  return result;
}

export function emitUnOp(expr: parse.UnOpExpr, input: string, emit: Emit) {
  const value = emitExpr(expr.value, input, emit);
  const result = new Temp('r');

  emit([result, expr.op.type, value]); // t0 = ! x

  return result;
}

export function emitBinOp(expr: parse.BinOpExpr, input: string, emit: Emit): parse.Id {
  const lhs = emitExpr(expr.lhs, input, emit);
  const rhs = emitExpr(expr.rhs, input, emit);

  if (['+=', '-=', '*=', '/=', '>=', '<=', '%=', '^='].indexOf(expr.op.type) !== -1) {
    if (!(lhs instanceof parse.Id)) {
      throw new Error(codeFrame(input, `Emit error, expect lvalue, got ${typeof lhs}`, expr.op.pos));
    }

    emit([lhs, expr.op.type, lhs, rhs]);

    return lhs;
  }
  const result = new Temp('r');
  emit([result, expr.op.type, lhs, rhs]); // t0 = i + 1
  return result;
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function emitId(expr: parse.Id, _input: string, _emit: Emit) {
  return expr;
}

// literal: immediate value
// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function emitLit(expr: parse.Lit, input: string, _emit: Emit) {
  switch (expr.value.type) {
    case 'str':
      return new Immediate(expr.value.source.replace(/^'|'$/g, '').replace(/\\'/g, '\''));
    case 'bool':
      return new Immediate(expr.value.source === 'true');
    case 'dig':
      return new Immediate(Number(expr.value.source));
    case 'bin': {
      const match = /^0b(?<B>[01]+)(?<M>\.[01]+)?$/.exec(expr.value.source);

      if (match && match.groups) {
        const { B, M } = match.groups;

        return new Immediate(computeNumber(2, B, M));
      }

      throw new Error(codeFrame(input, `Syntax error, expect <bin>, got ${expr.value.type}`, expr.pos));
    } case 'hex': {
      const match = /^0x(?<B>[0-9a-fA-F]+)(?<M>\.[0-9a-fA-F]+)?$/.exec(expr.value.source);

      if (match && match.groups) {
        const { B, M } = match.groups;

        return new Immediate(computeNumber(16, B, M));
      }

      throw new Error(codeFrame(input, `Syntax error, expect <hex>, got ${expr.value.type}`, expr.pos));
    }
    default:
      throw new Error(codeFrame(input, `Emit error, expect <literal>, got ${expr.value.type}`, expr.pos));
  }
}

function computeNumber(base: number, B: string, M?: string) {
  const prefix = base === 2 ? '0b' : '0x';

  let value = 0;

  for (let i = 0; i < B.length; ++i) {
    value += Number(prefix + B[B.length - 1 - i]) * (base ** i);
  }

  if (M) {
    for (let i = 0; i < M.length - 1; ++i) { // M[0] is '.'
      value += Number(prefix + M[i + 1]) * (base ** -(i + 1));
    }
  }

  return value;
}

export function emitIf(expr: parse.Call, input: string, emit: Emit) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;
  const cond = expr.children[1] as parse.Expr;
  const then = expr.children[2] as parse.Expr;

  if (!cond) {
    throw new Error(codeFrame(input, 'Syntax error, no condition for <if>', keyword.pos));
  }

  if (!then) {
    throw new Error(codeFrame(input, 'Syntax error, no then statement for <if>', keyword.pos));
  }

  if (expr.children.length > 4) {
    throw new Error(codeFrame(input, 'Syntax error, extra statements for <if>', keyword.pos));
  }

  const condValue = emitExpr(cond, input, emit);
  const elseLabel = new Label('IfElse');
  const doneLabel = new Label('IfDone');

  emit(['test', condValue, elseLabel]);

  const result = new Temp();
  const thenValue = emitExpr(then, input, emit);

  emit([result, thenValue]);
  emit(['jump', doneLabel]);
  emit([elseLabel]);

  if (expr.children[3]) {
    const elseValue = emitExpr(expr.children[3] as parse.Expr, input, emit);

    emit([result, elseValue]);
  }

  emit([doneLabel]);

  return result;
}

export function emitBegin(expr: parse.Call, input: string, emit: Emit) {
  const temps = expr.children.slice(1).map((e) => emitExpr(e as parse.Expr, input, emit));
  const result = new Temp('r');

  emit([result, temps[temps.length - 1]]);

  return result;
}

export function emitWhile(expr: parse.Call, input: string, emit: Emit) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;
  const cond = expr.children[1] as parse.Expr;

  if (!cond) {
    throw new Error(codeFrame(input, 'Syntax error, no condition for <while>', keyword.pos));
  }

  const loopLabel = new Label('WhileLoop');
  const breakLabel = new Label('WhileBreak');

  emit([loopLabel]);

  const condValue = emitExpr(cond, input, emit);
  const notValue = new Temp();

  emit([notValue, '!', condValue]);
  emit(['test', notValue, breakLabel]);
  expr.children.slice(2).forEach((e) => emitExpr(e as parse.Expr, input, emit));
  emit(['jump', loopLabel]);
  emit([breakLabel]);

  const whileReturn = new Temp('r');

  emit([whileReturn, new Immediate()]);

  return whileReturn;
}

export function emitWordOp(expr: parse.Call, input: string, emit: Emit) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;
  const lhs = emitExpr(expr.children[1] as parse.Expr, input, emit);
  const rhs = expr.children[2] && emitExpr(expr.children[2] as parse.Expr, input, emit);

  if (expr.children.length > 3) {
    throw new Error(codeFrame(input, `Syntax error, extra statements for ${keyword.name.source}`, keyword.pos));
  }

  const result = new Temp('r');

  emit([result, 'perform', keyword.name.source, lhs, rhs]);

  return result;
}

export function emitMatch(expr: parse.Call, input: string, emit: Emit) {
  emitExpr(expr.children[1] as parse.Expr, input, emit);

  const doneLabel = new Label('Matched');
  const result = new Temp('r');

  emit([result, new Immediate()]);

  let rest = expr.children.slice(2);

  while (rest.length > 0) {
    const [first, second] = ((rest[0] as parse.Expr).master as parse.Call).children;
    const match = emitExpr(first as parse.Expr, input, emit);

    if (!second) {
      emit([result, match]);
      emit(['jump', doneLabel]);
    } else {
      const unmatchedLabel = new Label('Unmatched');
      const unmatch = new Temp();

      emit([unmatch, '!', match]);
      emit(['test', unmatch, unmatchedLabel]);
      emit([result, emitExpr(second as parse.Expr, input, emit)]);
      emit(['jump', doneLabel]);
      emit([unmatchedLabel]);
    }

    rest = rest.slice(1);
  }

  emit([doneLabel]);

  return result;
}

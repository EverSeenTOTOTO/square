/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-use-before-define */
import { repeat, codeFrame, Constants } from './utils';
import * as parse from './parse';

export class Env extends Map<string | symbol, unknown> {
  type?: string;

  parent?: Env;

  constructor(parent?: Env, type?: string) {
    super();
    this.parent = parent;
    this.type = type;
  }

  lookup(variable: string): { value: any, env: Env } {
    const value = this.get(variable);

    if (value === undefined && this.parent) {
      return this.parent.lookup(variable);
    }

    return { value, env: this };
  }

  obj(): Record<string, unknown> {
    const result: Record<string, unknown> = {};

    for (const [k, v] of [...this.entries()]) {
      if (typeof k === 'string') {
        result[k] = v;
      }
    }

    return result;
  }
}

export function evalExpr(expr: parse.Expr, input: string, env = new Env()): any {
  let value;

  switch (expr.master.type) {
    case 'BinOpExpr':
      value = evalBinOp(expr.master as parse.BinOpExpr, input, env);
      break;
    case 'UnOpExpr':
      value = evalUnOp(expr.master as parse.UnOpExpr, input, env);
      break;
    case 'Assign':
      value = evalAssign(expr.master as parse.Assign, input, env);
      break;
    case 'Id':
      value = evalId(expr.master as parse.Id, input, env);
      break;
    case 'Lit':
      value = evalLit(expr.master as parse.Lit, input, env);
      break;
    case 'Func':
      value = evalFunc(expr.master as parse.Func, input, env);
      break;
    case 'Call':
      value = evalCall(expr.master as parse.Call, input, env);
      break;
    default:
      throw new Error(`Eval error, expect <expr>, got ${expr.type}`);
  }

  if (expr.dot) {
    value = evalDot(expr.dot, input, env)(value);
  }

  return value;
}

export function evalCall(expr: parse.Call, input: string, env: Env) {
  if (expr.isEmpty()) return [];

  const { master } = expr.children[0] as parse.Expr;

  // aims to be plugable
  if (master.type === 'Id') {
    switch ((master as parse.Id).name.source) {
      case 'begin':
        return evalBegin(expr, input, env);
      case 'if':
        return evalIf(expr, input, env);
      case 'match':
        return evalMatch(expr, input, env);
      case 'while':
        return evalWhile(expr, input, env);
      case 'export':
        return evalExport(expr, input, env);
      case 'regex':
      case 'and':
      case 'or':
      case 'in':
        return evalWordOp(expr, input, env);
      default:
        break;
    }
  }

  const caller = evalExpr(expr.children[0] as parse.Expr, input, env);

  if (['BinOpExpr', 'Assign'].indexOf(master.type) !== -1) {
    return caller;
  }

  const rest = expr.children.slice(1).map((e) => evalExpr(e as parse.Expr, input, env));

  if (typeof caller !== 'function') {
    return [caller, ...rest];
  }

  return caller(...rest);
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function evalDot(expr: parse.Dot, input: string, _env: Env) {
  return (master: any, value?: any) => {
    let obj = master;
    let ptr = expr;
    let key = ptr.id.name.source;

    while (Object(obj) === obj && obj !== null && ptr.next) {
      const child = obj[ptr.id.name.source];

      obj = typeof child === 'function' ? child.bind(obj) : child;
      ptr = ptr.next;
      key += `.${ptr.id.name.source}`;
    }

    if (ptr.next || obj === undefined) {
      throw new Error(codeFrame(input, `Eval error, expect ${key} to be object, got ${typeof obj}`, expr.id.name.pos));
    }

    if (value !== undefined) {
      obj[ptr.id.name.source] = value;
      return value;
    }

    const result = obj[ptr.id.name.source];

    return typeof result === 'function' ? result.bind(obj) : result;
  };
}

export function evalExpand(expr: parse.Expand, input: string, env: Env) {
  return (...value: any[]) => {
    let cursor = 0;

    expr.items.forEach((item, idx) => {
      if (cursor >= value.length) {
        throw new Error(codeFrame(input, `Eval error, expect ${cursor + 1} arguments, got: ${value.length}`, expr.bracketL.pos));
      }

      switch (item.type) {
        case '.':
          cursor++;
          break;
        case '...': {
          const count = expr.items.slice(idx + 1).filter((it) => it.type === '...').length; // '...' placeholder can be nothing
          const newCursor = value.length - (expr.items.length - idx - 1) + count;

          if (newCursor < cursor) {
            throw new Error(codeFrame(input, `Eval error, expect ${cursor + 1} arguments, got: ${value.length}`, expr.bracketL.pos));
          }

          cursor = newCursor;
          break;
        } case 'Id':
          env.set((item as parse.Id).name.source, value[cursor++]);
          break;
        case 'Expand':
          evalExpand(item as parse.Expand, input, env)(...value[cursor++]);
          break;
        default:
          throw new Error(codeFrame(input, `Eval error, expect <expand>, got ${item.type}`, expr.bracketL.pos));
      }
    });
  };
}

export function evalFunc(expr: parse.Func, input: string, env: Env) {
  return (...params: any[]) => {
    const bodyEnv = new Env(env, 'func');

    if (expr.param.type === 'Expand') {
      evalExpand(expr.param as parse.Expand, input, bodyEnv)(...params);
    }// else expect no arguments

    return evalExpr(expr.body, input, bodyEnv);
  };
}

export function evalAssign(expr: parse.Assign, input: string, env: Env) {
  const value = evalExpr(expr.assignment, input, env);

  if (expr.variable.type === 'Id') {
    const key = (expr.variable as parse.Id).name.source;

    if (expr.dot) {
      const record = env.lookup(key);

      evalDot(expr.dot, input, env)(record.value, value);
      record.env.set(key, record.value);
    } else {
      env.set(key, value);
    }
  } else {
    evalExpand(expr.variable as parse.Expand, input, env)(...value);
  }
}

export function evalId(expr: parse.Id, input: string, env: Env) {
  const id = expr.name.source;
  const { value } = env.lookup(id);

  if (value === undefined) {
    throw new Error(codeFrame(input, `Eval error, undefined identifier: ${id}`, expr.name.pos));
  }

  return value;
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function evalLit(expr: parse.Lit, input: string, _env: Env) {
  switch (expr.value.type) {
    case 'num':
      return Number(expr.value.source);
    case 'str':
      return expr.value.source.replace(/^'|'$/g, '').replace(/\\'/g, '\'');
    case 'bool':
      return expr.value.source === 'true';
    default:
      throw new Error(codeFrame(input, `Eval error, expect <literal>, got ${expr.value.type}`, expr.value.pos));
  }
}

export function evalUnOp(expr: parse.UnOpExpr, input: string, env: Env) {
  const value = evalExpr(expr.value, input, env);
  switch (expr.op.type) {
    case '!':
      return !value;
    case '-':
      return -value;
    default:
      throw new Error(codeFrame(input, `Eval error, expect <unOp>, got ${expr.op.type}`, expr.op.pos));
  }
}

export function evalBinOp(expr: parse.BinOpExpr, input: string, env: Env) {
  const lhs = evalExpr(expr.lhs, input, env);
  const rhs = evalExpr(expr.rhs, input, env);

  const setLhs = (value: any) => {
    const { master, dot } = expr.lhs;

    if (master.type === 'Id') {
      const key = (master as parse.Id).name.source;
      const record = env.lookup(key);

      if (dot) {
        evalDot(dot, input, env)(record.value, value);
      } else {
        record.value = value;
      }

      record.env.set(key, record.value);
    } else {
      throw new Error(codeFrame(input, `Eval error, cannot assign on ${master.type}`, expr.op.pos));
    }

    return value;
  };

  switch (expr.op.type) {
    case '+':
      return lhs + rhs;
    case '+=':
      return setLhs(lhs + rhs);
    case '-':
      return lhs - rhs;
    case '-=':
      return setLhs(lhs - rhs);
    case '*':
      return lhs * rhs;
    case '*=':
      return setLhs(lhs * rhs);
    case '/':
      return lhs / rhs;
    case '/=':
      return setLhs(lhs / rhs);
    case '>':
      return lhs > rhs;
    case '>=':
      return lhs >= rhs;
    case '<':
      return lhs < rhs;
    case '<=':
      return lhs <= rhs;
    case '%':
      return lhs % rhs;
    case '%=':
      return setLhs(lhs % rhs);
    case '^':
      return lhs ** rhs;
    case '^=':
      return setLhs(lhs ** rhs);
    case '==':
      return lhs === rhs;
    case '!=':
      return lhs !== rhs;
    case '..': {
      if (typeof lhs === 'string' && typeof rhs === 'string') {
        return lhs + rhs;
      }

      if (Number.isInteger(lhs) && Number.isInteger(rhs)) { // range [lhs, rhs)
        return repeat(lhs, Math.abs(lhs - rhs)).map((x, idx) => x + (lhs > rhs ? -idx : idx));
      }

      if (Array.isArray(lhs) && Array.isArray(rhs)) {
        return [...lhs, ...rhs];
      }
      throw new Error(codeFrame(input, `Eval error, cannot concat ${typeof lhs} and ${typeof rhs}`, expr.op.pos));
    }
    default:
      throw new Error(codeFrame(input, `Eval error, expect < binOp >, got ${expr.op.type}`, expr.op.pos));
  }
}

export function evalIf(expr: parse.Call, input: string, env: Env) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;
  const cond = expr.children[1] as parse.Expr;
  const then = expr.children[2] as parse.Expr;

  if (!cond) {
    throw new Error(codeFrame(input, 'Syntax error, no condition for <if>', keyword.name.pos));
  }

  if (!then) {
    throw new Error(codeFrame(input, 'Syntax error, no then statement for <if>', keyword.name.pos));
  }

  if (expr.children.length > 4) {
    throw new Error(codeFrame(input, 'Syntax error, extra statements for <if>', keyword.name.pos));
  }

  const condValue = evalExpr(cond, input, env);

  if (condValue) {
    return evalExpr(then, input, new Env(env, 'if-then'));
  }

  const el = expr.children[3] as parse.Expr;

  return el && evalExpr(el, input, new Env(env, 'if-else'));
}

export function evalBegin(expr: parse.Call, input: string, env: Env) {
  const beginEnv = new Env(env, 'begin');

  let last;

  for (const e of expr.children.slice(1)) {
    last = evalExpr(e as parse.Expr, input, beginEnv);
  }

  return last;
}

export function evalWhile(expr: parse.Call, input: string, env: Env) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;
  const cond = expr.children[1] as parse.Expr;

  if (!cond) {
    throw new Error(codeFrame(input, 'Syntax error, no condition for <while>', keyword.name.pos));
  }

  const rest = expr.children.slice(2);
  const whileEnv = new Env(env, 'while');

  while (evalExpr(cond, input, env)) {
    rest.forEach((e) => evalExpr(e as parse.Expr, input, whileEnv));
  }
}

export function evalMatch(expr: parse.Call, input: string, env: Env) {
  evalExpr(expr.children[1] as parse.Expr, input, env);

  let rest = expr.children.slice(2);

  while (rest.length > 0) {
    const [first, second] = ((rest[0] as parse.Expr).master as parse.Call).children;
    const match = evalExpr(first as parse.Expr, input, env);

    if (!second) return match; // if no second expr, the fallback value
    if (second && match) return evalExpr(second as parse.Expr, input, new Env(env, 'match'));

    rest = rest.slice(1);
  }

  return undefined;
}

function evalWordOp(expr: parse.Call, input: string, env: Env) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;
  const lhs = evalExpr(expr.children[1] as parse.Expr, input, env);
  const rhs = evalExpr(expr.children[2] as parse.Expr, input, env);

  if (expr.children.length > 3) {
    throw new Error(codeFrame(input, `Syntax error, extra statements for ${keyword.name.source}`, keyword.name.pos));
  }

  switch (keyword.name.source) {
    case 'regex':
      if (typeof lhs !== 'string') {
        throw new Error(codeFrame(input, `Eval error, cannot construct regexp, expect <string>, got ${typeof lhs}`, keyword.name.pos));
      }

      return new RegExp(lhs, typeof rhs === 'string' ? rhs : undefined);
    case 'in':
      if (Array.isArray(rhs) || typeof rhs === 'string') {
        return rhs.indexOf(lhs) !== -1;
      }

      if (typeof rhs === 'object') {
        return Object.keys(rhs).indexOf(lhs) !== -1;
      }

      throw new Error(codeFrame(input, `Eval error, invalid "in" operator, rhs type is ${typeof rhs}`, keyword.name.pos));
    case 'and':
      return Boolean(lhs) && Boolean(rhs);
    case 'or':
      return Boolean(lhs) || Boolean(rhs);
    default:
      throw new Error(codeFrame(input, `Syntax error, expect reserved keywords, got ${keyword.name.source}`, keyword.name.pos));
  }
}

function evalExport(expr: parse.Call, input: string, env: Env) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;
  const wrap = expr.children[1] as parse.Expr;
  const id = wrap.master as parse.Id;
  const assignment = expr.children[2] as parse.Expr;

  if (wrap.dot) {
    throw new Error(codeFrame(input, 'Syntax error, expect <id>, got <id><dot>', wrap.dot.dot.pos));
  }

  if (id.type !== 'Id') {
    throw new Error(codeFrame(input, `Syntax error, expect <id>, got ${id.type}`, keyword.name.pos));
  }

  if (env.type !== 'file') {
    throw new Error(codeFrame(input, 'Eval error, can only export on top level', keyword.name.pos));
  }

  const exports = env.get(Constants.EXPORTS) as Env;

  if (assignment) {
    const value = evalExpr(assignment, input, env);

    env.set(id.name.source, value);
    exports.set(id.name.source, value);
  } else {
    exports.set(id.name.source, evalId(id, input, env));
  }
}

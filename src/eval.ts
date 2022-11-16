/* eslint-disable prefer-arrow-callback */
/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-use-before-define */
import { repeat, codeFrame, Constants, setProtoProp, reuseProto } from './utils';
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

  set(k: string | symbol, v: unknown) {
    return super.set(k, v === undefined ? null : v); // avoid undefined value, which is used to distinguish undefined identifier
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

type Cont = (value?: any) => any;

export function evalExpr(expr: parse.Expr, input: string, env = new Env(), cont: Cont = (x) => x): any {
  const exprCont = (value: any) => {
    if (expr.dot) {
      return evalDot(expr.dot, input, env, cont)(value);
    }
    return cont(value);
  };

  switch (expr.master.type) {
    case 'BinOpExpr':
      return evalBinOp(expr.master as parse.BinOpExpr, input, env, exprCont);
    case 'UnOpExpr':
      return evalUnOp(expr.master as parse.UnOpExpr, input, env, exprCont);
    case 'Assign':
      return evalAssign(expr.master as parse.Assign, input, env, exprCont);
    case 'Id':
      return evalId(expr.master as parse.Id, input, env, exprCont);
    case 'Lit':
      return evalLit(expr.master as parse.Lit, input, env, exprCont);
    case 'Func':
      return evalFunc(expr.master as parse.Func, input, env, exprCont);
    case 'Call':
      return evalCall(expr.master as parse.Call, input, env, exprCont);
    default:
      throw new Error(codeFrame(input, `Eval error, expect <expr>, got ${expr.type}`, expr.pos));
  }
}

// evaluate a sequence, continuation passing style
export function evalSeq<T, R>(seq: T[], each: (t: T, c: Cont) => R, cont: Cont) {
  const result: R[] = [];

  const helper = (s: T[]): T[] => {
    const [first, ...rest] = s;

    return s.length > 0
      ? each(first, function eachCont(value) {
        result.push(value);
        return helper(rest);
      })
      : cont(result);
  };

  return helper(seq);
}

export function evalCall(expr: parse.Call, input: string, env: Env, cont: Cont) {
  if (expr.isEmpty()) return cont([]);

  const callerExpr = expr.children[0] as parse.Expr;
  const { master } = callerExpr;

  // aims to be plugable
  if (master.type === 'Id') {
    switch ((master as parse.Id).name.source) {
      case 'begin':
        return evalBegin(expr, input, env, cont);
      case 'if':
        return evalIf(expr, input, env, cont);
      case 'match':
        return evalMatch(expr, input, env, cont);
      case 'while':
        return evalWhile(expr, input, env, cont);
      case 'export':
        return evalExport(expr, input, env, cont);
      case 'sleep':
      case 'exit':
        return evalUnaryWordOp(expr, input, env, cont);
      case 'regex':
      case 'and':
      case 'or':
      case 'in':
        return evalBinaryWordOp(expr, input, env, cont);
      case 'callcc':
        return evalCallCC(expr, input, env, cont);
      default:
        break;
    }
  }

  return evalExpr(callerExpr, input, env, function callerCont(caller) {
    if (['BinOpExpr', 'Assign'].indexOf(master.type) !== -1) {
      return cont(caller);
    }

    const seq = expr.children.slice(1).map((e) => e as parse.Expr);

    return evalSeq(
      seq,
      (e, c) => {
        return evalExpr(e, input, env, c);
      },
      function callCont(values) {
        if (typeof caller !== 'function') {
          return cont([caller, ...values]);
        }

        const proto = Object.getPrototypeOf(caller);

        if (proto[Constants.IS_SQUARE_FUNC]) { // square function call
          proto[Constants.RUNTIME_CONTINUATION] = cont; // save cc to prototype
          return caller(...values);
        }

        return proto[Constants.RUNTIME_CONTINUATION]
          ? caller(...values) // continuation function call
          : cont(caller(...values)); // js builtin function call, which won't call cont() automaticlly
      },
    );
  });
}

const wrapFn = (obj: any, key: string) => {
  const child = obj[key];

  if (typeof child === 'function') {
    const fn = child.bind(obj);

    reuseProto(child, fn);

    return fn;
  }

  return child;
};

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function evalDot(expr: parse.Dot, input: string, _env: Env, cont: Cont) {
  return (master: any, value?: any) => {
    let obj = master;
    let ptr = expr;
    let key = ptr.id.name.source; // only for debug

    while (Object(obj) === obj && obj !== null && ptr.next) {
      obj = wrapFn(obj, ptr.id.name.source);
      ptr = ptr.next;
      key += `.${ptr.id.name.source}`;
    }

    if (ptr.next || obj === undefined) {
      throw new Error(codeFrame(input, `Eval error, expect ${key} to be object, got ${typeof obj}`, expr.pos));
    }

    if (value !== undefined) {
      obj[ptr.id.name.source] = value;

      return cont(value);
    }

    return cont(wrapFn(obj, ptr.id.name.source));
  };
}

export function evalExpand(expr: parse.Expand, input: string, env: Env, cont: Cont) {
  return (...value: any[]): any => {
    let cursor = 0;

    return evalSeq(
      expr.items,
      (item, c) => {
        if (cursor >= value.length) {
          throw new Error(codeFrame(input, `Eval error, expect ${cursor + 1} arguments, got: ${value.length}`, expr.pos));
        }

        const idx = expr.items.indexOf(item);
        switch (item.type) {
          case '.':
            cursor++;
            break;
          case '...': {
            const count = expr.items.slice(idx + 1).filter((it) => it.type === '...').length; // '...' placeholder can be nothing
            const newCursor = value.length - (expr.items.length - idx - 1) + count;

            if (newCursor < cursor) {
              throw new Error(codeFrame(input, `Eval error, expect ${cursor + 1} arguments, got: ${value.length}`, expr.pos));
            }

            cursor = newCursor;
            break;
          } case 'Id':
            env.set((item as parse.Id).name.source, value[cursor++]);
            break;
          case 'Expand':
            return evalExpand(item as parse.Expand, input, env, c)(...value[cursor++]);
          default:
            throw new Error(codeFrame(input, `Eval error, expect <expand>, got ${item.type}`, expr.pos));
        }

        return c();
      },
      cont,
    );
  };
}

export function evalFunc(expr: parse.Func, input: string, env: Env, defCont: Cont) {
  function func(...params: any[]) {
    const bodyEnv = new Env(env, 'func');
    const proto = Object.getPrototypeOf(func);
    const runtimeCont = proto[Constants.RUNTIME_CONTINUATION];

    if (expr.param.type === 'Expand') {
      return evalExpand(
        expr.param as parse.Expand,
        input,
        bodyEnv,
        () => {
          return evalExpr(expr.body, input, bodyEnv, runtimeCont);
        },
      )(...params);
    }// else expect no arguments

    return evalExpr(expr.body, input, bodyEnv, runtimeCont);
  }

  setProtoProp(func, {
    [Constants.RUNTIME_CONTINUATION]: undefined, // runtimeCont will be set in evalCall
    [Constants.IS_SQUARE_FUNC]: true,
  });

  return defCont(func);
}

export function evalAssign(expr: parse.Assign, input: string, env: Env, cont: Cont) {
  return evalExpr(expr.assignment, input, env, function assignmentCont(value) {
    if (expr.variable.type === 'Id') {
      const key = (expr.variable as parse.Id).name.source;
      const record = env.lookup(key);

      if (expr.dot) {
        // set x.y = z
        return evalDot(
          expr.dot,
          input,
          env,
          function dotCont(v) {
            record.env.set(key, record.value);
            return cont(v);
          },
        )(record.value, value);
      }

      // set x = z
      env.set(key, value);
      return cont(value);
    }

    return evalExpand(
      expr.variable as parse.Expand,
      input,
      env,
      cont,
    )(...value);
  });
}

export function evalId(expr: parse.Id, input: string, env: Env, cont: Cont) {
  const id = expr.name.source;
  const { value } = env.lookup(id);

  if (value === undefined) {
    throw new Error(codeFrame(input, `Eval error, undefined identifier: ${id}`, expr.pos));
  }

  return cont(value);
}

function evalNum(base: number, B: string, M?: string) {
  const prefix = base === 2 ? '0b' : '0x';

  let value = 0;

  for (let i = 0; i < B.length; ++i) {
    value += Number(prefix + B[B.length - 1 - i]) * (base ** i);
  }

  if (M) {
    for (let i = 0; i < M.length - 1; ++i) {
      value += Number(prefix + M[i + 1]) * (base ** -(i + 1));
    }
  }

  return value;
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export function evalLit(expr: parse.Lit, input: string, _env: Env, cont: Cont) {
  switch (expr.value.type) {
    case 'dig':
      return cont(Number(expr.value.source));
    case 'bin': {
      const match = /^0b(?<B>[01]+)(?<M>\.[01]+)?$/.exec(expr.value.source);

      if (match && match.groups) {
        const { B, M } = match.groups;

        return cont(evalNum(2, B, M));
      }

      throw new Error(codeFrame(input, `Syntax error, expect <bin>, got ${expr.value.type}`, expr.pos));
    } case 'hex': {
      const match = /^0x(?<B>[0-9a-fA-F]+)(?<M>\.[0-9a-fA-F]+)?$/.exec(expr.value.source);

      if (match && match.groups) {
        const { B, M } = match.groups;

        return cont(evalNum(16, B, M));
      }

      throw new Error(codeFrame(input, `Syntax error, expect <hex>, got ${expr.value.type}`, expr.pos));
    } case 'str':
      return cont(expr.value.source.replace(/^'|'$/g, '').replace(/\\'/g, '\''));
    case 'bool':
      return cont(expr.value.source === 'true');
    default:
      throw new Error(codeFrame(input, `Eval error, expect <literal>, got ${expr.value.type}`, expr.pos));
  }
}

export function evalUnOp(expr: parse.UnOpExpr, input: string, env: Env, cont: Cont) {
  return evalExpr(expr.value, input, env, function unOpValueCont(value) {
    switch (expr.op.type) {
      case '!':
        return cont(!value);
      case '-':
        return cont(-value);
      default:
        throw new Error(codeFrame(input, `Eval error, expect <unOp>, got ${expr.op.type}`, expr.pos));
    }
  });
}

export function evalBinOp(expr: parse.BinOpExpr, input: string, env: Env, cont: Cont) {
  // evalLHS
  return evalExpr(expr.lhs, input, env, function lhsCont(lhs) {
    // evalRHS
    return evalExpr(expr.rhs, input, env, function rhsCont(rhs) {
      const setLhs = (value: any) => {
        const { master, dot } = expr.lhs;

        if (master.type === 'Id') {
          const key = (master as parse.Id).name.source;
          const record = env.lookup(key);

          if (dot) {
            // set x.y = z
            return evalDot(
              dot,
              input,
              env,
              function dotCont(v) {
                record.env.set(key, record.value);
                return cont(v);
              },
            )(record.value, value);
          }

          // set x = z
          record.env.set(key, value);
          return cont(value);
        }

        throw new Error(codeFrame(input, `Eval error, cannot assign on ${master.type}`, expr.pos));
      };

      switch (expr.op.type) {
        case '+':
          return cont(lhs + rhs);
        case '+=':
          return setLhs(lhs + rhs);
        case '-':
          return cont(lhs - rhs);
        case '-=':
          return setLhs(lhs - rhs);
        case '*':
          return cont(lhs * rhs);
        case '*=':
          return setLhs(lhs * rhs);
        case '/':
          return cont(lhs / rhs);
        case '/=':
          return setLhs(lhs / rhs);
        case '>':
          return cont(lhs > rhs);
        case '>=':
          return cont(lhs >= rhs);
        case '<':
          return cont(lhs < rhs);
        case '<=':
          return cont(lhs <= rhs);
        case '%':
          return cont(lhs % rhs);
        case '%=':
          return setLhs(lhs % rhs);
        case '^':
          return cont(lhs ** rhs);
        case '^=':
          return setLhs(lhs ** rhs);
        case '==':
          return cont(lhs === rhs);
        case '!=':
          return cont(lhs !== rhs);
        case '..': {
          if (typeof lhs === 'string' && typeof rhs === 'string') {
            return cont(lhs + rhs);
          }

          if (Number.isInteger(lhs) && Number.isInteger(rhs)) { // range [lhs, rhs)
            return cont(repeat(lhs, Math.abs(lhs - rhs)).map((x, idx) => x + (lhs > rhs ? -idx : idx)));
          }

          if (Array.isArray(lhs) && Array.isArray(rhs)) {
            return cont([...lhs, ...rhs]);
          }
          throw new Error(codeFrame(input, `Eval error, cannot concat ${typeof lhs} and ${typeof rhs}`, expr.pos));
        }
        default:
          throw new Error(codeFrame(input, `Eval error, expect < binOp >, got ${expr.op.type}`, expr.pos));
      }
    });
  });
}

export function evalBegin(expr: parse.Call, input: string, env: Env, cont: Cont) {
  const beginEnv = new Env(env, 'begin');

  return evalSeq(
    expr.children.slice(1),
    (e, c) => {
      return evalExpr(e as parse.Expr, input, beginEnv, c);
    },
    (values) => cont(values[values.length - 1]),
  );
}

export function evalIf(expr: parse.Call, input: string, env: Env, cont: Cont) {
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

  return evalExpr(cond, input, env, function condCont(condValue) {
    if (condValue) {
      return evalExpr(then, input, new Env(env, 'if-then'), cont);
    }

    const el = expr.children[3] as parse.Expr;

    return el ? evalExpr(el, input, new Env(env, 'if-else'), cont) : cont();
  });
}

export function evalWhile(expr: parse.Call, input: string, env: Env, cont: Cont) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;
  const cond = expr.children[1] as parse.Expr;

  if (!cond) {
    throw new Error(codeFrame(input, 'Syntax error, no condition for <while>', keyword.pos));
  }

  const rest = expr.children.slice(2);
  const whileEnv = new Env(env, 'while');

  const loop = () => evalExpr(cond, input, env, (value: boolean) => {
    return value
      ? evalSeq(
        rest,
        (e, c) => {
          return evalExpr(e as parse.Expr, input, whileEnv, c);
        },
        loop,
      )
      : cont();
  });

  return loop();
}

export function evalMatch(expr: parse.Call, input: string, env: Env, cont: Cont) {
  return evalExpr(expr.children[1] as parse.Expr, input, env, () => {
    const rest = expr.children.slice(2);

    return evalSeq(
      rest,
      (e, c) => {
        const [first, second] = ((e as parse.Expr).master as parse.Call).children;

        return evalExpr(first as parse.Expr, input, env, function matchCont(match) {
          if (!second) return cont(match); // if no second expr, regard as the fallback value
          if (second && match) return evalExpr(second as parse.Expr, input, new Env(env, 'match'), cont);

          // not matched, continue
          return c();
        });
      },
      () => cont(undefined),
    );
  });
}

export function evalUnaryWordOp(expr: parse.Call, input: string, env: Env, cont: Cont) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;

  return evalExpr(expr.children[1] as parse.Expr, input, env, function lhsCont(lhs) {
    switch (keyword.name.source) {
      case 'sleep':
        if (typeof lhs !== 'number' || lhs < 0) {
          throw new Error(codeFrame(input, `Eval error, invalid argument of sleep, expect unsigned, got ${lhs}`, keyword.pos));
        }

        setTimeout(cont, lhs);
        break;
      case 'exit':
        if (typeof lhs !== 'number') {
          throw new Error(codeFrame(input, `Eval error, invalid argument of exit, expect number, got ${lhs}`, keyword.pos));
        }

        if (lhs !== 0) {
          console.warn(`Promgram exit with code ${lhs}.`);
        }
        break;
      default:
        throw new Error(codeFrame(input, `Syntax error, expect reserved keywords, got ${keyword.name.source}`, keyword.pos));
    }
  });
}

export function evalBinaryWordOp(expr: parse.Call, input: string, env: Env, cont: Cont) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;

  return evalExpr(expr.children[1] as parse.Expr, input, env, function lhsCont(lhs) {
    return evalExpr(expr.children[2] as parse.Expr, input, env, function rhsCont(rhs) {
      if (expr.children.length > 3) {
        throw new Error(codeFrame(input, `Syntax error, extra statements for ${keyword.name.source}`, keyword.pos));
      }

      switch (keyword.name.source) {
        case 'regex':
          if (typeof lhs !== 'string') {
            throw new Error(codeFrame(input, `Eval error, cannot construct regexp, expect <string>, got ${typeof lhs}`, keyword.pos));
          }

          return cont(new RegExp(lhs, typeof rhs === 'string' ? rhs : undefined));
        case 'in':
          if (Array.isArray(rhs) || typeof rhs === 'string') {
            return cont(rhs.indexOf(lhs) !== -1);
          }

          if (typeof rhs === 'object') {
            return cont(Object.keys(rhs).indexOf(lhs) !== -1);
          }

          throw new Error(codeFrame(input, `Eval error, invalid "in" operator, rhs type is ${typeof rhs}`, keyword.pos));
        case 'and':
          return cont(Boolean(lhs) && Boolean(rhs));
        case 'or':
          return cont(Boolean(lhs) || Boolean(rhs));
        default:
          throw new Error(codeFrame(input, `Syntax error, expect reserved keywords, got ${keyword.name.source}`, keyword.pos));
      }
    });
  });
}

function evalExport(expr: parse.Call, input: string, env: Env, cont: Cont) {
  const keyword = (expr.children[0] as parse.Expr).master as parse.Id;
  const wrap = expr.children[1] as parse.Expr;
  const id = wrap.master as parse.Id;
  const assignment = expr.children[2] as parse.Expr;

  if (wrap.dot) {
    throw new Error(codeFrame(input, 'Syntax error, expect <id>, got <id><dot>', wrap.dot.pos));
  }

  if (id.type !== 'Id') {
    throw new Error(codeFrame(input, `Syntax error, expect <id>, got ${id.type}`, keyword.pos));
  }

  if (env.type !== 'file') {
    throw new Error(codeFrame(input, 'Eval error, can only export on top level', keyword.pos));
  }

  const exports = env.get(Constants.EXPORTS) as Env;

  if (assignment) {
    return evalExpr(assignment, input, env, function exportAssignCont(value) {
      env.set(id.name.source, value);
      exports.set(id.name.source, value);
      return cont();
    });
  }

  return evalId(id, input, env, function exportIdCont(value) {
    exports.set(id.name.source, value);
    return cont();
  });
}

function evalCallCC(expr: parse.Call, input: string, env: Env, cont: Cont) {
  return evalExpr(expr.children[1] as parse.Expr, input, env, (func) => {
    setProtoProp(func, { [Constants.RUNTIME_CONTINUATION]: cont });
    setProtoProp(cont, { [Constants.RUNTIME_CONTINUATION]: (x: unknown) => x });// useless, just mark is a continuation call

    return func(cont);
  });
}

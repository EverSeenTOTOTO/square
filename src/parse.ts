/* eslint-disable @typescript-eslint/no-use-before-define */
/* eslint-disable no-constant-condition */
/* eslint-disable max-classes-per-file */
import * as scan from './scan';
import { Position, codeFrame } from './utils';

export class Node {
  readonly type: 'Func' | 'Assign' | 'BinOpExpr' | 'UnOpExpr' | 'Id' | 'Lit' | 'Expr' | 'Call' | 'Dot' | 'Expand';

  constructor(type: Node['type']) {
    this.type = type;
  }

  str() {
    return JSON.stringify(this, null, 2);
  }
}

export class Call extends Node {
  readonly children: Node[];

  readonly bracketL: scan.Token;

  readonly bracketR: scan.Token;

  constructor(bl: scan.Token, br: scan.Token, children: Node[]) {
    super('Call');
    this.bracketL = bl;
    this.bracketR = br;
    this.children = children;
  }

  isEmpty() {
    return this.children.length === 0;
  }
}

export class Expr extends Node {
  readonly master: Node;

  readonly dot?: Dot;

  constructor(expr: Expr['master'], dot?: Dot) {
    super('Expr');
    this.master = expr;
    this.dot = dot;
  }
}

export class Dot extends Node {
  readonly dot: scan.Token;

  readonly id: Id;

  readonly next?: Dot;

  constructor(dot: scan.Token, id: Id, next?: Dot) {
    super('Dot');
    this.dot = dot;
    this.id = id;
    this.next = next;
  }
}

export class Id extends Node {
  readonly name: scan.Token;

  constructor(id: scan.Token) {
    super('Id');
    this.name = id;
  }
}

export class Expand extends Node {
  readonly bracketL: scan.Token;

  readonly bracketR: scan.Token;

  readonly items: (scan.Token | Node)[];

  constructor(bl: scan.Token, br: scan.Token, items: Expand['items']) {
    super('Expand');
    this.bracketL = bl;
    this.bracketR = br;
    this.items = items;
  }
}

export class Func extends Node {
  readonly slash: scan.Token;

  readonly param: Expand | Call; // Call can be [] while Expand cannot

  readonly body: Expr;

  constructor(slash: scan.Token, param: Func['param'], body: Expr) {
    super('Func');
    this.slash = slash;
    this.param = param;
    this.body = body;
  }
}

export class Assign extends Node {
  readonly eq: scan.Token;

  readonly variable: Id | Expand;

  readonly assignment: Expr;

  readonly dot?: Dot;

  constructor(eq: scan.Token, variable: Assign['variable'], expr: Expr, dot?: Dot) {
    super('Assign');
    this.eq = eq;
    this.variable = variable;
    this.assignment = expr;
    this.dot = dot;
  }
}

export class Lit extends Node {
  readonly value: scan.Token;

  constructor(value: scan.Token) {
    super('Lit');
    this.value = value;
  }
}

export class UnOpExpr extends Node {
  readonly op: scan.Token;

  readonly value: Expr;

  constructor(op: scan.Token, expr: Expr) {
    super('UnOpExpr');
    this.op = op;
    this.value = expr;
  }
}

export class BinOpExpr extends Node {
  readonly op: scan.Token;

  readonly lhs: Expr;

  readonly rhs: Expr;

  constructor(op: scan.Token, lhs: Expr, rhs: Expr) {
    super('BinOpExpr');
    this.op = op;
    this.lhs = lhs;
    this.rhs = rhs;
  }
}

export function parseExpr(input: string, pos = new Position()): Expr {
  scan.skipWhitespace(input, pos);

  const next = scan.lookahead(input, pos);

  const expr = next.type === '['
    ? parseCallExprs(input, pos)
    : parseOtherExprs(input, pos);

  scan.skipWhitespace(input, pos);

  const dot = scan.lookahead(input, pos);

  return new Expr(expr, dot.type === '.' ? parseDot(input, pos) : undefined);
}

function parseCallExprs(input: string, pos: Position) {
  const bl = scan.expect('[', input, pos);
  scan.skipWhitespace(input, pos);

  let next: scan.Token | undefined = scan.lookahead(input, pos);
  const children: Node[] = [];

  while (next.type !== ']') {
    scan.skipWhitespace(input, pos);
    const expr = parseExpr(input, pos);

    children.push(expr);
    // [opExpr]
    if (['Assign', 'BinOpExpr', 'UnOpExpr'].indexOf(expr.master.type) !== -1) break;

    next = scan.lookahead(input, pos);
  }

  scan.skipWhitespace(input, pos);
  next = scan.expect(']', input, pos);

  return new Call(bl, next, children);
}

function parseOtherExprs(input: string, pos: Position) {
  const next = scan.lookahead(input, pos);

  switch (next.type) {
    case '..':
    case '-':
    case '-=':
    case '+':
    case '+=':
    case '/':
    case '/=':
    case '*':
    case '*=':
    case '>':
    case '>=':
    case '<':
    case '<=':
    case '^':
    case '^=':
    case '%':
    case '%=':
    case '==':
    case '!=':
      return parseBinOpExpr(input, pos);
    case '=':
      return parseAssign(input, pos);
    case '!':
    case '...':
      return parseUnOpExpr(input, pos);
    case '/[':
      return parseFunc(input, pos);
    case 'id':
      return parseId(input, pos);
    case 'str':
    case 'num':
    case 'bool':
      return parseLit(input, pos);
    default:
      throw new Error(codeFrame(input, `Syntax error, expect <expr>, got ${next.type}`, pos, next.pos));
  }
}

export function parseFunc(input: string, pos: Position): Func {
  const slash = scan.makeToken('/', pos, '/'); // left [ to parseExpand

  scan.skipWhitespace(input, pos);

  const next = scan.lookahead(input, pos, 2);
  const param = next.type === ']'
    ? new Call(scan.expect('[', input, pos), scan.expect(']', input, pos), [])
    : parseExpand(input, pos);

  scan.skipWhitespace(input, pos);

  const body = parseExpr(input, pos);

  return new Func(slash, param, body);
}

export function parseExpand(input: string, pos: Position) {
  const bl = scan.expect('[', input, pos);

  scan.skipWhitespace(input, pos);

  const items: Expand['items'] = [];
  let next = scan.lookahead(input, pos);

  while (next.type !== ']') {
    switch (next.type) {
      case '.':
      case '...':
        items.push(scan.expect(next.type, input, pos));
        break;
      case 'id':
        items.push(parseId(input, pos));
        break;
      case '[':
        items.push(parseExpand(input, pos));
        break;
      default:
        throw new Error(codeFrame(input, `Syntax error, expect <expand>, got ${next.type}`, pos));
    }
    scan.skipWhitespace(input, pos);
    next = scan.lookahead(input, pos);
  }

  next = scan.expect(']', input, pos);

  return new Expand(bl, next, items);
}

export function parseAssign(input: string, pos: Position) {
  const eq = scan.expect('=', input, pos);

  scan.skipWhitespace(input, pos);

  const next = scan.lookahead(input, pos);
  const variable = next.type === '[' ? parseExpand(input, pos) : parseId(input, pos);

  scan.skipWhitespace(input, pos);

  let dot: Dot | undefined;
  if (next.type === 'id' && scan.lookahead(input, pos).type === '.') {
    dot = parseDot(input, pos);
  }

  scan.skipWhitespace(input, pos);

  const expr = parseExpr(input, pos);

  return new Assign(eq, variable, expr, dot);
}

export function parseId(input: string, pos: Position) {
  const id = scan.expect('id', input, pos);

  return new Id(id);
}

export function parseLit(input: string, pos: Position) {
  const token = scan.expect(['str', 'num', 'bool'], input, pos);

  return new Lit(token);
}

export function parseDot(input: string, pos: Position): Dot {
  const dot = scan.expect('.', input, pos);

  scan.skipWhitespace(input, pos);

  const id = parseId(input, pos);

  scan.skipWhitespace(input, pos);

  const nextDot = scan.lookahead(input, pos);

  return new Dot(dot, id, nextDot.type === '.' ? parseDot(input, pos) : undefined);
}

export function parseUnOpExpr(input: string, pos: Position) {
  const op = scan.raise(input, pos);

  scan.skipWhitespace(input, pos);

  const expr = parseExpr(input, pos);

  return new UnOpExpr(op, expr);
}

export function parseBinOpExpr(input: string, pos: Position) {
  const op = scan.raise(input, pos);

  scan.skipWhitespace(input, pos);

  const lhs = parseExpr(input, pos);

  scan.skipWhitespace(input, pos);

  const rhs = parseExpr(input, pos);

  return new BinOpExpr(op, lhs, rhs);
}

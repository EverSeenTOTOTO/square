/* eslint-disable @typescript-eslint/no-explicit-any */
import { Env, evalExpr } from './eval';
import { parseExpr, Expr } from './parse';
import { lookahead } from './scan';
import { Position } from './utils';

export const parse = (input: string, pos = new Position()) => {
  const nodes: Expr[] = [];

  while (lookahead(input, pos).type !== 'eof') {
    nodes.push(parseExpr(input, pos));
  }

  return nodes;
};

export const evaluate = (input: string, env = new Env(), pos = new Position()) => {
  return parse(input, pos).map((e) => evalExpr(e, input, env));
};
export const createGlobalEnv = () => {
  const env = new Env(undefined, 'global');

  for (const key of Object.getOwnPropertyNames(globalThis)) {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    env.set(key, globalThis[key]);
  }

  const fileEnv = new Env(env, 'file');

  return fileEnv;
};

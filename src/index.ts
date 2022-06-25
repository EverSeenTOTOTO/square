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

export const createGlobalEnv = () => {
  const env = new Env(undefined, 'globalJs');
  for (const key of Object.getOwnPropertyNames(globalThis)) {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    env.set(key, globalThis[key]);
  }

  env.set('import', (pkg: string) => import(pkg));
  // eslint-disable-next-line import/no-dynamic-require, global-require
  env.set('require', (pkg: string) => require(pkg));

  return new Env(env, 'global');
};

export const evaluate = (input: string, env = new Env(), pos = new Position()) => {
  return parse(input, pos).map((e) => evalExpr(e, input, env));
};

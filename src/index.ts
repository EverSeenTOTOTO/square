/* eslint-disable @typescript-eslint/no-explicit-any */
import { Env, evalExpr } from './eval';
import { parseExpr, Expr } from './parse';
import { lookahead } from './scan';
import { Constants, Position } from './utils';

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

// TODO: module cache
const createImportMethod = (global: Env, fs: any, lazy = false) => (pkg: string) => {
  if (/\.sq(uare)?$/g.test(pkg)) {
    const env = new Env(global, 'file');

    env.set(Constants.EXPORTS, new Env());

    // 1. evaluate module
    evaluate(fs.readFileSync(pkg, 'utf8'), env);

    // 2. get module exports
    const value = (env.get(Constants.EXPORTS) as Env).obj();

    return lazy ? Promise.resolve(value) : value;
  }

  // eslint-disable-next-line import/no-dynamic-require, global-require
  return lazy ? import(pkg) : require(pkg);
};

export const createGlobalEnv = (fs: any) => {
  const env = new Env(undefined, 'global');

  for (const key of Object.getOwnPropertyNames(globalThis)) {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    env.set(key, globalThis[key]);
  }

  env.set('import', createImportMethod(env, fs));
  env.set('importDyn', createImportMethod(env, fs, true));

  const fileEnv = new Env(env, 'file');

  fileEnv.set(Constants.EXPORTS, new Env());

  return fileEnv;
};

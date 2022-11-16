/* eslint-disable @typescript-eslint/no-explicit-any */
import { Env, evalExpr, evalSeq } from './eval';
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
  const exprs = parse(input, pos);

  return evalSeq(
    exprs,
    (e, c) => {
      return evalExpr(e as Expr, input, env, c);
    },
    (x) => x, // program level continuation
  );
};

// TODO: module cache
const createImportMethod = (global: Env, fs: any, lazy = false) => (pkg: string) => {
  if (/\.sq(uare)?$/g.test(pkg)) {
    const fileEnv = new Env(global, 'file');

    fileEnv.set(Constants.EXPORTS, new Env());
    evaluate(fs.readFileSync(pkg, 'utf8'), fileEnv);// 1. evaluate module

    const value = (fileEnv.get(Constants.EXPORTS) as Env).obj();// 2. get module exports

    return lazy ? Promise.resolve(value) : value;
  }

  // eslint-disable-next-line import/no-dynamic-require, global-require
  return lazy ? import(pkg) : require(pkg);
};

export const createGlobalEnv = (fs: any) => {
  const globalEnv = new Env(undefined, 'global');

  for (const key of Object.getOwnPropertyNames(globalThis)) {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    globalEnv.set(key, globalThis[key]);
  }

  globalEnv.set('import', createImportMethod(globalEnv, fs));
  globalEnv.set('importDyn', createImportMethod(globalEnv, fs, true));

  const fileEnv = new Env(globalEnv, 'file');

  fileEnv.set(Constants.EXPORTS, new Env());

  return fileEnv;
};

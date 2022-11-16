import * as parse from './parse';
import * as ev from './eval';
import { Position, Constants } from './utils';

Constants.MAX_STACK_DEPTH = 999;

const factory = (parseMethod: any, evalMethod: any) => (input: string, env = new ev.Env()) => evalMethod(parseMethod(input, new Position()), input, env, (x: any) => x);

it('evalLit', () => {
  const bool = 'false';
  const str = "'str;\\'str\\''";

  const ep = factory(parse.parseLit, ev.evalLit);

  expect(ep(bool)).toBe(false);
  expect(ep(str)).toBe('str;\'str\'');

  expect([
    '010',
    '010.010',
    '010e010',
    '010.010e010',
  ].map((num) => ep(num))).toEqual([
    10,
    10.01,
    10e10,
    10.01e10,
  ]);

  expect([
    '0b010',
    '0b010.010',
  ].map((num) => ep(num))).toEqual([
    2,
    2.25,
  ]);

  expect([
    '0x0f0',
    '0x0f0.0f0',
  ].map((num) => ep(num))).toEqual([
    16 * 15,
    16 * 15 + (16 ** -2) * 15,
  ]);
});

it('evalBinOp', () => {
  const add = '+ x 2';
  const sub = '- x 2';
  const mul = '* x 2';
  const div = '/ x 2';
  const lt = '< x 2';
  const gt = '> x 2';
  const mod = '% x 2';
  const hat = '^ x 2';
  const addEq = '+= x 2';
  const subEq = '-= x 2';
  const mulEq = '*= x 2';
  const divEq = '/= x 2';
  const le = '<= x 2';
  const ge = '>= x 2';
  const eq = '== x 2';
  const ne = '!= x 2';
  const modEq = '%= x 2';
  const hatEq = '^= x 2';
  const concatStr = ".. 'str' 'ing'";
  const concatNum = '.. 4 1';
  const concatNum2 = '.. 1 4';
  const concatArray = '.. [1 2] [3 4]';

  const ep = factory(parse.parseBinOpExpr, ev.evalBinOp);
  const env = new ev.Env();

  env.set('x', 1);

  expect(ep(add, env)).toBe(3);
  expect(ep(sub, env)).toBe(-1);
  expect(ep(mul, env)).toBe(2);
  expect(ep(div, env)).toBe(0.5);
  expect(ep(mod, env)).toBe(1);
  expect(ep(hat, env)).toBe(1);
  expect(ep(lt, env)).toBe(true);
  expect(ep(gt, env)).toBe(false);
  expect(ep(eq, env)).toBe(false);
  expect(ep(ne, env)).toBe(true);
  expect(ep(le, env)).toBe(true);
  expect(ep(ge, env)).toBe(false);
  env.set('x', 1);
  expect(ep(addEq, env)).toBe(3);
  env.set('x', 1);
  expect(ep(subEq, env)).toBe(-1);
  env.set('x', 1);
  expect(ep(mulEq, env)).toBe(2);
  env.set('x', 1);
  expect(ep(divEq, env)).toBe(0.5);
  env.set('x', 1);
  expect(ep(modEq, env)).toBe(1);
  env.set('x', 1);
  expect(ep(hatEq, env)).toBe(1);
  expect(ep(concatStr)).toBe('string');
  expect(ep(concatNum)).toEqual([4, 3, 2]);
  expect(ep(concatNum2)).toEqual([1, 2, 3]);
  expect(ep(concatArray)).toEqual([1, 2, 3, 4]);
  expect(() => ep('.. 1 [1]')).toThrow();
  expect(() => ep('+= 1 1')).toThrow();
});

it('evalUnOp', () => {
  const notNum = '! 2';
  const notBool = '! false';
  const notObj = '! []';
  const minus = '- 2';

  const ep = factory(parse.parseUnOpExpr, ev.evalUnOp);

  expect(ep(notNum)).toBe(false);
  expect(ep(notBool)).toBe(true);
  expect(ep(notObj)).toBe(false);
  expect(ep(minus)).toBe(-2);
});

it('evalId', () => {
  const parent = new ev.Env();
  const env = new ev.Env(parent);

  env.set('x', 2);
  env.set('y', 'Y');
  env.set('y', 'y');
  parent.set('x', 1);
  parent.set('z', 'z');

  const ep = factory(parse.parseId, ev.evalId);

  expect(ep('x', env)).toBe(2);
  expect(ep('y', env)).toBe('y');
  expect(ep('z', env)).toBe('z');
  expect(() => ep('x')).toThrow();
});

it('evalDot', () => {
  const ep = factory(parse.parseDot, ev.evalDot);
  const obj = { x: 2, y: { z: { x: 1 } } };

  expect(ep('.x')(obj)).toBe(2);
  expect(ep('.y.z.x')(obj)).toBe(1);
  expect(() => ep('.x.y.z')(obj)).toThrow();

  ep('.x')(obj, 3);
  expect(obj.x).toBe(3);

  ep('.y.z')(obj, []);
  expect(obj.y.z).toEqual([]);
});

it('evalSeq', () => {
  ev.evalSeq([1, 2, 3], (x, c) => { c(x + 1); }, (s) => expect(s).toEqual([2, 3, 4]));

  const result = ev.evalSeq([1, 2, 3], (x, c) => c(x + 1), (x) => x[x.length - 1]);

  expect(result).toBe(4);

  const fn = jest.fn().mockImplementation((x) => {
    expect(x).toBe(2);
  });

  ev.evalSeq([1, 2, 3], (x, c) => {
    if (x === 2) {
      fn(x); // stop at 2
    } else {
      c(x);
    }
  }, fn);
});

it('evalExpand', () => {
  const ep = factory(parse.parseExpand, ev.evalExpand);
  const env = new ev.Env();

  ep('[x]', env)(1);
  expect(env.get('x')).toBe(1);

  ep('[. x]', env)(1, 2);
  expect(env.get('x')).toBe(2);

  ep('[... x]', env)(1, 2, 3, 4);
  expect(env.get('x')).toBe(4);

  ep('[... . . x]', env)(1, 2, 3, 4);
  expect(env.get('x')).toBe(4);

  ep('[... x . .]', env)(1, 2, 3, 4);
  expect(env.get('x')).toBe(2);

  ep('[... [. x] . .]', env)(1, ['ign', 2], 3, 4);
  expect(env.get('x')).toBe(2);

  ep('[... [[[[x]]]] . .]', env)(1, [[[[2]]]], 3, 4);
  expect(env.get('x')).toBe(2);

  ep('[. x . y]', env)(1, 2, 3, 4);
  expect(env.get('x')).toBe(2);
  expect(env.get('y')).toBe(4);

  ep('[... . . x y]', env)(1, 2, 3, 4);
  expect(env.get('x')).toBe(3);
  expect(env.get('y')).toBe(4);

  ep('[... x y .]', env)(1, 2, 3, 4, 5);
  expect(env.get('x')).toBe(3);
  expect(env.get('y')).toBe(4);

  ep('[x . ... y ... . z]', env)(1, 2, 3, 4, 5, 6);
  expect(env.get('x')).toBe(1);
  expect(env.get('y')).toBe(4);
  expect(env.get('z')).toBe(6);

  ep('[. x . ... y ... . z .]', env)(1, 2, 3, 4, 5, 6, 7);
  expect(env.get('x')).toBe(2);
  expect(env.get('y')).toBe(4);
  expect(env.get('z')).toBe(6);

  ep('[. x . ... [y] ... [. z .]]', env)(1, 2, 3, [4], [5, 6, 7]);
  expect(env.get('x')).toBe(2);
  expect(env.get('y')).toBe(4);
  expect(env.get('z')).toBe(6);

  expect(() => ep('[... x]')()).toThrow();
  expect(() => ep('[... x y]')(1)).toThrow();
  expect(() => ep('[. . x]')(1, 2)).toThrow();
  expect(() => ep('[x . .]')(1, 2)).toThrow();
});

it('evalFunc', () => {
  const parent = new ev.Env();
  const env = new ev.Env(parent);

  env.set('x', 2);
  parent.set('z', 4);

  const ep = factory(parse.parseFunc, ev.evalFunc);

  expect(ep('/[] 2')()).toBe(2);
  expect(ep('/[x] x')(2)).toBe(2);
  expect(ep('/[. x] x')(1, 2)).toBe(2);
  expect(ep('/[] [+ z x]', env)()).toBe(6);
  expect(ep('/[] /[] /[] /[y] [+ [+ z x] y]', env)()()()(1)).toBe(7);
  expect(ep('/[x] /[y] /[z] [+ [+ z x] y]', env)(1)(2)(3)).toBe(6);
  expect(ep('/[x] /[y] /[] [+ [+ z x] y]', env)(1)(2)()).toBe(7);
  expect(() => ep('/[x] /[y] /[z] [+ [+ z x] y]', env)(1)(2)()).toThrow();
});

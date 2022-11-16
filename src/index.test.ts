import path from 'path';
import fs from 'fs';
import os from 'os';
import { evaluate, createGlobalEnv } from '.';
import { Constants } from './utils';
import { Env } from './eval';

it('evaluate 01', () => {
  const code = `[= a 'foo']
  [= b 2]
  [= c [.. 1 10]]
  [= [. [x] y] [1 [2] 3]]
  [= b -b]
  [= e 0b11.11]
  [= f 0xff.ff]
  [= g [Object]]
  [= h Object]
  `;

  const env = createGlobalEnv(fs);

  evaluate(code, env);
  expect(env.get('a')).toBe('foo');
  expect(env.get('b')).toBe(-2);
  expect(env.get('c')).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9]);
  expect(env.get('x')).toBe(2);
  expect(env.get('y')).toBe(3);
  expect(env.get('e')).toBe(3.75);
  expect(env.get('f')).toBe(16 * 15 + 15 + 16 ** -1 * 15 + 16 ** -2 * 15);
  expect(typeof env.get('g')).toBe('object');
  expect(Array.isArray(env.get('g'))).toBe(false); // [Object] will be Object() call
  expect(env.get('h')).toBe(Object);
});

it('evaluate 02', () => {
  const code = `
[/[] !true]
[/[] [!true]]


[!0 ![] !0]
[- [+ [- 0 -1] 2] [% 13 4]]
[- [-0 -1] [- 0 -1]]
`;

  const result = evaluate(code, createGlobalEnv(fs));
  expect(result).toEqual([
    false,
    [false],
    [true, false, true],
    2,
    NaN,
  ]);
});

it('evaluate 03', () => {
  const code = `
[in 2 [1 2 3]]
[in 's' 'str']
[begin 
  [= o [Object]]
  [= o.x 2]
  [[in 'x' o] [in 'y' o]]]
[regex '^$' 'g'].source
[and [or true false] 2]
`;

  const result = evaluate(code, createGlobalEnv(fs));

  expect(result).toEqual([
    true,
    true,
    [true, false],
    '^$',
    true,
  ]);

  expect(() => evaluate('[in 2]')).toThrow();
  expect(() => evaluate('[in 2 2]')).toThrow();
  expect(() => evaluate('[in 2 [2] 3]')).toThrow();
  expect(() => evaluate('[regex 2]')).toThrow();
});

it('evaluate 04', () => {
  const code = `
[if true true false]
[if false true false]
[if false any]
`;
  const result = evaluate(code);

  expect(result).toEqual([
    true,
    false,
    undefined,
  ]);

  expect(() => evaluate('[if ]')).toThrow();
  expect(() => evaluate('[if true]')).toThrow();
  expect(() => evaluate('[if true true false extra]')).toThrow();
});

it('evaluate 05', () => {
  const code = `
[begin [= r [regex '^str|ing$' 'i']] [[r.test 'STR'] [r.test 'rts']]]
[begin 1 2]
[begin [= x 1] [= y 2] [begin [+ x y]]]
`;
  const result = evaluate(code);

  expect(result).toEqual([
    [true, false],
    2,
    3,
  ]);
});

it('evaluate 06', () => {
  const code = `
[begin [= i 1] [while [< i 4] [+= i 1]] i]
`;
  const result = evaluate(code);

  expect(result).toEqual([4]);
  expect(() => evaluate('[while ]')).toThrow();
});

it('evaluate 07', () => {
  const code = `
[match true]
[begin 
  [= x 2]
  [match x
    [false 1]
    [2 2]]]
[begin 
  [= y 'str']
  [match y
    ['s' 's']
    [[/[] 'st'] 'st']]]
`;

  const result = evaluate(code);

  expect(result).toEqual([undefined, 2, 's']);
  expect(() => evaluate("[regex [] 'g']")).toThrow();
});

it('evaluate 08', () => {
  const code = `
[= o [Object]]
[= o.a 1]
[= o.b o]
[= o.b.c 2]
[^= o.c o.c]

[o.a o.c]
`;

  const result = evaluate(code, createGlobalEnv(fs));

  expect(result[result.length - 1]).toEqual([1, 4]);
});

it('evaluate 09', () => {
  const code = `
[= stack /[vec] [begin 
  [= this [Object]]
  [= this.vec [vec.slice 0]]
  [= this.clear /[] [= this.vec []]]
  [= this.push /[x] [begin 
    [= this.vec [.. this.vec [x]]]]]
  [= this.pop /[] [begin
    [= [... x] this.vec]
    [this.vec.splice [- this.vec.length 1] 1]
    x]]
  this]]

[= v [1 2 3]]
[= s [stack v]]
[= x [s.pop]]
[s.clear]
[s.push 1]
[s.push 0]
[= y [s.pop]]

[x y v]
    `;

  const result = evaluate(code, createGlobalEnv(fs));

  expect(result[result.length - 1]).toEqual([3, 0, [1, 2, 3]]);
});

it('evaluate 10', () => {
  const code = `
[begin
  [= fib /[n] [begin
    ; [console.log n]
    [match n 
      [[< n 0] 0]
      [[< n 2] 1]
      [+ 
        [fib [- n 1]] 
        [fib [- n 2]]]]]]
  [[.. 1 5].map /[x] [fib x]]]
    `;

  const result = evaluate(code, createGlobalEnv(fs));

  expect(result).toEqual([[1, 2, 3, 5]]);
});

it('evaluate 11', () => {
  const code = `
  [= [a b c d] [.. 1 5]]
  [begin [= a 9]]
  [begin [= i 0] [while [< i 1] [+= i 1] [= b 9]]]
  [if true [= c 9]]
  [match true [true [= d 9]]]

  [a b c d]
`;
  const result = evaluate(code, createGlobalEnv(fs));

  expect(result[result.length - 1]).toEqual([1, 2, 3, 4]);
});

it('evaluate 12', () => {
  const code = `
[process.cwd]
[/[] process.arch]

 [[import 'os'].cpus].length `;

  const result = evaluate(code, createGlobalEnv(fs));
  expect(result).toEqual([
    process.cwd(),
    process.arch,
    os.cpus().length,
  ]);
});

it('evaluate 13', async () => {
  const code = `
[[importDyn 'path'].then /[path] [path.resolve '.']]
`;
  const result = await Promise.all(evaluate(code, createGlobalEnv(fs)));

  expect(result).toEqual([
    path.resolve('.'),
  ]);
});

it('evaluate 14', () => {
  const code = `
  [export a [begin [= a [Object]] [= a.x 1] a]]
  [= b []]
  [export b]
  [= c b]
  `;
  const env = createGlobalEnv(fs);

  evaluate(code, env);

  const exports = env.get(Constants.EXPORTS) as Env;

  expect(exports.obj()).toEqual({
    a: {
      x: 1,
    },
    b: [],
  });
  expect(exports.get('c')).toBeUndefined();

  expect(() => evaluate('[begin [export x 2]]')).toThrow();
  expect(() => evaluate('[export]')).toThrow();
  expect(() => evaluate('[export 2]')).toThrow();
  expect(() => evaluate('[= x [Object]] [= x.a 2] [export x.a]', createGlobalEnv(fs))).toThrow();
  expect(() => evaluate('[= x [Object]] [= x.a 2] [export x]', createGlobalEnv(fs))).not.toThrow();
});

it('evaluate 14', () => {
  const code = `
    [ = test [import 'test.sq']]
    [= os [import 'os']]
    [test.a [os.cpus].length]
  `;

  const mockFs = {
    readFileSync: jest.fn().mockImplementation((file: string) => {
      expect(file).toBe('test.sq');

      return `[export a [begin 
                [= os [import 'os']] 
                [os.cpus].length]]`;
    }),
  };

  const result = evaluate(code, createGlobalEnv(mockFs));

  expect(result[result.length - 1]).toEqual([
    os.cpus().length,
    os.cpus().length,
  ]);
});

it('evaluate 15', () => {
  const code = `
  
[+ 1 [callcc /[cc] [begin
  [begin [begin [begin [cc 2]]]]
  3]]]
  `;

  const result = evaluate(code, createGlobalEnv(fs));

  expect(result).toEqual([
    3,
  ]);
});

// it('evaluate 16', () => {
//   const code = `
// [= o [Object]]
//
// [+ 1 [callcc /[cc] [begin
//                [= o.foo cc]
//                3]]]
//
// [o.foo [o.foo 0]]
//   `;
//
//   const result = evaluate(code, createGlobalEnv(fs));
//
//   expect(result[result.length - 1]).toEqual(1);
// });

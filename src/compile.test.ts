import path from 'path';
import { evaluate, createGlobalEnv } from '.';
import { Env } from './eval';

it('evaluate 1', () => {
  const code = `[= a 'foo']
  [= b 2]
  [= c [.. 1 10]]
  [= [. [x] y] [1 [2] 3]]
  `;

  const env = new Env();

  evaluate(code, env);
  expect(env.get('a')).toBe('foo');
  expect(env.get('b')).toBe(2);
  expect(env.get('c')).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9]);
  expect(env.get('x')).toBe(2);
  expect(env.get('y')).toBe(3);
});

it('evaluate 2', () => {
  const code = `
[= foo /[x y] [== x y]]

[foo 1 2]

[process.cwd]

[/[] process.arch]

[console.log [/[] process].argv]

[[require 'os'].cpus].length

[- [+ [-0 1] 2] [% 13 4]]
`;

  const result = evaluate(code, createGlobalEnv());
  expect(result).toEqual([
    undefined,
    false,
    process.cwd(),
    process.arch,
    undefined,
    // eslint-disable-next-line global-require, @typescript-eslint/no-var-requires
    require('os').cpus().length,
    0,
  ]);
});

it('evaluate 3', async () => {
  const code = `
[[import 'path'].then /[path] [path.resolve '.']]
`;
  const result = await Promise.all(evaluate(code, createGlobalEnv()));

  expect(result).toEqual([
    path.resolve('.'),
  ]);
});

it('evaluate 4', () => {
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

it('evaluate 5', () => {
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

it('evaluate 6', () => {
  const code = `
[begin [= i 1] [while [< i 4] [+= i 1]] i]
`;
  const result = evaluate(code);

  expect(result).toEqual([4]);
});

it('evaluate 7', () => {
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
});

it('evaluate 8', () => {
  const code = `
[= o [Object]]
[= o.a 1]
[= o.b o]
[= o.b.c 2]
[^= o.c o.c]

[o.a o.c]
`;

  const result = evaluate(code, createGlobalEnv());

  expect(result[result.length - 1]).toEqual([1, 4]);
});

it('evaluate 9', () => {
  const code = `
[= stack /[vec] [begin 
  [= this [Object]]
  [= this.vec [... vec]]
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

  const result = evaluate(code, createGlobalEnv());

  expect(result[result.length - 1]).toEqual([3, 0, [1, 2, 3]]);
});

it('evaluate 10', () => {
  const code = `
[in 2 [1 2 3]]
[in 's' 'str']
[begin 
  [= o [Object]]
  [= o.x 2]
  [[in 'x' o] [in 'y' o]]]
[regex '^$' 'g'].source
[and [or true false] 2]

[begin
  [= fib /[n] [begin
    [console.log n]
    [match n 
      [[< n 0] 0]
      [[< n 2] 1]
      [+ 
        [fib [- n 1]] 
        [fib [- n 2]]]]]]
  [[.. 1 5].map /[x] [fib x]]]
    `;

  const result = evaluate(code, createGlobalEnv());

  expect(result).toEqual([
    true,
    true,
    [true, false],
    '^$',
    true,
    [1, 2, 3, 5]]);

  expect(() => evaluate('[in 2]')).toThrow();
  expect(() => evaluate('[in 2 [2] 3]')).toThrow();
  expect(() => evaluate('[regex 2]')).toThrow();
});

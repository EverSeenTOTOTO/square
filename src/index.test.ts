import { evaluate, createGlobalEnv } from '.';

it('evaluate assign', () => {
  const code = `[= a 'foo']
  [= b 2]
  [= c [.. 1 10]]
  [= [. [x] y] [1 [2] 3]]
  [= b -b]
  [= e 0b11.11]
  [= f 0xff.ff]
  [= g [Object]]
  `;
  const env = createGlobalEnv();

  evaluate(code, env);
  expect(env.get('a')).toBe('foo');
  expect(env.get('b')).toBe(-2);
  expect(env.get('c')).toEqual([1, 2, 3, 4, 5, 6, 7, 8, 9]);
  expect(env.get('x')).toBe(2);
  expect(env.get('y')).toBe(3);
  expect(env.get('e')).toBe(3.75);
  expect(env.get('f')).toBe(16 * 15 + 15 + 16 ** -1 * 15 + 16 ** -2 * 15);
  expect(typeof env.get('g')).toBe('object');
  expect(Object(env.get('g')) === env.get('g')).toBe(true);
});

it('evaluate op', () => {
  const code = `
[/[] !true]
[/[] [!true]]


[!0 ![] !0]
[- [+ [- 0 -1] 2] [% 13 4]]
[- [-0 -1] [- 0 -1]]
`;

  const result = evaluate(code);
  expect(result).toEqual([
    false,
    [false],
    [true, false, true],
    2,
    NaN,
  ]);
});

it('evaluate op2', () => {
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

  const result = evaluate(code);

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

it('evaluate if', () => {
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

it('evaluate begin', () => {
  const code = `
[begin [= r [regex '^str|ing$' 'i']] [[r.test 'STR'] [r.test 'rts']]]
`;
  const result = evaluate(code);

  expect(result).toEqual([[true, false]]);
});

it('evaluate while', () => {
  const code = `
[begin [= i 1] [while [< i 4] [+= i 1]] i]
`;
  const result = evaluate(code);

  expect(result).toEqual([4]);
  expect(() => evaluate('[while ]')).toThrow();
});

it('evaluate match', () => {
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

it('evaluate obj', () => {
  const code = `
[= o [Object]]
[= o.a 1]
[= o.b o]
[= o.b.c 2]
[^= o.c o.c]

[o.a o.c]
`;

  const result = evaluate(code);

  expect(result[result.length - 1]).toEqual([1, 4]);
});

it('evaluate obj2', () => {
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

  const result = evaluate(code);

  expect(result[result.length - 1]).toEqual([3, 0, [1, 2, 3]]);
});

it('evaluate recursive', () => {
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
  [[.. 1 9].map /[x] [fib x]]]
    `;

  const result = evaluate(code);

  expect(result).toEqual([[1, 2, 3, 5, 8, 13, 21, 34]]);
});

it('evaluate mixed', () => {
  const code = `
  [= [a b c d] [.. 1 5]]
  [begin [= a 9]]
  [begin [= i 0] [while [< i 1] [+= i 1] [= b 9]]]
  [if true [= c 9]]
  [match true [true [= d 9]]]

  [a b c d]
`;
  const result = evaluate(code);

  expect(result[result.length - 1]).toEqual([1, 2, 3, 4]);
});

it('evaluate callcc', () => {
  const code = `
[+ 1 [callcc /[cc] [begin
  [begin [begin [begin [cc 2]]]]
  3]]]
  `;

  const result = evaluate(code);

  expect(result).toEqual([3]);
});

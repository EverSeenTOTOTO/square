import * as scan from './scan';
import { Position } from './utils';

it('skipWhitespace', () => {
  expect(scan.skipWhitespace('', new Position())).toEqual({
    line: 0,
    column: 0,
    cursor: 0,
  });
  expect(scan.skipWhitespace(' ', new Position())).toEqual({
    line: 0,
    column: 1,
    cursor: 1,
  });
  expect(scan.skipWhitespace('\t', new Position())).toEqual({
    line: 0,
    column: 1,
    cursor: 1,
  });
  expect(scan.skipWhitespace('\r\n', new Position())).toEqual({
    line: 1,
    column: 0,
    cursor: 2,
  });
  expect(scan.skipWhitespace(' \t\t ', new Position())).toEqual({
    line: 0,
    column: 4,
    cursor: 4,
  });
  expect(scan.skipWhitespace(' \n \n\t\r\n\t\n ', new Position())).toEqual({
    line: 4,
    column: 1,
    cursor: 10,
  });
  expect(scan.skipWhitespace('; aa\na', new Position())).toEqual({
    line: 1,
    column: 0,
    cursor: 5,
  });
  expect(scan.skipWhitespace(' ;aa\\;aa;a', new Position())).toEqual({
    line: 0,
    column: 9,
    cursor: 9,
  });
});

it('readString', () => {
  expect(scan.readString("''", new Position()).source).toBe("''");
  expect(scan.readString("'\\''", new Position()).source).toBe("'\\''");
  expect(scan.readString("'str'", new Position()).source).toBe("'str'");
  expect(scan.readString("'str\tstr'", new Position()).source).toBe("'str\tstr'");
  expect(() => scan.readString("'str\nstr'", new Position())).toThrow();
});

it('readNumber', () => {
  expect(scan.readNumber('00', new Position()).source).toBe('00');
  expect(scan.readNumber('01.11', new Position()).source).toBe('01.11');
  expect(scan.readNumber('1.1.1', new Position()).source).toBe('1.1');
  expect(scan.readNumber('01e11', new Position()).source).toBe('01e11');
  expect(scan.readNumber('01.11e7', new Position()).source).toBe('01.11e7');
  expect(scan.readNumber('1e7.1', new Position()).source).toBe('1e7');
  expect(() => scan.readNumber('.1', new Position())).toThrow();
  expect(() => scan.readNumber('e7', new Position())).toThrow();
});

it('readIdentifier', () => {
  expect(scan.readIdentifier('id', new Position()).source).toBe('id');
  expect(scan.readIdentifier('_', new Position()).source).toBe('_');
  expect(scan.readIdentifier('_0', new Position()).source).toBe('_0');
  expect(scan.readIdentifier('true', new Position()).type).toBe('bool');
  expect(scan.readIdentifier('false', new Position()).type).toBe('bool');
  expect(scan.readIdentifier('_0', new Position()).source).toBe('_0');
  expect(() => scan.readIdentifier('0_', new Position())).toThrow();
});

it('readComment', () => {
  expect(scan.readComment(';', new Position()).source).toBe(';');
  expect(scan.readComment(';..', new Position()).source).toBe(';..');
  expect(scan.readComment(';.\n.', new Position()).source).toBe(';.');
  expect(scan.readComment(';.\\;..', new Position()).source).toBe(';.\\;..');
  expect(scan.readComment(';..;..', new Position()).source).toBe(';..;');
  expect(scan.readComment(';..\\; ..;.', new Position()).source).toBe(';..\\; ..;');
  expect(scan.readComment(';..\\;\n ..;.', new Position()).source).toBe(';..\\;');
  expect(() => scan.readComment('\\;;', new Position())).toThrow();
});

it('readWhitespace', () => {
  expect(scan.readWhitespace(' ', new Position()).source).toBe(' ');
  expect(scan.readWhitespace('\t', new Position()).source).toBe('\t');
  expect(scan.readWhitespace('\n', new Position()).source).toBe('\n');
  expect(scan.readWhitespace('\r\n', new Position()).source).toBe('\r\n');
  expect(scan.readWhitespace('  \n\t', new Position()).source).toBe(' ');
  expect(() => scan.readWhitespace('\\n', new Position())).toThrow();
});

it('readOp', () => {
  expect(() => scan.readOp('@', new Position())).toThrow();
});

it('lookahead', () => {
  const pos = new Position();
  expect(scan.lookahead("'str'", pos).type).toBe('str');
  expect(scan.lookahead('=', pos).type).toBe('=');
  expect(scan.lookahead(' -', pos).type).toBe('space');
  expect(scan.lookahead(' -', pos, 2).type).toBe('-');

  expect(pos.cursor).toBe(0);
});

it('expect', () => {
  expect(scan.expect(['str'], "'str'", new Position())).not.toBeUndefined();
  expect(() => scan.expect(['-'], '=', new Position())).toThrow();
  expect(() => scan.expect(['-'], ' -', new Position())).toThrow();
  expect(() => scan.expect(['-'], '@-', new Position())).toThrow();
});

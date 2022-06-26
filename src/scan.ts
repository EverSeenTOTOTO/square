/* eslint-disable @typescript-eslint/no-use-before-define */
/* eslint-disable no-param-reassign */
/* eslint-disable no-constant-condition */
import { Position, codeFrame } from './utils';

export type Token = {
  readonly type: 'space' | 'str' | 'num' | 'bool' | 'id' | 'comment' | 'eof' | '.' | '..' | '...' | ';' | '[' | ']' | '/' | '/=' | '=' | '!' | '-' | '-=' | '+' | '+=' | '*' | '*=' | '>' | '>=' | '<' | '<=' | '%' | '%=' | '^' | '^=' | '==' | '!=' | '/[';
  readonly source: string;
  readonly pos: Position;
};

export const makeToken = (type: Token['type'], pos: Position, source: string): Token => {
  const snapshot = pos.clone();

  pos.column += source.length;
  pos.cursor += source.length;

  return {
    type,
    pos: snapshot,
    source,
  };
};

export function raise(input: string, pos: Position): Token {
  if (pos.cursor >= input.length) return { type: 'eof', pos, source: 'eof' };

  const pivot = input[pos.cursor];

  if (/'/.test(pivot)) return readString(input, pos);
  if (/[0-9]/.test(pivot)) return readNumber(input, pos);
  if (/[a-zA-Z_]/.test(pivot)) return readIdentifier(input, pos);
  if (/\.|=|!|-|\+|\*|\/|<|>|\^|%/.test(pivot)) return readOp(input, pos);
  if (/\[|\]/.test(pivot)) return makeToken(pivot as '.', pos, pivot);
  if (/\s/.test(pivot)) return readWhitespace(input, pos);

  throw new Error(codeFrame(input, `Syntax error, unrecogonized character: ${pivot}`, pos));
}

const STRING_REGEX = /^'(?:[^'\\\n\r]|\\')*'/;
export function readString(input: string, pos: Position): Token {
  const match = STRING_REGEX.exec(input.slice(pos.cursor));

  if (match) {
    return makeToken('str', pos, match[0]);
  }

  throw new Error(codeFrame(input, 'Syntax error, expected <string>', pos));
}

const NUMBER_REGEX = /^\d+(?:\.\d+)?(?:e\d+)?/;
export function readNumber(input: string, pos: Position): Token {
  const match = NUMBER_REGEX.exec(input.slice(pos.cursor));

  if (match) {
    return makeToken('num', pos, match[0]);
  }

  throw new Error(codeFrame(input, 'Syntax error, expected <number>', pos));
}

const ID_REGEX = /^[a-zA-Z_][a-zA-Z0-9_]*/;
const BOOL_REGEX = /^(?:true|false)$/;
export function readIdentifier(input: string, pos: Position): Token {
  const match = ID_REGEX.exec(input.slice(pos.cursor));

  if (match) {
    const text = match[0];

    return BOOL_REGEX.test(text)
      ? makeToken('bool', pos, text)
      : makeToken('id', pos, text);
  }

  throw new Error(codeFrame(input, 'Syntax error, expected <identifier>', pos));
}

const OP_REGEX = /^(\.\.?\.?|\/\[|(=|!|-|\+|\*|\/|<|>|\^|%)=?)/;
export function readOp(input: string, pos: Position): Token {
  const match = OP_REGEX.exec(input.slice(pos.cursor));

  if (match) {
    return makeToken(match[0] as '.', pos, match[0]);
  }

  throw new Error(codeFrame(input, 'Syntax error, expected <eq>', pos));
}

const COMMENT_REGEX = /^;([^;\\\r\n]|\\;)*;?/;
export function readComment(input: string, pos: Position): Token {
  const match = COMMENT_REGEX.exec(input.slice(pos.cursor));

  if (match) {
    return makeToken('comment', pos, match[0]);
  }

  throw new Error(codeFrame(input, 'Syntax error, expected <comment>', pos));
}

// read only 1 space character, comment is not considered
export function readWhitespace(input: string, pos: Position): Token {
  const backup = pos.clone();

  if (/^\r\n/.test(input.slice(pos.cursor))) { // \r\n first
    pos.cursor += 2;
    pos.line += 1;
    pos.column = 0;
  } else if (/ |\t/.test(input[pos.cursor])) {
    pos.cursor++;
    pos.column++;
  } else if (/\n/.test(input[pos.cursor])) {
    pos.cursor += 1;
    pos.line += 1;
    pos.column = 0;
  } else {
    throw new Error(codeFrame(input, 'Syntax error, expected <space>', pos));
  }

  return { type: 'space', source: input.slice(0, pos.cursor - backup.cursor), pos: backup };
}

export function skipWhitespace(input: string, pos: Position) {
  while (true) {
    if (/\s/m.test(input[pos.cursor])) {
      readWhitespace(input, pos);
    } else if (/;/.test(input[pos.cursor])) {
      readComment(input, pos);
    } else {
      break;
    }
  }

  return pos;
}

export function lookahead(input: string, pos: Position, count = 1) {
  const backup = pos.clone();
  let token = raise(input, pos);

  while (--count > 0) {
    token = raise(input, pos);
  }

  pos.copy(backup);

  return token;
}

// try read expected token, throw if fail
export function expect(expected: Token['type'] | Token['type'][], input: string, pos: Position) {
  const token = raise(input, pos);
  const types = Array.isArray(expected) ? expected : [expected];

  if (types.indexOf(token.type) === -1) {
    const message = codeFrame(input, `Syntax error, expect "${types.join(',')}", got ${token.type}`, token.pos, pos);

    pos.copy(token.pos); // rollback
    throw new Error(message);
  }

  return token;
}

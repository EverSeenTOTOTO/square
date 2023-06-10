use crate::errors::*;

use lazy_static::lazy_static;
use regex::Regex;

type RaiseResult<'a> = Result<Token<'a>, ParseError<'a>>;

#[derive(Debug, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub cursor: usize,
}

impl Position {
    pub fn new(line: usize, column: usize, cursor: usize) -> Self {
        Self {
            line,
            column,
            cursor,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenName {
    BOOL,
    EOF,
    ID,
    NUM,
    OP,
    STR,
    COMMENT,
    WHITESPACE,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub name: TokenName,
    pub source: &'a str,
    pub position: Position,
}

impl<'a> Token<'a> {
    pub fn new(name: TokenName, source: &'a str, position: Position) -> Self {
        Self {
            name,
            source,
            position,
        }
    }
}

pub fn make_token<'a>(name: TokenName, source: &'a str, pos: &mut Position) -> Token<'a> {
    let snapshot = pos.clone();

    pos.column += source.len();
    pos.cursor += source.len();

    Token::new(name, source, snapshot)
}

pub fn raise_token<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    if pos.cursor >= input.len() {
        return Ok(make_token(TokenName::EOF, "", pos));
    }

    lazy_static! {
        static ref STRING_PIVOT: Regex = Regex::new("'").unwrap();
        static ref NUMBER_PIVOT: Regex = Regex::new("[0-9]").unwrap();
        static ref IDENT_PIVOT: Regex = Regex::new("[a-zA-Z_]").unwrap();
        static ref OP_PIVOT: Regex = Regex::new(r"[.,=!+\-*/<>^%]").unwrap();
        static ref WHITESPACE_PIVOT: Regex = Regex::new(r"\s+").unwrap();
    }

    let pivot = &input[pos.cursor..=pos.cursor];

    match pivot {
        "'" => raise_string(input, pos),
        _ if NUMBER_PIVOT.is_match(pivot) => raise_number(input, pos),
        _ if IDENT_PIVOT.is_match(pivot) => raise_ident(input, pos),
        _ if OP_PIVOT.is_match(pivot) => raise_operator(input, pos),
        _ if WHITESPACE_PIVOT.is_match(pivot) => raise_whitespace(input, pos),
        _ => todo!(),
    }
}

lazy_static! {
    static ref STRING_PATTERN: Regex = Regex::new(r"'(?:\\'|[^'])*'").unwrap();
}

pub fn raise_string<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let source = STRING_PATTERN
        .find(input)
        .ok_or_else(|| ParseError::InvalidToken(input, pos.clone()))?
        .as_str();
    Ok(make_token(TokenName::STR, source, &mut pos.clone()))
}

lazy_static! {
    static ref NUMBER_PATTERN: Regex = Regex::new("[0-9]+").unwrap();
}

pub fn raise_number<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let source = NUMBER_PATTERN
        .find(input)
        .ok_or_else(|| ParseError::InvalidToken(input, pos.clone()))?
        .as_str();
    Ok(make_token(TokenName::NUM, source, &mut pos.clone()))
}

lazy_static! {
    static ref ID_PATTERN: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
    static ref BOOL_PATTERN: Regex = Regex::new(r#"^(?:true|false)$"#).unwrap();
}

pub fn raise_ident<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    if let Some(matched) = ID_PATTERN.find(&input[pos.cursor..]) {
        let text = matched.as_str();
        return if BOOL_PATTERN.is_match(text) {
            Ok(Token::new(TokenName::BOOL, text, pos.clone()))
        } else {
            Ok(Token::new(TokenName::ID, text, pos.clone()))
        };
    }

    Err(ParseError::InvalidToken(input, pos.clone()))
}

lazy_static! {
    static ref OP_PATTERN: Regex =
        Regex::new(r#"^(\.\.?\.?|\/\[|(=|!|-|\+|\*|\/|<|>|\^|%)=?) "#).unwrap();
}

pub fn raise_operator<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let source = OP_PATTERN
        .find(input)
        .ok_or_else(|| ParseError::InvalidToken(input, pos.clone()))?
        .as_str();

    Ok(make_token(TokenName::OP, source, &mut pos.clone()))
}

lazy_static! {
    static ref COMMENT_REGEX: Regex = Regex::new(r#";([^;\\\r\n]|\\;)*;?"#).unwrap();
}

pub fn raise_comment<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let source = COMMENT_REGEX
        .find(input)
        .ok_or_else(|| ParseError::InvalidToken(input, pos.clone()))?
        .as_str();
    Ok(make_token(TokenName::COMMENT, source, &mut pos.clone()))
}

pub fn is_whitespace<'a>(input: &'a str, index: usize) -> bool {
    if let Some(c) = input.chars().nth(index) {
        return c.is_whitespace();
    }
    return false;
}

pub fn raise_whitespace<'a>(input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let mut backup = pos.clone();

    while is_whitespace(&input, pos.cursor) {
        if &input[pos.cursor..pos.cursor + 1] == "\n" {
            pos.line += 1;
            pos.column = 0;
        } else {
            pos.column += 1;
        }
        pos.cursor += 1;
    }

    if pos.cursor == backup.cursor {
        return Err(ParseError::InvalidToken(input, backup));
    }

    Ok(make_token(
        TokenName::WHITESPACE,
        &input[backup.cursor..pos.cursor],
        &mut backup,
    ))
}

pub fn skip_whitespace<'a>(input: &'a str, mut pos: &mut Position) -> Result<(), ParseError<'a>> {
    loop {
        if let Some(c) = input.chars().nth(pos.cursor) {
            if c.is_whitespace() {
                raise_whitespace(input, &mut pos)?;
            } else if c == ';' {
                raise_comment(input, &mut pos)?;
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(())
}

pub fn lookahead<'a>(input: &'a str, pos: &mut Position, count: usize) -> RaiseResult<'a> {
    let backup = pos.clone();
    let mut token = raise_token(input, pos);

    for _ in 1..count {
        token = raise_token(input, pos)
    }

    *pos = backup;

    return token;
}

pub fn expect<'a>(expected: &'a str, input: &'a str, pos: &mut Position) -> RaiseResult<'a> {
    let token = raise_token(input, pos)?;

    if token.source != expected {
        *pos = token.position;

        return Err(ParseError::InvalidToken(input, pos.clone()));
    }

    Ok(token)
}

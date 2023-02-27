use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub cursor: usize,
}

impl Position {
    pub fn new(line: usize, column: usize, cursor: usize) -> Self {
        return Position {
            line,
            column,
            cursor,
        };
    }

    pub fn clone(&self) -> Position {
        return Position::new(self.line, self.column, self.cursor);
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
        return Token {
            name,
            source,
            position,
        };
    }
}

pub fn make_token<'a>(name: TokenName, source: &'a str, pos: &mut Position) -> Token<'a> {
    let snapshot = pos.clone();

    pos.column += source.len();
    pos.cursor += source.len();

    return Token::new(name, source, snapshot);
}

pub fn raise_token<'a>(input: &'a str, pos: &mut Position) -> Token<'a> {
    if pos.cursor >= input.len() {
        return make_token(TokenName::EOF, "", pos);
    }

    let pivot = &input[pos.cursor..1];

    lazy_static! {
        static ref STRING_PIVOT: Regex = Regex::new("'").unwrap();
        static ref NUMBER_PIVOT: Regex = Regex::new("[0-9]").unwrap();
        static ref IDENT_PIVOT: Regex = Regex::new("[a-zA-Z_]").unwrap();
        static ref OP_PIVOT: Regex = Regex::new(r"\.|=|!|\+|-|\*|/|<|>|\^|%").unwrap();
        static ref WHITESPACE_PIVOT: Regex = Regex::new(r"\s+").unwrap();
    }

    if STRING_PIVOT.is_match(pivot) {
        return raise_string(input, pos);
    }

    if NUMBER_PIVOT.is_match(pivot) {
        return raise_number(input, pos);
    }

    if IDENT_PIVOT.is_match(pivot) {
        return raise_ident(input, pos);
    }

    if OP_PIVOT.is_match(pivot) {
        return raise_operator(input, pos);
    }

    if WHITESPACE_PIVOT.is_match(pivot) {
        return raise_whitespace(input, pos);
    }

    todo!();
}

lazy_static! {
    static ref STRING_PATTERN: Regex = Regex::new(r"^'([^']|\\')*'").unwrap();
}

pub fn raise_string<'a>(input: &'a str, pos: &mut Position) -> Token<'a> {
    if let Some(caps) = STRING_PATTERN.captures(input) {
        return make_token(TokenName::STR, caps.get(0).unwrap().as_str(), pos);
    }

    todo!();
}

fn raise_number<'a>(input: &'a str, pos: &mut Position) -> Token<'a> {
    todo!()
}

fn raise_ident<'a>(input: &'a str, pos: &mut Position) -> Token<'a> {
    todo!()
}

fn raise_operator<'a>(input: &'a str, pos: &mut Position) -> Token<'a> {
    todo!()
}

fn raise_whitespace<'a>(input: &'a str, pos: &mut Position) -> Token<'a> {
    todo!()
}

use alloc::{format, string::String};

use core::{cmp, fmt};

#[derive(Debug, Clone, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub cursor: usize, // char indices, not byte indices
}

impl Default for Position {
    fn default() -> Self {
        Self::new()
    }
}

impl Position {
    pub fn new() -> Self {
        Self {
            line: 1,
            column: 1,
            cursor: 0,
        }
    }

    pub fn advance(&mut self) {
        self.column += 1;
        self.cursor += 1;
    }

    pub fn advance_newline(&mut self) {
        self.column = 1;
        self.cursor += 1;
        self.line += 1;
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "line {}, column {}, cursor {}",
            self.line, self.column, self.cursor
        )
    }
}

fn first_non_whitespace_index(s: &str) -> usize {
    for (i, c) in s.char_indices() {
        if !c.is_whitespace() {
            return i;
        }
    }
    0
}

pub fn code_frame<'a>(source_code: &str, start: &'a Position, end: &'a Position) -> String {
    let lines = source_code.lines().enumerate();
    let mut codes = String::new();

    for (index, line) in lines {
        let line_number = index + 1;
        let start_col = cmp::max(1, first_non_whitespace_index(line) + 1);

        codes.push_str(&hl_line(line_number, line));

        if line_number == start.line {
            if start.line == end.line {
                // same line
                codes.push_str(&hl_cursor(line_number, start.column, end.column));
            } else {
                codes.push_str(&hl_cursor(line_number, start.column, line.len()));
            }
        } else if line_number > start.line && line_number < end.line {
            codes.push_str(&hl_cursor(line_number, start_col, line.len()));
        } else if line_number == end.line {
            codes.push_str(&hl_cursor(line_number, start_col, end.column));
        }
    }

    codes
}

fn hl_line(line_number: usize, line: &str) -> String {
    format!("{:>4} | {}\n", line_number, line)
}

fn hl_cursor(_line_number: usize, start_column: usize, end_column: usize) -> String {
    format!(
        "{:>4} | {}{}\n",
        " ",
        " ".repeat(start_column - 1),
        "^".repeat(end_column - start_column + 1)
    )
}

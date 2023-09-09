#[cfg(not(test))]
use alloc::{format, string::String};
#[cfg(test)]
use std::{format, string::String};

use core::{cmp, fmt};

#[derive(Debug, Clone, PartialEq)]
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

    pub fn default() -> Self {
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
    return 0;
}

pub fn code_frame<'a>(source_code: &str, start: &'a Position, end: &'a Position) -> String {
    let mut lines = source_code.lines().enumerate();
    let mut current_pos = Position::new(1, 1, 0);
    let mut hl_codes = String::new();

    while let Some((line_number, line)) = lines.next() {
        let ln = line_number + 1;
        let start_col = cmp::max(1, first_non_whitespace_index(line) + 1);

        hl_codes.push_str(&hl_line(ln, line));

        if ln == start.line {
            if start.line == end.line {
                // same line
                hl_codes.push_str(&hl_cursor(ln, start.column, end.column));
            } else {
                hl_codes.push_str(&hl_cursor(ln, start.column, line.len()));
            }
        } else if ln > start.line && ln < end.line {
            hl_codes.push_str(&hl_cursor(ln, start_col, line.len()));
        } else if ln == end.line {
            hl_codes.push_str(&hl_cursor(ln, start_col, end.column));
        }

        // Update the current position
        current_pos.line += 1;
    }

    return hl_codes;
}

fn hl_line(line_number: usize, line: &str) -> String {
    format!("{:>4} | {}\n", line_number + 1, line)
}

fn hl_cursor(_line_number: usize, start_column: usize, end_column: usize) -> String {
    format!(
        "{:>4} | {}{}\n",
        " ",
        " ".repeat(start_column - 1),
        "^".repeat(end_column - start_column + 1)
    )
}

use alloc::{format, string::String, vec::Vec};

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

/// 指令地址 → 源码位置的反查表。emit 时按"每条顶层语句的首条指令"稀疏记录，
/// 条目按 pc 升序；`lookup(pc)` 返回包含该 pc 的源码段位置（即 `pc` 所在的顶层语句）。
#[derive(Debug, Clone, Default)]
pub struct SourceMap(Vec<(usize, Position)>);

impl SourceMap {
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, pc: usize, pos: Position) {
        self.0.push((pc, pos));
    }

    pub fn lookup(&self, pc: usize) -> Option<&Position> {
        let i = self.0.partition_point(|(p, _)| *p <= pc);
        if i == 0 {
            None
        } else {
            Some(&self.0[i - 1].1)
        }
    }

    /// 条目按 pc 升序的 (pc, Position) 视图
    pub fn entries(&self) -> &[(usize, Position)] {
        &self.0
    }

    /// 超指令融合收缩指令后，按 old→new 下标映射回迁各条目 pc。
    /// 条目都记在顶层语句的 DELIMITER 位（不参与任何融合模式），映射逐条精确。
    pub fn relocate(&mut self, map: &[usize]) {
        for (pc, _) in self.0.iter_mut() {
            *pc = map[*pc];
        }
    }

    /// 首个 cursor 越过给定字符数的条目 pc（定位拼接前缀之后的第一条语句）
    pub fn pc_after_cursor(&self, cursor: usize) -> Option<usize> {
        self.0
            .iter()
            .find(|(_, pos)| pos.cursor > cursor)
            .map(|(pc, _)| *pc)
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

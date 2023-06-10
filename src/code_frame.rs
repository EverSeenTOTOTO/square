use console::style;

use crate::scan::Position;

fn first_non_whitespace_index(s: &str) -> usize {
    for (i, c) in s.char_indices() {
        if !c.is_whitespace() {
            return i;
        }
    }
    return 0;
}

pub fn code_frame(source_code: &str, start: Position, end: Position) -> String {
    let mut lines = source_code.lines().enumerate();
    let mut current_pos = Position::new(1, 1, 0);
    let mut hl_codes = String::new();

    while let Some((ln, line)) = lines.next() {
        let line_number = ln + 1;
        let start_col = std::cmp::max(1, first_non_whitespace_index(line) + 1);

        hl_codes.push_str(&hl_line(line_number, line));

        if line_number == start.line {
            if start.line == end.line {
                hl_codes.push_str(&hl_cursor(line_number, start.column, end.column));
            } else {
                hl_codes.push_str(&hl_cursor(line_number, start.column, line.len()));
            }
        } else if line_number > start.line && line_number < end.line {
            hl_codes.push_str(&hl_cursor(line_number, start_col, line.len()));
        } else if line_number == end.line {
            if start.line != end.line {
                hl_codes.push_str(&hl_cursor(line_number, start_col, end.column));
            }
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
        &style("^".repeat(end_column - start_column + 1)).red()
    )
}

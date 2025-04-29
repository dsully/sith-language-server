use std::{fmt::Display, path::Path};

use lsp_types::Location;
use ruff_source_file::{LineIndex, OneIndexed};

use crate::{edit::RangeExt, PositionEncoding};

mod goto_definition;
mod lsp_client;
mod references;

fn strip_temp_dir(path: &Path) -> &Path {
    let temp_dir = std::env::temp_dir();
    match path.strip_prefix(temp_dir) {
        Ok(stripped_path) => stripped_path,
        Err(_) => path
            .strip_prefix(dirs::home_dir().unwrap())
            .expect("Failed to strip home dir prefix!"),
    }
}

struct SymbolLocationPrinter<'a> {
    location: &'a Location,
    encoding: PositionEncoding,
    underline_whole_symbol: bool,
}

impl<'a> SymbolLocationPrinter<'a> {
    fn new(location: &'a Location, encoding: PositionEncoding) -> Self {
        Self {
            location,
            encoding,
            underline_whole_symbol: false,
        }
    }

    fn underline_whole_symbol(mut self) -> Self {
        self.underline_whole_symbol = true;
        self
    }
}

impl Display for SymbolLocationPrinter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let file_path = self.location.uri.to_file_path().unwrap();
        let stripped_path = strip_temp_dir(&file_path);

        let contents = std::fs::read_to_string(&file_path).expect("Failed to read file!");
        let index = LineIndex::from_source_text(&contents);

        let line_idx = self.location.range.start.line;
        let line_str = contents.lines().nth(line_idx as usize).unwrap();
        let line_start_offset =
            index.line_start(OneIndexed::from_zero_indexed(line_idx as usize), &contents);
        let range = self
            .location
            .range
            .to_text_range(&contents, &index, self.encoding);
        let symbol = &contents[range];
        // Normalize the range to be within the line range.
        let start_symbol_idx = range
            .sub_start(line_start_offset)
            .sub_end(line_start_offset)
            .start()
            .to_usize();

        writeln!(
            f,
            "{}\n{}",
            stripped_path.display(),
            "-".repeat(stripped_path.as_os_str().len())
        )?;
        writeln!(
            f,
            "{line_str}   LINE: {} ({}..{})\n{}{}\n\n",
            line_idx + 1,
            self.location.range.start.character + 1,
            self.location.range.end.character + 1,
            " ".repeat(start_symbol_idx),
            "^".repeat(if self.underline_whole_symbol {
                symbol.len()
            } else {
                1
            })
        )?;

        Ok(())
    }
}

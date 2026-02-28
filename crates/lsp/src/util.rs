use asciidoxide_parser::asg::Location;
use tower_lsp::lsp_types::{Position, Range};

#[allow(clippy::cast_possible_truncation)]
pub(crate) fn asg_location_to_lsp_range(loc: &Location) -> Range {
    Range {
        start: Position {
            line: (loc[0].line - 1) as u32,
            character: (loc[0].col - 1) as u32,
        },
        end: Position {
            line: (loc[1].line - 1) as u32,
            character: loc[1].col as u32,
        },
    }
}

pub(crate) fn zero_range() -> Range {
    Range {
        start: Position {
            line: 0,
            character: 0,
        },
        end: Position {
            line: 0,
            character: 0,
        },
    }
}

use asciidoxide_parser::asg::{InlineNode, Location};
use tower_lsp::lsp_types::{Position, Range};

/// Check whether an ASG location (1-based, end-inclusive) contains a given LSP position (0-based).
pub(crate) fn location_contains(loc: &Location, pos: Position) -> bool {
    let line = pos.line as usize + 1;
    let col = pos.character as usize + 1;

    let start = &loc[0];
    let end = &loc[1];

    if line < start.line || line > end.line {
        return false;
    }
    if line == start.line && col < start.col {
        return false;
    }
    if line == end.line && col > end.col {
        return false;
    }
    true
}

/// Recursively extract plain text from inline nodes.
pub(crate) fn inlines_to_text(inlines: &[InlineNode]) -> String {
    let mut out = String::new();
    for node in inlines {
        match node {
            InlineNode::Text(t) => out.push_str(t.value),
            InlineNode::Span(s) => out.push_str(&inlines_to_text(&s.inlines)),
            InlineNode::Ref(r) => out.push_str(&inlines_to_text(&r.inlines)),
            InlineNode::Raw(r) => out.push_str(r.value),
        }
    }
    out
}

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

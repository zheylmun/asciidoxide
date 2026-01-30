//! Section heading parsing.

use super::{Spanned, build_blocks, content_span};
use crate::asg::Block;
use crate::diagnostic::ParseDiagnostic;
use crate::parser::inline::run_inline_parser;
use crate::span::SourceIndex;
use crate::token::Token;

/// Check whether position `i` starts a section heading (2+ `Eq` followed by
/// `Whitespace`). Returns `(level, title_start_index)` where level is the
/// number of `Eq` tokens minus one.
pub(super) fn is_section_heading(tokens: &[Spanned<'_>], i: usize) -> Option<(usize, usize)> {
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Eq) {
        j += 1;
    }
    let eq_count = j - i;
    if eq_count < 2 {
        return None;
    }
    if j >= tokens.len() || !matches!(tokens[j].0, Token::Whitespace) {
        return None;
    }
    Some((eq_count - 1, j + 1))
}

/// Try to parse a section starting at position `i`.
///
/// A section heading is 2+ `Eq` tokens followed by `Whitespace` and a title.
/// The section's body is all subsequent content (processed recursively through
/// [`build_blocks`]).
pub(super) fn try_section<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    let (level, title_start) = is_section_heading(tokens, i)?;

    // Find end of title line.
    let mut title_end = title_start;
    while title_end < tokens.len() && !matches!(tokens[title_end].0, Token::Newline) {
        title_end += 1;
    }

    if title_start >= title_end {
        return None;
    }

    // Parse title through inline parser.
    let title_tokens = &tokens[title_start..title_end];
    let (title_inlines, mut diagnostics) = run_inline_parser(title_tokens, source, idx);

    // Body starts after the title's Newline.
    let body_start = if title_end < tokens.len() {
        title_end + 1
    } else {
        title_end
    };

    // The section consumes all remaining tokens.
    // TODO: Handle section nesting (stop at a heading of equal or higher level).
    let section_end = tokens.len();

    // Build body blocks.
    let body_tokens = &tokens[body_start..section_end];
    let (body_blocks, body_diags) = build_blocks(body_tokens, source, idx);
    diagnostics.extend(body_diags);

    // Section location: from first Eq to last content token.
    let section_tokens = &tokens[i..section_end];
    let section_span = content_span(section_tokens);
    let section_location = section_span.map(|s| idx.location(&s));

    Some((
        Block {
            name: "section",
            form: None,
            delimiter: None,
            id: None,
            style: None,
            reftext: None,
            metadata: None,
            title: Some(title_inlines),
            level: Some(level),
            variant: None,
            marker: None,
            inlines: None,
            blocks: Some(body_blocks),
            items: None,
            principal: None,
            location: section_location,
        },
        section_end,
        diagnostics,
    ))
}

//! Paragraph parsing.

use super::breaks::{is_page_break, is_thematic_break};
use super::comments::{is_block_comment_delimiter, is_line_comment};
use super::delimited::{
    is_example_delimiter, is_fenced_code_delimiter, is_listing_delimiter, is_literal_delimiter,
    is_open_delimiter, is_passthrough_delimiter, is_sidebar_delimiter,
};
use super::lists::is_list_item;
use super::sections::is_section_heading;
use super::{Spanned, content_span, strip_trailing_newline_index};
use crate::asg::Block;
use crate::diagnostic::ParseDiagnostic;
use crate::parser::inline::run_inline_parser;
use crate::span::SourceIndex;
use crate::token::Token;

/// Find the end of a paragraph starting at `start`.
///
/// Stops at a blank line (2+ consecutive `Newline` tokens), at a line that
/// starts a listing delimiter, or at the end of the token stream. Returns
/// the index of the first token past the paragraph content (trailing
/// newlines are excluded from the paragraph).
pub(super) fn find_paragraph_end(tokens: &[Spanned<'_>], start: usize) -> usize {
    let mut i = start;
    while i < tokens.len() {
        if matches!(tokens[i].0, Token::Newline) {
            let nl_start = i;
            let mut nl_count = 0;
            while i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
                nl_count += 1;
                i += 1;
            }
            if nl_count >= 2 {
                // Blank line — end paragraph before the newlines.
                return nl_start;
            }
            // After a single newline, check if the next line starts a new block.
            if is_listing_delimiter(tokens, i).is_some()
                || is_literal_delimiter(tokens, i).is_some()
                || is_fenced_code_delimiter(tokens, i).is_some()
                || is_sidebar_delimiter(tokens, i).is_some()
                || is_example_delimiter(tokens, i).is_some()
                || is_open_delimiter(tokens, i).is_some()
                || is_passthrough_delimiter(tokens, i).is_some()
                || is_thematic_break(tokens, i).is_some()
                || is_page_break(tokens, i).is_some()
                || is_line_comment(tokens, i).is_some()
                || is_block_comment_delimiter(tokens, i).is_some()
                || is_list_item(tokens, i)
                || is_section_heading(tokens, i).is_some()
            {
                return nl_start;
            }
        } else {
            i += 1;
        }
    }
    // Reached end of tokens — strip trailing newlines.
    strip_trailing_newline_index(tokens, start)
}

/// Build a paragraph `Block` from its content tokens.
pub(super) fn make_paragraph<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Block<'src>, Vec<ParseDiagnostic>) {
    let trimmed = super::strip_trailing_newlines(tokens);
    let (inlines, diagnostics) = run_inline_parser(trimmed, source, idx);

    let span = content_span(trimmed);
    let location = span.map(|s| idx.location(&s));

    (
        Block {
            name: "paragraph",
            form: None,
            delimiter: None,
            id: None,
            style: None,
            reftext: None,
            metadata: None,
            title: None,
            level: None,
            variant: None,
            marker: None,
            inlines: Some(inlines),
            blocks: None,
            items: None,
            principal: None,
            location,
        },
        diagnostics,
    )
}

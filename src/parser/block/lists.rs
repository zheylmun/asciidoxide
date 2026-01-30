//! List parsing (unordered lists).

use super::Spanned;
use crate::parser::inline::run_inline_parser;
use crate::asg::Block;
use crate::diagnostic::ParseDiagnostic;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Check whether position `i` starts an unordered list item (`Star Whitespace`).
pub(super) fn is_list_item(tokens: &[Spanned<'_>], i: usize) -> bool {
    i + 1 < tokens.len()
        && matches!(tokens[i].0, Token::Star)
        && matches!(tokens[i + 1].0, Token::Whitespace)
}

/// Try to parse an unordered list starting at position `i`.
///
/// Collects consecutive list items (each starting with `Star Whitespace`) into
/// a single `list` block. Returns `None` if position `i` does not start a list
/// item.
pub(super) fn try_list<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    if !is_list_item(tokens, i) {
        return None;
    }

    let mut items = Vec::new();
    let mut diagnostics = Vec::new();
    let mut j = i;
    let list_span_start = tokens[i].1.start;
    let mut list_span_end = tokens[i].1.end;

    while is_list_item(tokens, j) {
        let marker = &source[tokens[j].1.start..tokens[j].1.end];
        let item_start = tokens[j].1.start;

        // Content starts after Star Whitespace.
        let content_start = j + 2;
        let mut content_end = content_start;
        while content_end < tokens.len() && !matches!(tokens[content_end].0, Token::Newline) {
            content_end += 1;
        }

        // Parse principal content through the inline parser.
        let content_tokens = &tokens[content_start..content_end];
        let (principal, diags) = run_inline_parser(content_tokens, source, idx);
        diagnostics.extend(diags);

        // Item location: from the marker to the last content token.
        let item_end = if content_end > content_start {
            tokens[content_end - 1].1.end
        } else {
            tokens[j + 1].1.end
        };
        let item_span = SourceSpan {
            start: item_start,
            end: item_end,
        };

        items.push(Block {
            name: "listItem",
            form: None,
            delimiter: None,
            id: None,
            style: None,
            reftext: None,
            metadata: None,
            title: None,
            level: None,
            variant: None,
            marker: Some(marker),
            inlines: None,
            blocks: None,
            items: None,
            principal: Some(principal),
            location: Some(idx.location(&item_span)),
        });

        list_span_end = item_end;

        // Advance past the Newline (if present).
        j = if content_end < tokens.len() {
            content_end + 1
        } else {
            content_end
        };
    }

    let list_span = SourceSpan {
        start: list_span_start,
        end: list_span_end,
    };
    let marker = items[0].marker;

    Some((
        Block {
            name: "list",
            form: None,
            delimiter: None,
            id: None,
            style: None,
            reftext: None,
            metadata: None,
            title: None,
            level: None,
            variant: Some("unordered"),
            marker,
            inlines: None,
            blocks: None,
            items: Some(items),
            principal: None,
            location: Some(idx.location(&list_span)),
        },
        j,
        diagnostics,
    ))
}

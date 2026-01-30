//! Break block detection (thematic and page breaks).

use super::Spanned;
use crate::asg::Block;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Check whether position `i` starts a thematic break (`'''`).
///
/// A thematic break is exactly 3 `SingleQuote` tokens on their own line.
/// Returns `Some(next_index)` if matched.
pub(super) fn is_thematic_break(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 2 >= tokens.len() {
        return None;
    }
    // Must be exactly 3 SingleQuote tokens.
    if !matches!(tokens[i].0, Token::SingleQuote)
        || !matches!(tokens[i + 1].0, Token::SingleQuote)
        || !matches!(tokens[i + 2].0, Token::SingleQuote)
    {
        return None;
    }
    let j = i + 3;
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        Some(j + 1)
    } else {
        Some(j)
    }
}

/// Check whether position `i` starts a page break (`<<<`).
///
/// A page break is `DoubleLeftAngle` followed by `Text("<")` on its own line.
/// Returns `Some(next_index)` if matched.
pub(super) fn is_page_break(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 1 >= tokens.len() {
        return None;
    }
    // Must be DoubleLeftAngle followed by Text("<").
    if !matches!(tokens[i].0, Token::DoubleLeftAngle) {
        return None;
    }
    if !matches!(&tokens[i + 1].0, Token::Text(s) if *s == "<") {
        return None;
    }
    let j = i + 2;
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        Some(j + 1)
    } else {
        Some(j)
    }
}

/// Try to parse a break block (thematic or page) at position `i`.
pub(super) fn try_break<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    // Try thematic break (''')
    if let Some(next) = is_thematic_break(tokens, i) {
        let span = SourceSpan {
            start: tokens[i].1.start,
            end: tokens[i + 2].1.end,
        };
        let mut block = Block::new("break");
        block.variant = Some("thematic");
        block.location = Some(idx.location(&span));
        return Some((block, next));
    }

    // Try page break (<<<)
    if let Some(next) = is_page_break(tokens, i) {
        let span = SourceSpan {
            start: tokens[i].1.start,
            end: tokens[i + 1].1.end,
        };
        let mut block = Block::new("break");
        block.variant = Some("page");
        block.location = Some(idx.location(&span));
        return Some((block, next));
    }

    None
}

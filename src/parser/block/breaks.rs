//! Break block parsing (thematic and page breaks) using chumsky.

use chumsky::prelude::*;

use super::combinators::{line_ending, ParseExtra};
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

/// Chumsky parser for a thematic break (`'''`).
///
/// A thematic break is exactly 3 `SingleQuote` tokens followed by a line ending.
fn thematic_break_parser<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Block<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: chumsky::input::ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::SingleQuote)
        .then(just(Token::SingleQuote))
        .then(just(Token::SingleQuote))
        .then_ignore(line_ending())
        .map_with(move |_, e| {
            let span: SourceSpan = e.span();
            let mut block = Block::new("break");
            block.variant = Some("thematic");
            block.location = Some(idx.location(&span));
            block
        })
}

/// Chumsky parser for a page break (`<<<`).
///
/// A page break is `DoubleLeftAngle` followed by `Text("<")` and a line ending.
fn page_break_parser<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Block<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: chumsky::input::ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::DoubleLeftAngle)
        .then(just(Token::Text("<")))
        .then_ignore(line_ending())
        .map_with(move |_, e| {
            let span: SourceSpan = e.span();
            let mut block = Block::new("break");
            block.variant = Some("page");
            block.location = Some(idx.location(&span));
            block
        })
}

/// Chumsky parser for any break block (thematic or page).
#[allow(dead_code)]
pub(super) fn break_parser<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Block<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: chumsky::input::ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    choice((thematic_break_parser(idx), page_break_parser(idx)))
}

/// Try to parse a break block (thematic or page) at position `i`.
///
/// This function provides the procedural interface expected by `build_blocks`.
/// TODO: Once the full block parser is converted to chumsky, this will use
/// `break_parser` directly. For now, it uses the detection functions.
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

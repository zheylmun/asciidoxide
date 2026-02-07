//! Break and comment parsers.

use chumsky::{input::ValueInput, prelude::*};

use super::utility::{BlockExtra, line_end, rest_of_line};
use crate::parser::block::raw_block::RawBlock;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Parse a thematic break (`'''`).
pub(super) fn thematic_break<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::SingleQuote)
        .repeated()
        .at_least(3)
        .map_with(move |(), e| {
            let span: SourceSpan = e.span();
            let mut block = RawBlock::new("break");
            block.variant = Some("thematic");
            block.location = Some(idx.location(&span));
            block
        })
        .then_ignore(line_end())
}

/// Parse a page break (`<<<`).
pub(super) fn page_break<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::DoubleLeftAngle)
        .then(just(Token::Text("<")))
        .map_with(move |_, e| {
            let span: SourceSpan = e.span();
            let mut block = RawBlock::new("break");
            block.variant = Some("page");
            block.location = Some(idx.location(&span));
            block
        })
        .then_ignore(line_end())
}

/// Parse a line comment (`// ...`).
pub(super) fn line_comment<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, (), BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::Slash)
        .then(just(Token::Slash))
        .then_ignore(rest_of_line())
        .ignored()
}

/// Parse a block comment (`//// ... ////`).
pub(super) fn block_comment<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, (), BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(|inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Count opening slashes (need 4+)
        let mut open_count = 0;
        while matches!(inp.peek(), Some(Token::Slash)) {
            inp.skip();
            open_count += 1;
        }

        if open_count < 4 {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "not enough slashes for block comment",
            ));
        }

        // Must be followed by newline
        if !matches!(inp.peek(), Some(Token::Newline)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected newline after comment delimiter",
            ));
        }
        inp.skip(); // consume newline

        // Scan for matching closing delimiter
        let mut at_line_start = true;
        loop {
            match inp.peek() {
                None => {
                    // End of input without closing - fail
                    inp.rewind(before);
                    return Err(Rich::custom(
                        inp.span_since(&start_cursor),
                        "unclosed block comment",
                    ));
                }
                Some(Token::Newline) => {
                    inp.skip();
                    at_line_start = true;
                }
                Some(Token::Slash) if at_line_start => {
                    // Potential closing delimiter
                    let mut close_count = 0;
                    while matches!(inp.peek(), Some(Token::Slash)) {
                        inp.skip();
                        close_count += 1;
                    }

                    // Check if counts match and at line end
                    if close_count == open_count
                        && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
                    {
                        // Found matching closer! Consume trailing newline if present
                        if matches!(inp.peek(), Some(Token::Newline)) {
                            inp.skip();
                        }
                        return Ok(());
                    }

                    at_line_start = false;
                }
                _ => {
                    inp.skip();
                    at_line_start = false;
                }
            }
        }
    })
}

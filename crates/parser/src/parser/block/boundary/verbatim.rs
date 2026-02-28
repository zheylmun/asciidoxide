//! Verbatim block parsers (listing, literal, passthrough, fenced code).

use chumsky::{input::ValueInput, prelude::*};

use super::utility::BlockExtra;
use crate::parser::block::raw_block::RawBlock;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Parse a verbatim block with the given delimiter.
///
/// Uses `custom()` to handle delimiter count matching.
fn verbatim_block<'tokens, 'src: 'tokens, I>(
    delimiter_token: Token<'src>,
    min_count: usize,
    block_name: &'static str,
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Check for opening delimiter
        let delimiter_discriminant = std::mem::discriminant(&delimiter_token);
        let mut open_count = 0;
        while inp
            .peek()
            .is_some_and(|t| std::mem::discriminant(&t) == delimiter_discriminant)
        {
            inp.skip();
            open_count += 1;
        }

        if open_count < min_count {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "not enough delimiter tokens",
            ));
        }

        // Capture delimiter span
        let delimiter_span: SourceSpan = inp.span_since(&start_cursor);

        // Must be followed by newline
        if !matches!(inp.peek(), Some(Token::Newline)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected newline",
            ));
        }
        inp.skip(); // consume newline

        // Capture content start position
        let content_cursor = inp.cursor();
        let content_start_span: SourceSpan = inp.span_since(&content_cursor);
        let content_start = content_start_span.start;

        // Scan for matching closing delimiter
        let mut at_line_start = true;
        loop {
            match inp.peek() {
                None => {
                    inp.rewind(before);
                    return Err(Rich::custom(
                        inp.span_since(&start_cursor),
                        "unclosed block",
                    ));
                }
                Some(Token::Newline) => {
                    inp.skip();
                    at_line_start = true;
                }
                Some(t)
                    if at_line_start && std::mem::discriminant(&t) == delimiter_discriminant =>
                {
                    // Potential closing delimiter - count it
                    let close_cursor = inp.cursor();
                    let close_start_span: SourceSpan = inp.span_since(&close_cursor);
                    let content_end = close_start_span.start;

                    let mut close_count = 0;
                    while inp
                        .peek()
                        .is_some_and(|t| std::mem::discriminant(&t) == delimiter_discriminant)
                    {
                        inp.skip();
                        close_count += 1;
                    }

                    // Check if counts match and at line end
                    if close_count == open_count
                        && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
                    {
                        // Found matching closer!
                        // Capture span BEFORE consuming trailing newline
                        let block_end_span: SourceSpan = inp.span_since(&start_cursor);
                        if matches!(inp.peek(), Some(Token::Newline)) {
                            inp.skip();
                        }

                        // Build the block
                        let delimiter = &source[delimiter_span.start..delimiter_span.end];
                        // Adjust content_end to exclude the preceding newline
                        let actual_content_end = if content_end > content_start {
                            content_end - 1 // exclude newline before closer
                        } else {
                            content_end
                        };

                        let mut block = RawBlock::new(block_name);
                        block.form = Some("delimited");
                        block.delimiter = Some(delimiter);
                        if content_start < actual_content_end {
                            block.content_span = Some(SourceSpan {
                                start: content_start,
                                end: actual_content_end,
                            });
                        }
                        block.location = Some(idx.location(&block_end_span));

                        return Ok(block);
                    }

                    // Not a match, continue scanning
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

/// Parse a listing block (`----`).
pub(super) fn listing_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    verbatim_block(Token::Hyphen, 4, "listing", source, idx)
}

/// Parse a literal block (`....`).
pub(super) fn literal_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    verbatim_block(Token::Dot, 4, "literal", source, idx)
}

/// Parse a passthrough block (`++++`).
pub(super) fn passthrough_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    verbatim_block(Token::Plus, 4, "pass", source, idx)
}

/// Parse a fenced code block (`` ``` `` or `` ```lang ``).
///
/// Captures optional language text after the opening backticks.
#[allow(clippy::too_many_lines)]
pub(super) fn fenced_code_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Count opening backticks (need 3+)
        let mut open_count = 0;
        while matches!(inp.peek(), Some(Token::Backtick)) {
            inp.skip();
            open_count += 1;
        }

        if open_count < 3 {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "not enough backticks for fenced code",
            ));
        }

        let delimiter_span: SourceSpan = inp.span_since(&start_cursor);

        // Capture optional language text (everything until newline)
        let lang_cursor = inp.cursor();
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }
        let lang_span: SourceSpan = inp.span_since(&lang_cursor);
        let language = if lang_span.start < lang_span.end {
            let lang_text = &source[lang_span.start..lang_span.end].trim();
            if lang_text.is_empty() {
                None
            } else {
                Some(*lang_text)
            }
        } else {
            None
        };

        // Must be followed by newline
        if !matches!(inp.peek(), Some(Token::Newline)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected newline after fenced code opener",
            ));
        }
        inp.skip(); // consume newline

        // Capture content start
        let content_cursor = inp.cursor();
        let content_start_span: SourceSpan = inp.span_since(&content_cursor);
        let content_start = content_start_span.start;

        // Scan for matching closing delimiter
        let mut at_line_start = true;
        loop {
            match inp.peek() {
                None => {
                    inp.rewind(before);
                    return Err(Rich::custom(
                        inp.span_since(&start_cursor),
                        "unclosed fenced code block",
                    ));
                }
                Some(Token::Newline) => {
                    inp.skip();
                    at_line_start = true;
                }
                Some(Token::Backtick) if at_line_start => {
                    let close_cursor = inp.cursor();
                    let close_start_span: SourceSpan = inp.span_since(&close_cursor);
                    let content_end = close_start_span.start;

                    let mut close_count = 0;
                    while matches!(inp.peek(), Some(Token::Backtick)) {
                        inp.skip();
                        close_count += 1;
                    }

                    if close_count == open_count
                        && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
                    {
                        let block_end_span: SourceSpan = inp.span_since(&start_cursor);
                        if matches!(inp.peek(), Some(Token::Newline)) {
                            inp.skip();
                        }

                        let delimiter = &source[delimiter_span.start..delimiter_span.end];
                        let actual_content_end = if content_end > content_start {
                            content_end - 1
                        } else {
                            content_end
                        };

                        let mut block = RawBlock::new("listing");
                        block.form = Some("delimited");
                        block.delimiter = Some(delimiter);
                        if content_start < actual_content_end {
                            block.content_span = Some(SourceSpan {
                                start: content_start,
                                end: actual_content_end,
                            });
                        }
                        block.location = Some(idx.location(&block_end_span));

                        // Store language as a positional for transform to pick up
                        if let Some(lang) = language {
                            block.positionals = smallvec::smallvec!["", lang];
                        }

                        return Ok(block);
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

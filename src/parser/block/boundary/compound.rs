//! Compound block parsers (example, sidebar, quote, open).

use chumsky::{input::ValueInput, prelude::*};

use crate::parser::block::raw_block::RawBlock;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Parse a compound block with the given delimiter.
///
/// Compound blocks contain nested blocks. This version captures the content
/// span rather than recursively parsing - the recursive parsing happens in
/// phase 3 when converting `RawBlock` to Block.
fn compound_block<'tokens, 'src: 'tokens, I>(
    delimiter_token: Token<'src>,
    min_count: usize,
    block_name: &'static str,
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>> + Clone
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

        // Capture content start
        let content_cursor = inp.cursor();
        let content_start_span: SourceSpan = inp.span_since(&content_cursor);
        let content_start = content_start_span.start;

        // Scan for matching closing delimiter
        let mut at_line_start = true;
        loop {
            // Check for closing delimiter at line start
            if at_line_start {
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

                if close_count == open_count
                    && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
                {
                    // Found matching closer!
                    // Capture span BEFORE consuming trailing newline
                    let block_end_span: SourceSpan = inp.span_since(&start_cursor);
                    if matches!(inp.peek(), Some(Token::Newline)) {
                        inp.skip();
                    }
                    let delimiter = &source[delimiter_span.start..delimiter_span.end];

                    // Adjust content_end to exclude the preceding newline
                    let actual_content_end = if content_end > content_start {
                        content_end - 1
                    } else {
                        content_end
                    };

                    let mut block = RawBlock::new(block_name);
                    block.form = Some("delimited");
                    block.delimiter = Some(delimiter);
                    // Store content span - will be parsed recursively in phase 3
                    if content_start < actual_content_end {
                        block.content_span = Some(SourceSpan {
                            start: content_start,
                            end: actual_content_end,
                        });
                    }
                    block.location = Some(idx.location(&block_end_span));

                    return Ok(block);
                }

                // Not a closing delimiter, rewind the delimiter tokens we consumed
                // Actually, we can't rewind here easily. Instead, just continue.
            }

            // Check for end of input (unclosed block)
            if inp.peek().is_none() {
                inp.rewind(before);
                return Err(Rich::custom(
                    inp.span_since(&start_cursor),
                    "unclosed compound block",
                ));
            }

            // Skip token
            if matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
                at_line_start = true;
            } else {
                inp.skip();
                at_line_start = false;
            }
        }
    })
}

/// Parse an example block (`====`).
pub(super) fn example_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    compound_block(Token::Eq, 4, "example", source, idx)
}

/// Parse a sidebar block (`****`).
pub(super) fn sidebar_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    compound_block(Token::Star, 4, "sidebar", source, idx)
}

/// Parse a quote block (`____`).
pub(super) fn quote_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    compound_block(Token::Underscore, 4, "quote", source, idx)
}

/// Parse an open block (`--`).
///
/// Open blocks use exactly 2 hyphens (not 4+), so they don't conflict with listing blocks.
pub(super) fn open_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // Open block needs special handling: exactly 2 hyphens, not 4+
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Check for exactly 2 hyphens
        if !matches!(inp.peek(), Some(Token::Hyphen)) {
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected hyphen",
            ));
        }
        inp.skip();
        if !matches!(inp.peek(), Some(Token::Hyphen)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected hyphen",
            ));
        }
        inp.skip();

        // Must NOT have a third hyphen (would be listing block)
        if matches!(inp.peek(), Some(Token::Hyphen)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "too many hyphens for open block",
            ));
        }

        let delimiter_span: SourceSpan = inp.span_since(&start_cursor);

        // Must be followed by newline
        if !matches!(inp.peek(), Some(Token::Newline)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected newline",
            ));
        }
        inp.skip();

        // Capture content start
        let content_cursor = inp.cursor();
        let content_start_span: SourceSpan = inp.span_since(&content_cursor);
        let content_start = content_start_span.start;

        // Scan for closing delimiter
        let mut at_line_start = true;
        loop {
            // Check for closing delimiter (exactly 2 hyphens) at line start
            if at_line_start {
                let close_cursor = inp.cursor();
                let close_start_span: SourceSpan = inp.span_since(&close_cursor);
                let content_end = close_start_span.start;

                if matches!(inp.peek(), Some(Token::Hyphen)) {
                    inp.skip();
                    if matches!(inp.peek(), Some(Token::Hyphen)) {
                        inp.skip();
                        // Check it's exactly 2 (not more)
                        if !matches!(inp.peek(), Some(Token::Hyphen))
                            && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
                        {
                            // Found matching closer!
                            // Capture span BEFORE consuming trailing newline
                            let block_end_span: SourceSpan = inp.span_since(&start_cursor);
                            if matches!(inp.peek(), Some(Token::Newline)) {
                                inp.skip();
                            }
                            let delimiter = &source[delimiter_span.start..delimiter_span.end];

                            // Adjust content_end to exclude the preceding newline
                            let actual_content_end = if content_end > content_start {
                                content_end - 1
                            } else {
                                content_end
                            };

                            let mut block = RawBlock::new("open");
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
                    }
                }
            }

            // Check for end of input
            if inp.peek().is_none() {
                inp.rewind(before);
                return Err(Rich::custom(
                    inp.span_since(&start_cursor),
                    "unclosed open block",
                ));
            }

            // Skip token
            if matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
                at_line_start = true;
            } else {
                inp.skip();
                at_line_start = false;
            }
        }
    })
}

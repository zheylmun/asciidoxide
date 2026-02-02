//! Pure chumsky block parsers for phase 2 (block boundary identification).
//!
//! This module provides block parsing using only chumsky combinators.
//! Block content is captured as byte spans rather than parsed inline nodes,
//! deferring inline parsing to phase 3.

use chumsky::{input::ValueInput, prelude::*};

use super::raw_block::{PendingMetadata, RawBlock};
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Parser extra type (just errors, no state).
type BlockExtra<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>;

// ---------------------------------------------------------------------------
// Utility Parsers
// ---------------------------------------------------------------------------

/// Match end of line (Newline or end of input).
fn line_end<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, (), BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::Newline).ignored().or(end())
}

/// Match one or more newlines (blank lines between blocks).
fn blank_lines<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, (), BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::Newline).repeated().at_least(1).ignored()
}

/// Consume tokens until end of line, return the content span.
fn rest_of_line<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, SourceSpan, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    any()
        .filter(|t| !matches!(t, Token::Newline))
        .repeated()
        .map_with(|(), e| e.span())
        .then_ignore(line_end())
}

// ---------------------------------------------------------------------------
// Break Parsers
// ---------------------------------------------------------------------------

/// Parse a thematic break (`'''`).
fn thematic_break<'tokens, 'src: 'tokens, I>(
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
fn page_break<'tokens, 'src: 'tokens, I>(
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

// ---------------------------------------------------------------------------
// Comment Parsers
// ---------------------------------------------------------------------------

/// Parse a line comment (`// ...`).
fn line_comment<'tokens, 'src: 'tokens, I>()
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
fn block_comment<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, (), BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // Block comment uses the same matching delimiter pattern as verbatim blocks
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

// ---------------------------------------------------------------------------
// Metadata Parsers
// ---------------------------------------------------------------------------

/// Parse a block attribute line (`[...]`).
fn block_attr_line<'tokens, 'src: 'tokens, I>(
    source: &'src str,
) -> impl Parser<'tokens, I, PendingMetadata<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::LBracket)
        .ignore_then(
            any()
                .filter(|t| !matches!(t, Token::RBracket | Token::Newline))
                .repeated()
                .map_with(|(), e| e.span()),
        )
        .then_ignore(just(Token::RBracket))
        .then_ignore(line_end())
        .map(move |content_span: SourceSpan| {
            let content = &source[content_span.start..content_span.end];
            parse_attr_content(content)
        })
}

/// Parse attribute content string into `PendingMetadata`.
fn parse_attr_content(content: &str) -> PendingMetadata<'_> {
    let mut meta = PendingMetadata::default();

    // Split by comma for multiple arguments
    let mut first = true;
    for part in content.split(',') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }

        if first {
            first = false;
            // First positional may contain style#id.role%opt
            parse_first_positional(part, &mut meta);
        }
        // Additional positionals are ignored for now (could be attribution, etc.)
    }

    meta
}

/// Parse the first positional argument (style#id.role%opt).
fn parse_first_positional<'src>(part: &'src str, meta: &mut PendingMetadata<'src>) {
    let mut rest = part;

    // Extract style (text before first #, ., or %)
    let style_end = rest.find(['#', '.', '%']).unwrap_or(rest.len());
    if style_end > 0 {
        meta.style = Some(&rest[..style_end]);
    }
    rest = &rest[style_end..];

    // Parse remaining shorthand elements
    while !rest.is_empty() {
        if let Some(after_hash) = rest.strip_prefix('#') {
            let id_end = after_hash.find(['.', '%']).unwrap_or(after_hash.len());
            if id_end > 0 {
                meta.id = Some(&after_hash[..id_end]);
            }
            rest = &after_hash[id_end..];
        } else if let Some(after_dot) = rest.strip_prefix('.') {
            let role_end = after_dot.find(['.', '#', '%']).unwrap_or(after_dot.len());
            if role_end > 0 {
                meta.roles.push(&after_dot[..role_end]);
            }
            rest = &after_dot[role_end..];
        } else if let Some(after_pct) = rest.strip_prefix('%') {
            let opt_end = after_pct.find(['.', '#', '%']).unwrap_or(after_pct.len());
            if opt_end > 0 {
                meta.options.push(&after_pct[..opt_end]);
            }
            rest = &after_pct[opt_end..];
        } else {
            rest = &rest[1..];
        }
    }
}

/// Parse a block title line (`.Title`).
fn block_title_line<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, PendingMetadata<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // Block title starts with single dot followed by non-whitespace content.
    // We need to reject:
    // - `. text` (ordered list item - dot followed by whitespace)
    // - `....` or more (literal block delimiter)
    custom(|inp| {
        let start_cursor = inp.cursor();

        // Must start with a dot
        if !matches!(inp.peek(), Some(Token::Dot)) {
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected dot for block title",
            ));
        }

        // Save position after we've verified it starts with dot
        let after_first_dot = inp.save();
        inp.skip(); // consume the first dot

        // Check what follows
        match inp.peek() {
            // Whitespace means ordered list item
            Some(Token::Whitespace) => {
                inp.rewind(after_first_dot);
                return Err(Rich::custom(
                    inp.span_since(&start_cursor),
                    "dot followed by whitespace is list item, not title",
                ));
            }
            // More dots could be literal block delimiter
            Some(Token::Dot) => {
                // Count total dots (including the first one we already consumed)
                let mut dot_count = 1;
                while matches!(inp.peek(), Some(Token::Dot)) {
                    inp.skip();
                    dot_count += 1;
                }
                // If 4+ dots followed by newline/EOF, it's a literal delimiter
                if dot_count >= 4
                    && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
                {
                    inp.rewind(after_first_dot);
                    return Err(Rich::custom(
                        inp.span_since(&start_cursor),
                        "4+ dots is literal block delimiter, not title",
                    ));
                }
                // Otherwise, we're parsing a title that starts with dots (e.g., "..text")
                // Continue with what we've already consumed - the dots are part of the title
            }
            // Newline means empty title (invalid)
            Some(Token::Newline) | None => {
                inp.rewind(after_first_dot);
                return Err(Rich::custom(
                    inp.span_since(&start_cursor),
                    "block title cannot be empty",
                ));
            }
            // Any other token is valid title content
            _ => {}
        }

        // Now capture the full title content from after the leading dot
        // Note: we may have already consumed some dots if title starts with dots
        // So capture everything we have so far plus the rest until newline
        let current_span: SourceSpan = inp.span_since(&start_cursor);
        let title_start = current_span.start + 1; // skip the leading dot
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }
        let title_end_span: SourceSpan = inp.span_since(&start_cursor);
        let title_span = SourceSpan {
            start: title_start,
            end: title_end_span.end,
        };

        // Title must have content
        if title_span.start >= title_span.end {
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "block title cannot be empty",
            ));
        }

        // Consume trailing newline if present
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        Ok(PendingMetadata {
            title_span: Some(title_span),
            ..Default::default()
        })
    })
}

// ---------------------------------------------------------------------------
// Verbatim Block Parsers
// ---------------------------------------------------------------------------

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
                        // Consume trailing newline if present
                        if matches!(inp.peek(), Some(Token::Newline)) {
                            inp.skip();
                        }

                        let block_end_span: SourceSpan = inp.span_since(&start_cursor);

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
fn listing_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    verbatim_block(Token::Hyphen, 4, "listing", source, idx)
}

/// Parse a literal block (`....`).
fn literal_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    verbatim_block(Token::Dot, 4, "literal", source, idx)
}

/// Parse a passthrough block (`++++`).
fn passthrough_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    verbatim_block(Token::Plus, 4, "pass", source, idx)
}

/// Parse a fenced code block (`` ``` ``).
fn fenced_code_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    verbatim_block(Token::Backtick, 3, "listing", source, idx)
}

// ---------------------------------------------------------------------------
// Compound Block Parsers
// ---------------------------------------------------------------------------

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
                    if matches!(inp.peek(), Some(Token::Newline)) {
                        inp.skip();
                    }

                    let block_end_span: SourceSpan = inp.span_since(&start_cursor);
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
fn example_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    compound_block(Token::Eq, 4, "example", source, idx)
}

/// Parse a sidebar block (`****`).
fn sidebar_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    compound_block(Token::Star, 4, "sidebar", source, idx)
}

/// Parse a quote block (`____`).
fn quote_block<'tokens, 'src: 'tokens, I>(
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
fn open_block<'tokens, 'src: 'tokens, I>(
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
                            if matches!(inp.peek(), Some(Token::Newline)) {
                                inp.skip();
                            }

                            let block_end_span: SourceSpan = inp.span_since(&start_cursor);
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

// ---------------------------------------------------------------------------
// Section Parsers
// ---------------------------------------------------------------------------

/// Parse a section heading (`== Title`, `=== Title`, etc.).
///
/// Sections have level = (number of `=` signs) - 1, so:
/// - `== Title` is level 1
/// - `=== Title` is level 2
/// - etc.
///
/// The section captures both the title and the body content (everything until
/// a same-or-higher-level heading or end of input).
fn section_heading<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Count leading `=` tokens (need at least 2 for a section)
        let mut eq_count = 0;
        while matches!(inp.peek(), Some(Token::Eq)) {
            inp.skip();
            eq_count += 1;
        }

        if eq_count < 2 {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "need at least 2 equals signs for section",
            ));
        }

        // Must be followed by whitespace
        if !matches!(inp.peek(), Some(Token::Whitespace)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected whitespace after section marker",
            ));
        }
        inp.skip(); // consume whitespace

        // Capture title content (everything until newline)
        let title_cursor = inp.cursor();
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }
        let title_span: SourceSpan = inp.span_since(&title_cursor);

        // Title must have content
        if title_span.start >= title_span.end {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "section title cannot be empty",
            ));
        }

        // Capture heading line span (from start to end of title, before newline)
        let heading_line_span: SourceSpan = inp.span_since(&start_cursor);

        // Consume trailing newline if present
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        let level = eq_count - 1;

        // Capture body content: everything until next same-or-higher-level heading or EOF
        let body_cursor = inp.cursor();
        let body_start_span: SourceSpan = inp.span_since(&body_cursor);
        let body_start = body_start_span.start;

        // Scan for section end
        let mut at_line_start = true;
        loop {
            match inp.peek() {
                None => {
                    // End of input - section ends here
                    break;
                }
                Some(Token::Newline) => {
                    inp.skip();
                    at_line_start = true;
                }
                Some(Token::Eq) if at_line_start => {
                    // Potential section heading - check its level
                    let check_before = inp.save();
                    let mut check_eq_count = 0;
                    while matches!(inp.peek(), Some(Token::Eq)) {
                        inp.skip();
                        check_eq_count += 1;
                    }

                    // Check if this is a valid section heading (followed by whitespace)
                    if check_eq_count >= 2 && matches!(inp.peek(), Some(Token::Whitespace)) {
                        let check_level = check_eq_count - 1;
                        if check_level <= level {
                            // Found same or higher level section - rewind and stop
                            inp.rewind(check_before);
                            break;
                        }
                    }

                    // Not a section heading or lower level - continue
                    // Don't rewind, we've already consumed the Eq tokens
                    at_line_start = false;
                }
                _ => {
                    inp.skip();
                    at_line_start = false;
                }
            }
        }

        let body_end_span: SourceSpan = inp.span_since(&body_cursor);
        let body_end = body_end_span.end;

        // Trim trailing newlines from body content
        let mut actual_body_end = body_end;
        while actual_body_end > body_start
            && source.as_bytes().get(actual_body_end - 1) == Some(&b'\n')
        {
            actual_body_end -= 1;
        }

        let block_span: SourceSpan = inp.span_since(&start_cursor);

        let mut block = RawBlock::new("section");
        block.level = Some(level);
        block.title_span = Some(title_span);
        block.heading_line_location = Some(idx.location(&heading_line_span));
        if body_start < actual_body_end {
            block.content_span = Some(SourceSpan {
                start: body_start,
                end: actual_body_end,
            });
        }
        block.location = Some(idx.location(&block_span));

        Ok(block)
    })
}

// ---------------------------------------------------------------------------
// List Parsers
// ---------------------------------------------------------------------------

/// Count consecutive tokens of a specific type at current position.
/// Does NOT consume the tokens.
fn peek_token_count<'tokens, 'src: 'tokens, I, E>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, E>,
    token: &Token<'src>,
) -> usize
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
    E: chumsky::extra::ParserExtra<'tokens, I>,
{
    let before = inp.save();
    let discriminant = std::mem::discriminant(token);
    let mut count = 0;
    while inp
        .peek()
        .is_some_and(|t| std::mem::discriminant(&t) == discriminant)
    {
        inp.skip();
        count += 1;
    }
    inp.rewind(before);
    count
}

/// Parse a list item with the given marker token and expected level.
/// Returns `(RawBlock, was_parsed)`.
fn parse_list_item<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
    marker_token: &Token<'src>,
    expected_level: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<RawBlock<'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let before = inp.save();
    let start_cursor = inp.cursor();

    // Skip leading whitespace (for continuation items)
    while matches!(inp.peek(), Some(Token::Whitespace)) {
        inp.skip();
    }

    let marker_cursor = inp.cursor();
    let discriminant = std::mem::discriminant(marker_token);

    // Count and consume marker tokens
    let mut marker_count = 0;
    while inp
        .peek()
        .is_some_and(|t| std::mem::discriminant(&t) == discriminant)
    {
        inp.skip();
        marker_count += 1;
    }

    // Check we have the expected level
    if marker_count != expected_level {
        inp.rewind(before);
        return None;
    }

    // Must be followed by whitespace
    if !matches!(inp.peek(), Some(Token::Whitespace)) {
        inp.rewind(before);
        return None;
    }
    inp.skip(); // consume whitespace

    let marker_span: SourceSpan = inp.span_since(&marker_cursor);
    let marker = &source[marker_span.start..marker_span.end];

    // Capture principal content (everything until newline)
    let content_cursor = inp.cursor();
    while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
        inp.skip();
    }
    let content_span: SourceSpan = inp.span_since(&content_cursor);

    // Calculate item span BEFORE consuming trailing newline
    // (location should end at end of content, not include the newline)
    let item_span: SourceSpan = inp.span_since(&start_cursor);

    // Consume trailing newline if present
    if matches!(inp.peek(), Some(Token::Newline)) {
        inp.skip();
    }

    let mut item = RawBlock::new("listItem");
    item.marker = Some(marker.trim_end());
    if content_span.start < content_span.end {
        item.principal_span = Some(content_span);
    }
    item.location = Some(idx.location(&item_span));

    Some(item)
}

/// Parse a list (unordered or ordered).
///
/// Unordered lists use `*` markers, ordered use `.` markers.
/// Nesting is determined by marker count (`**` is deeper than `*`).
#[allow(clippy::too_many_lines)]
fn list<'tokens, 'src: 'tokens, I>(
    marker_token: Token<'src>,
    variant: &'static str,
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Skip leading whitespace
        while matches!(inp.peek(), Some(Token::Whitespace)) {
            inp.skip();
        }

        // Determine list level from first item's marker count
        let marker_count = peek_token_count(inp, &marker_token);
        if marker_count == 0 {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected list marker",
            ));
        }

        // Check it's followed by whitespace (to distinguish from other uses)
        let before_peek = inp.save();
        for _ in 0..marker_count {
            inp.skip();
        }
        let is_list = matches!(inp.peek(), Some(Token::Whitespace));
        inp.rewind(before_peek);

        if !is_list {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "not a list item",
            ));
        }

        let list_level = marker_count;
        let mut items: Vec<RawBlock<'src>> = Vec::new();

        // Parse items at this level
        loop {
            // Check what's next
            // Skip whitespace at start of line
            let line_before = inp.save();
            while matches!(inp.peek(), Some(Token::Whitespace)) {
                inp.skip();
            }

            // Check for list marker
            let current_count = peek_token_count(inp, &marker_token);

            // If no markers or different level, we're done with this list
            if current_count == 0 {
                inp.rewind(line_before);
                break;
            }

            // Check if followed by whitespace (valid list item)
            let before_check = inp.save();
            for _ in 0..current_count {
                inp.skip();
            }
            let is_valid = matches!(inp.peek(), Some(Token::Whitespace));
            inp.rewind(before_check);

            if !is_valid {
                inp.rewind(line_before);
                break;
            }

            if current_count < list_level {
                // Returning to parent level
                inp.rewind(line_before);
                break;
            }

            if current_count > list_level {
                // Nested list - need to parse it and attach to last item
                inp.rewind(line_before);

                // Parse nested list recursively
                let nested_start = inp.cursor();
                let nested_level = current_count;
                let mut nested_items: Vec<RawBlock<'src>> = Vec::new();

                // Parse nested items
                loop {
                    while matches!(inp.peek(), Some(Token::Whitespace)) {
                        inp.skip();
                    }

                    let nested_count = peek_token_count(inp, &marker_token);
                    if nested_count != nested_level {
                        break;
                    }

                    // Check followed by whitespace
                    let before_nested = inp.save();
                    for _ in 0..nested_count {
                        inp.skip();
                    }
                    if !matches!(inp.peek(), Some(Token::Whitespace)) {
                        inp.rewind(before_nested);
                        break;
                    }
                    inp.rewind(before_nested);

                    if let Some(item) =
                        parse_list_item(inp, &marker_token, nested_level, source, idx)
                    {
                        nested_items.push(item);
                    } else {
                        break;
                    }
                }

                if !nested_items.is_empty() {
                    let nested_span: SourceSpan = inp.span_since(&nested_start);
                    let first_item_marker = nested_items[0].marker.unwrap_or("");

                    let mut nested_list = RawBlock::new("list");
                    nested_list.variant = Some(variant);
                    nested_list.marker = Some(first_item_marker);
                    nested_list.items = Some(nested_items);
                    nested_list.location = Some(idx.location(&nested_span));

                    // Attach to last item
                    if let Some(last_item) = items.last_mut() {
                        if last_item.blocks.is_none() {
                            last_item.blocks = Some(Vec::new());
                        }
                        last_item.blocks.as_mut().unwrap().push(nested_list);
                    }
                }

                continue;
            }

            // Same level - parse item
            inp.rewind(line_before);
            if let Some(item) = parse_list_item(inp, &marker_token, list_level, source, idx) {
                items.push(item);
            } else {
                break;
            }
        }

        if items.is_empty() {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "no list items found",
            ));
        }

        let list_span: SourceSpan = inp.span_since(&start_cursor);

        // Get marker from first item
        let first_marker = items[0].marker.unwrap_or("");

        let mut list_block = RawBlock::new("list");
        list_block.variant = Some(variant);
        list_block.marker = Some(first_marker);
        list_block.items = Some(items);
        list_block.location = Some(idx.location(&list_span));

        Ok(list_block)
    })
}

/// Parse an unordered list (`*`, `**`, etc.).
fn unordered_list<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    list(Token::Star, "unordered", source, idx)
}

/// Parse an ordered list (`.`, `..`, etc.).
fn ordered_list<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    list(Token::Dot, "ordered", source, idx)
}

// ---------------------------------------------------------------------------
// Paragraph Parser
// ---------------------------------------------------------------------------

/// Check if current position looks like a block interrupt pattern.
///
/// This checks for patterns that should end a paragraph:
/// - Block delimiters (`----`, `====`, `****`, `....`, `++++`, `--`)
/// - Section headings (`== Title`)
/// - List markers (`*`, `.` at line start followed by space)
/// - Block attribute lines (`[...]`)
/// - Block title lines (`.Title` - single dot followed by non-space)
/// - Thematic break (`'''`)
/// - Page break (`<<<`)
fn is_block_interrupt<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
) -> bool
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let before = inp.save();
    let result = check_block_interrupt_impl(inp);
    inp.rewind(before);
    result
}

/// Implementation of block interrupt checking (consumes tokens, caller must rewind).
fn check_block_interrupt_impl<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
) -> bool
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    match inp.peek() {
        // End of input or block attribute line [...]
        None | Some(Token::LBracket) => true,

        // Section heading (== Title) - need 2+ equals followed by whitespace
        Some(Token::Eq) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Eq)) {
                inp.skip();
                count += 1;
            }
            count >= 2 && matches!(inp.peek(), Some(Token::Whitespace))
        }

        // Unordered list marker (* item) or sidebar delimiter (****)
        Some(Token::Star) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Star)) {
                inp.skip();
                count += 1;
            }
            // Single star + whitespace = list item
            // 4+ stars + newline/EOF = sidebar delimiter
            (count == 1 && matches!(inp.peek(), Some(Token::Whitespace)))
                || (count >= 4
                    && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline))))
        }

        // Ordered list marker (. item) or literal block (....)
        // Also block title (.Title)
        Some(Token::Dot) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Dot)) {
                inp.skip();
                count += 1;
            }
            // Single dot + whitespace = list item
            // Single dot + non-whitespace/non-newline = block title
            // 4+ dots + newline/EOF = literal delimiter
            if count == 1 {
                // Single dot: list item (. foo) or block title (.Title)
                // Not a title if followed by newline or EOF
                !matches!(inp.peek(), Some(Token::Newline) | None)
            } else {
                count >= 4 && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
            }
        }

        // Listing block (----) or open block (--)
        Some(Token::Hyphen) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Hyphen)) {
                inp.skip();
                count += 1;
            }
            // 2 hyphens + newline/EOF = open block
            // 4+ hyphens + newline/EOF = listing delimiter
            (count == 2 || count >= 4)
                && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
        }

        // Example block (====)
        // Note: We already handled Eq for section headings, but 4+ equals at line start
        // could also be example block. The section check requires whitespace after,
        // so 4+ equals followed by newline would be example block.
        // Actually Eq is already handled above, so this case won't be reached.

        // Passthrough block (++++)
        Some(Token::Plus) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Plus)) {
                inp.skip();
                count += 1;
            }
            count >= 4 && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
        }

        // Quote block (____)
        Some(Token::Underscore) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Underscore)) {
                inp.skip();
                count += 1;
            }
            count >= 4 && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
        }

        // Thematic break (''')
        Some(Token::SingleQuote) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::SingleQuote)) {
                inp.skip();
                count += 1;
            }
            count >= 3 && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
        }

        // Comment (// or ////)
        Some(Token::Slash) => {
            inp.skip();
            matches!(inp.peek(), Some(Token::Slash))
        }

        // Page break (<<<)
        Some(Token::DoubleLeftAngle) => {
            inp.skip();
            matches!(inp.peek(), Some(Token::Text("<")))
        }

        // Fenced code block (```)
        Some(Token::Backtick) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Backtick)) {
                inp.skip();
                count += 1;
            }
            count >= 3
        }

        _ => false,
    }
}

/// Parse a paragraph (fallback for unrecognized content).
///
/// A paragraph consists of one or more contiguous lines of text.
/// It ends at:
/// - A blank line (empty line / two newlines)
/// - A block delimiter or other block-starting pattern
/// - End of input
fn paragraph<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let start_cursor = inp.cursor();

        // Must have at least some content
        if inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)) {
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "empty paragraph",
            ));
        }

        // Consume lines until we hit a paragraph boundary
        loop {
            // Consume content on current line
            while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
            }

            // Check what's after the newline
            if inp.peek().is_none() {
                // End of input - paragraph ends here
                break;
            }

            // We're at a newline
            debug_assert!(matches!(inp.peek(), Some(Token::Newline)));

            // Peek ahead: is next line blank or a block interrupt?
            let before_newline = inp.save();
            inp.skip(); // consume the newline

            // Check for blank line (another newline immediately)
            if inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)) {
                // Blank line or EOF - paragraph ends before this newline
                inp.rewind(before_newline);
                break;
            }

            // Check for block interrupt pattern
            if is_block_interrupt(inp) {
                // Block interrupt - paragraph ends before this newline
                inp.rewind(before_newline);
                break;
            }

            // Continue with next line (newline is part of paragraph content)
            // We've already consumed the newline, continue the loop
        }

        let content_span: SourceSpan = inp.span_since(&start_cursor);

        // Consume trailing newline if present
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        if content_span.start >= content_span.end {
            return Err(Rich::custom(content_span, "empty paragraph"));
        }

        let mut block = RawBlock::new("paragraph");
        block.content_span = Some(content_span);
        block.location = Some(idx.location(&content_span));

        Ok(block)
    })
}

// ---------------------------------------------------------------------------
// Main Block Parser
// ---------------------------------------------------------------------------

/// Result type for the metadata/block parsing.
/// Either metadata to apply to next block, or a parsed block.
#[derive(Clone)]
#[allow(clippy::large_enum_variant)]
enum ParseItem<'src> {
    /// Metadata to apply to next block.
    Metadata(PendingMetadata<'src>),
    /// Skip this item (comment).
    Skip,
    /// A parsed block.
    Block(RawBlock<'src>),
}

/// Build the main block parser.
///
/// This parser handles:
/// - Metadata accumulation (attributes, titles)
/// - Block recognition (breaks, verbatim, compound, paragraph)
///
/// Note: Compound blocks store content as a span. Recursive parsing of
/// compound block content happens in phase 3.
pub(super) fn blocks_parser<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Vec<RawBlock<'src>>, BlockExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // Individual item parsers
    let item = choice((
        // Comments (skip) - block_comment must come before line_comment
        // because //// starts with // which would match line_comment
        block_comment().to(ParseItem::Skip),
        line_comment().to(ParseItem::Skip),
        // Metadata (accumulate)
        block_attr_line(source).map(ParseItem::Metadata),
        block_title_line().map(ParseItem::Metadata),
        // Breaks
        thematic_break(idx).map(ParseItem::Block),
        page_break(idx).map(ParseItem::Block),
        // Section headings
        section_heading(source, idx).map(ParseItem::Block),
        // Lists (must come before paragraph)
        unordered_list(source, idx).map(ParseItem::Block),
        ordered_list(source, idx).map(ParseItem::Block),
        // Verbatim blocks (must come before compound to avoid conflicts)
        listing_block(source, idx).map(ParseItem::Block),
        literal_block(source, idx).map(ParseItem::Block),
        passthrough_block(source, idx).map(ParseItem::Block),
        fenced_code_block(source, idx).map(ParseItem::Block),
        // Compound blocks (content parsed in phase 3)
        example_block(source, idx).map(ParseItem::Block),
        sidebar_block(source, idx).map(ParseItem::Block),
        quote_block(source, idx).map(ParseItem::Block),
        open_block(source, idx).map(ParseItem::Block),
        // Fallback paragraph
        paragraph(idx).map(ParseItem::Block),
    ));

    // Parse items separated by blank lines, fold metadata into blocks
    item.separated_by(blank_lines().or_not())
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(fold_items)
}

/// Fold parse items, applying pending metadata to blocks.
fn fold_items(items: Vec<ParseItem<'_>>) -> Vec<RawBlock<'_>> {
    let mut blocks = Vec::new();
    let mut pending_meta: Option<PendingMetadata<'_>> = None;

    for item in items {
        match item {
            ParseItem::Skip => {
                // Comments clear pending metadata
                pending_meta = None;
            }
            ParseItem::Metadata(meta) => {
                // Merge with any existing pending metadata
                if let Some(existing) = pending_meta.take() {
                    pending_meta = Some(merge_metadata(existing, meta));
                } else {
                    pending_meta = Some(meta);
                }
            }
            ParseItem::Block(mut block) => {
                // Check if [comment] style - skip this block entirely
                if pending_meta
                    .as_ref()
                    .is_some_and(PendingMetadata::is_comment)
                {
                    pending_meta = None;
                    continue; // Skip this block
                }

                // Apply pending metadata to block
                if let Some(meta) = pending_meta.take() {
                    meta.apply_to(&mut block);
                }
                blocks.push(block);
            }
        }
    }

    blocks
}

/// Merge two pending metadata values.
fn merge_metadata<'src>(
    mut base: PendingMetadata<'src>,
    overlay: PendingMetadata<'src>,
) -> PendingMetadata<'src> {
    // Overlay takes precedence for single values
    if overlay.title_span.is_some() {
        base.title_span = overlay.title_span;
    }
    if overlay.id.is_some() {
        base.id = overlay.id;
    }
    if overlay.style.is_some() {
        base.style = overlay.style;
    }
    if overlay.reftext_span.is_some() {
        base.reftext_span = overlay.reftext_span;
    }
    // Merge collections
    base.roles.extend(overlay.roles);
    base.options.extend(overlay.options);
    base
}

// ---------------------------------------------------------------------------
// Entry Point
// ---------------------------------------------------------------------------

use super::Spanned;
use crate::asg::Block;
use crate::diagnostic::ParseDiagnostic;

/// Parse tokens into raw blocks (phase 2).
///
/// This identifies block structure and boundaries without parsing inline content.
pub(super) fn parse_raw_blocks<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<RawBlock<'src>>, Vec<ParseDiagnostic>) {
    if tokens.is_empty() {
        return (Vec::new(), Vec::new());
    }

    let last_span = tokens.last().unwrap().1;
    let eoi = SourceSpan {
        start: last_span.end,
        end: last_span.end,
    };
    let input = tokens.split_token_span(eoi);

    let parser = blocks_parser(source, idx);
    let (output, errors) = parser.parse(input).into_output_errors();

    let diagnostics: Vec<ParseDiagnostic> = errors
        .into_iter()
        .map(|e| ParseDiagnostic {
            span: *e.span(),
            message: e.to_string(),
            severity: crate::diagnostic::Severity::Error,
        })
        .collect();

    (output.unwrap_or_default(), diagnostics)
}

// ---------------------------------------------------------------------------
// Combined Entry Point (Phase 2 + Phase 3)
// ---------------------------------------------------------------------------

/// Build blocks from a token stream using pure chumsky parsing.
///
/// This combines Phase 2 (block boundary identification) and Phase 3
/// (`RawBlock` → Block transformation with inline parsing).
pub(super) fn build_blocks_pure<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    // Phase 2: Parse into RawBlocks
    let (raw_blocks, mut diagnostics) = parse_raw_blocks(tokens, source, idx);

    // Phase 3: Transform to Blocks with inline parsing
    let (blocks, transform_diags) = super::transform::transform_raw_blocks(raw_blocks, source, idx);
    diagnostics.extend(transform_diags);

    (blocks, diagnostics)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;

    #[test]
    fn test_thematic_break() {
        let source = "'''\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "break");
        assert_eq!(blocks[0].variant, Some("thematic"));
    }

    #[test]
    fn test_page_break() {
        let source = "<<<\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "break");
        assert_eq!(blocks[0].variant, Some("page"));
    }

    #[test]
    fn test_paragraph() {
        let source = "hello world\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
        assert!(blocks[0].content_span.is_some());
    }

    #[test]
    fn test_multiline_paragraph() {
        let source = "line one\nline two\nline three\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(
            blocks.len(),
            1,
            "should be single paragraph, got {blocks:?}"
        );
        assert_eq!(blocks[0].name, "paragraph");

        let span = blocks[0].content_span.expect("should have content span");
        let content = &source[span.start..span.end];
        assert_eq!(content, "line one\nline two\nline three");
    }

    #[test]
    fn test_multiline_paragraph_ends_at_blank_line() {
        let source = "para one\nstill para one\n\npara two\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 2, "should be two paragraphs");

        let span1 = blocks[0].content_span.expect("first para should have span");
        assert_eq!(&source[span1.start..span1.end], "para one\nstill para one");

        let span2 = blocks[1]
            .content_span
            .expect("second para should have span");
        assert_eq!(&source[span2.start..span2.end], "para two");
    }

    #[test]
    fn test_multiline_paragraph_ends_at_block_delimiter() {
        let source = "paragraph text\ncontinued\n----\ncode\n----\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[0].name, "paragraph");
        assert_eq!(blocks[1].name, "listing");

        let span = blocks[0].content_span.expect("paragraph should have span");
        assert_eq!(&source[span.start..span.end], "paragraph text\ncontinued");
    }

    #[test]
    fn test_attr_parsing() {
        let content = "source#myid.role1.role2%opt1";
        let meta = parse_attr_content(content);

        assert_eq!(meta.style, Some("source"));
        assert_eq!(meta.id, Some("myid"));
        assert_eq!(meta.roles, vec!["role1", "role2"]);
        assert_eq!(meta.options, vec!["opt1"]);
    }

    #[test]
    fn test_literal_block_no_trailing_newline() {
        let source = "....\nline one\n\nline two\n....";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "literal");
    }

    #[test]
    fn test_block_comment_alone() {
        let source = "////\ncomment\n////\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 0, "block comment should be skipped");
    }

    #[test]
    fn test_block_comment_skipped() {
        let source = "first\n\n////\ncomment\n////\n\nsecond";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[0].name, "paragraph");
        assert_eq!(blocks[1].name, "paragraph");
    }

    #[test]
    fn test_listing_block() {
        let source = "----\ncode here\n----\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "listing");
        assert_eq!(blocks[0].form, Some("delimited"));
        assert_eq!(blocks[0].delimiter, Some("----"));
        let span = blocks[0].content_span.expect("should have content span");
        assert_eq!(&source[span.start..span.end], "code here");
    }

    #[test]
    fn test_multiple_blocks() {
        let source = "para1\n\n----\ncode\n----\n\npara2\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 3);
        assert_eq!(blocks[0].name, "paragraph");
        assert_eq!(blocks[1].name, "listing");
        assert_eq!(blocks[2].name, "paragraph");
    }

    #[test]
    fn test_example_block() {
        let source = "====\ninner paragraph\n====\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "example");
        assert_eq!(blocks[0].form, Some("delimited"));
        assert_eq!(blocks[0].delimiter, Some("===="));
        // Content stored as span (will be parsed in phase 3)
        let span = blocks[0].content_span.expect("should have content span");
        assert_eq!(&source[span.start..span.end], "inner paragraph");
    }

    #[test]
    fn test_sidebar_block() {
        let source = "****\nsidebar content\n****\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "sidebar");
        assert_eq!(blocks[0].form, Some("delimited"));
        let span = blocks[0].content_span.expect("should have content span");
        assert_eq!(&source[span.start..span.end], "sidebar content");
    }

    #[test]
    fn test_open_block() {
        let source = "--\nopen content\n--\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "open");
        assert_eq!(blocks[0].form, Some("delimited"));
        assert_eq!(blocks[0].delimiter, Some("--"));
        let span = blocks[0].content_span.expect("should have content span");
        assert_eq!(&source[span.start..span.end], "open content");
    }

    #[test]
    fn test_nested_example_in_sidebar() {
        // Tests that compound blocks capture their full content span
        // including nested delimiters (parsing nested structure happens in phase 3)
        let source = "====\n****\nnested\n****\n====\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "example");
        let span = blocks[0].content_span.expect("should have content span");
        // Content includes the nested sidebar block
        assert_eq!(&source[span.start..span.end], "****\nnested\n****");
    }

    #[test]
    fn test_section_heading() {
        let source = "== Section Title\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "section");
        assert_eq!(blocks[0].level, Some(1));
        let title_span = blocks[0].title_span.expect("should have title span");
        assert_eq!(&source[title_span.start..title_span.end], "Section Title");
    }

    #[test]
    fn test_section_level_2() {
        let source = "=== Subsection\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "section");
        assert_eq!(blocks[0].level, Some(2));
    }

    #[test]
    fn test_unordered_list_simple() {
        let source = "* item one\n* item two\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");
        assert_eq!(blocks[0].variant, Some("unordered"));

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].name, "listItem");
        assert_eq!(items[0].marker, Some("*"));

        let span = items[0].principal_span.expect("should have principal span");
        assert_eq!(&source[span.start..span.end], "item one");
    }

    #[test]
    fn test_ordered_list_simple() {
        let source = ". first\n. second\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");
        assert_eq!(blocks[0].variant, Some("ordered"));

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].marker, Some("."));
    }

    #[test]
    fn test_nested_unordered_list() {
        let source = "* outer\n** nested\n* back to outer\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2);

        // First item should have nested list in its blocks
        let nested = items[0]
            .blocks
            .as_ref()
            .expect("first item should have blocks");
        assert_eq!(nested.len(), 1);
        assert_eq!(nested[0].name, "list");
        assert_eq!(nested[0].variant, Some("unordered"));

        let nested_items = nested[0]
            .items
            .as_ref()
            .expect("nested list should have items");
        assert_eq!(nested_items.len(), 1);
        assert_eq!(nested_items[0].marker, Some("**"));
    }

    // Tests for combined entry point (Phase 2 + Phase 3)

    #[test]
    fn test_combined_paragraph_with_formatting() {
        let source = "hello *bold* world\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = build_blocks_pure(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
        assert!(blocks[0].inlines.is_some());
        // Should have parsed inlines with strong formatting
        let inlines = blocks[0].inlines.as_ref().unwrap();
        assert!(inlines.len() >= 2); // At least text + strong
    }

    #[test]
    fn test_combined_nested_compound() {
        let source = "====\ninner *bold* paragraph\n====\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = build_blocks_pure(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "example");

        // Should have child blocks with parsed inlines
        let child_blocks = blocks[0].blocks.as_ref().expect("should have child blocks");
        assert_eq!(child_blocks.len(), 1);
        assert_eq!(child_blocks[0].name, "paragraph");
        assert!(child_blocks[0].inlines.is_some());
    }

    #[test]
    fn test_combined_list_with_formatting() {
        let source = "* item *one*\n* item two\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = build_blocks_pure(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2);

        // First item should have parsed principal with formatting
        let principal = items[0].principal.as_ref().expect("should have principal");
        assert!(!principal.is_empty());
    }
}

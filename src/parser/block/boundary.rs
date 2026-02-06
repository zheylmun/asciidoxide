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

/// Result of extracting an embedded anchor from a title.
struct EmbeddedAnchor<'a> {
    /// The ID extracted from `[[id]]` or `[[id,reftext]]`.
    id: &'a str,
    /// Byte offset of the reftext within the original title string, if present.
    reftext_offset: Option<(usize, usize)>,
    /// Byte offset where the title content ends (before the anchor).
    title_end_offset: usize,
}

/// Extract an embedded anchor (`[[id]]` or `[[id,reftext]]`) from the end of a title.
///
/// Returns `None` if no valid anchor is found.
fn extract_embedded_anchor(title: &str, title_end_offset: usize) -> Option<EmbeddedAnchor<'_>> {
    let segment = title[..title_end_offset].trim_end();
    let anchor_start = segment.rfind("[[")?;
    let anchor_end = segment[anchor_start..].find("]]")?;

    let anchor_inner = &segment[anchor_start + 2..anchor_start + anchor_end];
    let (id_part, reftext_offset) = if let Some(comma) = anchor_inner.find(',') {
        let reftext_start = anchor_start + 2 + comma + 1;
        let reftext_len = anchor_inner.len() - comma - 1;
        (
            &anchor_inner[..comma],
            Some((reftext_start, reftext_start + reftext_len)),
        )
    } else {
        (anchor_inner, None)
    };

    if id_part.is_empty() || !is_valid_anchor_id(id_part) {
        return None;
    }

    let before_anchor = segment[..anchor_start].trim_end();
    Some(EmbeddedAnchor {
        id: &title[anchor_start + 2..anchor_start + 2 + id_part.len()],
        reftext_offset,
        title_end_offset: before_anchor.len(),
    })
}

/// Strip trailing symmetric heading marker (e.g., ` ==` for level 1).
fn strip_trailing_eq_marker(title: &str, eq_count: usize) -> usize {
    let trimmed = title.trim_end();
    let eq_marker: String = " ".to_string() + &"=".repeat(eq_count);
    if trimmed.ends_with(&eq_marker) {
        trimmed.len() - eq_marker.len()
    } else {
        title.len()
    }
}

/// Strip trailing symmetric hash marker (e.g., ` ##` for level 1).
#[allow(dead_code)] // Will be used when refactoring markdown_heading
fn strip_trailing_hash_marker(title: &str, hash_count: usize) -> usize {
    let trimmed = title.trim_end();
    let hash_marker: String = " ".to_string() + &"#".repeat(hash_count);
    if trimmed.ends_with(&hash_marker) {
        trimmed.len() - hash_marker.len()
    } else {
        title.len()
    }
}

/// Trim trailing newlines from a body content span.
fn trim_trailing_newlines(source: &str, start: usize, end: usize) -> usize {
    let mut actual_end = end;
    while actual_end > start && source.as_bytes().get(actual_end - 1) == Some(&b'\n') {
        actual_end -= 1;
    }
    actual_end
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
    let mut positional_index = 0;
    for part in content.split(',') {
        let part = part.trim();

        // Check for named attribute: key=value
        if let Some(eq_pos) = part.find('=') {
            let key = part[..eq_pos].trim();
            let value = part[eq_pos + 1..].trim();
            // Strip surrounding quotes from value if present
            let value = if (value.starts_with('"') && value.ends_with('"'))
                || (value.starts_with('\'') && value.ends_with('\''))
            {
                &value[1..value.len() - 1]
            } else {
                value
            };
            if !key.is_empty() {
                if key == "id" {
                    meta.id = Some(value);
                } else {
                    meta.named_attributes.insert(key, value);
                }
            }
            continue;
        }

        if positional_index == 0 {
            // First positional may contain style#id.role%opt
            if !part.is_empty() {
                parse_first_positional(part, &mut meta);
            }
            meta.positionals.push(part);
        } else {
            meta.positionals.push(part);
        }
        positional_index += 1;
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

/// Validate that an anchor ID contains only valid characters.
///
/// Valid anchor IDs: alphanumeric, `_`, `-`, `.`, `:`.
fn is_valid_anchor_id(id: &str) -> bool {
    !id.is_empty()
        && id
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.' || c == ':')
}

/// Parse a block anchor line (`[[id]]` or `[[id,reftext]]`).
fn block_anchor_line<'tokens, 'src: 'tokens, I>(
    source: &'src str,
) -> impl Parser<'tokens, I, PendingMetadata<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Must start with [[
        if !matches!(inp.peek(), Some(Token::LBracket)) {
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected [ for anchor",
            ));
        }
        inp.skip();
        if !matches!(inp.peek(), Some(Token::LBracket)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected [[ for anchor",
            ));
        }
        inp.skip();

        // Capture content between [[ and ]]
        let content_cursor = inp.cursor();
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::RBracket | Token::Newline))
        {
            inp.skip();
        }
        let content_span: SourceSpan = inp.span_since(&content_cursor);

        // Must end with ]]
        if !matches!(inp.peek(), Some(Token::RBracket)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected ]] for anchor",
            ));
        }
        inp.skip();
        if !matches!(inp.peek(), Some(Token::RBracket)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected ]] for anchor",
            ));
        }
        inp.skip();

        // Must be at end of line
        if inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "anchor must be at end of line",
            ));
        }
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        let content = &source[content_span.start..content_span.end];

        // Split on comma: [[id]] or [[id,reftext]]
        let mut meta = PendingMetadata::default();
        if let Some(comma_pos) = content.find(',') {
            let id = &content[..comma_pos];
            if !is_valid_anchor_id(id) {
                inp.rewind(before);
                return Err(Rich::custom(
                    inp.span_since(&start_cursor),
                    "invalid anchor ID",
                ));
            }
            meta.id = Some(id);
            let reftext_start = content_span.start + comma_pos + 1;
            let reftext_end = content_span.end;
            if reftext_start < reftext_end {
                meta.reftext_span = Some(SourceSpan {
                    start: reftext_start,
                    end: reftext_end,
                });
            }
        } else {
            if !is_valid_anchor_id(content) {
                inp.rewind(before);
                return Err(Rich::custom(
                    inp.span_since(&start_cursor),
                    "invalid anchor ID",
                ));
            }
            meta.id = Some(content);
        }

        Ok(meta)
    })
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

/// Parse a fenced code block (`` ``` `` or `` ```lang ``).
///
/// Captures optional language text after the opening backticks.
#[allow(clippy::too_many_lines)]
fn fenced_code_block<'tokens, 'src: 'tokens, I>(
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
                            block.positionals = vec!["", lang];
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
#[allow(clippy::too_many_lines)]
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
        let raw_title_span: SourceSpan = inp.span_since(&title_cursor);

        // Title must have content
        if raw_title_span.start >= raw_title_span.end {
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

        // Post-process title to extract embedded anchors and strip trailing symmetric markers
        let raw_title = &source[raw_title_span.start..raw_title_span.end];
        let mut title_end_offset = strip_trailing_eq_marker(raw_title, eq_count);
        let mut embedded_id: Option<&'src str> = None;
        let mut embedded_reftext_span: Option<SourceSpan> = None;

        // Check for embedded anchor
        if let Some(anchor) = extract_embedded_anchor(raw_title, title_end_offset) {
            embedded_id = Some(anchor.id);
            if let Some((start_off, end_off)) = anchor.reftext_offset {
                embedded_reftext_span = Some(SourceSpan {
                    start: raw_title_span.start + start_off,
                    end: raw_title_span.start + end_off,
                });
            }
            title_end_offset = anchor.title_end_offset;
        }

        let title_span = SourceSpan {
            start: raw_title_span.start,
            end: raw_title_span.start + title_end_offset,
        };

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
        let actual_body_end = trim_trailing_newlines(source, body_start, body_end);

        Ok(build_section_block(
            &SectionParams {
                level,
                title_span,
                heading_line_span,
                embedded_id,
                reftext_span: embedded_reftext_span,
                body_start,
                body_end: actual_body_end,
            },
            idx,
        ))
    })
}

/// Parse a Markdown-style section heading (`## Title`, `### Title`, etc.).
///
/// Maps `##` to level 1, `###` to level 2, etc. (same as `==`/`===`).
/// Produces the same `RawBlock` as `section_heading`.
#[allow(clippy::too_many_lines)]
fn markdown_heading<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Count leading `#` tokens (need at least 2 for level 1)
        let mut hash_count = 0;
        while matches!(inp.peek(), Some(Token::Hash)) {
            inp.skip();
            hash_count += 1;
        }

        if hash_count < 2 {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "need at least 2 hash signs for markdown heading",
            ));
        }

        // Must be followed by whitespace
        if !matches!(inp.peek(), Some(Token::Whitespace)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected whitespace after markdown heading marker",
            ));
        }
        inp.skip(); // consume whitespace

        // Capture title content (everything until newline)
        let title_cursor = inp.cursor();
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }
        let title_span: SourceSpan = inp.span_since(&title_cursor);

        if title_span.start >= title_span.end {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "markdown heading title cannot be empty",
            ));
        }

        let heading_line_span: SourceSpan = inp.span_since(&start_cursor);

        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        let level = hash_count - 1; // ## = level 1, ### = level 2, etc.

        // Capture body content (same logic as section_heading)
        let body_cursor = inp.cursor();
        let body_start_span: SourceSpan = inp.span_since(&body_cursor);
        let body_start = body_start_span.start;

        let mut at_line_start = true;
        loop {
            match inp.peek() {
                None => break,
                Some(Token::Newline) => {
                    inp.skip();
                    at_line_start = true;
                }
                // Check for AsciiDoc section heading (==)
                Some(Token::Eq) if at_line_start => {
                    let check_before = inp.save();
                    let mut count = 0;
                    while matches!(inp.peek(), Some(Token::Eq)) {
                        inp.skip();
                        count += 1;
                    }
                    if count >= 2 && matches!(inp.peek(), Some(Token::Whitespace)) {
                        let check_level = count - 1;
                        if check_level <= level {
                            inp.rewind(check_before);
                            break;
                        }
                    }
                    at_line_start = false;
                }
                // Check for Markdown heading (##)
                Some(Token::Hash) if at_line_start => {
                    let check_before = inp.save();
                    let mut count = 0;
                    while matches!(inp.peek(), Some(Token::Hash)) {
                        inp.skip();
                        count += 1;
                    }
                    if count >= 2 && matches!(inp.peek(), Some(Token::Whitespace)) {
                        let check_level = count - 1;
                        if check_level <= level {
                            inp.rewind(check_before);
                            break;
                        }
                    }
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
        let actual_body_end = trim_trailing_newlines(source, body_start, body_end);

        Ok(build_section_block(
            &SectionParams {
                level,
                title_span,
                heading_line_span,
                embedded_id: None,
                reftext_span: None,
                body_start,
                body_end: actual_body_end,
            },
            idx,
        ))
    })
}

/// Parameters for building a section block.
struct SectionParams<'src> {
    level: usize,
    title_span: SourceSpan,
    heading_line_span: SourceSpan,
    embedded_id: Option<&'src str>,
    reftext_span: Option<SourceSpan>,
    body_start: usize,
    body_end: usize,
}

/// Build a section `RawBlock` with common fields.
fn build_section_block<'src>(params: &SectionParams<'src>, idx: &SourceIndex) -> RawBlock<'src> {
    let SectionParams {
        level,
        title_span,
        heading_line_span,
        embedded_id,
        reftext_span,
        body_start,
        body_end,
    } = *params;
    let mut block = RawBlock::new("section");
    block.level = Some(level);
    block.title_span = Some(title_span);
    block.heading_line_location = Some(idx.location(&heading_line_span));
    if let Some(eid) = embedded_id {
        block.id = Some(eid);
    }
    if let Some(refspan) = reftext_span {
        block.reftext_span = Some(refspan);
    }
    if body_start < body_end {
        block.content_span = Some(SourceSpan {
            start: body_start,
            end: body_end,
        });
        let block_span = SourceSpan {
            start: heading_line_span.start,
            end: body_end,
        };
        block.location = Some(idx.location(&block_span));
    } else {
        block.location = Some(idx.location(&heading_line_span));
    }
    block
}

/// Parse a setext-style section heading (title followed by underline of 10+ hyphens).
///
/// Produces a level-1 section.
#[allow(clippy::too_many_lines)]
fn setext_heading<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // First line: title content (must not be empty, must not start with special tokens)
        // Skip if starts with a block-forming token
        match inp.peek() {
            None
            | Some(
                Token::Newline
                | Token::Eq
                | Token::Hash
                | Token::Star
                | Token::Dot
                | Token::Hyphen
                | Token::LBracket
                | Token::Slash
                | Token::Plus
                | Token::Underscore
                | Token::Backtick,
            ) => {
                inp.rewind(before);
                return Err(Rich::custom(
                    inp.span_since(&start_cursor),
                    "setext heading cannot start with block-forming token",
                ));
            }
            _ => {}
        }

        // Consume title line
        let title_cursor = inp.cursor();
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }
        let title_span: SourceSpan = inp.span_since(&title_cursor);

        if title_span.start >= title_span.end {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "setext heading title cannot be empty",
            ));
        }

        // Must have newline after title
        if !matches!(inp.peek(), Some(Token::Newline)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected newline after setext title",
            ));
        }
        inp.skip(); // consume newline

        // Second line: 10+ hyphens followed by newline or EOF
        let underline_cursor = inp.cursor();
        let mut hyphen_count = 0;
        while matches!(inp.peek(), Some(Token::Hyphen)) {
            inp.skip();
            hyphen_count += 1;
        }

        if hyphen_count < 10
            || (inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)))
        {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "setext underline needs 10+ hyphens",
            ));
        }

        let underline_span: SourceSpan = inp.span_since(&underline_cursor);

        // Consume trailing newline
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        // Heading line spans from title start to end of underline
        let heading_span = SourceSpan {
            start: title_span.start,
            end: underline_span.end,
        };

        // Capture body content (same as section_heading, level 1)
        let level = 1;
        let body_cursor = inp.cursor();
        let body_start_span: SourceSpan = inp.span_since(&body_cursor);
        let body_start = body_start_span.start;

        let mut at_line_start = true;
        loop {
            match inp.peek() {
                None => break,
                Some(Token::Newline) => {
                    inp.skip();
                    at_line_start = true;
                }
                Some(Token::Eq) if at_line_start => {
                    let check_before = inp.save();
                    let mut count = 0;
                    while matches!(inp.peek(), Some(Token::Eq)) {
                        inp.skip();
                        count += 1;
                    }
                    if count >= 2 && matches!(inp.peek(), Some(Token::Whitespace)) {
                        let check_level = count - 1;
                        if check_level <= level {
                            inp.rewind(check_before);
                            break;
                        }
                    }
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

        let mut actual_body_end = body_end;
        while actual_body_end > body_start
            && source.as_bytes().get(actual_body_end - 1) == Some(&b'\n')
        {
            actual_body_end -= 1;
        }

        let mut block = RawBlock::new("section");
        block.level = Some(level);
        block.title_span = Some(title_span);
        block.heading_line_location = Some(idx.location(&heading_span));
        if body_start < actual_body_end {
            block.content_span = Some(SourceSpan {
                start: body_start,
                end: actual_body_end,
            });
            let block_span = SourceSpan {
                start: heading_span.start,
                end: actual_body_end,
            };
            block.location = Some(idx.location(&block_span));
        } else {
            block.location = Some(idx.location(&heading_span));
        }

        Ok(block)
    })
}

// ---------------------------------------------------------------------------
// Block Macro Parsers
// ---------------------------------------------------------------------------

/// Parse an `image::target[attrlist]` block macro.
///
/// Produces a `RawBlock` with `name="image"`, `form="macro"`, `target="path"`.
fn image_block_macro<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Match "image" text
        if let Some(Token::Text("image")) = inp.peek() {
            inp.skip();
        } else {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected 'image'",
            ));
        }

        // Match `::`
        if !matches!(inp.peek(), Some(Token::Colon)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected ':' after 'image'",
            ));
        }
        inp.skip();
        if !matches!(inp.peek(), Some(Token::Colon)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected '::' after 'image'",
            ));
        }
        inp.skip();

        // Capture target (everything until `[`)
        let target_cursor = inp.cursor();
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::LBracket | Token::Newline))
        {
            inp.skip();
        }
        let target_span: SourceSpan = inp.span_since(&target_cursor);

        if target_span.start >= target_span.end {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "image macro target cannot be empty",
            ));
        }

        // Match `[`
        if !matches!(inp.peek(), Some(Token::LBracket)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected '[' after image target",
            ));
        }
        inp.skip();

        // Consume attrlist until `]` (we ignore the content for now)
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::RBracket | Token::Newline))
        {
            inp.skip();
        }

        // Match `]`
        if !matches!(inp.peek(), Some(Token::RBracket)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected ']' to close image macro",
            ));
        }
        inp.skip();

        let block_span: SourceSpan = inp.span_since(&start_cursor);

        // Consume trailing newline
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        let target = &source[target_span.start..target_span.end];
        let mut block = RawBlock::new("image");
        block.form = Some("macro");
        block.target = Some(target);
        block.location = Some(idx.location(&block_span));

        Ok(block)
    })
}

// ---------------------------------------------------------------------------
// Description List Parser
// ---------------------------------------------------------------------------

/// Parse a description list (`term:: description`).
///
/// Collects consecutive description list items into a `dlist` block.
/// Each item has a term (before `::`) and optional principal (after `:: `),
/// or block content via list continuation (`+`).
#[allow(clippy::too_many_lines)]
fn description_list<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        let mut items: Vec<RawBlock<'src>> = Vec::new();

        loop {
            let item_cursor = inp.cursor();

            // Scan for `::` pattern on this line: consume tokens until we find Colon+Colon
            // followed by whitespace+content or newline (end of term)
            let term_cursor = inp.cursor();
            let mut found_marker = false;

            loop {
                match inp.peek() {
                    None | Some(Token::Newline) => break,
                    Some(Token::Colon) => {
                        let colon_before = inp.save();
                        inp.skip();
                        if matches!(inp.peek(), Some(Token::Colon)) {
                            inp.skip();
                            // Check what follows: whitespace+content or newline/EOF
                            if inp.peek().is_none()
                                || matches!(inp.peek(), Some(Token::Newline | Token::Whitespace))
                            {
                                found_marker = true;
                                break;
                            }
                            // Not a valid marker, continue
                            // Already consumed two colons, keep going
                        } else {
                            // Single colon, not a marker
                            inp.rewind(colon_before);
                            inp.skip();
                        }
                    }
                    _ => {
                        inp.skip();
                    }
                }
            }

            if !found_marker {
                if items.is_empty() {
                    inp.rewind(before);
                    return Err(Rich::custom(
                        inp.span_since(&start_cursor),
                        "not a description list",
                    ));
                }
                // No more items, break the collection loop
                break;
            }

            // We found `term::` — capture the term span (before the ::)
            let term_span_raw: SourceSpan = inp.span_since(&term_cursor);
            // The term span ends 2 bytes before current position (before the `::`)
            let term_span = SourceSpan {
                start: term_span_raw.start,
                end: term_span_raw.end - 2, // exclude the `::`
            };

            // Check for principal content on same line
            let mut principal_span: Option<SourceSpan> = None;
            let mut content_span: Option<SourceSpan> = None;

            if matches!(inp.peek(), Some(Token::Whitespace)) {
                inp.skip(); // consume space after ::
                let principal_cursor = inp.cursor();
                while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
                    inp.skip();
                }
                let ps: SourceSpan = inp.span_since(&principal_cursor);
                if ps.start < ps.end {
                    principal_span = Some(ps);
                }
            }

            // Consume trailing newline
            if matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
            }

            // Check for list continuation `+` on next line
            if principal_span.is_none() {
                let cont_before = inp.save();
                if matches!(inp.peek(), Some(Token::Plus)) {
                    inp.skip();
                    if inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)) {
                        // Found list continuation
                        if matches!(inp.peek(), Some(Token::Newline)) {
                            inp.skip();
                        }
                        // Capture content span until blank line or EOF
                        let cont_cursor = inp.cursor();
                        let cont_start_span: SourceSpan = inp.span_since(&cont_cursor);
                        let cont_start = cont_start_span.start;

                        let mut at_line_start = true;
                        loop {
                            match inp.peek() {
                                None => break,
                                Some(Token::Newline) if at_line_start => break,
                                Some(Token::Newline) => {
                                    inp.skip();
                                    at_line_start = true;
                                }
                                _ => {
                                    inp.skip();
                                    at_line_start = false;
                                }
                            }
                        }

                        let cont_end_span: SourceSpan = inp.span_since(&cont_cursor);
                        let mut cont_end = cont_end_span.end;
                        while cont_end > cont_start
                            && source.as_bytes().get(cont_end - 1) == Some(&b'\n')
                        {
                            cont_end -= 1;
                        }

                        if cont_start < cont_end {
                            content_span = Some(SourceSpan {
                                start: cont_start,
                                end: cont_end,
                            });
                        }

                        // Consume the blank line
                        if matches!(inp.peek(), Some(Token::Newline)) {
                            inp.skip();
                        }
                    } else {
                        inp.rewind(cont_before);
                    }
                }
            }

            let item_span: SourceSpan = inp.span_since(&item_cursor);
            // Adjust item span to exclude trailing newline
            let mut item_end = item_span.end;
            while item_end > item_span.start && source.as_bytes().get(item_end - 1) == Some(&b'\n')
            {
                item_end -= 1;
            }
            let adjusted_item_span = SourceSpan {
                start: item_span.start,
                end: item_end,
            };

            let mut item = RawBlock::new("dlistItem");
            item.marker = Some("::");
            item.term_spans.push(term_span);
            item.principal_span = principal_span;
            item.content_span = content_span;
            item.location = Some(idx.location(&adjusted_item_span));

            items.push(item);

            // Check if next line is another dlist item (peek ahead)
            // If blank line or EOF, stop collecting
            if inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)) {
                break;
            }
        }

        if items.is_empty() {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "not a description list",
            ));
        }

        let list_span: SourceSpan = inp.span_since(&start_cursor);
        let mut list_end = list_span.end;
        while list_end > list_span.start && source.as_bytes().get(list_end - 1) == Some(&b'\n') {
            list_end -= 1;
        }
        let adjusted_list_span = SourceSpan {
            start: list_span.start,
            end: list_end,
        };

        let mut dlist = RawBlock::new("dlist");
        dlist.marker = Some("::");
        dlist.items = Some(items);
        dlist.location = Some(idx.location(&adjusted_list_span));

        Ok(dlist)
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

    // Skip leading whitespace (for continuation items)
    while matches!(inp.peek(), Some(Token::Whitespace)) {
        inp.skip();
    }

    // Capture start AFTER whitespace so location begins at the marker
    let start_cursor = inp.cursor();
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
    let mut item_span: SourceSpan = inp.span_since(&start_cursor);

    // Consume trailing newline if present
    if matches!(inp.peek(), Some(Token::Newline)) {
        inp.skip();
    }

    // Check for list continuation `+` on next line
    let mut continuation_span: Option<SourceSpan> = None;
    let before_cont = inp.save();
    if matches!(inp.peek(), Some(Token::Plus)) {
        inp.skip();
        if matches!(inp.peek(), Some(Token::Newline)) {
            // Found `+\n` — consume the newline and capture following block content
            inp.skip();
            let cont_start = inp.cursor();
            // Capture everything until end of input or a line that starts with
            // a list marker at our level or shallower (next sibling item).
            let mut last_content_end: Option<SourceSpan> = None;
            loop {
                if inp.peek().is_none() {
                    break;
                }
                // Check if this line starts a new list item at our level or shallower
                let line_save = inp.save();
                while matches!(inp.peek(), Some(Token::Whitespace)) {
                    inp.skip();
                }
                let mc = peek_token_count(inp, marker_token);
                if mc > 0 && mc <= expected_level {
                    let check_save = inp.save();
                    for _ in 0..mc {
                        inp.skip();
                    }
                    let is_list_marker = matches!(inp.peek(), Some(Token::Whitespace));
                    inp.rewind(check_save);
                    if is_list_marker {
                        inp.rewind(line_save);
                        break;
                    }
                }
                inp.rewind(line_save);

                // Consume this line
                while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
                    inp.skip();
                }
                last_content_end = Some(inp.span_since(&cont_start));
                if matches!(inp.peek(), Some(Token::Newline)) {
                    inp.skip();
                }
            }
            if let Some(span) = last_content_end {
                continuation_span = Some(span);
                // Update item_span to include continuation
                item_span = SourceSpan {
                    start: item_span.start,
                    end: span.end,
                };
            }
        } else {
            inp.rewind(before_cont);
        }
    }

    let mut item = RawBlock::new("listItem");
    item.marker = Some(marker.trim_end());
    if content_span.start < content_span.end {
        item.principal_span = Some(content_span);
    }
    item.content_span = continuation_span;
    item.location = Some(idx.location(&item_span));

    Some(item)
}

/// Recursively parse list items at the given nesting level.
///
/// Returns a `RawBlock` list node containing items at `list_level`.
/// When a deeper marker is encountered, recurses and attaches the
/// nested list to the last item's `blocks`. When a shallower marker
/// (or non-marker) is encountered, returns to the caller.
fn parse_list_at_level<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
    marker_token: &Token<'src>,
    list_level: usize,
    variant: &'static str,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<RawBlock<'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let list_start = inp.cursor();
    let mut items: Vec<RawBlock<'src>> = Vec::new();

    loop {
        // Peek ahead, skipping leading whitespace
        let line_before = inp.save();
        while matches!(inp.peek(), Some(Token::Whitespace)) {
            inp.skip();
        }

        let current_count = peek_token_count(inp, marker_token);

        // No markers at all → done
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
            // Nested list — recurse
            inp.rewind(line_before);

            if let Some(nested_list) =
                parse_list_at_level(inp, marker_token, current_count, variant, source, idx)
            {
                // Attach to last item and extend its location
                if let Some(last_item) = items.last_mut() {
                    let nested_end = nested_list.location.map(|loc| loc[1]);
                    last_item
                        .blocks
                        .get_or_insert_with(Vec::new)
                        .push(nested_list);
                    if let (Some(item_loc), Some(end_pos)) = (&mut last_item.location, nested_end) {
                        item_loc[1] = end_pos;
                    }
                }
            }

            continue;
        }

        // Same level — parse item
        inp.rewind(line_before);
        if let Some(item) = parse_list_item(inp, marker_token, list_level, source, idx) {
            items.push(item);
        } else {
            break;
        }
    }

    if items.is_empty() {
        return None;
    }

    let first_marker = items[0].marker.unwrap_or("");

    // Derive list location from first item start to last item end
    // (avoids including trailing newlines from span_since)
    let list_location = match (items.first(), items.last()) {
        (Some(first), Some(last)) => match (first.location, last.location) {
            (Some(first_loc), Some(last_loc)) => Some([first_loc[0], last_loc[1]]),
            _ => Some(idx.location(&inp.span_since(&list_start))),
        },
        _ => Some(idx.location(&inp.span_since(&list_start))),
    };

    let mut list_block = RawBlock::new("list");
    list_block.variant = Some(variant);
    list_block.marker = Some(first_marker);
    list_block.items = Some(items);
    list_block.location = list_location;

    Some(list_block)
}

/// Parse a list (unordered or ordered).
///
/// Unordered lists use `*` markers, ordered use `.` markers.
/// Nesting is determined by marker count (`**` is deeper than `*`).
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

        // Delegate to recursive parser (rewind first so it sees the full list)
        inp.rewind(before);
        let before2 = inp.save();
        if let Some(list_block) =
            parse_list_at_level(inp, &marker_token, marker_count, variant, source, idx)
        {
            Ok(list_block)
        } else {
            inp.rewind(before2);
            Err(Rich::custom(
                inp.span_since(&start_cursor),
                "no list items found",
            ))
        }
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
        // End of input, block attribute line [...], or block anchor [[...]]
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

        // Markdown heading (## Title)
        Some(Token::Hash) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Hash)) {
                inp.skip();
                count += 1;
            }
            count >= 2 && matches!(inp.peek(), Some(Token::Whitespace))
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
        block_anchor_line(source).map(ParseItem::Metadata),
        block_attr_line(source).map(ParseItem::Metadata),
        block_title_line().map(ParseItem::Metadata),
        // Breaks
        thematic_break(idx).map(ParseItem::Block),
        page_break(idx).map(ParseItem::Block),
        // Section headings (AsciiDoc, Markdown, and setext)
        section_heading(source, idx).map(ParseItem::Block),
        markdown_heading(source, idx).map(ParseItem::Block),
        setext_heading(source, idx).map(ParseItem::Block),
        // Lists (must come before paragraph)
        description_list(source, idx).map(ParseItem::Block),
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
        // Block macros
        image_block_macro(source, idx).map(ParseItem::Block),
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
    if !overlay.positionals.is_empty() {
        base.positionals = overlay.positionals;
    }
    base.named_attributes.extend(overlay.named_attributes);
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

    #[test]
    fn test_nested_unordered_list_3_levels() {
        let source = "* outer\n** middle\n*** inner\n** middle2\n* outer2\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2, "top-level list should have 2 items");

        // First top-level item should have a nested ** list
        let level2 = items[0]
            .blocks
            .as_ref()
            .expect("first item should have nested blocks");
        assert_eq!(level2.len(), 1);
        assert_eq!(level2[0].name, "list");
        assert_eq!(level2[0].variant, Some("unordered"));

        let level2_items = level2[0]
            .items
            .as_ref()
            .expect("level-2 list should have items");
        assert_eq!(level2_items.len(), 2, "level-2 list should have 2 items");
        assert_eq!(level2_items[0].marker, Some("**"));
        assert_eq!(level2_items[1].marker, Some("**"));

        // First level-2 item should have a nested *** list
        let level3 = level2_items[0]
            .blocks
            .as_ref()
            .expect("first level-2 item should have nested blocks");
        assert_eq!(level3.len(), 1);
        assert_eq!(level3[0].name, "list");
        assert_eq!(level3[0].variant, Some("unordered"));

        let level3_items = level3[0]
            .items
            .as_ref()
            .expect("level-3 list should have items");
        assert_eq!(level3_items.len(), 1);
        assert_eq!(level3_items[0].marker, Some("***"));
    }

    #[test]
    fn test_nested_ordered_list_3_levels() {
        let source = ". Foo\n.. Boo\n... Snoo\n. Blech\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");
        assert_eq!(blocks[0].variant, Some("ordered"));

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2, "top-level list should have 2 items");
        assert_eq!(items[0].marker, Some("."));
        assert_eq!(items[1].marker, Some("."));

        // First top-level item should have a nested .. list
        let level2 = items[0]
            .blocks
            .as_ref()
            .expect("first item should have nested blocks");
        assert_eq!(level2.len(), 1);
        assert_eq!(level2[0].name, "list");
        assert_eq!(level2[0].marker, Some(".."));

        let level2_items = level2[0]
            .items
            .as_ref()
            .expect("level-2 list should have items");
        assert_eq!(level2_items.len(), 1);
        assert_eq!(level2_items[0].marker, Some(".."));

        // The level-2 item should have a nested ... list
        let level3 = level2_items[0]
            .blocks
            .as_ref()
            .expect("level-2 item should have nested blocks");
        assert_eq!(level3.len(), 1);
        assert_eq!(level3[0].name, "list");
        assert_eq!(level3[0].marker, Some("..."));

        let level3_items = level3[0]
            .items
            .as_ref()
            .expect("level-3 list should have items");
        assert_eq!(level3_items.len(), 1);
        assert_eq!(level3_items[0].marker, Some("..."));
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

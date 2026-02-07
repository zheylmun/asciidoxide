//! Metadata parsers for block attributes, anchors, and titles.

use chumsky::{input::ValueInput, prelude::*};

use super::utility::{BlockExtra, is_valid_anchor_id, line_end};
use crate::parser::block::raw_block::PendingMetadata;
use crate::span::SourceSpan;
use crate::token::Token;

/// Parse a block attribute line (`[...]`).
pub(super) fn block_attr_line<'tokens, 'src: 'tokens, I>(
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
pub(super) fn parse_attr_content(content: &str) -> PendingMetadata<'_> {
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

/// Parse a block anchor line (`[[id]]` or `[[id,reftext]]`).
pub(super) fn block_anchor_line<'tokens, 'src: 'tokens, I>(
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
pub(super) fn block_title_line<'tokens, 'src: 'tokens, I>()
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

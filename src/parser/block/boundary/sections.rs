//! Section heading parsers (`AsciiDoc`, Markdown, setext).

use chumsky::{input::ValueInput, prelude::*};

use super::utility::{
    BlockExtra, SectionParams, extract_embedded_anchor, strip_trailing_eq_marker,
    trim_trailing_newlines,
};
use crate::parser::block::raw_block::RawBlock;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Build a section `RawBlock` with common fields.
pub(super) fn build_section_block<'src>(
    params: &SectionParams<'src>,
    idx: &SourceIndex,
) -> RawBlock<'src> {
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
pub(super) fn section_heading<'tokens, 'src: 'tokens, I>(
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

        // Scan for section end.
        //
        // Block attribute lines (`[...]`) that immediately precede a
        // same-or-higher-level heading belong to that *next* section, not
        // the current one.  We track `attr_run_start` — a save-point just
        // before the first `[` line in any consecutive run of attribute /
        // blank lines.  When we hit a heading that ends this section, we
        // rewind to `attr_run_start` so those tokens remain unconsumed.
        let mut at_line_start = true;
        let mut attr_run_start: Option<_> = None;
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
                Some(Token::LBracket) if at_line_start => {
                    // Potential block attribute line — start tracking a run
                    // if we haven't already.
                    if attr_run_start.is_none() {
                        attr_run_start = Some(inp.save());
                        // Rewind past the newline(s) before this `[` line
                        // so the save-point includes them.
                        // Actually we need to save *before* we consumed
                        // the newlines leading here. We saved after the
                        // peek, so the cursor is at `[`.  That's fine —
                        // the separator blank lines will be re-parsed by
                        // `blocks_parser` which tolerates leading blanks.
                    }
                    // Consume the rest of this line.
                    while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
                        inp.skip();
                    }
                    at_line_start = false;
                }
                Some(Token::Eq) if at_line_start => {
                    // Potential section heading — check its level
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
                            // Found same or higher level section — rewind to
                            // before any trailing attribute lines so they
                            // remain in the token stream for the next section.
                            if let Some(run_start) = attr_run_start {
                                inp.rewind(run_start);
                            } else {
                                inp.rewind(check_before);
                            }
                            break;
                        }
                    }

                    // Not a section heading or lower level — those `[` lines
                    // (if any) were body content, not trailing metadata.
                    attr_run_start = None;
                    at_line_start = false;
                }
                _ => {
                    inp.skip();
                    if at_line_start {
                        // Non-`[` content at line start resets the run.
                        attr_run_start = None;
                    }
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
pub(super) fn markdown_heading<'tokens, 'src: 'tokens, I>(
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
        let mut attr_run_start: Option<_> = None;
        loop {
            match inp.peek() {
                None => break,
                Some(Token::Newline) => {
                    inp.skip();
                    at_line_start = true;
                }
                Some(Token::LBracket) if at_line_start => {
                    if attr_run_start.is_none() {
                        attr_run_start = Some(inp.save());
                    }
                    while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
                        inp.skip();
                    }
                    at_line_start = false;
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
                            if let Some(run_start) = attr_run_start {
                                inp.rewind(run_start);
                            } else {
                                inp.rewind(check_before);
                            }
                            break;
                        }
                    }
                    attr_run_start = None;
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
                            if let Some(run_start) = attr_run_start {
                                inp.rewind(run_start);
                            } else {
                                inp.rewind(check_before);
                            }
                            break;
                        }
                    }
                    attr_run_start = None;
                    at_line_start = false;
                }
                _ => {
                    inp.skip();
                    if at_line_start {
                        attr_run_start = None;
                    }
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

/// Parse a setext-style section heading (title followed by underline of 10+ hyphens).
///
/// Produces a level-1 section.
#[allow(clippy::too_many_lines)]
pub(super) fn setext_heading<'tokens, 'src: 'tokens, I>(
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
        let mut attr_run_start: Option<_> = None;
        loop {
            match inp.peek() {
                None => break,
                Some(Token::Newline) => {
                    inp.skip();
                    at_line_start = true;
                }
                Some(Token::LBracket) if at_line_start => {
                    if attr_run_start.is_none() {
                        attr_run_start = Some(inp.save());
                    }
                    while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
                        inp.skip();
                    }
                    at_line_start = false;
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
                            if let Some(run_start) = attr_run_start {
                                inp.rewind(run_start);
                            } else {
                                inp.rewind(check_before);
                            }
                            break;
                        }
                    }
                    attr_run_start = None;
                    at_line_start = false;
                }
                _ => {
                    inp.skip();
                    if at_line_start {
                        attr_run_start = None;
                    }
                    at_line_start = false;
                }
            }
        }

        let body_end_span: SourceSpan = inp.span_since(&body_cursor);
        let body_end = body_end_span.end;
        let actual_body_end = trim_trailing_newlines(source, body_start, body_end);

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

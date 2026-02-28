//! List parsers (ordered, unordered, description).

use chumsky::{input::ValueInput, prelude::*};

use super::utility::BlockExtra;
use crate::parser::block::raw_block::RawBlock;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

// ---------------------------------------------------------------------------
// Description List Parser
// ---------------------------------------------------------------------------

/// Parse a description list (`term:: description`).
///
/// Collects consecutive description list items into a `dlist` block.
/// Each item has a term (before `::`) and optional principal (after `:: `),
/// or block content via list continuation (`+`).
#[allow(clippy::too_many_lines)]
pub(super) fn description_list<'tokens, 'src: 'tokens, I>(
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
pub(super) fn unordered_list<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    list(Token::Star, "unordered", source, idx)
}

/// Parse an ordered list (`.`, `..`, etc.).
pub(super) fn ordered_list<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    list(Token::Dot, "ordered", source, idx)
}

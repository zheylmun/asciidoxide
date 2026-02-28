//! Document header and attribute parsing.
//!
//! Token-level parsing uses chumsky combinators (`custom()` for complex
//! patterns, declarative combinators for simple ones), matching the style
//! used throughout the rest of the block parser.

use std::collections::HashMap;

use chumsky::{input::ValueInput, prelude::*};

use super::super::inline::run_inline_parser;
use super::Spanned;
use super::boundary::utility::BlockExtra;
use crate::asg::{AttributeValue, Author, Header};
use crate::diagnostic::ParseDiagnostic;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

// ---------------------------------------------------------------------------
// String-level helpers (unchanged)
// ---------------------------------------------------------------------------

/// Substitute `{name}` attribute references in a value string.
///
/// Returns `None` if no substitutions were made (value is unchanged).
fn substitute_attributes<'src>(
    value: &str,
    attrs: &HashMap<&'src str, AttributeValue<'src>>,
) -> Option<String> {
    if !value.contains('{') {
        return None;
    }
    let mut result = String::with_capacity(value.len());
    let mut rest = value;
    let mut changed = false;
    while let Some(open) = rest.find('{') {
        result.push_str(&rest[..open]);
        let after_open = &rest[open + 1..];
        if let Some(close) = after_open.find('}') {
            let name = &after_open[..close];
            if let Some(attr_val) = attrs.get(name) {
                result.push_str(&attr_val.resolve());
                changed = true;
            } else {
                // Unresolved reference — keep as-is
                result.push('{');
                result.push_str(name);
                result.push('}');
            }
            rest = &after_open[close + 1..];
        } else {
            // No closing brace — keep the `{` and move on
            result.push('{');
            rest = after_open;
        }
    }
    result.push_str(rest);
    if changed { Some(result) } else { None }
}

/// Apply attribute substitution to an `AttributeValue`, returning a new
/// `Resolved` variant if any references were expanded.
fn resolve_attr_refs<'src>(
    value: AttributeValue<'src>,
    attrs: &HashMap<&'src str, AttributeValue<'src>>,
) -> AttributeValue<'src> {
    let resolved_str = value.resolve();
    match substitute_attributes(&resolved_str, attrs) {
        Some(substituted) => AttributeValue::Resolved(substituted),
        None => value,
    }
}

/// Check if an attribute name is valid per the `AsciiDoc` spec.
///
/// Valid names match: `^[a-zA-Z0-9_][-a-zA-Z0-9_]*$`
fn is_valid_attribute_name(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphanumeric() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
}

/// Parse a single author from a string like `"Doc Writer"` or `"Doc Writer <email>"`.
fn parse_single_author(text: &str) -> Option<Author<'_>> {
    let text = text.trim();
    if text.is_empty() {
        return None;
    }

    // Check for email in angle brackets
    let (name_part, address) = if let Some(angle_start) = text.find('<') {
        if let Some(angle_end) = text[angle_start..].find('>') {
            let email = &text[angle_start + 1..angle_start + angle_end];
            let name = text[..angle_start].trim();
            (name, Some(email))
        } else {
            (text, None)
        }
    } else {
        (text, None)
    };

    let parts: Vec<&str> = name_part.split_whitespace().collect();
    if parts.is_empty() {
        return None;
    }

    let (firstname, middlename, lastname) = match parts.len() {
        1 => (parts[0], None, None),
        2 => (parts[0], None, Some(parts[1])),
        _ => (parts[0], Some(parts[1]), Some(*parts.last().unwrap())),
    };

    // Compute initials from all name parts
    let initials: String = parts
        .iter()
        .filter_map(|p| p.chars().next())
        .map(|c| c.to_uppercase().next().unwrap_or(c))
        .collect();

    Some(Author {
        fullname: name_part.trim(),
        initials,
        firstname,
        middlename,
        lastname,
        address,
    })
}

/// Parse an author line, splitting by `;` for multiple authors.
fn parse_authors(line: &str) -> Vec<Author<'_>> {
    line.split(';').filter_map(parse_single_author).collect()
}

/// Check if a line looks like a revision line.
///
/// A revision line starts with `v` followed by a digit, or starts with a digit
/// (date format like `2012-01-01`).
fn is_revision_line(line: &str) -> bool {
    let trimmed = line.trim();
    if trimmed.is_empty() {
        return false;
    }
    // Starts with 'v' followed by digit
    if trimmed.starts_with('v') && trimmed.len() > 1 && trimmed.as_bytes()[1].is_ascii_digit() {
        return true;
    }
    // Starts with digit (date)
    trimmed.as_bytes()[0].is_ascii_digit()
}

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// Result of header extraction from the token stream.
pub(crate) struct HeaderResult<'src> {
    pub(crate) header: Option<Header<'src>>,
    /// Index of the first body token (past the header and its attributes).
    pub(crate) body_start: usize,
    pub(crate) diagnostics: Vec<ParseDiagnostic>,
    /// Document attributes parsed from `:key: value` lines below the title.
    /// `Some` when a header is present (even if no attribute entries exist).
    pub(crate) attributes: Option<HashMap<&'src str, AttributeValue<'src>>>,
}

/// Result of parsing an attribute entry.
#[derive(Clone)]
enum AttributeEntry<'src> {
    /// Set an attribute: `:key: value`
    Set {
        key: &'src str,
        value: AttributeValue<'src>,
    },
    /// Delete an attribute: `:!key:` or `:key!:`
    Delete { key: &'src str },
}

/// Apply an attribute entry to the attributes map.
fn apply_attribute_entry<'src>(
    entry: AttributeEntry<'src>,
    attributes: &mut HashMap<&'src str, AttributeValue<'src>>,
) {
    match entry {
        AttributeEntry::Set { key, value } => {
            let value = resolve_attr_refs(value, attributes);
            attributes.insert(key, value);
        }
        AttributeEntry::Delete { key } => {
            attributes.remove(key);
        }
    }
}

// ---------------------------------------------------------------------------
// Chumsky helper functions for use inside `custom()` parsers
// ---------------------------------------------------------------------------

/// Parse the attribute name imperatively from an `InputRef`.
///
/// This is called from within `custom()` parsers.
fn parse_attr_name<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
    source: &'src str,
) -> Result<&'src str, ()>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let start_cursor = inp.cursor();

    // Must start with Text.
    if !matches!(inp.peek(), Some(Token::Text(_))) {
        return Err(());
    }
    inp.skip();

    // Continue consuming separator + Text pairs.
    while let Some(Token::Hyphen | Token::Underscore) = inp.peek() {
        let before = inp.save();
        inp.skip();
        if matches!(inp.peek(), Some(Token::Text(_))) {
            inp.skip();
        } else {
            inp.rewind(before);
            break;
        }
    }

    let span: SourceSpan = inp.span_since(&start_cursor);
    let key = &source[span.start..span.end];
    if is_valid_attribute_name(key) {
        Ok(key)
    } else {
        Err(())
    }
}

/// Collect multiline continuation segments.
///
/// `is_backslash` controls trimming behaviour:
/// - `true`: backslash continuation — trim trailing whitespace, join with spaces.
/// - `false`: legacy `+` continuation — preserve trailing whitespace, concatenate.
fn collect_multiline_segments<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
    source: &'src str,
    first_val_start: usize,
    first_line_end: usize,
    is_backslash: bool,
) -> Vec<&'src str>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let mut segments: Vec<&'src str> = Vec::new();

    // First segment: everything before the continuation marker.
    // The continuation marker is the last token on the line, so we need to
    // find where the content before it ends. We look backwards from `first_line_end`
    // for the marker (1 byte for `\` or `+`).
    let marker_byte = if is_backslash { b'\\' } else { b'+' };
    let mut content_end = first_line_end;
    // Walk backwards past the marker character.
    while content_end > first_val_start && source.as_bytes()[content_end - 1] == marker_byte {
        content_end -= 1;
    }

    if first_val_start < content_end {
        let seg = &source[first_val_start..content_end];
        segments.push(if is_backslash { seg.trim_end() } else { seg });
    }

    // Consume newline after the continuation marker.
    if matches!(inp.peek(), Some(Token::Newline)) {
        inp.skip();
    }

    // Read continuation lines.
    loop {
        // Skip leading whitespace.
        if matches!(inp.peek(), Some(Token::Whitespace)) {
            inp.skip();
        }

        // Capture line start.
        let line_cursor = inp.cursor();
        let line_span: SourceSpan = inp.span_since(&line_cursor);
        let line_byte_start = line_span.start;
        let mut line_last_end = line_byte_start;
        let mut line_last_token: Option<Token<'src>> = None;
        let mut line_empty = true;

        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
            let before = inp.cursor();
            let tok = inp.next().unwrap();
            let span: SourceSpan = inp.span_since(&before);
            line_last_end = span.end;
            line_last_token = Some(tok);
            line_empty = false;
        }

        if line_empty {
            // Empty continuation line — stop.
            if matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
            }
            break;
        }

        let cont_marker = if is_backslash {
            matches!(line_last_token, Some(Token::Backslash))
        } else {
            matches!(line_last_token, Some(Token::Plus))
        };

        if cont_marker {
            // Extract segment before marker.
            let mut seg_end = line_last_end;
            while seg_end > line_byte_start && source.as_bytes()[seg_end - 1] == marker_byte {
                seg_end -= 1;
            }
            if line_byte_start < seg_end {
                let seg = &source[line_byte_start..seg_end];
                segments.push(if is_backslash { seg.trim_end() } else { seg });
            }
            // Consume newline.
            if matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
            }
        } else {
            // Final line.
            if line_byte_start < line_last_end {
                segments.push(&source[line_byte_start..line_last_end]);
            }
            if matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
            }
            break;
        }
    }

    segments
}

/// Consume all tokens until newline/EOI and return `(byte_start, byte_end)`.
///
/// Returns `None` if the line is empty (no content tokens before newline).
fn consume_line_bytes<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
) -> Option<(usize, usize)>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let cursor = inp.cursor();
    let start_span: SourceSpan = inp.span_since(&cursor);
    let byte_start = start_span.start;
    let mut byte_end = byte_start;

    while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
        let before = inp.cursor();
        inp.skip();
        let s: SourceSpan = inp.span_since(&before);
        byte_end = s.end;
    }

    if byte_start < byte_end {
        Some((byte_start, byte_end))
    } else {
        None
    }
}

/// Try to parse author and revision lines after the title.
///
/// Returns `(authors, span_end)` where `span_end` is updated if authors/revision
/// were found.
fn try_parse_author_revision<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
    source: &'src str,
    current_span_end: usize,
) -> (Option<Vec<Author<'src>>>, usize)
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // Don't attempt if next token can't start an author line.
    if matches!(
        inp.peek(),
        None | Some(Token::Colon | Token::Eq | Token::Newline)
    ) {
        return (None, current_span_end);
    }

    let mut span_end = current_span_end;
    let author_save = inp.save();

    let Some((line_start, line_end)) = consume_line_bytes(inp) else {
        inp.rewind(author_save);
        return (None, span_end);
    };

    let author_text = &source[line_start..line_end];
    let parsed = parse_authors(author_text);

    if parsed.is_empty() {
        inp.rewind(author_save);
        return (None, span_end);
    }

    span_end = line_end;

    // Consume trailing newline after author line.
    if matches!(inp.peek(), Some(Token::Newline)) {
        inp.skip();
    }

    // Try revision line.
    if inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Colon | Token::Newline)) {
        let rev_save = inp.save();
        if let Some((rev_start, rev_end)) = consume_line_bytes(inp) {
            let rev_text = &source[rev_start..rev_end];
            if is_revision_line(rev_text) {
                span_end = rev_end;
                if matches!(inp.peek(), Some(Token::Newline)) {
                    inp.skip();
                }
            } else {
                inp.rewind(rev_save);
            }
        }
    }

    (Some(parsed), span_end)
}

// ---------------------------------------------------------------------------
// Top-level header parsers
// ---------------------------------------------------------------------------

/// Parse a titled header: `= Title\n` followed by optional author/revision
/// and attribute entries.
fn titled_header<'tokens, 'src: 'tokens, I>(
    tokens: &'tokens [Spanned<'src>],
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, HeaderResult<'src>, BlockExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let start_cursor = inp.cursor();

        // Skip block attribute lines (`[...]`).
        while matches!(inp.peek(), Some(Token::LBracket)) {
            while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
            }
            if matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
            }
        }

        // Must start with `= ` (single Eq followed by Whitespace).
        if !matches!(inp.peek(), Some(Token::Eq)) {
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected '=' for title",
            ));
        }
        let eq_cursor = inp.cursor();
        inp.skip(); // consume `=`

        if !matches!(inp.peek(), Some(Token::Whitespace)) {
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected whitespace after '='",
            ));
        }
        inp.skip(); // consume whitespace

        // Read title content until newline/EOI.
        let Some((title_byte_start, title_byte_end)) = consume_line_bytes(inp) else {
            return Err(Rich::custom(inp.span_since(&start_cursor), "empty title"));
        };

        // Extract title tokens from the original slice for inline parsing.
        let eq_span: SourceSpan = inp.span_since(&eq_cursor);
        let header_byte_start = eq_span.start;

        let title_token_start = tokens
            .iter()
            .position(|t| t.1.start >= title_byte_start)
            .unwrap_or(tokens.len());
        let title_token_end = tokens
            .iter()
            .position(|t| t.1.start >= title_byte_end)
            .unwrap_or(tokens.len());
        let title_tokens = &tokens[title_token_start..title_token_end];
        let (title_inlines, diagnostics) = run_inline_parser(title_tokens, source, idx);

        let mut span_end = title_byte_end;

        // Consume trailing newline.
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        // Try to parse author/revision lines.
        let (authors, new_span_end) = try_parse_author_revision(inp, source, span_end);
        span_end = new_span_end;

        // Parse attribute entries and line comments.
        let mut attributes = HashMap::new();
        let attr_cursor = inp.cursor();
        parse_attr_entries_loop(inp, source, &mut attributes);

        // When no authors, extend header span to cover attribute entries.
        if authors.is_none() {
            let attr_span: SourceSpan = inp.span_since(&attr_cursor);
            let mut entry_end = attr_span.end;
            if entry_end > 0 && source.as_bytes().get(entry_end - 1) == Some(&b'\n') {
                entry_end -= 1;
            }
            if entry_end > span_end {
                span_end = entry_end;
            }
        }

        // Compute body_start as a token index.
        let consumed_span: SourceSpan = inp.span_since(&start_cursor);
        let body_start = tokens
            .iter()
            .position(|t| t.1.start >= consumed_span.end)
            .unwrap_or(tokens.len());

        let header_span = SourceSpan {
            start: header_byte_start,
            end: span_end,
        };

        let header = Header {
            title: title_inlines,
            authors,
            location: Some(idx.location(&header_span)),
        };

        Ok(HeaderResult {
            header: Some(header),
            body_start,
            diagnostics,
            attributes: Some(attributes),
        })
    })
}

/// Parse attribute value imperatively (called from within `custom()` body).
fn parse_attribute_value_imperative<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
    source: &'src str,
    key: &'src str,
) -> AttributeEntry<'src>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let val_cursor = inp.cursor();
    let val_start_span: SourceSpan = inp.span_since(&val_cursor);
    let val_byte_start = val_start_span.start;

    let mut last_end = val_byte_start;
    let mut last_token: Option<Token<'src>> = None;
    let mut line_empty = true;

    while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
        let before = inp.cursor();
        let tok = inp.next().unwrap();
        let span: SourceSpan = inp.span_since(&before);
        last_end = span.end;
        last_token = Some(tok);
        line_empty = false;
    }

    if line_empty {
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }
        return AttributeEntry::Set {
            key,
            value: AttributeValue::Single(""),
        };
    }

    let has_backslash = matches!(last_token, Some(Token::Backslash));
    let has_plus = matches!(last_token, Some(Token::Plus));

    if has_backslash {
        let segments = collect_multiline_segments(inp, source, val_byte_start, last_end, true);
        AttributeEntry::Set {
            key,
            value: AttributeValue::Multiline(segments),
        }
    } else if has_plus {
        let segments = collect_multiline_segments(inp, source, val_byte_start, last_end, false);
        AttributeEntry::Set {
            key,
            value: AttributeValue::MultilineLegacy(segments),
        }
    } else {
        let value = &source[val_byte_start..last_end];
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }
        AttributeEntry::Set {
            key,
            value: AttributeValue::Single(value),
        }
    }
}

/// Try to parse a single attribute entry from the current position.
///
/// Expects the leading `:` to have already been consumed. On failure, the
/// caller is responsible for rewinding via a saved checkpoint.
fn try_parse_attr_entry<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
    source: &'src str,
) -> Result<AttributeEntry<'src>, ()>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // Check bang prefix for delete syntax.
    let (key, is_delete) = if matches!(inp.peek(), Some(Token::Bang)) {
        inp.skip();
        let key = parse_attr_name(inp, source)?;
        if !matches!(inp.peek(), Some(Token::Colon)) {
            return Err(());
        }
        inp.skip();
        (key, true)
    } else {
        let key = parse_attr_name(inp, source)?;
        if matches!(inp.peek(), Some(Token::Bang)) {
            inp.skip();
            if !matches!(inp.peek(), Some(Token::Colon)) {
                return Err(());
            }
            inp.skip();
            (key, true)
        } else if matches!(inp.peek(), Some(Token::Colon)) {
            inp.skip();
            (key, false)
        } else {
            return Err(());
        }
    };

    if is_delete {
        if inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
            return Err(());
        }
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }
        return Ok(AttributeEntry::Delete { key });
    }

    // After the second colon, expect whitespace before value.
    match inp.peek() {
        None => {
            return Ok(AttributeEntry::Set {
                key,
                value: AttributeValue::Single(""),
            });
        }
        Some(Token::Newline) => {
            inp.skip();
            return Ok(AttributeEntry::Set {
                key,
                value: AttributeValue::Single(""),
            });
        }
        Some(Token::Whitespace) => {
            inp.skip();
        }
        _ => return Err(()),
    }

    Ok(parse_attribute_value_imperative(inp, source, key))
}

/// Skip line comments (`// ...`) imperatively. Returns `true` if a comment was consumed.
fn try_skip_line_comment<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
) -> bool
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    if !matches!(inp.peek(), Some(Token::Slash)) {
        return false;
    }
    let save = inp.save();
    inp.skip();
    if matches!(inp.peek(), Some(Token::Slash)) {
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }
        true
    } else {
        inp.rewind(save);
        false
    }
}

/// Parse attribute entries and line comments in a loop, applying entries to the
/// given attributes map. Returns `true` if at least one entry was parsed.
fn parse_attr_entries_loop<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
    source: &'src str,
    attributes: &mut HashMap<&'src str, AttributeValue<'src>>,
) -> bool
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let mut parsed_any = false;
    loop {
        if try_skip_line_comment(inp) {
            continue;
        }

        if !matches!(inp.peek(), Some(Token::Colon)) {
            break;
        }

        let attr_save = inp.save();
        inp.skip(); // consume leading `:`

        if let Ok(entry) = try_parse_attr_entry(inp, source) {
            parsed_any = true;
            apply_attribute_entry(entry, attributes);
        } else {
            inp.rewind(attr_save);
            break;
        }
    }
    parsed_any
}

/// Parse body-only attribute entries (no title).
fn body_only_attrs<'tokens, 'src: 'tokens, I>(
    tokens: &'tokens [Spanned<'src>],
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, HeaderResult<'src>, BlockExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let start_cursor = inp.cursor();

        // Must start with a valid attribute entry (colon).
        if !matches!(inp.peek(), Some(Token::Colon)) {
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected attribute entry",
            ));
        }

        // Parse all attribute entries and comments.
        let mut attributes = HashMap::new();
        if !parse_attr_entries_loop(inp, source, &mut attributes) {
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "no attribute entries found",
            ));
        }

        // Compute body_start token index.
        let consumed_span: SourceSpan = inp.span_since(&start_cursor);
        let body_start = tokens
            .iter()
            .position(|t| t.1.start >= consumed_span.end)
            .unwrap_or(tokens.len());

        // Check for :doctitle: → create header.
        if attributes.contains_key("doctitle") {
            return Ok(
                try_create_doctitle_header(tokens, body_start, source, idx, attributes).unwrap_or(
                    HeaderResult {
                        header: None,
                        body_start,
                        diagnostics: Vec::new(),
                        attributes: None,
                    },
                ),
            );
        }

        // Only return attributes if there are any remaining after deletions.
        let attrs = if attributes.is_empty() {
            None
        } else {
            Some(attributes)
        };

        Ok(HeaderResult {
            header: None,
            body_start,
            diagnostics: Vec::new(),
            attributes: attrs,
        })
    })
}

/// Try to create a header from a `:doctitle:` attribute.
fn try_create_doctitle_header<'src>(
    tokens: &[Spanned<'src>],
    body_start: usize,
    source: &'src str,
    idx: &SourceIndex,
    mut attributes: HashMap<&'src str, AttributeValue<'src>>,
) -> Option<HeaderResult<'src>> {
    let doctitle_val = attributes.remove("doctitle")?;
    let doctitle_str = doctitle_val.resolve();

    // Find the doctitle value span for the title inlines location
    let title_value = match &doctitle_val {
        AttributeValue::Single(s) => *s,
        _ => {
            // For non-single values, we can't borrow — return without header
            return Some(HeaderResult {
                header: None,
                body_start,
                diagnostics: Vec::new(),
                attributes: Some(attributes),
            });
        }
    };

    let title_location = {
        let val_str: &str = doctitle_str.as_ref();
        source.find(val_str).map(|offset| {
            let val_span = SourceSpan {
                start: offset,
                end: offset + val_str.len(),
            };
            idx.location(&val_span)
        })
    };

    let title_inlines = vec![crate::asg::InlineNode::Text(crate::asg::TextNode {
        value: title_value,
        location: title_location,
    })];

    // Compute header location (from first attribute to end of doctitle line)
    let mut header_end = tokens[0].1.start;
    let mut ti = 0;
    while ti < tokens.len() && !matches!(tokens[ti].0, Token::Newline) {
        header_end = tokens[ti].1.end;
        ti += 1;
    }
    let header_span = SourceSpan {
        start: tokens[0].1.start,
        end: header_end,
    };

    let header = Header {
        title: title_inlines,
        authors: None,
        location: Some(idx.location(&header_span)),
    };

    Some(HeaderResult {
        header: Some(header),
        body_start,
        diagnostics: Vec::new(),
        attributes: Some(attributes),
    })
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

/// Detect a document header (`= Title`) at the start of the token stream.
///
/// When a header is found, `attributes` is `Some(map)` (possibly empty).
/// Attribute entry lines (`:key: value`) immediately following the title are
/// parsed into the map.
///
/// Also handles body-only attribute documents (no title, just `:key: value` lines).
pub(crate) fn extract_header<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> HeaderResult<'src> {
    if tokens.is_empty() {
        return HeaderResult {
            header: None,
            body_start: 0,
            diagnostics: Vec::new(),
            attributes: None,
        };
    }

    // Build chumsky input from the token slice.
    let last_span = tokens.last().unwrap().1;
    let eoi = SourceSpan {
        start: last_span.end,
        end: last_span.end,
    };
    let input = tokens.split_token_span(eoi);

    // Try titled header parser.
    let titled = titled_header(tokens, source, idx).then_ignore(any().repeated());
    if let Some(result) = titled.parse(input.clone()).into_output() {
        return result;
    }

    // Try body-only attribute entries.
    let body = body_only_attrs(tokens, source, idx).then_ignore(any().repeated());
    if let Some(result) = body.parse(input).into_output() {
        return result;
    }

    HeaderResult {
        header: None,
        body_start: 0,
        diagnostics: Vec::new(),
        attributes: None,
    }
}

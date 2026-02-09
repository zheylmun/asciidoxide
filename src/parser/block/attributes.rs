//! Document header and attribute parsing.

use std::collections::HashMap;

use super::super::inline::run_inline_parser;
use super::Spanned;
use crate::asg::{AttributeValue, Author, Header};
use crate::diagnostic::ParseDiagnostic;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

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

/// Result of parsing an attribute entry.
enum AttributeEntry<'src> {
    /// Set an attribute: `:key: value`
    Set {
        key: &'src str,
        value: AttributeValue<'src>,
    },
    /// Delete an attribute: `:!key:` or `:key!:`
    Delete { key: &'src str },
}

/// Parse a multiline attribute value with backslash continuation.
///
/// `val_start` is the index of the first value token.
/// `line_end` is the index of the Newline token (or end of tokens).
/// Returns the parsed segments and the token index to continue from.
///
/// Backslash continuation trims trailing whitespace and joins with spaces.
fn parse_backslash_multiline<'src>(
    tokens: &[Spanned<'src>],
    val_start: usize,
    line_end: usize,
    source: &'src str,
) -> (Vec<&'src str>, usize) {
    let mut segments: Vec<&'src str> = Vec::new();

    // Extract first segment (before the backslash).
    let seg_end = line_end - 1; // Exclude the backslash
    if val_start < seg_end {
        let start = tokens[val_start].1.start;
        let end = tokens[seg_end - 1].1.end;
        let segment = &source[start..end];
        // Trim trailing whitespace before backslash.
        segments.push(segment.trim_end());
    }

    // Advance past newline to continuation line.
    let mut pos = if line_end < tokens.len() {
        line_end + 1
    } else {
        line_end
    };

    // Read continuation lines.
    while pos < tokens.len() {
        // Skip leading whitespace on continuation line.
        if matches!(tokens[pos].0, Token::Whitespace) {
            pos += 1;
        }

        // Find end of this line.
        let cont_start = pos;
        while pos < tokens.len() && !matches!(tokens[pos].0, Token::Newline) {
            pos += 1;
        }

        // Check if this line also has continuation.
        let line_has_continuation =
            pos > cont_start && matches!(tokens[pos - 1].0, Token::Backslash);

        if line_has_continuation {
            // Extract segment before backslash.
            let seg_end = pos - 1;
            if cont_start < seg_end {
                let start = tokens[cont_start].1.start;
                let end = tokens[seg_end - 1].1.end;
                let segment = &source[start..end];
                segments.push(segment.trim_end());
            }
            // Advance past newline.
            if pos < tokens.len() {
                pos += 1;
            }
        } else {
            // Final line (no continuation).
            if cont_start < pos {
                let start = tokens[cont_start].1.start;
                let end = tokens[pos - 1].1.end;
                segments.push(&source[start..end]);
            }
            // Advance past newline if present.
            if pos < tokens.len() {
                pos += 1;
            }
            break;
        }
    }

    (segments, pos)
}

/// Parse a multiline attribute value with legacy `+` continuation.
///
/// `val_start` is the index of the first value token.
/// `line_end` is the index of the Newline token (or end of tokens).
/// Returns the parsed segments and the token index to continue from.
///
/// Legacy `+` continuation preserves trailing whitespace and concatenates directly.
fn parse_legacy_plus_multiline<'src>(
    tokens: &[Spanned<'src>],
    val_start: usize,
    line_end: usize,
    source: &'src str,
) -> (Vec<&'src str>, usize) {
    let mut segments: Vec<&'src str> = Vec::new();

    // Extract first segment (before the +).
    // Preserve trailing whitespace before the `+`.
    let seg_end = line_end - 1; // Exclude the `+`
    if val_start < seg_end {
        let start = tokens[val_start].1.start;
        let end = tokens[seg_end - 1].1.end;
        segments.push(&source[start..end]);
    }

    // Advance past newline to continuation line.
    let mut pos = if line_end < tokens.len() {
        line_end + 1
    } else {
        line_end
    };

    // Read continuation lines.
    while pos < tokens.len() {
        // Skip leading whitespace on continuation line.
        if matches!(tokens[pos].0, Token::Whitespace) {
            pos += 1;
        }

        // Find end of this line.
        let cont_start = pos;
        while pos < tokens.len() && !matches!(tokens[pos].0, Token::Newline) {
            pos += 1;
        }

        // Check if this line also has `+` continuation.
        let line_has_continuation = pos > cont_start && matches!(tokens[pos - 1].0, Token::Plus);

        if line_has_continuation {
            // Extract segment before `+`, preserving trailing whitespace.
            let seg_end = pos - 1;
            if cont_start < seg_end {
                let start = tokens[cont_start].1.start;
                let end = tokens[seg_end - 1].1.end;
                segments.push(&source[start..end]);
            }
            // Advance past newline.
            if pos < tokens.len() {
                pos += 1;
            }
        } else {
            // Final line (no continuation).
            if cont_start < pos {
                let start = tokens[cont_start].1.start;
                let end = tokens[pos - 1].1.end;
                segments.push(&source[start..end]);
            }
            // Advance past newline if present.
            if pos < tokens.len() {
                pos += 1;
            }
            break;
        }
    }

    (segments, pos)
}

/// Consume an attribute name starting at token index `start`.
///
/// Attribute names may contain hyphens and underscores, which the lexer
/// tokenizes as separate `Hyphen`/`Underscore` tokens. This function
/// consumes `Text` tokens interleaved with `Hyphen`/`Underscore` tokens
/// and returns the full name as a source slice.
///
/// Returns `Some((key_slice, next_index))` where `next_index` is the
/// token after the last consumed name token.
fn consume_attribute_name<'src>(
    tokens: &[Spanned<'src>],
    start: usize,
    source: &'src str,
) -> Option<(&'src str, usize)> {
    // Must start with a Text token.
    if start >= tokens.len() {
        return None;
    }
    if !matches!(tokens[start].0, Token::Text(_)) {
        return None;
    }

    let span_start = tokens[start].1.start;
    let mut pos = start + 1;

    // Continue consuming Hyphen/Underscore followed by Text.
    while pos + 1 < tokens.len()
        && matches!(tokens[pos].0, Token::Hyphen | Token::Underscore)
        && matches!(tokens[pos + 1].0, Token::Text(_))
    {
        pos += 2;
    }

    let span_end = tokens[pos - 1].1.end;
    let key = &source[span_start..span_end];
    if !is_valid_attribute_name(key) {
        return None;
    }
    Some((key, pos))
}

/// Try to skip a line comment (`// ...`) at position `i`.
///
/// Returns the token index after the comment (past the newline) if a
/// comment was found, or `None` if the tokens at `i` are not a comment.
fn try_skip_line_comment(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 1 < tokens.len()
        && matches!(tokens[i].0, Token::Slash)
        && matches!(tokens[i + 1].0, Token::Slash)
    {
        let mut pos = i + 2;
        while pos < tokens.len() && !matches!(tokens[pos].0, Token::Newline) {
            pos += 1;
        }
        // Skip the newline itself.
        if pos < tokens.len() {
            pos += 1;
        }
        Some(pos)
    } else {
        None
    }
}

/// Try to parse a single attribute entry at position `i`.
///
/// Patterns:
/// - `:key: value` — set attribute
/// - `:key:` — set attribute to empty string
/// - `:!key:` — delete attribute (bang prefix)
/// - `:key!:` — delete attribute (bang suffix)
///
/// Returns `Some((entry, next_index))` on success.
fn try_parse_attribute_entry<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
) -> Option<(AttributeEntry<'src>, usize)> {
    if i + 2 >= tokens.len() {
        return None;
    }
    if !matches!(tokens[i].0, Token::Colon) {
        return None;
    }

    // Check for bang-prefix deletion: `:!key:`
    let (key, is_delete, key_end) = if matches!(tokens[i + 1].0, Token::Bang) {
        // Need at least 4 tokens: Colon Bang <name...> Colon
        if i + 3 >= tokens.len() {
            return None;
        }
        let (key, after_name) = consume_attribute_name(tokens, i + 2, source)?;
        if after_name >= tokens.len() || !matches!(tokens[after_name].0, Token::Colon) {
            return None;
        }
        (key, true, after_name + 1)
    } else {
        // Regular or bang-suffix: `:key:` or `:key!:`
        let (key, after_name) = consume_attribute_name(tokens, i + 1, source)?;

        // Check for bang-suffix: `:key!:`
        if after_name < tokens.len() && matches!(tokens[after_name].0, Token::Bang) {
            if after_name + 1 >= tokens.len()
                || !matches!(tokens[after_name + 1].0, Token::Colon)
            {
                return None;
            }
            (key, true, after_name + 2)
        } else if after_name < tokens.len() && matches!(tokens[after_name].0, Token::Colon) {
            (key, false, after_name + 1)
        } else {
            return None;
        }
    };

    // For deletions, just need to reach end of line
    if is_delete {
        // Must be followed by Newline or end-of-tokens
        if key_end < tokens.len() && !matches!(tokens[key_end].0, Token::Newline) {
            return None;
        }
        let next = if key_end < tokens.len() {
            key_end + 1
        } else {
            key_end
        };
        return Some((AttributeEntry::Delete { key }, next));
    }

    // After the second colon, must have:
    // - Newline or end-of-tokens (empty value)
    // - Whitespace followed by value
    let after_colon = key_end;
    if after_colon < tokens.len()
        && !matches!(tokens[after_colon].0, Token::Newline)
        && !matches!(tokens[after_colon].0, Token::Whitespace)
    {
        // Something other than whitespace/newline immediately after colon - not valid.
        return None;
    }

    // Skip optional whitespace after second colon.
    let mut val_start = after_colon;
    if val_start < tokens.len() && matches!(tokens[val_start].0, Token::Whitespace) {
        val_start += 1;
    }

    // Scan to Newline or end-of-tokens, collecting multiline segments if needed.
    let mut line_end = val_start;
    while line_end < tokens.len() && !matches!(tokens[line_end].0, Token::Newline) {
        line_end += 1;
    }

    // Check if line ends with backslash (continuation) or `+` (legacy continuation).
    let has_backslash_continuation =
        line_end > val_start && matches!(tokens[line_end - 1].0, Token::Backslash);
    let has_plus_continuation =
        line_end > val_start && matches!(tokens[line_end - 1].0, Token::Plus);

    if has_backslash_continuation {
        let (segments, pos) = parse_backslash_multiline(tokens, val_start, line_end, source);
        let value = AttributeValue::Multiline(segments);
        Some((AttributeEntry::Set { key, value }, pos))
    } else if has_plus_continuation {
        let (segments, pos) = parse_legacy_plus_multiline(tokens, val_start, line_end, source);
        let value = AttributeValue::MultilineLegacy(segments);
        Some((AttributeEntry::Set { key, value }, pos))
    } else {
        // Single-line attribute value.
        let value = if val_start < line_end {
            let start = tokens[val_start].1.start;
            let end = tokens[line_end - 1].1.end;
            AttributeValue::Single(&source[start..end])
        } else {
            AttributeValue::Single("")
        };

        // Advance past Newline (if present).
        let next = if line_end < tokens.len() {
            line_end + 1
        } else {
            line_end
        };

        Some((AttributeEntry::Set { key, value }, next))
    }
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

/// Check if a line looks like an author line (not an attribute entry, not empty,
/// not a heading, not a block delimiter).
fn is_author_line(tokens: &[Spanned<'_>], pos: usize) -> bool {
    if pos >= tokens.len() {
        return false;
    }
    // Must not start with `:` (attribute entry) or `=` (heading) or be a newline (blank)
    !matches!(tokens[pos].0, Token::Colon | Token::Eq | Token::Newline)
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

/// Parse all attribute entries starting from a given token index.
///
/// Returns the final token index after all entries are consumed.
fn parse_all_attribute_entries<'src>(
    tokens: &[Spanned<'src>],
    mut i: usize,
    source: &'src str,
    attributes: &mut HashMap<&'src str, AttributeValue<'src>>,
) -> usize {
    loop {
        // Skip line comments between attribute entries.
        if let Some(after_comment) = try_skip_line_comment(tokens, i) {
            i = after_comment;
            continue;
        }
        if let Some((entry, next)) = try_parse_attribute_entry(tokens, i, source) {
            apply_attribute_entry(entry, attributes);
            i = next;
        } else {
            break;
        }
    }
    i
}

/// Skip block attribute lines (e.g., `[glossary]`) at the start of tokens.
fn skip_block_attribute_lines(tokens: &[Spanned<'_>]) -> usize {
    let mut i = 0;
    while i < tokens.len() && matches!(tokens[i].0, Token::LBracket) {
        // Skip to end of line
        while i < tokens.len() && !matches!(tokens[i].0, Token::Newline) {
            i += 1;
        }
        // Skip newline
        if i < tokens.len() {
            i += 1;
        }
    }
    i
}

/// Try to extract a titled header (`= Title`) starting at `start_idx`.
///
/// Returns `Some(HeaderResult)` if a valid title is found, `None` otherwise.
fn try_extract_titled_header<'src>(
    tokens: &[Spanned<'src>],
    start_idx: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<HeaderResult<'src>> {
    // Check for titled document: Eq Whitespace <content>
    if start_idx + 2 >= tokens.len()
        || !matches!(tokens[start_idx].0, Token::Eq)
        || !matches!(tokens[start_idx + 1].0, Token::Whitespace)
    {
        return None;
    }

    // Find the Newline (or end-of-tokens) that terminates the title line.
    let title_start = start_idx + 2;
    let mut title_end = title_start;
    while title_end < tokens.len() && !matches!(tokens[title_end].0, Token::Newline) {
        title_end += 1;
    }

    if title_start >= title_end {
        return None;
    }

    let title_tokens = &tokens[title_start..title_end];
    let (title_inlines, diagnostics) = run_inline_parser(title_tokens, source, idx);

    let header_start = tokens[start_idx].1.start;
    let mut span_end = tokens[title_end - 1].1.end;

    // Advance past the title's terminating Newline (if present).
    let mut i = if title_end < tokens.len() {
        title_end + 1
    } else {
        title_end
    };

    // Try to parse author and revision lines
    let (authors, new_i, new_span_end) = parse_author_and_revision(tokens, i, span_end, source);
    i = new_i;
    span_end = new_span_end;

    // Parse attribute entry lines, skipping line comments.
    // When there's no author/revision, attribute entries extend the header span.
    let mut attributes = HashMap::new();
    loop {
        // Skip line comments between attribute entries.
        if let Some(after_comment) = try_skip_line_comment(tokens, i) {
            i = after_comment;
            continue;
        }
        if let Some((entry, next)) = try_parse_attribute_entry(tokens, i, source) {
            if authors.is_none() {
                // Extend header span to include attribute entries
                span_end = tokens[next - 1].1.end;
                if next > 0 && matches!(tokens[next - 1].0, Token::Newline) {
                    span_end = tokens[next - 2].1.end;
                }
            }
            apply_attribute_entry(entry, &mut attributes);
            i = next;
        } else {
            break;
        }
    }

    let header_span = SourceSpan {
        start: header_start,
        end: span_end,
    };

    let header = Header {
        title: title_inlines,
        authors,
        location: Some(idx.location(&header_span)),
    };

    Some(HeaderResult {
        header: Some(header),
        body_start: i,
        diagnostics,
        attributes: Some(attributes),
    })
}

/// Parse author line and optional revision line following a title.
///
/// Returns `(authors, next_token_index, span_end)`.
fn parse_author_and_revision<'src>(
    tokens: &[Spanned<'src>],
    mut i: usize,
    mut span_end: usize,
    source: &'src str,
) -> (Option<Vec<Author<'src>>>, usize, usize) {
    let mut authors: Option<Vec<Author<'src>>> = None;

    if !is_author_line(tokens, i) {
        return (authors, i, span_end);
    }

    let line_start = i;
    while i < tokens.len() && !matches!(tokens[i].0, Token::Newline) {
        i += 1;
    }
    let author_text = &source[tokens[line_start].1.start..tokens[i - 1].1.end];
    let parsed = parse_authors(author_text);
    if !parsed.is_empty() {
        span_end = tokens[i - 1].1.end;
        authors = Some(parsed);
    }
    // Consume trailing newline
    if i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
        i += 1;
    }

    // Try to parse revision line (only valid after author line)
    if i < tokens.len() && !matches!(tokens[i].0, Token::Colon | Token::Newline) {
        let rev_line_start = i;
        while i < tokens.len() && !matches!(tokens[i].0, Token::Newline) {
            i += 1;
        }
        let rev_text = &source[tokens[rev_line_start].1.start..tokens[i - 1].1.end];
        if is_revision_line(rev_text) {
            span_end = tokens[i - 1].1.end;
        } else {
            // Not a revision line — rewind
            i = rev_line_start;
        }
        // Consume trailing newline
        if i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
            i += 1;
        }
    }

    (authors, i, span_end)
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
    // Skip optional block attribute lines (e.g., `[glossary]`) before the title.
    let header_token_start = skip_block_attribute_lines(tokens);

    // First, check for titled document: Eq Whitespace <content>
    if let Some(result) = try_extract_titled_header(tokens, header_token_start, source, idx) {
        return result;
    }

    // Second, check for body-only attribute entries (no title).
    if let Some((entry, first_next)) = try_parse_attribute_entry(tokens, 0, source) {
        let mut attributes = HashMap::new();
        apply_attribute_entry(entry, &mut attributes);

        // Continue parsing additional attribute entries.
        let next = parse_all_attribute_entries(tokens, first_next, source, &mut attributes);

        // Check for :doctitle: attribute → create header
        if attributes.contains_key("doctitle") {
            if let Some(result) = try_create_doctitle_header(tokens, next, source, idx, attributes)
            {
                return result;
            }
            // If try_create_doctitle_header returned None, we can't continue
            // because attributes was consumed. This shouldn't happen in practice.
            return HeaderResult {
                header: None,
                body_start: next,
                diagnostics: Vec::new(),
                attributes: None,
            };
        }

        // Only return attributes if there are any remaining after deletions.
        let attrs = if attributes.is_empty() {
            None
        } else {
            Some(attributes)
        };

        return HeaderResult {
            header: None,
            body_start: next,
            diagnostics: Vec::new(),
            attributes: attrs,
        };
    }

    HeaderResult {
        header: None,
        body_start: 0,
        diagnostics: Vec::new(),
        attributes: None,
    }
}

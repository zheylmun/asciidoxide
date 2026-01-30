//! Document header and attribute parsing.

use std::collections::HashMap;

use super::super::inline::run_inline_parser;
use super::Spanned;
use crate::asg::{AttributeValue, Header};
use crate::diagnostic::ParseDiagnostic;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

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
        // Need at least 4 tokens: Colon Bang Text Colon
        if i + 3 >= tokens.len() {
            return None;
        }
        let key = match &tokens[i + 2].0 {
            Token::Text(s) => *s,
            _ => return None,
        };
        if !is_valid_attribute_name(key) {
            return None;
        }
        if !matches!(tokens[i + 3].0, Token::Colon) {
            return None;
        }
        (key, true, i + 4)
    } else {
        // Regular or bang-suffix: `:key:` or `:key!:`
        let key = match &tokens[i + 1].0 {
            Token::Text(s) => *s,
            _ => return None,
        };
        if !is_valid_attribute_name(key) {
            return None;
        }

        // Check for bang-suffix: `:key!:`
        if matches!(tokens[i + 2].0, Token::Bang) {
            if i + 3 >= tokens.len() || !matches!(tokens[i + 3].0, Token::Colon) {
                return None;
            }
            (key, true, i + 4)
        } else if matches!(tokens[i + 2].0, Token::Colon) {
            (key, false, i + 3)
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
    // First, check for titled document: Eq Whitespace <content>
    if tokens.len() >= 3
        && matches!(tokens[0].0, Token::Eq)
        && matches!(tokens[1].0, Token::Whitespace)
    {
        // Find the Newline (or end-of-tokens) that terminates the title line.
        let title_start = 2;
        let mut title_end = title_start;
        while title_end < tokens.len() && !matches!(tokens[title_end].0, Token::Newline) {
            title_end += 1;
        }

        if title_start < title_end {
            let title_tokens = &tokens[title_start..title_end];
            let (title_inlines, diagnostics) = run_inline_parser(title_tokens, source, idx);

            let header_start = tokens[0].1.start;
            let mut span_end = tokens[title_end - 1].1.end;

            // Advance past the title's terminating Newline (if present).
            let mut i = if title_end < tokens.len() {
                title_end + 1
            } else {
                title_end
            };

            // Parse attribute entry lines using the shared parser.
            let mut attributes = HashMap::new();
            while let Some((entry, next)) = try_parse_attribute_entry(tokens, i, source) {
                // Update header span end to last content token on this line.
                span_end = tokens[next - 1].1.end;
                if next > 0 && matches!(tokens[next - 1].0, Token::Newline) {
                    // Span end should be the token before the newline.
                    span_end = tokens[next - 2].1.end;
                }

                match entry {
                    AttributeEntry::Set { key, value } => {
                        attributes.insert(key, value);
                    }
                    AttributeEntry::Delete { key } => {
                        attributes.remove(key);
                    }
                }
                i = next;
            }

            let header_span = SourceSpan {
                start: header_start,
                end: span_end,
            };

            let header = Header {
                title: title_inlines,
                location: Some(idx.location(&header_span)),
            };

            return HeaderResult {
                header: Some(header),
                body_start: i,
                diagnostics,
                attributes: Some(attributes),
            };
        }
    }

    // Second, check for body-only attribute entries (no title).
    if let Some((entry, mut next)) = try_parse_attribute_entry(tokens, 0, source) {
        let mut attributes = HashMap::new();
        match entry {
            AttributeEntry::Set { key, value } => {
                attributes.insert(key, value);
            }
            AttributeEntry::Delete { key } => {
                attributes.remove(key);
            }
        }

        // Continue parsing additional attribute entries.
        while let Some((e, n)) = try_parse_attribute_entry(tokens, next, source) {
            match e {
                AttributeEntry::Set { key, value } => {
                    attributes.insert(key, value);
                }
                AttributeEntry::Delete { key } => {
                    attributes.remove(key);
                }
            }
            next = n;
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

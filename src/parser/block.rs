//! Block/document parser: procedural, line-oriented block structure detection.

use std::collections::HashMap;

use super::inline::run_inline_parser;
use super::{Spanned, content_span, strip_trailing_newline_index, strip_trailing_newlines};
use crate::asg::{AttributeValue, Block, Header, InlineNode, TextNode};
use crate::diagnostic::ParseDiagnostic;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Result of header extraction from the token stream.
pub(super) struct HeaderResult<'src> {
    pub(super) header: Option<Header<'src>>,
    /// Index of the first body token (past the header and its attributes).
    pub(super) body_start: usize,
    pub(super) diagnostics: Vec<ParseDiagnostic>,
    /// Document attributes parsed from `:key: value` lines below the title.
    /// `Some` when a header is present (even if no attribute entries exist).
    pub(super) attributes: Option<HashMap<&'src str, AttributeValue<'src>>>,
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

/// Try to parse a single attribute entry at position `i`.
///
/// Patterns:
/// - `:key: value` — set attribute
/// - `:key:` — set attribute to empty string
/// - `:!key:` — delete attribute (bang prefix)
/// - `:key!:` — delete attribute (bang suffix)
///
/// Returns `Some((entry, next_index))` on success.
/// Parse a multiline attribute value with backslash continuation.
///
/// `val_start` is the index of the first value token.
/// `line_end` is the index of the Newline token (or end of tokens).
/// Returns the parsed segments and the token index to continue from.
fn parse_multiline_attribute_value<'src>(
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

    // Check if line ends with backslash (continuation).
    let has_continuation =
        line_end > val_start && line_end >= 2 && matches!(tokens[line_end - 1].0, Token::Backslash);

    if has_continuation {
        let (segments, pos) = parse_multiline_attribute_value(tokens, val_start, line_end, source);
        let value = AttributeValue::Multiline(segments);
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
pub(super) fn extract_header<'src>(
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

/// Check whether position `i` starts a thematic break (`'''`).
///
/// A thematic break is exactly 3 `SingleQuote` tokens on their own line.
/// Returns `Some(next_index)` if matched.
fn is_thematic_break(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 2 >= tokens.len() {
        return None;
    }
    // Must be exactly 3 SingleQuote tokens.
    if !matches!(tokens[i].0, Token::SingleQuote)
        || !matches!(tokens[i + 1].0, Token::SingleQuote)
        || !matches!(tokens[i + 2].0, Token::SingleQuote)
    {
        return None;
    }
    let j = i + 3;
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        Some(j + 1)
    } else {
        Some(j)
    }
}

/// Check whether position `i` starts a page break (`<<<`).
///
/// A page break is `DoubleLeftAngle` followed by `Text("<")` on its own line.
/// Returns `Some(next_index)` if matched.
fn is_page_break(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 1 >= tokens.len() {
        return None;
    }
    // Must be DoubleLeftAngle followed by Text("<").
    if !matches!(tokens[i].0, Token::DoubleLeftAngle) {
        return None;
    }
    if !matches!(&tokens[i + 1].0, Token::Text(s) if *s == "<") {
        return None;
    }
    let j = i + 2;
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        Some(j + 1)
    } else {
        Some(j)
    }
}

/// Try to parse a break block (thematic or page) at position `i`.
fn try_break<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    // Try thematic break (''')
    if let Some(next) = is_thematic_break(tokens, i) {
        let span = SourceSpan {
            start: tokens[i].1.start,
            end: tokens[i + 2].1.end,
        };
        return Some((
            Block {
                name: "break",
                form: None,
                delimiter: None,
                id: None,
                style: None,
                reftext: None,
                metadata: None,
                title: None,
                level: None,
                variant: Some("thematic"),
                marker: None,
                inlines: None,
                blocks: None,
                items: None,
                principal: None,
                location: Some(idx.location(&span)),
            },
            next,
        ));
    }

    // Try page break (<<<)
    if let Some(next) = is_page_break(tokens, i) {
        let span = SourceSpan {
            start: tokens[i].1.start,
            end: tokens[i + 1].1.end,
        };
        return Some((
            Block {
                name: "break",
                form: None,
                delimiter: None,
                id: None,
                style: None,
                reftext: None,
                metadata: None,
                title: None,
                level: None,
                variant: Some("page"),
                marker: None,
                inlines: None,
                blocks: None,
                items: None,
                principal: None,
                location: Some(idx.location(&span)),
            },
            next,
        ));
    }

    None
}

/// Check whether position `i` starts a line comment (`//`).
///
/// A line comment starts with 2 `Slash` tokens and continues to end of line.
/// Returns `Some(next_index)` pointing past the newline if matched.
fn is_line_comment(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 1 >= tokens.len() {
        return None;
    }
    // Must be exactly 2 Slash tokens (not 4+ which is block comment).
    if !matches!(tokens[i].0, Token::Slash) || !matches!(tokens[i + 1].0, Token::Slash) {
        return None;
    }
    // Check it's not a block comment (4+ slashes).
    if i + 3 < tokens.len()
        && matches!(tokens[i + 2].0, Token::Slash)
        && matches!(tokens[i + 3].0, Token::Slash)
    {
        return None;
    }
    // Skip to end of line.
    let mut j = i + 2;
    while j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        j += 1;
    }
    if j < tokens.len() {
        j += 1;
    }
    Some(j)
}

/// Check whether position `i` starts a block comment delimiter (`////`).
///
/// A block comment delimiter is 4 or more consecutive `Slash` tokens on their own line.
/// Returns `Some(next_index)` past the newline if matched.
fn is_block_comment_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 3 >= tokens.len() {
        return None;
    }
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Slash) {
        j += 1;
    }
    if j - i < 4 {
        return None;
    }
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        j += 1;
    }
    Some(j)
}

/// Try to skip a block comment at position `i`.
///
/// Block comments are delimited by `////` (4+ slashes) and are completely skipped
/// from the ASG output. Returns `Some(next_index)` past the closing delimiter.
fn try_skip_block_comment(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    let content_start = is_block_comment_delimiter(tokens, i)?;

    // Count opening slashes to match against the closing delimiter.
    let mut delim_end = i;
    while delim_end < tokens.len() && matches!(tokens[delim_end].0, Token::Slash) {
        delim_end += 1;
    }
    let open_count = delim_end - i;

    // Scan line-by-line for a matching closing delimiter.
    let mut j = content_start;
    loop {
        if j >= tokens.len() {
            return None;
        }
        if let Some(after_close) = is_block_comment_delimiter(tokens, j) {
            let mut k = j;
            while k < tokens.len() && matches!(tokens[k].0, Token::Slash) {
                k += 1;
            }
            if k - j == open_count {
                return Some(after_close);
            }
        }
        // Advance to the next line.
        while j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
            j += 1;
        }
        if j < tokens.len() {
            j += 1;
        }
    }
}

/// Result of parsing a block attribute line.
struct BlockAttributeResult {
    /// Index past the attribute line (past the Newline if present).
    next: usize,
    /// Whether this is a `[comment]` attribute.
    is_comment: bool,
}

/// Check whether position `i` starts a block attribute line.
///
/// A block attribute line is `[` ... `]` on its own line (followed by `Newline`
/// or end-of-tokens). Returns information about the attribute including whether
/// it's a comment.
fn is_block_attribute_line(tokens: &[Spanned<'_>], i: usize) -> Option<BlockAttributeResult> {
    if i >= tokens.len() || !matches!(tokens[i].0, Token::LBracket) {
        return None;
    }
    let mut j = i + 1;
    // Scan to the closing bracket.
    while j < tokens.len()
        && !matches!(tokens[j].0, Token::RBracket)
        && !matches!(tokens[j].0, Token::Newline)
    {
        j += 1;
    }
    // Must have found a closing bracket.
    if j >= tokens.len() || !matches!(tokens[j].0, Token::RBracket) {
        return None;
    }

    // Check if content is "comment".
    let is_comment = j == i + 2 && matches!(&tokens[i + 1].0, Token::Text(s) if *s == "comment");
    j += 1;
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        j += 1;
    }
    Some(BlockAttributeResult {
        next: j,
        is_comment,
    })
}

/// Result of parsing a block title line.
struct BlockTitleResult<'src> {
    /// The title inline nodes.
    inlines: Vec<InlineNode<'src>>,
    /// Index past the title line (past the Newline if present).
    next: usize,
    /// Diagnostics from parsing the title.
    diagnostics: Vec<ParseDiagnostic>,
}

/// Try to parse a block title at position `i`.
///
/// A block title is `.` followed by text on its own line. Returns the parsed
/// title inlines and the index past the title line.
fn try_block_title<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<BlockTitleResult<'src>> {
    // Must start with a Dot.
    if i >= tokens.len() || !matches!(tokens[i].0, Token::Dot) {
        return None;
    }

    // Find end of line.
    let content_start = i + 1;
    let mut line_end = content_start;
    while line_end < tokens.len() && !matches!(tokens[line_end].0, Token::Newline) {
        line_end += 1;
    }

    // Must have content after the dot.
    if content_start >= line_end {
        return None;
    }

    // Parse the title content.
    let title_tokens = &tokens[content_start..line_end];
    let (inlines, diagnostics) = run_inline_parser(title_tokens, source, idx);

    // Advance past the Newline if present.
    let next = if line_end < tokens.len() {
        line_end + 1
    } else {
        line_end
    };

    Some(BlockTitleResult {
        inlines,
        next,
        diagnostics,
    })
}

/// Skip a block following a `[comment]` attribute.
///
/// Returns the index past the skipped block content.
fn skip_comment_block<'src>(
    tokens: &[Spanned<'src>],
    start: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> usize {
    let mut i = start;

    // Skip any newlines between attribute and block.
    while i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
        i += 1;
    }

    // Skip the next block (delimited or paragraph).
    if let Some((_, next, _)) = try_open(tokens, i, source, idx) {
        next
    } else if let Some((_, next)) = try_listing(tokens, i, source, idx) {
        next
    } else if let Some((_, next, _)) = try_sidebar(tokens, i, source, idx) {
        next
    } else if let Some((_, next, _)) = try_example(tokens, i, source, idx, None) {
        next
    } else if let Some(next) = try_skip_block_comment(tokens, i) {
        next
    } else {
        // Skip paragraph content until blank line.
        find_paragraph_end(tokens, i)
    }
}

/// Build block-level ASG nodes from a body token stream.
///
/// Scans tokens linearly, detecting delimited blocks (e.g., listing) before
/// falling back to paragraph collection. Blocks are separated by blank lines
/// (two or more consecutive `Newline` tokens).
pub(super) fn build_blocks<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let mut blocks = Vec::new();
    let mut diagnostics = Vec::new();
    let mut i = 0;
    let mut pending_title: Option<Vec<InlineNode<'src>>> = None;

    while i < tokens.len() {
        // Skip inter-block newlines.
        while i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
            i += 1;
        }
        if i >= tokens.len() {
            break;
        }

        // Skip line comments (// ...).
        if let Some(next) = is_line_comment(tokens, i) {
            i = next;
            continue;
        }

        // Skip block comments (////...////).
        if let Some(next) = try_skip_block_comment(tokens, i) {
            i = next;
            continue;
        }

        // Handle block attribute lines (e.g., [abstract], [source,ruby], [comment]).
        if let Some(attr) = is_block_attribute_line(tokens, i) {
            i = attr.next;
            // If [comment], skip the following block entirely.
            if attr.is_comment {
                i = skip_comment_block(tokens, i, source, idx);
                pending_title = None;
            }
            continue;
        }

        // Try block title (.Title).
        if let Some(title_result) = try_block_title(tokens, i, source, idx) {
            pending_title = Some(title_result.inlines);
            diagnostics.extend(title_result.diagnostics);
            i = title_result.next;
            continue;
        }

        // Try break blocks (thematic or page).
        if let Some((block, next)) = try_break(tokens, i, idx) {
            blocks.push(block);
            pending_title = None;
            i = next;
            continue;
        }

        // Try delimited listing block.
        if let Some((block, next)) = try_listing(tokens, i, source, idx) {
            blocks.push(block);
            pending_title = None;
            i = next;
            continue;
        }

        // Try fenced code block (``` delimiters).
        if let Some((block, next)) = try_fenced_code(tokens, i, source, idx) {
            blocks.push(block);
            pending_title = None;
            i = next;
            continue;
        }

        // Try delimited example block.
        if let Some((block, next, diags)) =
            try_example(tokens, i, source, idx, pending_title.take())
        {
            blocks.push(block);
            diagnostics.extend(diags);
            i = next;
            continue;
        }

        // Try delimited sidebar block.
        if let Some((block, next, diags)) = try_sidebar(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            i = next;
            continue;
        }

        // Try delimited open block.
        if let Some((block, next, diags)) = try_open(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            i = next;
            continue;
        }

        // Try section heading.
        if let Some((block, next, diags)) = try_section(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            i = next;
            continue;
        }

        // Try unordered list.
        if let Some((block, next, diags)) = try_list(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            i = next;
            continue;
        }

        // Collect paragraph tokens until a blank line or delimited block.
        let para_end = find_paragraph_end(tokens, i);
        if i < para_end {
            let (block, diags) = make_paragraph(&tokens[i..para_end], source, idx);
            blocks.push(block);
            diagnostics.extend(diags);
        }
        pending_title = None;
        i = para_end;
    }

    (blocks, diagnostics)
}

/// Find the end of a paragraph starting at `start`.
///
/// Stops at a blank line (2+ consecutive `Newline` tokens), at a line that
/// starts a listing delimiter, or at the end of the token stream. Returns
/// the index of the first token past the paragraph content (trailing
/// newlines are excluded from the paragraph).
fn find_paragraph_end(tokens: &[Spanned<'_>], start: usize) -> usize {
    let mut i = start;
    while i < tokens.len() {
        if matches!(tokens[i].0, Token::Newline) {
            let nl_start = i;
            let mut nl_count = 0;
            while i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
                nl_count += 1;
                i += 1;
            }
            if nl_count >= 2 {
                // Blank line — end paragraph before the newlines.
                return nl_start;
            }
            // After a single newline, check if the next line starts a new block.
            if is_listing_delimiter(tokens, i).is_some()
                || is_fenced_code_delimiter(tokens, i).is_some()
                || is_sidebar_delimiter(tokens, i).is_some()
                || is_example_delimiter(tokens, i).is_some()
                || is_open_delimiter(tokens, i).is_some()
                || is_thematic_break(tokens, i).is_some()
                || is_page_break(tokens, i).is_some()
                || is_line_comment(tokens, i).is_some()
                || is_block_comment_delimiter(tokens, i).is_some()
                || is_list_item(tokens, i)
                || is_section_heading(tokens, i).is_some()
            {
                return nl_start;
            }
        } else {
            i += 1;
        }
    }
    // Reached end of tokens — strip trailing newlines.
    strip_trailing_newline_index(tokens, start)
}

/// Check whether position `i` starts an open block delimiter line.
///
/// An open block delimiter is exactly 2 consecutive `Hyphen` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
fn is_open_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 1 >= tokens.len() {
        return None;
    }
    // Must be exactly 2 hyphens.
    if !matches!(tokens[i].0, Token::Hyphen) || !matches!(tokens[i + 1].0, Token::Hyphen) {
        return None;
    }
    let j = i + 2;
    // Third token must NOT be a Hyphen (otherwise it's a listing delimiter).
    if j < tokens.len() && matches!(tokens[j].0, Token::Hyphen) {
        return None;
    }
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        Some(j + 1)
    } else {
        Some(j)
    }
}

/// Check whether position `i` starts a fenced code delimiter line.
///
/// A fenced code delimiter is 3 or more consecutive `Backtick` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
fn is_fenced_code_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 2 >= tokens.len() {
        return None;
    }
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Backtick) {
        j += 1;
    }
    if j - i < 3 {
        return None;
    }
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        j += 1;
    }
    Some(j)
}

/// Try to parse a fenced code block starting at position `i`.
///
/// A fenced code block uses backtick delimiters and produces a `listing` block.
/// Returns `Some((block, next_index))` if a complete block is found.
fn try_fenced_code<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    let content_start = is_fenced_code_delimiter(tokens, i)?;

    // Count opening backticks to match against the closing delimiter.
    let mut delim_end_tok = i;
    while delim_end_tok < tokens.len() && matches!(tokens[delim_end_tok].0, Token::Backtick) {
        delim_end_tok += 1;
    }
    let open_backtick_count = delim_end_tok - i;
    let delimiter = &source[tokens[i].1.start..tokens[delim_end_tok - 1].1.end];

    // Scan line-by-line for a matching closing delimiter.
    let mut j = content_start;
    loop {
        if j >= tokens.len() {
            // No closing delimiter found.
            return None;
        }
        // Check for closing delimiter at start of this line.
        if let Some(after_close) = is_fenced_code_delimiter(tokens, j) {
            let mut k = j;
            while k < tokens.len() && matches!(tokens[k].0, Token::Backtick) {
                k += 1;
            }
            if k - j == open_backtick_count {
                // Matching closing delimiter found.

                // Content tokens are content_start..before the Newline preceding
                // the closing delimiter.
                let content_end = if j > content_start && matches!(tokens[j - 1].0, Token::Newline)
                {
                    j - 1
                } else {
                    j
                };

                let (value, content_location) = if content_start < content_end {
                    let start_byte = tokens[content_start].1.start;
                    let end_byte = tokens[content_end - 1].1.end;
                    let span = SourceSpan {
                        start: start_byte,
                        end: end_byte,
                    };
                    (&source[start_byte..end_byte], Some(idx.location(&span)))
                } else {
                    ("", None)
                };

                // Block location: opening delimiter through closing delimiter.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k - 1].1.end,
                };

                let text_node = InlineNode::Text(TextNode {
                    value,
                    location: content_location,
                });

                let block = Block {
                    name: "listing",
                    form: Some("delimited"),
                    delimiter: Some(delimiter),
                    id: None,
                    style: None,
                    reftext: None,
                    metadata: None,
                    title: None,
                    level: None,
                    variant: None,
                    marker: None,
                    inlines: Some(vec![text_node]),
                    blocks: None,
                    items: None,
                    principal: None,
                    location: Some(idx.location(&block_span)),
                };

                return Some((block, after_close));
            }
        }
        // Advance to the next line.
        while j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
            j += 1;
        }
        if j < tokens.len() {
            j += 1;
        }
    }
}

/// Check whether position `i` starts a listing delimiter line.
///
/// A listing delimiter is 4 or more consecutive `Hyphen` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
fn is_listing_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 3 >= tokens.len() {
        return None;
    }
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Hyphen) {
        j += 1;
    }
    if j - i < 4 {
        return None;
    }
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        j += 1;
    }
    Some(j)
}

/// Try to parse a delimited listing block starting at position `i`.
///
/// Returns `Some((block, next_index))` if a complete listing block (opening
/// **and** matching closing delimiter) is found, or `None` otherwise.
fn try_listing<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    let content_start = is_listing_delimiter(tokens, i)?;

    // Count opening hyphens to match against the closing delimiter.
    let mut delim_end_tok = i;
    while delim_end_tok < tokens.len() && matches!(tokens[delim_end_tok].0, Token::Hyphen) {
        delim_end_tok += 1;
    }
    let open_hyphen_count = delim_end_tok - i;
    let delimiter = &source[tokens[i].1.start..tokens[delim_end_tok - 1].1.end];

    // Scan line-by-line for a matching closing delimiter.
    let mut j = content_start;
    loop {
        if j >= tokens.len() {
            // No closing delimiter found.
            return None;
        }
        // Check for closing delimiter at start of this line.
        if let Some(after_close) = is_listing_delimiter(tokens, j) {
            let mut k = j;
            while k < tokens.len() && matches!(tokens[k].0, Token::Hyphen) {
                k += 1;
            }
            if k - j == open_hyphen_count {
                // Matching closing delimiter found.

                // Content tokens are content_start..before the Newline preceding
                // the closing delimiter.
                let content_end = if j > content_start && matches!(tokens[j - 1].0, Token::Newline)
                {
                    j - 1
                } else {
                    j
                };

                let (value, content_location) = if content_start < content_end {
                    let start_byte = tokens[content_start].1.start;
                    let end_byte = tokens[content_end - 1].1.end;
                    let span = SourceSpan {
                        start: start_byte,
                        end: end_byte,
                    };
                    (&source[start_byte..end_byte], Some(idx.location(&span)))
                } else {
                    ("", None)
                };

                // Block location: opening delimiter through closing delimiter.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k - 1].1.end,
                };

                let text_node = InlineNode::Text(TextNode {
                    value,
                    location: content_location,
                });

                let block = Block {
                    name: "listing",
                    form: Some("delimited"),
                    delimiter: Some(delimiter),
                    id: None,
                    style: None,
                    reftext: None,
                    metadata: None,
                    title: None,
                    level: None,
                    variant: None,
                    marker: None,
                    inlines: Some(vec![text_node]),
                    blocks: None,
                    items: None,
                    principal: None,
                    location: Some(idx.location(&block_span)),
                };

                return Some((block, after_close));
            }
        }
        // Advance to the next line.
        while j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
            j += 1;
        }
        if j < tokens.len() {
            j += 1;
        }
    }
}

/// Check whether position `i` starts an example delimiter line.
///
/// An example delimiter is 4 or more consecutive `Eq` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
fn is_example_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 3 >= tokens.len() {
        return None;
    }
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Eq) {
        j += 1;
    }
    if j - i < 4 {
        return None;
    }
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        j += 1;
    }
    Some(j)
}

/// Try to parse a delimited example block starting at position `i`.
///
/// An example block uses `====` delimiters and has a compound content model — its
/// content is recursively parsed through [`build_blocks`]. Returns `None` if
/// no complete example block (opening **and** matching closing delimiter) is found.
fn try_example<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
    title: Option<Vec<InlineNode<'src>>>,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    let content_start = is_example_delimiter(tokens, i)?;

    // Count opening equals to match against the closing delimiter.
    let mut delim_end_tok = i;
    while delim_end_tok < tokens.len() && matches!(tokens[delim_end_tok].0, Token::Eq) {
        delim_end_tok += 1;
    }
    let open_eq_count = delim_end_tok - i;
    let delimiter = &source[tokens[i].1.start..tokens[delim_end_tok - 1].1.end];

    // Scan line-by-line for a matching closing delimiter.
    let mut j = content_start;
    loop {
        if j >= tokens.len() {
            return None;
        }
        if let Some(after_close) = is_example_delimiter(tokens, j) {
            let mut k = j;
            while k < tokens.len() && matches!(tokens[k].0, Token::Eq) {
                k += 1;
            }
            if k - j == open_eq_count {
                // Matching closing delimiter found.

                // Content tokens: exclude the Newline before the closing delimiter.
                let content_end = if j > content_start && matches!(tokens[j - 1].0, Token::Newline)
                {
                    j - 1
                } else {
                    j
                };

                // Recursively parse content as blocks.
                let content_tokens = &tokens[content_start..content_end];
                let (body_blocks, body_diags) = build_blocks(content_tokens, source, idx);

                // Block location: opening delimiter through closing delimiter.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k - 1].1.end,
                };

                return Some((
                    Block {
                        name: "example",
                        form: Some("delimited"),
                        delimiter: Some(delimiter),
                        id: None,
                        style: None,
                        reftext: None,
                        metadata: None,
                        title,
                        level: None,
                        variant: None,
                        marker: None,
                        inlines: None,
                        blocks: Some(body_blocks),
                        items: None,
                        principal: None,
                        location: Some(idx.location(&block_span)),
                    },
                    after_close,
                    body_diags,
                ));
            }
        }
        // Advance to the next line.
        while j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
            j += 1;
        }
        if j < tokens.len() {
            j += 1;
        }
    }
}

/// Check whether position `i` starts a sidebar delimiter line.
///
/// A sidebar delimiter is 4 or more consecutive `Star` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
fn is_sidebar_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 3 >= tokens.len() {
        return None;
    }
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Star) {
        j += 1;
    }
    if j - i < 4 {
        return None;
    }
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        j += 1;
    }
    Some(j)
}

/// Try to parse a delimited sidebar block starting at position `i`.
///
/// A sidebar uses `****` delimiters and has a compound content model — its
/// content is recursively parsed through [`build_blocks`]. Returns `None` if
/// no complete sidebar (opening **and** matching closing delimiter) is found.
fn try_sidebar<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    let content_start = is_sidebar_delimiter(tokens, i)?;

    // Count opening stars to match against the closing delimiter.
    let mut delim_end_tok = i;
    while delim_end_tok < tokens.len() && matches!(tokens[delim_end_tok].0, Token::Star) {
        delim_end_tok += 1;
    }
    let open_star_count = delim_end_tok - i;
    let delimiter = &source[tokens[i].1.start..tokens[delim_end_tok - 1].1.end];

    // Scan line-by-line for a matching closing delimiter.
    let mut j = content_start;
    loop {
        if j >= tokens.len() {
            return None;
        }
        if let Some(after_close) = is_sidebar_delimiter(tokens, j) {
            let mut k = j;
            while k < tokens.len() && matches!(tokens[k].0, Token::Star) {
                k += 1;
            }
            if k - j == open_star_count {
                // Matching closing delimiter found.

                // Content tokens: exclude the Newline before the closing delimiter.
                let content_end = if j > content_start && matches!(tokens[j - 1].0, Token::Newline)
                {
                    j - 1
                } else {
                    j
                };

                // Recursively parse content as blocks.
                let content_tokens = &tokens[content_start..content_end];
                let (body_blocks, body_diags) = build_blocks(content_tokens, source, idx);

                // Block location: opening delimiter through closing delimiter.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k - 1].1.end,
                };

                return Some((
                    Block {
                        name: "sidebar",
                        form: Some("delimited"),
                        delimiter: Some(delimiter),
                        id: None,
                        style: None,
                        reftext: None,
                        metadata: None,
                        title: None,
                        level: None,
                        variant: None,
                        marker: None,
                        inlines: None,
                        blocks: Some(body_blocks),
                        items: None,
                        principal: None,
                        location: Some(idx.location(&block_span)),
                    },
                    after_close,
                    body_diags,
                ));
            }
        }
        // Advance to the next line.
        while j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
            j += 1;
        }
        if j < tokens.len() {
            j += 1;
        }
    }
}

/// Try to parse a delimited open block starting at position `i`.
///
/// An open block uses `--` delimiters and has a compound content model — its
/// content is recursively parsed through [`build_blocks`]. Returns `None` if
/// no complete open block (opening **and** matching closing delimiter) is found.
fn try_open<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    let content_start = is_open_delimiter(tokens, i)?;

    // The delimiter is always exactly "--".
    let delimiter = &source[tokens[i].1.start..tokens[i + 1].1.end];

    // Scan line-by-line for the closing delimiter.
    let mut j = content_start;
    loop {
        if j >= tokens.len() {
            return None;
        }
        if let Some(after_close) = is_open_delimiter(tokens, j) {
            // Found closing delimiter.

            // Content tokens: exclude the Newline before the closing delimiter.
            let content_end = if j > content_start && matches!(tokens[j - 1].0, Token::Newline) {
                j - 1
            } else {
                j
            };

            // Recursively parse content as blocks.
            let content_tokens = &tokens[content_start..content_end];
            let (body_blocks, body_diags) = build_blocks(content_tokens, source, idx);

            // Block location: opening delimiter through closing delimiter.
            let block_span = SourceSpan {
                start: tokens[i].1.start,
                end: tokens[j + 1].1.end,
            };

            return Some((
                Block {
                    name: "open",
                    form: Some("delimited"),
                    delimiter: Some(delimiter),
                    id: None,
                    style: None,
                    reftext: None,
                    metadata: None,
                    title: None,
                    level: None,
                    variant: None,
                    marker: None,
                    inlines: None,
                    blocks: Some(body_blocks),
                    items: None,
                    principal: None,
                    location: Some(idx.location(&block_span)),
                },
                after_close,
                body_diags,
            ));
        }
        // Advance to the next line.
        while j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
            j += 1;
        }
        if j < tokens.len() {
            j += 1;
        }
    }
}

/// Check whether position `i` starts a section heading (2+ `Eq` followed by
/// `Whitespace`). Returns `(level, title_start_index)` where level is the
/// number of `Eq` tokens minus one.
fn is_section_heading(tokens: &[Spanned<'_>], i: usize) -> Option<(usize, usize)> {
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Eq) {
        j += 1;
    }
    let eq_count = j - i;
    if eq_count < 2 {
        return None;
    }
    if j >= tokens.len() || !matches!(tokens[j].0, Token::Whitespace) {
        return None;
    }
    Some((eq_count - 1, j + 1))
}

/// Try to parse a section starting at position `i`.
///
/// A section heading is 2+ `Eq` tokens followed by `Whitespace` and a title.
/// The section's body is all subsequent content (processed recursively through
/// [`build_blocks`]).
fn try_section<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    let (level, title_start) = is_section_heading(tokens, i)?;

    // Find end of title line.
    let mut title_end = title_start;
    while title_end < tokens.len() && !matches!(tokens[title_end].0, Token::Newline) {
        title_end += 1;
    }

    if title_start >= title_end {
        return None;
    }

    // Parse title through inline parser.
    let title_tokens = &tokens[title_start..title_end];
    let (title_inlines, mut diagnostics) = run_inline_parser(title_tokens, source, idx);

    // Body starts after the title's Newline.
    let body_start = if title_end < tokens.len() {
        title_end + 1
    } else {
        title_end
    };

    // The section consumes all remaining tokens.
    // TODO: Handle section nesting (stop at a heading of equal or higher level).
    let section_end = tokens.len();

    // Build body blocks.
    let body_tokens = &tokens[body_start..section_end];
    let (body_blocks, body_diags) = build_blocks(body_tokens, source, idx);
    diagnostics.extend(body_diags);

    // Section location: from first Eq to last content token.
    let section_tokens = &tokens[i..section_end];
    let section_span = content_span(section_tokens);
    let section_location = section_span.map(|s| idx.location(&s));

    Some((
        Block {
            name: "section",
            form: None,
            delimiter: None,
            id: None,
            style: None,
            reftext: None,
            metadata: None,
            title: Some(title_inlines),
            level: Some(level),
            variant: None,
            marker: None,
            inlines: None,
            blocks: Some(body_blocks),
            items: None,
            principal: None,
            location: section_location,
        },
        section_end,
        diagnostics,
    ))
}

/// Check whether position `i` starts an unordered list item (`Star Whitespace`).
fn is_list_item(tokens: &[Spanned<'_>], i: usize) -> bool {
    i + 1 < tokens.len()
        && matches!(tokens[i].0, Token::Star)
        && matches!(tokens[i + 1].0, Token::Whitespace)
}

/// Try to parse an unordered list starting at position `i`.
///
/// Collects consecutive list items (each starting with `Star Whitespace`) into
/// a single `list` block. Returns `None` if position `i` does not start a list
/// item.
fn try_list<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    if !is_list_item(tokens, i) {
        return None;
    }

    let mut items = Vec::new();
    let mut diagnostics = Vec::new();
    let mut j = i;
    let list_span_start = tokens[i].1.start;
    let mut list_span_end = tokens[i].1.end;

    while is_list_item(tokens, j) {
        let marker = &source[tokens[j].1.start..tokens[j].1.end];
        let item_start = tokens[j].1.start;

        // Content starts after Star Whitespace.
        let content_start = j + 2;
        let mut content_end = content_start;
        while content_end < tokens.len() && !matches!(tokens[content_end].0, Token::Newline) {
            content_end += 1;
        }

        // Parse principal content through the inline parser.
        let content_tokens = &tokens[content_start..content_end];
        let (principal, diags) = run_inline_parser(content_tokens, source, idx);
        diagnostics.extend(diags);

        // Item location: from the marker to the last content token.
        let item_end = if content_end > content_start {
            tokens[content_end - 1].1.end
        } else {
            tokens[j + 1].1.end
        };
        let item_span = SourceSpan {
            start: item_start,
            end: item_end,
        };

        items.push(Block {
            name: "listItem",
            form: None,
            delimiter: None,
            id: None,
            style: None,
            reftext: None,
            metadata: None,
            title: None,
            level: None,
            variant: None,
            marker: Some(marker),
            inlines: None,
            blocks: None,
            items: None,
            principal: Some(principal),
            location: Some(idx.location(&item_span)),
        });

        list_span_end = item_end;

        // Advance past the Newline (if present).
        j = if content_end < tokens.len() {
            content_end + 1
        } else {
            content_end
        };
    }

    let list_span = SourceSpan {
        start: list_span_start,
        end: list_span_end,
    };
    let marker = items[0].marker;

    Some((
        Block {
            name: "list",
            form: None,
            delimiter: None,
            id: None,
            style: None,
            reftext: None,
            metadata: None,
            title: None,
            level: None,
            variant: Some("unordered"),
            marker,
            inlines: None,
            blocks: None,
            items: Some(items),
            principal: None,
            location: Some(idx.location(&list_span)),
        },
        j,
        diagnostics,
    ))
}

/// Build a paragraph `Block` from its content tokens.
fn make_paragraph<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Block<'src>, Vec<ParseDiagnostic>) {
    let trimmed = strip_trailing_newlines(tokens);
    let (inlines, diagnostics) = run_inline_parser(trimmed, source, idx);

    let span = content_span(trimmed);
    let location = span.map(|s| idx.location(&s));

    (
        Block {
            name: "paragraph",
            form: None,
            delimiter: None,
            id: None,
            style: None,
            reftext: None,
            metadata: None,
            title: None,
            level: None,
            variant: None,
            marker: None,
            inlines: Some(inlines),
            blocks: None,
            items: None,
            principal: None,
            location,
        },
        diagnostics,
    )
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::asg::{InlineNode, Position};
    use crate::parser::parse_doc;

    #[test]
    fn doc_empty() {
        let (doc, diags) = parse_doc("");
        assert!(diags.is_empty());
        assert!(doc.blocks.is_empty());
        assert!(doc.header.is_none());
        assert!(doc.location.is_none());
    }

    #[test]
    fn doc_single_paragraph() {
        let (doc, diags) = parse_doc("hello world");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        assert_eq!(doc.blocks[0].name, "paragraph");
        let inlines = doc.blocks[0].inlines.as_ref().unwrap();
        assert_eq!(inlines.len(), 1);
        match &inlines[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "hello world"),
            _ => panic!("expected Text"),
        }
    }

    #[test]
    fn doc_sibling_paragraphs() {
        let (doc, diags) = parse_doc("para1\n\npara2");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 2);
        assert_eq!(doc.blocks[0].name, "paragraph");
        assert_eq!(doc.blocks[1].name, "paragraph");
    }

    #[test]
    fn doc_header_body() {
        let (doc, diags) = parse_doc("= Title\n\nbody");
        assert!(diags.is_empty());
        assert!(doc.header.is_some());
        let header = doc.header.as_ref().unwrap();
        assert_eq!(header.title.len(), 1);
        match &header.title[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "Title"),
            _ => panic!("expected Text"),
        }
        assert_eq!(doc.blocks.len(), 1);
        assert!(doc.attributes.is_some());
    }

    #[test]
    fn doc_body_only_no_attributes() {
        let (doc, diags) = parse_doc("just text");
        assert!(diags.is_empty());
        assert!(doc.attributes.is_none());
        assert!(doc.header.is_none());
    }

    #[test]
    fn doc_multiple_blank_lines() {
        let (doc, diags) = parse_doc("a\n\n\nb");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 2);
    }

    #[test]
    fn doc_listing_block() {
        let (doc, diags) = parse_doc("----\ndef main\n  puts 'hello'\nend\n----");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let block = &doc.blocks[0];
        assert_eq!(block.name, "listing");
        assert_eq!(block.form, Some("delimited"));
        assert_eq!(block.delimiter, Some("----"));
        let inlines = block.inlines.as_ref().unwrap();
        assert_eq!(inlines.len(), 1);
        match &inlines[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "def main\n  puts 'hello'\nend");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 2, col: 1 });
                assert_eq!(loc[1], Position { line: 4, col: 3 });
            }
            _ => panic!("expected Text node"),
        }
        let loc = block.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 5, col: 4 });
    }

    #[test]
    fn doc_unordered_list_single_item() {
        let (doc, diags) = parse_doc("* water");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let list = &doc.blocks[0];
        assert_eq!(list.name, "list");
        assert_eq!(list.variant, Some("unordered"));
        assert_eq!(list.marker, Some("*"));
        let items = list.items.as_ref().unwrap();
        assert_eq!(items.len(), 1);
        let item = &items[0];
        assert_eq!(item.name, "listItem");
        assert_eq!(item.marker, Some("*"));
        let principal = item.principal.as_ref().unwrap();
        assert_eq!(principal.len(), 1);
        match &principal[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "water");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 1, col: 3 });
                assert_eq!(loc[1], Position { line: 1, col: 7 });
            }
            _ => panic!("expected Text node"),
        }
        let loc = list.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 1, col: 7 });
    }

    #[test]
    fn doc_section_with_body() {
        let (doc, diags) = parse_doc("== Section Title\n\nparagraph");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let section = &doc.blocks[0];
        assert_eq!(section.name, "section");
        assert_eq!(section.level, Some(1));
        let title = section.title.as_ref().unwrap();
        assert_eq!(title.len(), 1);
        match &title[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "Section Title");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 1, col: 4 });
                assert_eq!(loc[1], Position { line: 1, col: 16 });
            }
            _ => panic!("expected Text node"),
        }
        let blocks = section.blocks.as_ref().unwrap();
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
        let loc = section.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 3, col: 9 });
    }

    #[test]
    fn doc_sidebar_with_list() {
        let (doc, diags) = parse_doc("****\n* phone\n* wallet\n* keys\n****");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let sidebar = &doc.blocks[0];
        assert_eq!(sidebar.name, "sidebar");
        assert_eq!(sidebar.form, Some("delimited"));
        assert_eq!(sidebar.delimiter, Some("****"));
        let blocks = sidebar.blocks.as_ref().unwrap();
        assert_eq!(blocks.len(), 1);
        let list = &blocks[0];
        assert_eq!(list.name, "list");
        assert_eq!(list.variant, Some("unordered"));
        let items = list.items.as_ref().unwrap();
        assert_eq!(items.len(), 3);
        assert_eq!(items[0].principal.as_ref().unwrap().len(), 1);
        let loc = sidebar.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 5, col: 4 });
    }

    #[test]
    fn doc_open_block() {
        let (doc, diags) = parse_doc("--\nhello\n--");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let open = &doc.blocks[0];
        assert_eq!(open.name, "open");
        assert_eq!(open.form, Some("delimited"));
        assert_eq!(open.delimiter, Some("--"));
        let blocks = open.blocks.as_ref().unwrap();
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
    }

    #[test]
    fn doc_body_only_attributes() {
        let (doc, diags) = parse_doc(":frog: Tanglefoot");
        assert!(diags.is_empty());
        assert!(doc.header.is_none());
        assert!(doc.blocks.is_empty());
        let attrs = doc.attributes.as_ref().expect("should have attributes");
        assert_eq!(
            attrs.get("frog"),
            Some(&crate::asg::AttributeValue::Single("Tanglefoot"))
        );
        let loc = doc.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 1, col: 17 });
    }

    #[test]
    fn doc_attribute_delete_bang_prefix() {
        let (doc, diags) = parse_doc(":frog: Tanglefoot\n:!frog:");
        assert!(diags.is_empty());
        assert!(doc.header.is_none());
        assert!(doc.blocks.is_empty());
        // Attribute was set then deleted, so no attributes remain.
        assert!(doc.attributes.is_none());
    }

    #[test]
    fn doc_attribute_delete_bang_suffix() {
        let (doc, diags) = parse_doc(":frog: Tanglefoot\n:frog!:");
        assert!(diags.is_empty());
        assert!(doc.header.is_none());
        assert!(doc.blocks.is_empty());
        // Attribute was set then deleted, so no attributes remain.
        assert!(doc.attributes.is_none());
    }

    #[test]
    fn doc_invalid_attr_name_becomes_paragraph() {
        // `:foo:bar: baz` has a colon in the name position - should be a paragraph
        let (doc, diags) = parse_doc(":foo:bar: baz");
        assert!(diags.is_empty());
        assert!(doc.attributes.is_none());
        assert_eq!(doc.blocks.len(), 1);
        assert_eq!(doc.blocks[0].name, "paragraph");
    }

    #[test]
    fn doc_thematic_break() {
        let (doc, diags) = parse_doc("'''");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let br = &doc.blocks[0];
        assert_eq!(br.name, "break");
        assert_eq!(br.variant, Some("thematic"));
        let loc = br.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 1, col: 3 });
    }

    #[test]
    fn doc_page_break() {
        let (doc, diags) = parse_doc("page 1\n\n<<<\n\npage 2");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 3);
        assert_eq!(doc.blocks[0].name, "paragraph");
        assert_eq!(doc.blocks[1].name, "break");
        assert_eq!(doc.blocks[1].variant, Some("page"));
        assert_eq!(doc.blocks[2].name, "paragraph");
    }

    #[test]
    fn doc_line_comment_skipped() {
        let (doc, diags) = parse_doc("first\n\n// comment\n\nsecond");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 2);
        assert_eq!(doc.blocks[0].name, "paragraph");
        assert_eq!(doc.blocks[1].name, "paragraph");
    }

    #[test]
    fn doc_block_comment_skipped() {
        let (doc, diags) = parse_doc("first\n\n////\ncomment\n////\n\nsecond");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 2);
        assert_eq!(doc.blocks[0].name, "paragraph");
        assert_eq!(doc.blocks[1].name, "paragraph");
    }

    #[test]
    fn doc_comment_style_paragraph_skipped() {
        let (doc, diags) = parse_doc("[comment]\nskip this\n\nkeep this");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        assert_eq!(doc.blocks[0].name, "paragraph");
    }

    #[test]
    fn doc_example_block() {
        let (doc, diags) = parse_doc("====\nThis is an example.\n====");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let example = &doc.blocks[0];
        assert_eq!(example.name, "example");
        assert_eq!(example.form, Some("delimited"));
        assert_eq!(example.delimiter, Some("===="));
        let blocks = example.blocks.as_ref().unwrap();
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
        let loc = example.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 3, col: 4 });
    }

    #[test]
    fn doc_example_block_with_title() {
        let (doc, diags) = parse_doc(".My Example\n====\ncontent\n====");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let example = &doc.blocks[0];
        assert_eq!(example.name, "example");
        let title = example.title.as_ref().unwrap();
        assert_eq!(title.len(), 1);
        match &title[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "My Example"),
            _ => panic!("expected Text"),
        }
    }

    #[test]
    fn doc_fenced_code_block() {
        let (doc, diags) = parse_doc("```\nputs 'hello'\n```");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let listing = &doc.blocks[0];
        assert_eq!(listing.name, "listing");
        assert_eq!(listing.form, Some("delimited"));
        assert_eq!(listing.delimiter, Some("```"));
        let inlines = listing.inlines.as_ref().unwrap();
        assert_eq!(inlines.len(), 1);
        match &inlines[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "puts 'hello'"),
            _ => panic!("expected Text node"),
        }
        let loc = listing.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 3, col: 3 });
    }
}

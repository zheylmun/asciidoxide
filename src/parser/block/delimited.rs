//! Delimited block parsing (listing, literal, example, sidebar, open, fenced code).

use super::{Spanned, build_blocks};
use crate::asg::{Block, InlineNode, TextNode};
use crate::diagnostic::ParseDiagnostic;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

// ---------------------------------------------------------------------------
// Open blocks
// ---------------------------------------------------------------------------

/// Check whether position `i` starts an open block delimiter line.
///
/// An open block delimiter is exactly 2 consecutive `Hyphen` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
pub(super) fn is_open_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
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

/// Try to parse a delimited open block starting at position `i`.
///
/// An open block uses `--` delimiters and has a compound content model — its
/// content is recursively parsed through [`build_blocks`]. Returns `None` if
/// no complete open block (opening **and** matching closing delimiter) is found.
pub(super) fn try_open<'src>(
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

// ---------------------------------------------------------------------------
// Fenced code blocks
// ---------------------------------------------------------------------------

/// Check whether position `i` starts a fenced code delimiter line.
///
/// A fenced code delimiter is 3 or more consecutive `Backtick` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
pub(super) fn is_fenced_code_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
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
pub(super) fn try_fenced_code<'src>(
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

                // Build inlines: only create a text node if there's actual content.
                let inlines = if content_start < content_end {
                    let start_byte = tokens[content_start].1.start;
                    let end_byte = tokens[content_end - 1].1.end;
                    let span = SourceSpan {
                        start: start_byte,
                        end: end_byte,
                    };
                    vec![InlineNode::Text(TextNode {
                        value: &source[start_byte..end_byte],
                        location: Some(idx.location(&span)),
                    })]
                } else {
                    vec![]
                };

                // Block location: opening delimiter through closing delimiter.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k - 1].1.end,
                };

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
                    inlines: Some(inlines),
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

// ---------------------------------------------------------------------------
// Listing blocks
// ---------------------------------------------------------------------------

/// Check whether position `i` starts a listing delimiter line.
///
/// A listing delimiter is 4 or more consecutive `Hyphen` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
pub(super) fn is_listing_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
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
pub(super) fn try_listing<'src>(
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

                // Build inlines: only create a text node if there's actual content.
                let inlines = if content_start < content_end {
                    let start_byte = tokens[content_start].1.start;
                    let end_byte = tokens[content_end - 1].1.end;
                    let span = SourceSpan {
                        start: start_byte,
                        end: end_byte,
                    };
                    vec![InlineNode::Text(TextNode {
                        value: &source[start_byte..end_byte],
                        location: Some(idx.location(&span)),
                    })]
                } else {
                    vec![]
                };

                // Block location: opening delimiter through closing delimiter.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k - 1].1.end,
                };

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
                    inlines: Some(inlines),
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

// ---------------------------------------------------------------------------
// Literal blocks
// ---------------------------------------------------------------------------

/// Check whether position `i` starts a literal delimiter line.
///
/// A literal delimiter is 4 or more consecutive `Dot` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
pub(super) fn is_literal_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 3 >= tokens.len() {
        return None;
    }
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Dot) {
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

/// Try to parse a delimited literal block starting at position `i`.
///
/// Returns `Some((block, next_index))` if a complete literal block (opening
/// **and** matching closing delimiter) is found, or `None` otherwise.
pub(super) fn try_literal<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    let content_start = is_literal_delimiter(tokens, i)?;

    // Count opening dots to match against the closing delimiter.
    let mut delim_end_tok = i;
    while delim_end_tok < tokens.len() && matches!(tokens[delim_end_tok].0, Token::Dot) {
        delim_end_tok += 1;
    }
    let open_dot_count = delim_end_tok - i;
    let delimiter = &source[tokens[i].1.start..tokens[delim_end_tok - 1].1.end];

    // Scan line-by-line for a matching closing delimiter.
    let mut j = content_start;
    loop {
        if j >= tokens.len() {
            // No closing delimiter found.
            return None;
        }
        // Check for closing delimiter at start of this line.
        if let Some(after_close) = is_literal_delimiter(tokens, j) {
            let mut k = j;
            while k < tokens.len() && matches!(tokens[k].0, Token::Dot) {
                k += 1;
            }
            if k - j == open_dot_count {
                // Matching closing delimiter found.

                // Content tokens are content_start..before the Newline preceding
                // the closing delimiter.
                let content_end = if j > content_start && matches!(tokens[j - 1].0, Token::Newline)
                {
                    j - 1
                } else {
                    j
                };

                // Build inlines: only create a text node if there's actual content.
                let inlines = if content_start < content_end {
                    let start_byte = tokens[content_start].1.start;
                    let end_byte = tokens[content_end - 1].1.end;
                    let span = SourceSpan {
                        start: start_byte,
                        end: end_byte,
                    };
                    vec![InlineNode::Text(TextNode {
                        value: &source[start_byte..end_byte],
                        location: Some(idx.location(&span)),
                    })]
                } else {
                    vec![]
                };

                // Block location: opening delimiter through closing delimiter.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k - 1].1.end,
                };

                let block = Block {
                    name: "literal",
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
                    inlines: Some(inlines),
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

// ---------------------------------------------------------------------------
// Example blocks
// ---------------------------------------------------------------------------

/// Check whether position `i` starts an example delimiter line.
///
/// An example delimiter is 4 or more consecutive `Eq` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
pub(super) fn is_example_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
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
pub(super) fn try_example<'src>(
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

// ---------------------------------------------------------------------------
// Sidebar blocks
// ---------------------------------------------------------------------------

/// Check whether position `i` starts a sidebar delimiter line.
///
/// A sidebar delimiter is 4 or more consecutive `Star` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
pub(super) fn is_sidebar_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
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
pub(super) fn try_sidebar<'src>(
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

// ---------------------------------------------------------------------------
// Passthrough blocks
// ---------------------------------------------------------------------------

/// Check whether position `i` starts a passthrough delimiter line.
///
/// A passthrough delimiter is 4 or more consecutive `Plus` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
pub(super) fn is_passthrough_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 3 >= tokens.len() {
        return None;
    }
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Plus) {
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

/// Try to parse a delimited passthrough block starting at position `i`.
///
/// A passthrough block uses `++++` delimiters and has a verbatim content model.
/// Returns `Some((block, next_index))` if a complete passthrough block (opening
/// **and** matching closing delimiter) is found, or `None` otherwise.
pub(super) fn try_passthrough<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    let content_start = is_passthrough_delimiter(tokens, i)?;

    // Count opening plus signs to match against the closing delimiter.
    let mut delim_end_tok = i;
    while delim_end_tok < tokens.len() && matches!(tokens[delim_end_tok].0, Token::Plus) {
        delim_end_tok += 1;
    }
    let open_plus_count = delim_end_tok - i;
    let delimiter = &source[tokens[i].1.start..tokens[delim_end_tok - 1].1.end];

    // Scan line-by-line for a matching closing delimiter.
    let mut j = content_start;
    loop {
        if j >= tokens.len() {
            // No closing delimiter found.
            return None;
        }
        // Check for closing delimiter at start of this line.
        if let Some(after_close) = is_passthrough_delimiter(tokens, j) {
            let mut k = j;
            while k < tokens.len() && matches!(tokens[k].0, Token::Plus) {
                k += 1;
            }
            if k - j == open_plus_count {
                // Matching closing delimiter found.

                // Content tokens are content_start..before the Newline preceding
                // the closing delimiter.
                let content_end = if j > content_start && matches!(tokens[j - 1].0, Token::Newline)
                {
                    j - 1
                } else {
                    j
                };

                // Build inlines: only create a text node if there's actual content.
                let inlines = if content_start < content_end {
                    let start_byte = tokens[content_start].1.start;
                    let end_byte = tokens[content_end - 1].1.end;
                    let span = SourceSpan {
                        start: start_byte,
                        end: end_byte,
                    };
                    vec![InlineNode::Text(TextNode {
                        value: &source[start_byte..end_byte],
                        location: Some(idx.location(&span)),
                    })]
                } else {
                    vec![]
                };

                // Block location: opening delimiter through closing delimiter.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k - 1].1.end,
                };

                let block = Block {
                    name: "pass",
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
                    inlines: Some(inlines),
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

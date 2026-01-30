//! Comment detection (line and block comments).

use super::Spanned;
use crate::token::Token;

/// Check whether position `i` starts a line comment (`//`).
///
/// A line comment starts with 2 `Slash` tokens and continues to end of line.
/// Returns `Some(next_index)` pointing past the newline if matched.
pub(super) fn is_line_comment(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
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
pub(super) fn is_block_comment_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
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
pub(super) fn try_skip_block_comment(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
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

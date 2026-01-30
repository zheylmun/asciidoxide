//! Block metadata parsing (attributes, titles).

use super::delimited::{try_example, try_listing, try_open, try_sidebar};
use super::paragraphs::find_paragraph_end;
use super::{comments::try_skip_block_comment, Spanned};
use crate::parser::inline::run_inline_parser;
use crate::asg::InlineNode;
use crate::diagnostic::ParseDiagnostic;
use crate::span::SourceIndex;
use crate::token::Token;

/// Result of parsing a block attribute line.
pub(super) struct BlockAttributeResult {
    /// Index past the attribute line (past the Newline if present).
    pub(super) next: usize,
    /// Whether this is a `[comment]` attribute.
    pub(super) is_comment: bool,
}

/// Check whether position `i` starts a block attribute line.
///
/// A block attribute line is `[` ... `]` on its own line (followed by `Newline`
/// or end-of-tokens). Returns information about the attribute including whether
/// it's a comment.
pub(super) fn is_block_attribute_line(
    tokens: &[Spanned<'_>],
    i: usize,
) -> Option<BlockAttributeResult> {
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
pub(super) struct BlockTitleResult<'src> {
    /// The title inline nodes.
    pub(super) inlines: Vec<InlineNode<'src>>,
    /// Index past the title line (past the Newline if present).
    pub(super) next: usize,
    /// Diagnostics from parsing the title.
    pub(super) diagnostics: Vec<ParseDiagnostic>,
}

/// Try to parse a block title at position `i`.
///
/// A block title is `.` followed by text on its own line. Returns the parsed
/// title inlines and the index past the title line.
pub(super) fn try_block_title<'src>(
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
pub(super) fn skip_comment_block<'src>(
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

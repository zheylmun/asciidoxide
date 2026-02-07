//! Utility parsers and helper functions for block boundary parsing.

use chumsky::{input::ValueInput, prelude::*};

use crate::span::SourceSpan;
use crate::token::Token;

/// Parser extra type (just errors, no state).
pub(super) type BlockExtra<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>;

/// Match end of line (Newline or end of input).
pub(super) fn line_end<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, (), BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::Newline).ignored().or(end())
}

/// Match one or more newlines (blank lines between blocks).
pub(super) fn blank_lines<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, (), BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::Newline).repeated().at_least(1).ignored()
}

/// Consume tokens until end of line, return the content span.
pub(super) fn rest_of_line<'tokens, 'src: 'tokens, I>()
-> impl Parser<'tokens, I, SourceSpan, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    any()
        .filter(|t| !matches!(t, Token::Newline))
        .repeated()
        .map_with(|(), e| e.span())
        .then_ignore(line_end())
}

/// Result of extracting an embedded anchor from a title.
pub(super) struct EmbeddedAnchor<'a> {
    /// The ID extracted from `[[id]]` or `[[id,reftext]]`.
    pub id: &'a str,
    /// Byte offset of the reftext within the original title string, if present.
    pub reftext_offset: Option<(usize, usize)>,
    /// Byte offset where the title content ends (before the anchor).
    pub title_end_offset: usize,
}

/// Extract an embedded anchor (`[[id]]` or `[[id,reftext]]`) from the end of a title.
///
/// Returns `None` if no valid anchor is found.
pub(super) fn extract_embedded_anchor(
    title: &str,
    title_end_offset: usize,
) -> Option<EmbeddedAnchor<'_>> {
    let segment = title[..title_end_offset].trim_end();
    let anchor_start = segment.rfind("[[")?;
    let anchor_end = segment[anchor_start..].find("]]")?;

    let anchor_inner = &segment[anchor_start + 2..anchor_start + anchor_end];
    let (id_part, reftext_offset) = if let Some(comma) = anchor_inner.find(',') {
        let reftext_start = anchor_start + 2 + comma + 1;
        let reftext_len = anchor_inner.len() - comma - 1;
        (
            &anchor_inner[..comma],
            Some((reftext_start, reftext_start + reftext_len)),
        )
    } else {
        (anchor_inner, None)
    };

    if id_part.is_empty() || !is_valid_anchor_id(id_part) {
        return None;
    }

    let before_anchor = segment[..anchor_start].trim_end();
    Some(EmbeddedAnchor {
        id: &title[anchor_start + 2..anchor_start + 2 + id_part.len()],
        reftext_offset,
        title_end_offset: before_anchor.len(),
    })
}

/// Strip trailing symmetric heading marker (e.g., ` ==` for level 1).
pub(super) fn strip_trailing_eq_marker(title: &str, eq_count: usize) -> usize {
    let trimmed = title.trim_end();
    let bytes = trimmed.as_bytes();
    let marker_len = 1 + eq_count; // space + eq_count '=' chars

    if bytes.len() >= marker_len {
        let marker_start = bytes.len() - marker_len;
        // Check for space followed by eq_count '=' characters
        if bytes[marker_start] == b' ' && bytes[marker_start + 1..].iter().all(|&b| b == b'=') {
            return marker_start;
        }
    }
    title.len()
}

/// Strip trailing symmetric hash marker (e.g., ` ##` for level 1).
#[allow(dead_code)] // Will be used when refactoring markdown_heading
pub(super) fn strip_trailing_hash_marker(title: &str, hash_count: usize) -> usize {
    let trimmed = title.trim_end();
    let bytes = trimmed.as_bytes();
    let marker_len = 1 + hash_count; // space + hash_count '#' chars

    if bytes.len() >= marker_len {
        let marker_start = bytes.len() - marker_len;
        // Check for space followed by hash_count '#' characters
        if bytes[marker_start] == b' ' && bytes[marker_start + 1..].iter().all(|&b| b == b'#') {
            return marker_start;
        }
    }
    title.len()
}

/// Trim trailing newlines from a body content span.
pub(super) fn trim_trailing_newlines(source: &str, start: usize, end: usize) -> usize {
    let mut actual_end = end;
    while actual_end > start && source.as_bytes().get(actual_end - 1) == Some(&b'\n') {
        actual_end -= 1;
    }
    actual_end
}

/// Check if a string is a valid anchor ID.
///
/// Valid IDs contain only alphanumeric characters, hyphens, underscores, and periods.
pub(super) fn is_valid_anchor_id(id: &str) -> bool {
    !id.is_empty()
        && id
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_' || c == '.')
}

/// Parameters for building a section block.
pub(super) struct SectionParams<'src> {
    pub level: usize,
    pub title_span: SourceSpan,
    pub heading_line_span: SourceSpan,
    pub embedded_id: Option<&'src str>,
    pub reftext_span: Option<SourceSpan>,
    pub body_start: usize,
    pub body_end: usize,
}

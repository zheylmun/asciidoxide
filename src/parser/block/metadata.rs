//! Block metadata parsing (attributes, titles).

use std::collections::HashMap;

use super::delimited::{try_example, try_listing, try_open, try_sidebar};
use super::paragraphs::find_paragraph_end;
use super::{Spanned, comments::try_skip_block_comment};
use crate::asg::{BlockMetadata, InlineNode};
use crate::diagnostic::ParseDiagnostic;
use crate::parser::inline::run_inline_parser;
use crate::span::SourceIndex;
use crate::token::Token;

/// Parsed block attributes from a `[...]` attribute line.
#[derive(Debug, Clone, Default)]
pub(super) struct BlockAttrs<'src> {
    /// Block style (first positional argument, e.g., `quote`, `source`, `discrete`).
    pub(super) style: Option<&'src str>,
    /// Block ID (from `#id` shorthand).
    pub(super) id: Option<&'src str>,
    /// Role classes (from `.role` shorthand).
    pub(super) roles: Vec<&'src str>,
    /// Options (from `%option` shorthand).
    pub(super) options: Vec<&'src str>,
    /// Named attributes (from `name=value`).
    pub(super) attributes: HashMap<&'src str, &'src str>,
    /// Positional arguments (after the style, used for attribution, citetitle, etc.).
    pub(super) positional: Vec<&'src str>,
}

impl<'src> BlockAttrs<'src> {
    /// Returns `true` if this attribute set indicates a comment block.
    pub(super) fn is_comment(&self) -> bool {
        self.style == Some("comment")
    }

    /// Returns `true` if this attribute set has any meaningful content.
    #[allow(dead_code)]
    pub(super) fn is_empty(&self) -> bool {
        self.style.is_none()
            && self.id.is_none()
            && self.roles.is_empty()
            && self.options.is_empty()
            && self.attributes.is_empty()
            && self.positional.is_empty()
    }

    /// Convert to ASG `BlockMetadata`, returning `None` if empty.
    #[allow(dead_code)]
    pub(super) fn to_metadata(&self) -> Option<BlockMetadata<'src>> {
        if self.roles.is_empty() && self.options.is_empty() && self.attributes.is_empty() {
            return None;
        }
        Some(BlockMetadata {
            roles: self.roles.clone(),
            options: self.options.clone(),
            attributes: self.attributes.clone(),
        })
    }
}

/// Result of parsing a block attribute line.
pub(super) struct BlockAttributeResult<'src> {
    /// Index past the attribute line (past the Newline if present).
    pub(super) next: usize,
    /// Parsed block attributes.
    pub(super) attrs: BlockAttrs<'src>,
}

/// Parse the content of a block attribute list (between `[` and `]`).
///
/// Handles:
/// - Style (first positional): `[quote]`, `[source]`, `[discrete]`
/// - ID shorthand: `[#myid]`
/// - Role shorthand: `[.role1.role2]`
/// - Option shorthand: `[%option]`
/// - Named attributes: `[name=value]`
/// - Positional arguments: `[quote, author, source]`
/// - Combined: `[quote#id.role, author]`
fn parse_block_attr_content<'src>(
    tokens: &[Spanned<'src>],
    start: usize,
    end: usize,
    source: &'src str,
) -> BlockAttrs<'src> {
    let mut attrs = BlockAttrs::default();

    // Empty attribute list.
    if start >= end {
        return attrs;
    }

    // Collect all content between brackets as a string for parsing.
    let content_start = tokens[start].1.start;
    let content_end = tokens[end - 1].1.end;
    let content = &source[content_start..content_end];

    // Parse comma-separated arguments.
    let mut first_positional = true;
    for part in content.split(',') {
        let part = part.trim();
        if part.is_empty() {
            continue;
        }

        // Parse the first positional argument specially (may contain style#id.role%opt).
        if first_positional {
            first_positional = false;
            parse_first_positional(part, &mut attrs);
        } else if let Some((name, value)) = part.split_once('=') {
            // Named attribute: name=value
            let name = name.trim();
            let value = value.trim().trim_matches('"');
            attrs.attributes.insert(name, value);
        } else {
            // Additional positional argument.
            attrs.positional.push(part);
        }
    }

    attrs
}

/// Parse the first positional argument which may contain style, id, roles, and options.
///
/// Format: `style#id.role1.role2%opt1%opt2`
fn parse_first_positional<'src>(part: &'src str, attrs: &mut BlockAttrs<'src>) {
    let mut rest = part;

    // Extract style (text before first #, ., or %).
    let style_end = rest.find(['#', '.', '%']).unwrap_or(rest.len());
    if style_end > 0 {
        attrs.style = Some(&rest[..style_end]);
    }
    rest = &rest[style_end..];

    // Parse remaining shorthand elements.
    while !rest.is_empty() {
        if let Some(after_hash) = rest.strip_prefix('#') {
            // ID shorthand: #id
            let id_end = after_hash.find(['.', '%']).unwrap_or(after_hash.len());
            if id_end > 0 {
                attrs.id = Some(&after_hash[..id_end]);
            }
            rest = &after_hash[id_end..];
        } else if let Some(after_dot) = rest.strip_prefix('.') {
            // Role shorthand: .role
            let role_end = after_dot.find(['.', '#', '%']).unwrap_or(after_dot.len());
            if role_end > 0 {
                attrs.roles.push(&after_dot[..role_end]);
            }
            rest = &after_dot[role_end..];
        } else if let Some(after_pct) = rest.strip_prefix('%') {
            // Option shorthand: %opt
            let opt_end = after_pct.find(['.', '#', '%']).unwrap_or(after_pct.len());
            if opt_end > 0 {
                attrs.options.push(&after_pct[..opt_end]);
            }
            rest = &after_pct[opt_end..];
        } else {
            // Unexpected character, skip it.
            rest = &rest[1..];
        }
    }
}

/// Check whether position `i` starts a block attribute line.
///
/// A block attribute line is `[` ... `]` on its own line (followed by `Newline`
/// or end-of-tokens). Returns parsed attributes.
pub(super) fn is_block_attribute_line<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
) -> Option<BlockAttributeResult<'src>> {
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

    // Parse the attribute content.
    let content_end = j;
    let attrs = parse_block_attr_content(tokens, i + 1, content_end, source);

    j += 1;
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        j += 1;
    }
    Some(BlockAttributeResult { next: j, attrs })
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

    // Check if this is a literal block delimiter (4+ dots on a line).
    // A literal delimiter is all Dot tokens, and we already have one dot,
    // so if the rest of the line is 3+ more dots, this is a literal delimiter.
    let mut dot_count = 1; // We already matched the first dot
    let mut j = content_start;
    while j < line_end && matches!(tokens[j].0, Token::Dot) {
        dot_count += 1;
        j += 1;
    }
    // If the entire line is 4+ dots, it's a literal delimiter, not a title.
    if dot_count >= 4 && j == line_end {
        return None;
    }

    // If content after the dot(s) starts with whitespace, this is an ordered
    // list item (`. Item` or `.. Item`), not a block title.
    if j < line_end && matches!(tokens[j].0, Token::Whitespace) {
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

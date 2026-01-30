//! List parsing (unordered and ordered lists with nesting support).

use super::Spanned;
use crate::asg::Block;
use crate::diagnostic::ParseDiagnostic;
use crate::parser::inline::run_inline_parser;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// List variant (unordered or ordered).
#[derive(Clone, Copy)]
enum ListVariant {
    Unordered,
    Ordered,
}

impl ListVariant {
    /// Get the marker level for this list variant at position `i`.
    fn marker_level(self, tokens: &[Spanned<'_>], i: usize) -> (usize, usize) {
        match self {
            Self::Unordered => unordered_marker_level(tokens, i),
            Self::Ordered => ordered_marker_level(tokens, i),
        }
    }

    /// Get the variant name as a static string.
    const fn name(self) -> &'static str {
        match self {
            Self::Unordered => "unordered",
            Self::Ordered => "ordered",
        }
    }
}

/// Count consecutive star tokens at position `i`.
fn count_stars(tokens: &[Spanned<'_>], i: usize) -> usize {
    let mut count = 0;
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Star) {
        count += 1;
        j += 1;
    }
    count
}

/// Count consecutive dot tokens at position `i`.
fn count_dots(tokens: &[Spanned<'_>], i: usize) -> usize {
    let mut count = 0;
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Dot) {
        count += 1;
        j += 1;
    }
    count
}

/// Check whether position `i` starts an unordered list item (one or more stars followed by whitespace).
/// Returns `(marker_level, actual_start_position)` if it's a list item, or `(0, i)` if not.
/// The `actual_start_position` accounts for any leading whitespace.
fn unordered_marker_level(tokens: &[Spanned<'_>], i: usize) -> (usize, usize) {
    // Skip leading whitespace
    let mut start = i;
    while start < tokens.len() && matches!(tokens[start].0, Token::Whitespace) {
        start += 1;
    }

    let star_count = count_stars(tokens, start);
    if star_count == 0 {
        return (0, i);
    }
    let after_stars = start + star_count;
    if after_stars < tokens.len() && matches!(tokens[after_stars].0, Token::Whitespace) {
        (star_count, start)
    } else {
        (0, i)
    }
}

/// Check whether position `i` starts an ordered list item (one or more dots followed by whitespace).
/// Returns `(marker_level, actual_start_position)` if it's a list item, or `(0, i)` if not.
/// The `actual_start_position` accounts for any leading whitespace.
fn ordered_marker_level(tokens: &[Spanned<'_>], i: usize) -> (usize, usize) {
    // Skip leading whitespace
    let mut start = i;
    while start < tokens.len() && matches!(tokens[start].0, Token::Whitespace) {
        start += 1;
    }

    let dot_count = count_dots(tokens, start);
    if dot_count == 0 {
        return (0, i);
    }
    let after_dots = start + dot_count;
    if after_dots < tokens.len() && matches!(tokens[after_dots].0, Token::Whitespace) {
        (dot_count, start)
    } else {
        (0, i)
    }
}

/// Check whether position `i` starts any list item (unordered or ordered).
pub(super) fn is_list_item(tokens: &[Spanned<'_>], i: usize) -> bool {
    unordered_marker_level(tokens, i).0 > 0 || ordered_marker_level(tokens, i).0 > 0
}

/// Try to parse a list starting at position `i`.
///
/// Handles both unordered (`*`, `**`, etc.) and ordered (`.`, `..`, etc.) lists
/// with nesting support. Returns `None` if position `i` does not start a list item.
pub(super) fn try_list<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    let (unordered_level, _) = unordered_marker_level(tokens, i);
    let (ordered_level, _) = ordered_marker_level(tokens, i);

    if unordered_level > 0 {
        let (block, next, diags, _span_end) = parse_list(
            tokens,
            i,
            source,
            idx,
            unordered_level,
            ListVariant::Unordered,
        )?;
        Some((block, next, diags))
    } else if ordered_level > 0 {
        let (block, next, diags, _span_end) =
            parse_list(tokens, i, source, idx, ordered_level, ListVariant::Ordered)?;
        Some((block, next, diags))
    } else {
        None
    }
}

/// Parse a single list item at the current level.
///
/// Returns `(Block, content_end, item_end_byte, diagnostics)`.
fn parse_list_item<'src>(
    tokens: &[Spanned<'src>],
    actual_start: usize,
    current_level: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> (Block<'src>, usize, usize, Vec<ParseDiagnostic>) {
    let item_marker =
        &source[tokens[actual_start].1.start..tokens[actual_start + current_level - 1].1.end];
    let item_start = tokens[actual_start].1.start;

    // Content starts after markers and whitespace
    let content_start = actual_start + current_level + 1;
    let mut content_end = content_start;
    while content_end < tokens.len() && !matches!(tokens[content_end].0, Token::Newline) {
        content_end += 1;
    }

    // Parse principal content through the inline parser
    let content_tokens = &tokens[content_start..content_end];
    let (principal, diagnostics) = run_inline_parser(content_tokens, source, idx);

    // Item location: from the marker to the last content token
    let item_end = if content_end > content_start {
        tokens[content_end - 1].1.end
    } else {
        tokens[actual_start + current_level].1.end
    };
    let item_span = SourceSpan {
        start: item_start,
        end: item_end,
    };

    let mut item = Block::new("listItem");
    item.marker = Some(item_marker);
    item.principal = Some(principal);
    item.location = Some(idx.location(&item_span));

    (item, content_end, item_end, diagnostics)
}

/// Handle a nested list by adding it to the last item's blocks.
fn handle_nested_list<'src>(
    last_item: &mut Block<'src>,
    nested_list: Block<'src>,
    nested_span_end: usize,
    list_span_end: &mut usize,
) {
    // Add nested list to the last item's blocks
    if last_item.blocks.is_none() {
        last_item.blocks = Some(Vec::new());
    }
    last_item.blocks.as_mut().unwrap().push(nested_list);

    // Update the last item's location to include the nested content
    if let Some(ref mut loc) = last_item.location
        && let Some(nested_loc) = last_item
            .blocks
            .as_ref()
            .and_then(|b| b.last())
            .and_then(|nested| nested.location.as_ref())
    {
        loc[1] = nested_loc[1].clone();
    }

    // Update list span end using the byte offset from nested list
    *list_span_end = (*list_span_end).max(nested_span_end);
}

/// Parse a list at the given nesting level.
///
/// Returns `(Block, next_token_pos, diagnostics, span_end_byte)`.
fn parse_list<'src>(
    tokens: &[Spanned<'src>],
    start: usize,
    source: &'src str,
    idx: &SourceIndex,
    level: usize,
    variant: ListVariant,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>, usize)> {
    let mut items: Vec<Block<'src>> = Vec::new();
    let mut diagnostics = Vec::new();
    let mut j = start;

    // Get the actual marker start position (skipping leading whitespace)
    let (_, marker_start) = variant.marker_level(tokens, start);
    let list_span_start = tokens[marker_start].1.start;
    let mut list_span_end = tokens[marker_start].1.end;
    let marker = &source[tokens[marker_start].1.start..tokens[marker_start + level - 1].1.end];

    while j < tokens.len() {
        let (current_level, actual_start) = variant.marker_level(tokens, j);

        if current_level == 0 || current_level < level {
            // Not a list item or return to parent level
            break;
        }

        if current_level > level {
            // This is a nested list - add to the previous item's blocks
            if let Some(last_item) = items.last_mut() {
                let (nested_list, next_pos, nested_diags, nested_span_end) =
                    parse_list(tokens, j, source, idx, current_level, variant)?;
                diagnostics.extend(nested_diags);

                handle_nested_list(last_item, nested_list, nested_span_end, &mut list_span_end);

                j = next_pos;
                continue;
            }
        }

        // Parse item at current level
        let (item, content_end, item_end, item_diags) =
            parse_list_item(tokens, actual_start, current_level, source, idx);
        diagnostics.extend(item_diags);
        items.push(item);

        list_span_end = item_end;

        // Advance past the Newline (if present)
        j = if content_end < tokens.len() {
            content_end + 1
        } else {
            content_end
        };
    }

    if items.is_empty() {
        return None;
    }

    let list_span = SourceSpan {
        start: list_span_start,
        end: list_span_end,
    };

    let mut list = Block::new("list");
    list.variant = Some(variant.name());
    list.marker = Some(marker);
    list.items = Some(items);
    list.location = Some(idx.location(&list_span));

    Some((list, j, diagnostics, list_span_end))
}

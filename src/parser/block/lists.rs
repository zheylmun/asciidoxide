//! List parsing (unordered and ordered lists with nesting support).

use super::Spanned;
use crate::asg::Block;
use crate::diagnostic::ParseDiagnostic;
use crate::parser::inline::run_inline_parser;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

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
        let (block, next, diags, _span_end) =
            parse_unordered_list(tokens, i, source, idx, unordered_level)?;
        Some((block, next, diags))
    } else if ordered_level > 0 {
        let (block, next, diags, _span_end) =
            parse_ordered_list(tokens, i, source, idx, ordered_level)?;
        Some((block, next, diags))
    } else {
        None
    }
}

/// Parse an unordered list at the given nesting level.
/// Returns `(Block, next_token_pos, diagnostics, span_end_byte)`.
#[allow(clippy::too_many_lines)]
fn parse_unordered_list<'src>(
    tokens: &[Spanned<'src>],
    start: usize,
    source: &'src str,
    idx: &SourceIndex,
    level: usize,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>, usize)> {
    let mut items: Vec<Block<'src>> = Vec::new();
    let mut diagnostics = Vec::new();
    let mut j = start;

    // Get the actual marker start position (skipping leading whitespace)
    let (_, marker_start) = unordered_marker_level(tokens, start);
    let list_span_start = tokens[marker_start].1.start;
    let mut list_span_end = tokens[marker_start].1.end;
    let marker = &source[tokens[marker_start].1.start..tokens[marker_start + level - 1].1.end];

    while j < tokens.len() {
        let (current_level, actual_start) = unordered_marker_level(tokens, j);

        if current_level == 0 {
            // Not a list item - end of list
            break;
        }

        if current_level < level {
            // Return to parent level
            break;
        }

        if current_level > level {
            // This is a nested list - it should be added to the previous item's blocks
            // If there's no previous item, treat it as current level
            if let Some(last_item) = items.last_mut() {
                let (nested_list, next_pos, nested_diags, nested_span_end) =
                    parse_unordered_list(tokens, j, source, idx, current_level)?;
                diagnostics.extend(nested_diags);

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
                list_span_end = list_span_end.max(nested_span_end);

                j = next_pos;
                continue;
            }
        }

        // Parse item at current level
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
        let (principal, diags) = run_inline_parser(content_tokens, source, idx);
        diagnostics.extend(diags);

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
            marker: Some(item_marker),
            inlines: None,
            blocks: None,
            items: None,
            principal: Some(principal),
            location: Some(idx.location(&item_span)),
        });

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
            marker: Some(marker),
            inlines: None,
            blocks: None,
            items: Some(items),
            principal: None,
            location: Some(idx.location(&list_span)),
        },
        j,
        diagnostics,
        list_span_end,
    ))
}

/// Parse an ordered list at the given nesting level.
/// Returns `(Block, next_token_pos, diagnostics, span_end_byte)`.
#[allow(clippy::too_many_lines)]
fn parse_ordered_list<'src>(
    tokens: &[Spanned<'src>],
    start: usize,
    source: &'src str,
    idx: &SourceIndex,
    level: usize,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>, usize)> {
    let mut items: Vec<Block<'src>> = Vec::new();
    let mut diagnostics = Vec::new();
    let mut j = start;

    // Get the actual marker start position (skipping leading whitespace)
    let (_, marker_start) = ordered_marker_level(tokens, start);
    let list_span_start = tokens[marker_start].1.start;
    let mut list_span_end = tokens[marker_start].1.end;
    let marker = &source[tokens[marker_start].1.start..tokens[marker_start + level - 1].1.end];

    while j < tokens.len() {
        let (current_level, actual_start) = ordered_marker_level(tokens, j);

        if current_level == 0 {
            // Not a list item - end of list
            break;
        }

        if current_level < level {
            // Return to parent level
            break;
        }

        if current_level > level {
            // This is a nested list - it should be added to the previous item's blocks
            if let Some(last_item) = items.last_mut() {
                let (nested_list, next_pos, nested_diags, nested_span_end) =
                    parse_ordered_list(tokens, j, source, idx, current_level)?;
                diagnostics.extend(nested_diags);

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
                list_span_end = list_span_end.max(nested_span_end);

                j = next_pos;
                continue;
            }
        }

        // Parse item at current level
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
        let (principal, diags) = run_inline_parser(content_tokens, source, idx);
        diagnostics.extend(diags);

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
            marker: Some(item_marker),
            inlines: None,
            blocks: None,
            items: None,
            principal: Some(principal),
            location: Some(idx.location(&item_span)),
        });

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
            variant: Some("ordered"),
            marker: Some(marker),
            inlines: None,
            blocks: None,
            items: Some(items),
            principal: None,
            location: Some(idx.location(&list_span)),
        },
        j,
        diagnostics,
        list_span_end,
    ))
}

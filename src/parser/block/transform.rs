//! Phase 3: Transform `RawBlock` to Block.
//!
//! This module walks the `RawBlock` tree from phase 2 and:
//! 1. Parses inline content for basic-content-model blocks
//! 2. Recursively parses compound block content
//! 3. Parses title spans as inlines

use super::raw_block::RawBlock;
use crate::asg::{Block, BlockMetadata, InlineNode, Location, Position, TextNode};
use crate::diagnostic::ParseDiagnostic;
use crate::lexer::lex;
use crate::parser::inline::run_inline_parser;
use crate::span::{SourceIndex, SourceSpan};

/// Offset a location by a base position.
///
/// When we parse content from a substring, the resulting locations are relative
/// to the substring's start (line 1, col 1). This function adjusts them to be
/// relative to the original source using the byte offset of the substring.
fn offset_location(loc: Option<Location>, base: Position) -> Option<Location> {
    loc.map(|[start, end]| {
        [
            Position {
                line: start.line + base.line - 1,
                col: if start.line == 1 {
                    start.col + base.col - 1
                } else {
                    start.col
                },
            },
            Position {
                line: end.line + base.line - 1,
                col: if end.line == 1 {
                    end.col + base.col - 1
                } else {
                    end.col
                },
            },
        ]
    })
}

/// Offset all locations in a block tree by a base position.
fn offset_block_locations(block: &mut Block<'_>, base: Position) {
    block.location = offset_location(block.location, base);

    // Offset inlines
    if let Some(ref mut inlines) = block.inlines {
        for inline in inlines.iter_mut() {
            offset_inline_location(inline, base);
        }
    }

    // Offset title
    if let Some(ref mut title) = block.title {
        for inline in title.iter_mut() {
            offset_inline_location(inline, base);
        }
    }

    // Offset principal
    if let Some(ref mut principal) = block.principal {
        for inline in principal.iter_mut() {
            offset_inline_location(inline, base);
        }
    }

    // Offset child blocks
    if let Some(ref mut blocks) = block.blocks {
        for child in blocks.iter_mut() {
            offset_block_locations(child, base);
        }
    }

    // Offset items
    if let Some(ref mut items) = block.items {
        for item in items.iter_mut() {
            offset_block_locations(item, base);
        }
    }
}

/// Offset a location in an inline node.
fn offset_inline_location(inline: &mut InlineNode<'_>, base: Position) {
    match inline {
        InlineNode::Text(text) => {
            text.location = offset_location(text.location, base);
        }
        InlineNode::Span(span) => {
            span.location = offset_location(span.location, base);
            for child in &mut span.inlines {
                offset_inline_location(child, base);
            }
        }
        InlineNode::Ref(r) => {
            r.location = offset_location(r.location, base);
            for child in &mut r.inlines {
                offset_inline_location(child, base);
            }
        }
        InlineNode::Raw(raw) => {
            raw.location = offset_location(raw.location, base);
        }
    }
}

/// Get the starting position for a byte offset in the original source.
fn get_base_position(_source: &str, idx: &SourceIndex, byte_offset: usize) -> Position {
    let loc = idx.location(&SourceSpan {
        start: byte_offset,
        end: byte_offset,
    });
    loc[0]
}

/// Transform a `RawBlock` into one or more Blocks by parsing inline content.
///
/// This is phase 3 of the parsing pipeline.
/// The `source` is the original full source, and `idx` is its `SourceIndex`.
///
/// Returns a vector because discrete sections expand into a heading plus sibling blocks.
#[allow(clippy::too_many_lines)]
pub(super) fn transform_raw_block<'src>(
    raw: RawBlock<'src>,
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    // Handle discrete sections specially - they expand into heading + sibling blocks
    if raw.name == "section" && raw.style == Some("discrete") {
        return transform_discrete_section(raw, source, idx);
    }

    let mut diagnostics = Vec::new();

    let mut block = Block::new(raw.name);
    block.form = raw.form;
    block.delimiter = raw.delimiter;
    block.id = raw.id;
    block.style = raw.style;
    block.level = raw.level;
    block.variant = raw.variant;
    block.marker = raw.marker;
    block.location = raw.location;

    // Build metadata if we have roles or options
    if !raw.roles.is_empty() || !raw.options.is_empty() {
        block.metadata = Some(BlockMetadata {
            roles: raw.roles,
            options: raw.options,
            attributes: std::collections::HashMap::new(),
        });
    }

    // Parse title span as inlines (with location adjustment)
    if let Some(title_span) = raw.title_span {
        let title_content = &source[title_span.start..title_span.end];
        let title_tokens = lex(title_content);
        let title_idx = SourceIndex::new(title_content);
        let (mut title_inlines, title_diags) =
            run_inline_parser(&title_tokens, title_content, &title_idx);
        diagnostics.extend(title_diags);

        // Adjust locations to be relative to original source
        let base = get_base_position(source, idx, title_span.start);
        for inline in &mut title_inlines {
            offset_inline_location(inline, base);
        }
        block.title = Some(title_inlines);
    }

    // Parse reftext span as inlines (with location adjustment)
    if let Some(reftext_span) = raw.reftext_span {
        let reftext_content = &source[reftext_span.start..reftext_span.end];
        let reftext_tokens = lex(reftext_content);
        let reftext_idx = SourceIndex::new(reftext_content);
        let (mut reftext_inlines, reftext_diags) =
            run_inline_parser(&reftext_tokens, reftext_content, &reftext_idx);
        diagnostics.extend(reftext_diags);

        let base = get_base_position(source, idx, reftext_span.start);
        for inline in &mut reftext_inlines {
            offset_inline_location(inline, base);
        }
        block.reftext = Some(reftext_inlines);
    }

    // Handle content based on block type
    match raw.name {
        // Verbatim blocks: content stays as raw text (with proper location)
        "listing" | "literal" | "pass" => {
            if let Some(content_span) = raw.content_span {
                let content = &source[content_span.start..content_span.end];
                // Use the original idx to get the correct location
                let location = idx.location(&content_span);
                block.inlines = Some(vec![InlineNode::Text(TextNode {
                    value: content,
                    location: Some(location),
                })]);
            } else {
                block.inlines = Some(vec![]);
            }
        }

        // Basic content blocks: parse content as inlines
        "paragraph" => {
            if let Some(content_span) = raw.content_span {
                let content = &source[content_span.start..content_span.end];
                let content_tokens = lex(content);
                let content_idx = SourceIndex::new(content);
                let (mut inlines, content_diags) =
                    run_inline_parser(&content_tokens, content, &content_idx);
                diagnostics.extend(content_diags);

                // Adjust locations
                let base = get_base_position(source, idx, content_span.start);
                for inline in &mut inlines {
                    offset_inline_location(inline, base);
                }
                block.inlines = Some(inlines);
            } else {
                block.inlines = Some(vec![]);
            }
        }

        // Compound blocks: recursively parse content as blocks
        "example" | "sidebar" | "quote" | "open" => {
            if let Some(content_span) = raw.content_span {
                let content = &source[content_span.start..content_span.end];
                let content_tokens = lex(content);
                let content_idx = SourceIndex::new(content);
                let (raw_blocks, block_diags) =
                    super::boundary::parse_raw_blocks(&content_tokens, content, &content_idx);
                diagnostics.extend(block_diags);

                // Recursively transform child blocks
                let base = get_base_position(source, idx, content_span.start);
                let mut child_blocks = Vec::new();
                for raw_child in raw_blocks {
                    let (mut children, child_diags) =
                        transform_raw_block(raw_child, content, &content_idx);
                    diagnostics.extend(child_diags);
                    // Adjust all locations in the child block tree
                    for child in &mut children {
                        offset_block_locations(child, base);
                    }
                    child_blocks.extend(children);
                }
                block.blocks = Some(child_blocks);
            } else {
                block.blocks = Some(vec![]);
            }
        }

        // Sections: title is already handled above, body content is recursively parsed
        "section" => {
            if let Some(content_span) = raw.content_span {
                let content = &source[content_span.start..content_span.end];
                let content_tokens = lex(content);
                let content_idx = SourceIndex::new(content);
                let (raw_blocks, block_diags) =
                    super::boundary::parse_raw_blocks(&content_tokens, content, &content_idx);
                diagnostics.extend(block_diags);

                // Recursively transform child blocks
                let base = get_base_position(source, idx, content_span.start);
                let mut child_blocks = Vec::new();
                for raw_child in raw_blocks {
                    let (mut children, child_diags) =
                        transform_raw_block(raw_child, content, &content_idx);
                    diagnostics.extend(child_diags);
                    for child in &mut children {
                        offset_block_locations(child, base);
                    }
                    child_blocks.extend(children);
                }
                block.blocks = Some(child_blocks);
            } else {
                block.blocks = Some(vec![]);
            }
        }

        // Lists: items need to be transformed (list items don't expand to multiple blocks)
        "list" => {
            if let Some(items) = raw.items {
                let mut transformed_items = Vec::new();
                for item in items {
                    let (transformed, item_diags) = transform_raw_block(item, source, idx);
                    diagnostics.extend(item_diags);
                    // List items always produce exactly one block
                    transformed_items.extend(transformed);
                }
                block.items = Some(transformed_items);
            }
        }

        // List items: parse principal span as inlines
        "listItem" => {
            if let Some(principal_span) = raw.principal_span {
                let content = &source[principal_span.start..principal_span.end];
                let content_tokens = lex(content);
                let content_idx = SourceIndex::new(content);
                let (mut principal, principal_diags) =
                    run_inline_parser(&content_tokens, content, &content_idx);
                diagnostics.extend(principal_diags);

                // Adjust locations
                let base = get_base_position(source, idx, principal_span.start);
                for inline in &mut principal {
                    offset_inline_location(inline, base);
                }
                block.principal = Some(principal);
            }

            // Transform nested blocks (nested lists)
            if let Some(nested_blocks) = raw.blocks {
                let mut transformed_nested = Vec::new();
                for nested in nested_blocks {
                    let (transformed, nested_diags) = transform_raw_block(nested, source, idx);
                    diagnostics.extend(nested_diags);
                    transformed_nested.extend(transformed);
                }
                block.blocks = Some(transformed_nested);
            }
        }

        // Breaks and headings: no content to transform
        // (Headings are standalone with no body, from [discrete] style)
        "break" | "heading" => {}

        // Default: try to parse content_span as inlines if present
        _ => {
            if let Some(content_span) = raw.content_span {
                let content = &source[content_span.start..content_span.end];
                let content_tokens = lex(content);
                let content_idx = SourceIndex::new(content);
                let (mut inlines, content_diags) =
                    run_inline_parser(&content_tokens, content, &content_idx);
                diagnostics.extend(content_diags);

                let base = get_base_position(source, idx, content_span.start);
                for inline in &mut inlines {
                    offset_inline_location(inline, base);
                }
                block.inlines = Some(inlines);
            }
        }
    }

    (vec![block], diagnostics)
}

/// Transform a discrete section into a heading block plus sibling blocks.
///
/// Discrete headings don't capture body content as children - instead, the
/// body content becomes sibling blocks at the same level.
fn transform_discrete_section<'src>(
    raw: RawBlock<'src>,
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let mut diagnostics = Vec::new();
    let mut result_blocks = Vec::new();

    // Create the heading block
    let mut heading = Block::new("heading");
    heading.level = raw.level;
    heading.id = raw.id;
    // Note: style is NOT copied - discrete is not shown in output

    // Use heading line location instead of full section location
    heading.location = raw.heading_line_location.or(raw.location);

    // Build metadata if we have roles or options
    if !raw.roles.is_empty() || !raw.options.is_empty() {
        heading.metadata = Some(BlockMetadata {
            roles: raw.roles,
            options: raw.options,
            attributes: std::collections::HashMap::new(),
        });
    }

    // Parse title span as inlines
    if let Some(title_span) = raw.title_span {
        let title_content = &source[title_span.start..title_span.end];
        let title_tokens = lex(title_content);
        let title_idx = SourceIndex::new(title_content);
        let (mut title_inlines, title_diags) =
            run_inline_parser(&title_tokens, title_content, &title_idx);
        diagnostics.extend(title_diags);

        let base = get_base_position(source, idx, title_span.start);
        for inline in &mut title_inlines {
            offset_inline_location(inline, base);
        }
        heading.title = Some(title_inlines);
    }

    result_blocks.push(heading);

    // Parse body content as sibling blocks (not children)
    if let Some(content_span) = raw.content_span {
        let content = &source[content_span.start..content_span.end];
        let content_tokens = lex(content);
        let content_idx = SourceIndex::new(content);
        let (raw_blocks, block_diags) =
            super::boundary::parse_raw_blocks(&content_tokens, content, &content_idx);
        diagnostics.extend(block_diags);

        // Transform and add as siblings
        let base = get_base_position(source, idx, content_span.start);
        for raw_child in raw_blocks {
            let (mut child_blocks, child_diags) =
                transform_raw_block(raw_child, content, &content_idx);
            diagnostics.extend(child_diags);
            for child in &mut child_blocks {
                offset_block_locations(child, base);
            }
            result_blocks.extend(child_blocks);
        }
    }

    (result_blocks, diagnostics)
}

/// Transform a vector of `RawBlock`s into Blocks.
pub(super) fn transform_raw_blocks<'src>(
    raw_blocks: Vec<RawBlock<'src>>,
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let mut blocks = Vec::new();
    let mut diagnostics = Vec::new();

    for raw in raw_blocks {
        let (transformed, diags) = transform_raw_block(raw, source, idx);
        diagnostics.extend(diags);
        blocks.extend(transformed);
    }

    (blocks, diagnostics)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::span::SourceIndex;

    #[test]
    fn test_transform_paragraph() {
        let source = "hello *world*\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
        assert!(blocks[0].inlines.is_some());
    }

    #[test]
    fn test_transform_listing() {
        let source = "----\ncode here\n----\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "listing");

        // Content should be raw text
        let inlines = blocks[0].inlines.as_ref().expect("should have inlines");
        assert_eq!(inlines.len(), 1);
        if let InlineNode::Text(text) = &inlines[0] {
            assert_eq!(text.value, "code here");
        } else {
            panic!("expected text node");
        }
    }

    #[test]
    fn test_transform_example_with_paragraph() {
        let source = "====\ninner paragraph\n====\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "example");

        // Should have child blocks
        let child_blocks = blocks[0].blocks.as_ref().expect("should have child blocks");
        assert_eq!(child_blocks.len(), 1);
        assert_eq!(child_blocks[0].name, "paragraph");
    }

    #[test]
    fn test_transform_list() {
        let source = "* item one\n* item two\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2);

        // Check that principal content was parsed
        let principal = items[0].principal.as_ref().expect("should have principal");
        assert!(!principal.is_empty());
    }

    #[test]
    fn test_transform_section() {
        let source = "== Section Title\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "section");
        assert_eq!(blocks[0].level, Some(1));

        // Title should be parsed as inlines
        let title = blocks[0].title.as_ref().expect("should have title");
        assert!(!title.is_empty());
    }
}

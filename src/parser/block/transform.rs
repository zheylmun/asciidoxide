//! Phase 3: Transform RawBlock to Block.
//!
//! This module walks the RawBlock tree from phase 2 and:
//! 1. Parses inline content for basic-content-model blocks
//! 2. Recursively parses compound block content
//! 3. Parses title spans as inlines

use super::raw_block::RawBlock;
use crate::asg::{Block, BlockMetadata, InlineNode, TextNode};
use crate::diagnostic::ParseDiagnostic;
use crate::lexer::lex;
use crate::parser::inline::run_inline_parser;
use crate::span::SourceIndex;

/// Transform a RawBlock into a Block by parsing inline content.
///
/// This is phase 3 of the parsing pipeline.
pub(super) fn transform_raw_block<'src>(
    raw: RawBlock<'src>,
    source: &'src str,
    idx: &SourceIndex,
) -> (Block<'src>, Vec<ParseDiagnostic>) {
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

    // Parse title span as inlines
    if let Some(title_span) = raw.title_span {
        let title_content = &source[title_span.start..title_span.end];
        let title_tokens = lex(title_content);
        // Create a new SourceIndex for the title substring
        let title_idx = SourceIndex::new(title_content);
        let (title_inlines, title_diags) = run_inline_parser(&title_tokens, title_content, &title_idx);
        diagnostics.extend(title_diags);
        block.title = Some(title_inlines);
    }

    // Parse reftext span as inlines
    if let Some(reftext_span) = raw.reftext_span {
        let reftext_content = &source[reftext_span.start..reftext_span.end];
        let reftext_tokens = lex(reftext_content);
        let reftext_idx = SourceIndex::new(reftext_content);
        let (reftext_inlines, reftext_diags) =
            run_inline_parser(&reftext_tokens, reftext_content, &reftext_idx);
        diagnostics.extend(reftext_diags);
        block.reftext = Some(reftext_inlines);
    }

    // Handle content based on block type
    match raw.name {
        // Verbatim blocks: content stays as raw text
        "listing" | "literal" | "pass" => {
            if let Some(content_span) = raw.content_span {
                let content = &source[content_span.start..content_span.end];
                let content_idx = SourceIndex::new(content);
                block.inlines = Some(vec![InlineNode::Text(TextNode {
                    value: content,
                    location: Some(content_idx.location(&crate::span::SourceSpan {
                        start: 0,
                        end: content.len(),
                    })),
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
                let (inlines, content_diags) =
                    run_inline_parser(&content_tokens, content, &content_idx);
                diagnostics.extend(content_diags);
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
                    super::chumsky_blocks::parse_raw_blocks(&content_tokens, content, &content_idx);
                diagnostics.extend(block_diags);

                // Recursively transform child blocks
                let mut child_blocks = Vec::new();
                for raw_child in raw_blocks {
                    let (child, child_diags) = transform_raw_block(raw_child, content, &content_idx);
                    diagnostics.extend(child_diags);
                    child_blocks.push(child);
                }
                block.blocks = Some(child_blocks);
            } else {
                block.blocks = Some(vec![]);
            }
        }

        // Sections: title is already handled above, body is in blocks field
        // Note: In phase 2, sections don't capture body content yet
        // For now, sections have no body blocks (that would need section nesting logic)
        "section" => {
            // Sections currently don't have body content from phase 2
            // This would require more complex section nesting logic
            block.blocks = Some(vec![]);
        }

        // Lists: items need to be transformed
        "list" => {
            if let Some(items) = raw.items {
                let mut transformed_items = Vec::new();
                for item in items {
                    let (transformed_item, item_diags) = transform_raw_block(item, source, idx);
                    diagnostics.extend(item_diags);
                    transformed_items.push(transformed_item);
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
                let (principal, principal_diags) =
                    run_inline_parser(&content_tokens, content, &content_idx);
                diagnostics.extend(principal_diags);
                block.principal = Some(principal);
            }

            // Transform nested blocks (nested lists)
            if let Some(nested_blocks) = raw.blocks {
                let mut transformed_nested = Vec::new();
                for nested in nested_blocks {
                    let (transformed, nested_diags) = transform_raw_block(nested, source, idx);
                    diagnostics.extend(nested_diags);
                    transformed_nested.push(transformed);
                }
                block.blocks = Some(transformed_nested);
            }
        }

        // Breaks: no content to transform
        "break" => {}

        // Default: try to parse content_span as inlines if present
        _ => {
            if let Some(content_span) = raw.content_span {
                let content = &source[content_span.start..content_span.end];
                let content_tokens = lex(content);
                let content_idx = SourceIndex::new(content);
                let (inlines, content_diags) =
                    run_inline_parser(&content_tokens, content, &content_idx);
                diagnostics.extend(content_diags);
                block.inlines = Some(inlines);
            }
        }
    }

    (block, diagnostics)
}

/// Transform a vector of RawBlocks into Blocks.
pub(super) fn transform_raw_blocks<'src>(
    raw_blocks: Vec<RawBlock<'src>>,
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let mut blocks = Vec::new();
    let mut diagnostics = Vec::new();

    for raw in raw_blocks {
        let (block, diags) = transform_raw_block(raw, source, idx);
        diagnostics.extend(diags);
        blocks.push(block);
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

        let (raw_blocks, _) =
            super::super::chumsky_blocks::parse_raw_blocks(&tokens, source, &idx);
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

        let (raw_blocks, _) =
            super::super::chumsky_blocks::parse_raw_blocks(&tokens, source, &idx);
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

        let (raw_blocks, _) =
            super::super::chumsky_blocks::parse_raw_blocks(&tokens, source, &idx);
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

        let (raw_blocks, _) =
            super::super::chumsky_blocks::parse_raw_blocks(&tokens, source, &idx);
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

        let (raw_blocks, _) =
            super::super::chumsky_blocks::parse_raw_blocks(&tokens, source, &idx);
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

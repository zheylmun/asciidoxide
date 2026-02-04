//! Phase 3: Transform `RawBlock` to Block.
//!
//! This module walks the `RawBlock` tree from phase 2 and:
//! 1. Parses inline content for basic-content-model blocks
//! 2. Recursively parses compound block content
//! 3. Parses title spans as inlines

use super::raw_block::RawBlock;
use std::borrow::Cow;
use std::collections::HashMap;

use crate::asg::{Block, BlockMetadata, InlineNode, Location, Position, TextNode};
use crate::diagnostic::ParseDiagnostic;
use crate::lexer::lex;
use crate::parser::inline::run_inline_parser;
use crate::span::{SourceIndex, SourceSpan};

/// Generate a slug from a section title for auto-ID generation.
///
/// Algorithm: lowercase, non-alphanumeric → `_`, prefix with `_`, collapse
/// consecutive underscores, trim trailing underscores.
fn slugify(title: &str) -> String {
    let mut slug = String::with_capacity(title.len() + 1);
    slug.push('_');
    for ch in title.chars() {
        if ch.is_ascii_alphanumeric() {
            slug.push(ch.to_ascii_lowercase());
        } else {
            slug.push('_');
        }
    }
    // Collapse consecutive underscores
    let mut collapsed = String::with_capacity(slug.len());
    let mut prev_underscore = false;
    for ch in slug.chars() {
        if ch == '_' {
            if !prev_underscore {
                collapsed.push('_');
            }
            prev_underscore = true;
        } else {
            collapsed.push(ch);
            prev_underscore = false;
        }
    }
    // Trim trailing underscores
    collapsed.truncate(collapsed.trim_end_matches('_').len());
    collapsed
}

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
///
/// An `id_registry` is threaded through for section ID deduplication.
#[allow(clippy::too_many_lines)]
fn transform_raw_block_inner<'src>(
    raw: RawBlock<'src>,
    source: &'src str,
    idx: &SourceIndex,
    id_registry: &mut HashMap<String, usize>,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    // Handle discrete sections specially - they expand into heading + sibling blocks
    if raw.name == "section" && raw.style == Some("discrete") {
        return transform_discrete_section(raw, source, idx, id_registry);
    }

    let mut diagnostics = Vec::new();

    // --- Style promotion ---
    // Apply style-based name changes and extract language attribute
    let mut promoted_name: &str = raw.name;
    let mut language: Option<&'src str> = None;
    let style = raw.style;
    let positionals = &raw.positionals;

    match (style, raw.name) {
        // [source] on literal block → listing
        (Some("source"), "literal") => {
            promoted_name = "listing";
            // Second positional is language
            if positionals.len() > 1 && !positionals[1].is_empty() {
                language = Some(positionals[1]);
            }
        }
        // [source] or [source,lang] on listing → listing (stays same) + language
        (Some("source"), "listing") => {
            if positionals.len() > 1 && !positionals[1].is_empty() {
                language = Some(positionals[1]);
            }
        }
        // [,lang] on listing - empty first positional implies source promotion
        (None, "listing")
            if positionals.len() > 1 && !positionals.is_empty() && positionals[0].is_empty() =>
        {
            if !positionals[1].is_empty() {
                language = Some(positionals[1]);
            }
        }
        // [stem] on pass → stem
        (Some("stem"), "pass") => {
            promoted_name = "stem";
        }
        // [verse] on quote → verse
        (Some("verse"), "quote") => {
            promoted_name = "verse";
        }
        _ => {}
    }

    // For fenced code blocks (listing with backtick delimiter), extract language from positionals
    if raw.name == "listing"
        && raw.delimiter.is_some_and(|d| d.starts_with('`'))
        && positionals.len() > 1
        && !positionals[1].is_empty()
    {
        language = Some(positionals[1]);
    }

    // For quote/verse blocks, extract attribution (2nd pos) and citetitle (3rd pos)
    let mut attribution: Option<&'src str> = None;
    let mut citetitle: Option<&'src str> = None;
    if matches!(promoted_name, "quote" | "verse") {
        if positionals.len() > 1 && !positionals[1].is_empty() {
            attribution = Some(positionals[1].trim());
        }
        if positionals.len() > 2 && !positionals[2].is_empty() {
            citetitle = Some(positionals[2].trim());
        }
    }

    // Style is consumed (not emitted in output) for promotion styles
    let emit_style = match style {
        Some("source" | "listing" | "stem" | "verse" | "quote") => None,
        other => other,
    };

    let mut block = Block::new(promoted_name);
    block.form = raw.form;
    block.delimiter = raw.delimiter;
    block.id = raw.id.map(Cow::Borrowed);
    block.style = emit_style;
    block.target = raw.target;
    block.level = raw.level;
    block.variant = raw.variant;
    block.marker = raw.marker;
    block.location = raw.location;

    // Build metadata if we have roles, options, named attributes, or language
    // Filter out consumed attributes (caption, id, style are not emitted)
    let mut meta_attributes: std::collections::HashMap<&str, &str> = raw
        .named_attributes
        .into_iter()
        .filter(|(k, _)| !matches!(*k, "caption" | "id" | "style"))
        .collect();
    if let Some(lang) = language {
        meta_attributes.insert("language", lang);
    }
    if let Some(attr) = attribution {
        meta_attributes.insert("attribution", attr);
    }
    if let Some(cite) = citetitle {
        meta_attributes.insert("citetitle", cite);
    }
    if !raw.roles.is_empty() || !raw.options.is_empty() || !meta_attributes.is_empty() {
        block.metadata = Some(BlockMetadata {
            roles: raw.roles,
            options: raw.options,
            attributes: meta_attributes,
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

    // Auto-generate section IDs when no explicit ID is set and no style is applied
    if promoted_name == "section"
        && block.id.is_none()
        && style.is_none()
        && let Some(title_span) = raw.title_span
    {
        let title_text = &source[title_span.start..title_span.end];
        let slug = slugify(title_text);
        let count = id_registry.entry(slug.clone()).or_insert(0);
        *count += 1;
        if *count == 1 {
            block.id = Some(Cow::Owned(slug));
        } else {
            block.id = Some(Cow::Owned(format!("{slug}_{count}")));
        }
    }

    // Handle content based on block type (using promoted name)
    match promoted_name {
        // Verbatim blocks: content stays as raw text (with proper location)
        // Leading and trailing blank lines are stripped.
        "listing" | "literal" | "pass" | "stem" => {
            if let Some(content_span) = raw.content_span {
                let content = &source[content_span.start..content_span.end];
                // Strip leading and trailing blank lines
                let trimmed = content.trim_matches('\n');
                if trimmed.is_empty() {
                    block.inlines = Some(vec![]);
                } else {
                    let leading_stripped = content.len() - content.trim_start_matches('\n').len();
                    let trimmed_start = content_span.start + leading_stripped;
                    let trimmed_end = trimmed_start + trimmed.len();
                    let trimmed_span = SourceSpan {
                        start: trimmed_start,
                        end: trimmed_end,
                    };
                    let location = idx.location(&trimmed_span);
                    block.inlines = Some(vec![InlineNode::Text(TextNode {
                        value: trimmed,
                        location: Some(location),
                    })]);
                }
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

        // Verse blocks: content preserved as raw text with newlines
        "verse" => {
            if let Some(content_span) = raw.content_span {
                let content = &source[content_span.start..content_span.end];
                let location = idx.location(&content_span);
                block.inlines = Some(vec![InlineNode::Text(TextNode {
                    value: content,
                    location: Some(location),
                })]);
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
                        transform_raw_block_inner(raw_child, content, &content_idx, id_registry);
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
                        transform_raw_block_inner(raw_child, content, &content_idx, id_registry);
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
        "list" | "dlist" => {
            if let Some(items) = raw.items {
                let mut transformed_items = Vec::new();
                for item in items {
                    let (transformed, item_diags) =
                        transform_raw_block_inner(item, source, idx, id_registry);
                    diagnostics.extend(item_diags);
                    // List items always produce exactly one block
                    transformed_items.extend(transformed);
                }
                block.items = Some(transformed_items);
            }
        }

        // List items: parse principal span as inlines
        "listItem" | "dlistItem" => {
            if let Some(principal_span) = raw.principal_span {
                let content = &source[principal_span.start..principal_span.end];
                let content_tokens = lex(content);
                let content_idx = SourceIndex::new(content);
                let (mut principal, principal_diags) =
                    run_inline_parser(&content_tokens, content, &content_idx);
                diagnostics.extend(principal_diags);

                let base = get_base_position(source, idx, principal_span.start);
                for inline in &mut principal {
                    offset_inline_location(inline, base);
                }
                block.principal = Some(principal);
            }

            // Parse term spans as inlines (for description list items)
            if !raw.term_spans.is_empty() {
                let mut terms = Vec::new();
                for term_span in &raw.term_spans {
                    let content = &source[term_span.start..term_span.end];
                    let content_tokens = lex(content);
                    let content_idx = SourceIndex::new(content);
                    let (mut term_inlines, term_diags) =
                        run_inline_parser(&content_tokens, content, &content_idx);
                    diagnostics.extend(term_diags);

                    let base = get_base_position(source, idx, term_span.start);
                    for inline in &mut term_inlines {
                        offset_inline_location(inline, base);
                    }
                    terms.push(term_inlines);
                }
                block.terms = Some(terms);
            }

            // Parse continuation content as blocks (for list continuation `+`)
            if let Some(content_span) = raw.content_span {
                let content = &source[content_span.start..content_span.end];
                let content_tokens = lex(content);
                let content_idx = SourceIndex::new(content);
                let (raw_blocks, block_diags) =
                    super::boundary::parse_raw_blocks(&content_tokens, content, &content_idx);
                diagnostics.extend(block_diags);

                let base = get_base_position(source, idx, content_span.start);
                let mut child_blocks = Vec::new();
                for raw_child in raw_blocks {
                    let (mut children, child_diags) =
                        transform_raw_block_inner(raw_child, content, &content_idx, id_registry);
                    diagnostics.extend(child_diags);
                    for child in &mut children {
                        offset_block_locations(child, base);
                    }
                    child_blocks.extend(children);
                }
                block.blocks = Some(child_blocks);
            }

            // Transform nested blocks (nested lists from boundary parser)
            if let Some(nested_blocks) = raw.blocks {
                let mut transformed_nested = Vec::new();
                for nested in nested_blocks {
                    let (transformed, nested_diags) =
                        transform_raw_block_inner(nested, source, idx, id_registry);
                    diagnostics.extend(nested_diags);
                    transformed_nested.extend(transformed);
                }
                if let Some(existing) = &mut block.blocks {
                    existing.extend(transformed_nested);
                } else {
                    block.blocks = Some(transformed_nested);
                }
            }
        }

        // Breaks, headings, and block macros: no content to transform
        "break" | "heading" | "image" => {}

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
    id_registry: &mut HashMap<String, usize>,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let mut diagnostics = Vec::new();
    let mut result_blocks = Vec::new();

    // Create the heading block
    let mut heading = Block::new("heading");
    heading.level = raw.level;
    heading.id = raw.id.map(Cow::Borrowed);
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
                transform_raw_block_inner(raw_child, content, &content_idx, id_registry);
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
    let mut id_registry: HashMap<String, usize> = HashMap::new();

    for raw in raw_blocks {
        let (transformed, diags) = transform_raw_block_inner(raw, source, idx, &mut id_registry);
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

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

/// Result of style promotion analysis.
struct StylePromotion<'a> {
    name: &'static str,
    language: Option<&'a str>,
    attribution: Option<&'a str>,
    citetitle: Option<&'a str>,
    emit_style: Option<&'a str>,
}

/// Map a raw block name to a static string.
fn static_block_name(name: &str) -> &'static str {
    match name {
        "literal" => "literal",
        "listing" => "listing",
        "pass" => "pass",
        "quote" => "quote",
        "section" => "section",
        "example" => "example",
        "sidebar" => "sidebar",
        "open" => "open",
        "list" => "list",
        "dlist" => "dlist",
        "listItem" => "listItem",
        "dlistItem" => "dlistItem",
        "break" => "break",
        "heading" => "heading",
        "image" => "image",
        "verse" => "verse",
        "stem" => "stem",
        _ => "paragraph",
    }
}

/// Analyze style attributes and determine promoted name and extracted attributes.
fn analyze_style_promotion<'a>(raw: &RawBlock<'a>) -> StylePromotion<'a> {
    let mut promoted_name: &'static str = static_block_name(raw.name);
    let mut language: Option<&'a str> = None;
    let style = raw.style;
    let positionals = &raw.positionals;

    match (style, raw.name) {
        // [source] on literal block → listing
        (Some("source"), "literal") => {
            promoted_name = "listing";
            if positionals.len() > 1 && !positionals[1].is_empty() {
                language = Some(positionals[1]);
            }
        }
        // [source] or [source,lang] on listing → listing + language
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
        (Some("stem"), "pass") => promoted_name = "stem",
        // [verse] on quote → verse
        (Some("verse"), "quote") => promoted_name = "verse",
        _ => {}
    }

    // For fenced code blocks, extract language from positionals
    if raw.name == "listing"
        && raw.delimiter.is_some_and(|d| d.starts_with('`'))
        && positionals.len() > 1
        && !positionals[1].is_empty()
    {
        language = Some(positionals[1]);
    }

    // For quote/verse blocks, extract attribution and citetitle
    let mut attribution: Option<&'a str> = None;
    let mut citetitle: Option<&'a str> = None;
    if matches!(promoted_name, "quote" | "verse") {
        if positionals.len() > 1 && !positionals[1].is_empty() {
            attribution = Some(positionals[1].trim());
        }
        if positionals.len() > 2 && !positionals[2].is_empty() {
            citetitle = Some(positionals[2].trim());
        }
    }

    // Style is consumed for promotion styles
    let emit_style = match style {
        Some("source" | "listing" | "stem" | "verse" | "quote") => None,
        other => other,
    };

    StylePromotion {
        name: promoted_name,
        language,
        attribution,
        citetitle,
        emit_style,
    }
}

/// Parse a span as inlines with location adjustment.
fn parse_span_as_inlines<'src>(
    span: SourceSpan,
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<InlineNode<'src>>, Vec<ParseDiagnostic>) {
    let content = &source[span.start..span.end];
    let tokens = lex(content);
    let content_idx = SourceIndex::new(content);
    let (mut inlines, diags) = run_inline_parser(&tokens, content, &content_idx);

    let base = get_base_position(source, idx, span.start);
    for inline in &mut inlines {
        offset_inline_location(inline, base);
    }

    (inlines, diags)
}

/// Parse content span as child blocks with location adjustment.
fn parse_span_as_blocks<'src>(
    span: SourceSpan,
    source: &'src str,
    idx: &SourceIndex,
    id_registry: &mut HashMap<String, usize>,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let content = &source[span.start..span.end];
    let tokens = lex(content);
    let content_idx = SourceIndex::new(content);
    let (raw_blocks, mut diagnostics) =
        super::boundary::parse_raw_blocks(&tokens, content, &content_idx);

    let base = get_base_position(source, idx, span.start);
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

    (child_blocks, diagnostics)
}

/// Handle verbatim block content (listing, literal, pass, stem).
fn handle_verbatim_content<'src>(
    content_span: Option<SourceSpan>,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<Vec<InlineNode<'src>>> {
    let span = content_span?;
    let content = &source[span.start..span.end];
    let trimmed = content.trim_matches('\n');

    if trimmed.is_empty() {
        return Some(vec![]);
    }

    let leading_stripped = content.len() - content.trim_start_matches('\n').len();
    let trimmed_start = span.start + leading_stripped;
    let trimmed_span = SourceSpan {
        start: trimmed_start,
        end: trimmed_start + trimmed.len(),
    };
    let location = idx.location(&trimmed_span);

    Some(vec![InlineNode::Text(TextNode {
        value: trimmed,
        location: Some(location),
    })])
}

/// Handle list item content (principal, terms, continuation blocks, nested lists).
fn handle_list_item_content<'src>(
    raw: &mut RawBlock<'src>,
    block: &mut Block<'src>,
    source: &'src str,
    idx: &SourceIndex,
    id_registry: &mut HashMap<String, usize>,
) -> Vec<ParseDiagnostic> {
    let mut diagnostics = Vec::new();

    // Parse principal span as inlines
    if let Some(principal_span) = raw.principal_span {
        let (principal, diags) = parse_span_as_inlines(principal_span, source, idx);
        diagnostics.extend(diags);
        block.principal = Some(principal);
    }

    // Parse term spans as inlines (for description list items)
    if !raw.term_spans.is_empty() {
        let mut terms = Vec::with_capacity(raw.term_spans.len());
        for term_span in &raw.term_spans {
            let (term_inlines, diags) = parse_span_as_inlines(*term_span, source, idx);
            diagnostics.extend(diags);
            terms.push(term_inlines);
        }
        block.terms = Some(terms);
    }

    // Parse continuation content as blocks
    if let Some(content_span) = raw.content_span {
        let (child_blocks, diags) = parse_span_as_blocks(content_span, source, idx, id_registry);
        diagnostics.extend(diags);
        block.blocks = Some(child_blocks);
    }

    // Transform nested blocks (nested lists from boundary parser)
    // Use .take() to avoid cloning the Vec
    if let Some(nested_blocks) = raw.blocks.take() {
        let mut transformed_nested = Vec::with_capacity(nested_blocks.len());
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

    diagnostics
}

/// Handle content parsing based on block type.
fn handle_block_content<'src>(
    raw: &mut RawBlock<'src>,
    block: &mut Block<'src>,
    promoted_name: &str,
    source: &'src str,
    idx: &SourceIndex,
    id_registry: &mut HashMap<String, usize>,
) -> Vec<ParseDiagnostic> {
    let mut diagnostics = Vec::new();

    match promoted_name {
        "listing" | "literal" | "pass" | "stem" => {
            block.inlines =
                handle_verbatim_content(raw.content_span, source, idx).or_else(|| Some(vec![]));
        }

        "paragraph" => {
            if let Some(content_span) = raw.content_span {
                let (inlines, diags) = parse_span_as_inlines(content_span, source, idx);
                diagnostics.extend(diags);
                block.inlines = Some(inlines);
            } else {
                block.inlines = Some(vec![]);
            }
        }

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

        "example" | "sidebar" | "quote" | "open" | "section" => {
            if let Some(content_span) = raw.content_span {
                let (children, diags) =
                    parse_span_as_blocks(content_span, source, idx, id_registry);
                diagnostics.extend(diags);
                block.blocks = Some(children);
            } else {
                block.blocks = Some(vec![]);
            }
        }

        "list" | "dlist" => {
            // Use .take() to avoid cloning the Vec
            if let Some(items) = raw.items.take() {
                let mut transformed_items = Vec::with_capacity(items.len());
                for item in items {
                    let (transformed, item_diags) =
                        transform_raw_block_inner(item, source, idx, id_registry);
                    diagnostics.extend(item_diags);
                    transformed_items.extend(transformed);
                }
                block.items = Some(transformed_items);
            }
        }

        "listItem" | "dlistItem" => {
            let item_diags = handle_list_item_content(raw, block, source, idx, id_registry);
            diagnostics.extend(item_diags);
        }

        "break" | "heading" | "image" => {}

        _ => {
            if let Some(content_span) = raw.content_span {
                let (inlines, diags) = parse_span_as_inlines(content_span, source, idx);
                diagnostics.extend(diags);
                block.inlines = Some(inlines);
            }
        }
    }

    diagnostics
}

/// Transform a `RawBlock` into one or more Blocks by parsing inline content.
///
/// This is phase 3 of the parsing pipeline.
/// The `source` is the original full source, and `idx` is its `SourceIndex`.
///
/// Returns a vector because discrete sections expand into a heading plus sibling blocks.
///
/// An `id_registry` is threaded through for section ID deduplication.
fn transform_raw_block_inner<'src>(
    mut raw: RawBlock<'src>,
    source: &'src str,
    idx: &SourceIndex,
    id_registry: &mut HashMap<String, usize>,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    // Handle discrete sections specially
    if raw.name == "section" && raw.style == Some("discrete") {
        return transform_discrete_section(raw, source, idx, id_registry);
    }

    let mut diagnostics = Vec::new();

    // Analyze style and extract promoted name, language, etc.
    let promotion = analyze_style_promotion(&raw);

    // Build the block with basic fields
    let mut block = Block::new(promotion.name);
    block.form = raw.form;
    block.delimiter = raw.delimiter;
    block.id = raw.id.map(Cow::Borrowed);
    block.style = promotion.emit_style;
    block.target = raw.target;
    block.level = raw.level;
    block.variant = raw.variant;
    block.marker = raw.marker;
    block.location = raw.location;

    // Build metadata - move roles/options instead of cloning
    let mut meta_attributes: std::collections::HashMap<&str, &str> = raw
        .named_attributes
        .iter()
        .filter(|(k, _)| !matches!(**k, "caption" | "id" | "style"))
        .map(|(k, v)| (*k, *v))
        .collect();
    if let Some(lang) = promotion.language {
        meta_attributes.insert("language", lang);
    }
    if let Some(attr) = promotion.attribution {
        meta_attributes.insert("attribution", attr);
    }
    if let Some(cite) = promotion.citetitle {
        meta_attributes.insert("citetitle", cite);
    }
    // Move roles and options, avoiding clone
    let roles = std::mem::take(&mut raw.roles);
    let options = std::mem::take(&mut raw.options);
    if !roles.is_empty() || !options.is_empty() || !meta_attributes.is_empty() {
        block.metadata = Some(BlockMetadata {
            roles,
            options,
            attributes: meta_attributes,
        });
    }

    // Parse title span as inlines
    if let Some(title_span) = raw.title_span {
        let (inlines, diags) = parse_span_as_inlines(title_span, source, idx);
        diagnostics.extend(diags);
        block.title = Some(inlines);
    }

    // Parse reftext span as inlines
    if let Some(reftext_span) = raw.reftext_span {
        let (inlines, diags) = parse_span_as_inlines(reftext_span, source, idx);
        diagnostics.extend(diags);
        block.reftext = Some(inlines);
    }

    // Auto-generate section IDs
    if promotion.name == "section"
        && block.id.is_none()
        && raw.style.is_none()
        && let Some(title_span) = raw.title_span
    {
        let title_text = &source[title_span.start..title_span.end];
        let slug = slugify(title_text);
        let count = id_registry.entry(slug.clone()).or_insert(0);
        *count += 1;
        block.id = Some(Cow::Owned(if *count == 1 {
            slug
        } else {
            format!("{slug}_{count}")
        }));
    }

    // Handle content based on block type
    let content_diags =
        handle_block_content(&mut raw, &mut block, promotion.name, source, idx, id_registry);
    diagnostics.extend(content_diags);

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

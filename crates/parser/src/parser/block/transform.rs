//! Phase 3: Transform `RawBlock` to Block.
//!
//! This module walks the `RawBlock` tree from phase 2 and:
//! 1. Parses inline content for basic-content-model blocks
//! 2. Recursively parses compound block content
//! 3. Parses title spans as inlines

use super::raw_block::RawBlock;
use smallvec::SmallVec;
use std::borrow::Cow;
use std::collections::HashMap;

use crate::asg::{Block, BlockMetadata, InlineNode, TextNode};
use crate::diagnostic::ParseDiagnostic;
use crate::lexer::lex_with_offset;
use crate::parser::inline::run_inline_parser;
use crate::span::{SourceIndex, SourceSpan};

/// Generate a slug from a section title for auto-ID generation.
///
/// Algorithm: lowercase, non-alphanumeric → `_`, prefix with `_`, collapse
/// consecutive underscores, trim trailing underscores.
fn slugify(title: &str) -> String {
    let mut slug = String::with_capacity(title.len() + 1);
    slug.push('_');
    let mut prev_underscore = true;
    for ch in title.chars() {
        if ch.is_ascii_alphanumeric() {
            slug.push(ch.to_ascii_lowercase());
            prev_underscore = false;
        } else if !prev_underscore {
            slug.push('_');
            prev_underscore = true;
        }
    }
    slug.truncate(slug.trim_end_matches('_').len());
    slug
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
        "table" => "table",
        "tableRow" => "tableRow",
        "tableCell" => "tableCell",
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

/// Parse a span as inlines using the full document source and index.
///
/// Tokens are lexed from the substring but their spans are shifted by
/// `span.start` so that all positions are global. The parent `SourceIndex`
/// converts these global spans directly — no post-parse offset traversal.
fn parse_span_as_inlines<'src>(
    span: SourceSpan,
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<InlineNode<'src>>, Vec<ParseDiagnostic>) {
    let content = &source[span.start..span.end];
    let tokens = lex_with_offset(content, span.start);
    run_inline_parser(&tokens, source, idx)
}

/// Parse content span as child blocks using the full document source and index.
///
/// Tokens are lexed from the substring but shifted by `span.start` so all
/// positions are global. The parent `source` and `SourceIndex` are passed
/// through to block/inline parsers — no post-parse offset traversal.
fn parse_span_as_blocks<'src>(
    span: SourceSpan,
    source: &'src str,
    idx: &SourceIndex,
    id_registry: &mut HashMap<String, usize>,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let content = &source[span.start..span.end];
    let tokens = lex_with_offset(content, span.start);
    let (raw_blocks, mut diagnostics) = super::boundary::parse_raw_blocks(&tokens, source, idx);

    let mut child_blocks = Vec::with_capacity(raw_blocks.len());
    for raw_child in raw_blocks {
        let (children, child_diags) =
            transform_raw_block_inner(raw_child, source, idx, id_registry);
        diagnostics.extend(child_diags);
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

/// Find cell boundaries in table content.
///
/// Returns a list of byte-offset pairs `(start, end)` for each cell's content,
/// relative to the original source string.
fn find_cell_boundaries(content: &str, base_offset: usize) -> Vec<(usize, usize)> {
    let mut cells = Vec::new();
    let mut i = 0;
    let bytes = content.as_bytes();

    while i < bytes.len() {
        if bytes[i] == b'|' {
            // Start of a new cell - find the content
            let cell_content_start = i + 1;

            // Scan forward for the next unescaped `|` or end of content
            let mut j = cell_content_start;
            while j < bytes.len() {
                if bytes[j] == b'|' && (j == 0 || bytes[j - 1] != b'\\') {
                    break;
                }
                j += 1;
            }

            let cell_text = &content[cell_content_start..j];
            let trimmed = cell_text.trim();
            if trimmed.is_empty() {
                // Empty cell - still track it with zero-width span
                cells.push((
                    base_offset + cell_content_start,
                    base_offset + cell_content_start,
                ));
            } else {
                // Find the trimmed content's offset within the cell
                let leading_ws = cell_text.len() - cell_text.trim_start().len();
                let start = base_offset + cell_content_start + leading_ws;
                let end = start + trimmed.len();
                cells.push((start, end));
            }

            i = j; // next iteration will see the `|` and start a new cell
        } else {
            i += 1;
        }
    }

    cells
}

/// Parse table content into rows and cells.
///
/// Table content between `|===` delimiters is parsed as follows:
/// - `|` characters delimit cells
/// - Blank lines separate rows
/// - If no blank lines, cells are auto-distributed based on column count from first row
/// - The first row is marked as a header if followed by a blank line
fn parse_table_content<'src>(
    content_span: SourceSpan,
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let content = &source[content_span.start..content_span.end];
    let mut diagnostics = Vec::new();
    let mut all_rows: Vec<Block<'src>> = Vec::new();

    // Split content into row groups by blank lines
    let mut row_groups: Vec<&str> = Vec::new();
    let mut row_group_offsets: Vec<usize> = Vec::new();
    let mut current_start: Option<usize> = None;

    for (line_idx, line) in content.split('\n').enumerate() {
        let line_offset: usize = if line_idx == 0 {
            0
        } else {
            content[..content
                .match_indices('\n')
                .nth(line_idx - 1)
                .map_or(0, |(pos, _)| pos + 1)]
                .len()
        };

        if line.trim().is_empty() {
            // Blank line - end current group
            if let Some(start) = current_start.take() {
                let end = line_offset.saturating_sub(1).max(start);
                if start < content.len() && start <= end {
                    row_groups.push(&content[start..end]);
                    row_group_offsets.push(content_span.start + start);
                }
            }
        } else if current_start.is_none() {
            current_start = Some(line_offset);
        }
    }

    // Handle the last group
    if let Some(start) = current_start {
        let end = content.len();
        if start < end {
            row_groups.push(&content[start..end]);
            row_group_offsets.push(content_span.start + start);
        }
    }

    if row_groups.is_empty() {
        return (all_rows, diagnostics);
    }

    // Detect implicit header: first group followed by a blank line
    let has_header = row_groups.len() > 1
        || (row_groups.len() == 1 && {
            // Check if there's a blank line after first non-blank content
            let first_group_end = row_group_offsets[0] - content_span.start + row_groups[0].len();
            first_group_end < content.len()
        });

    // Determine column count from the first LINE of the first group
    let first_line = row_groups[0].split('\n').next().unwrap_or("");
    let first_line_cells = find_cell_boundaries(first_line, row_group_offsets[0]);
    let col_count = first_line_cells.len().max(1);

    // Process each row group
    for (group_idx, (group, base_offset)) in
        row_groups.iter().zip(row_group_offsets.iter()).enumerate()
    {
        let cells = find_cell_boundaries(group, *base_offset);

        if cells.is_empty() {
            continue;
        }

        // If we have blank-line-separated groups, each group is one row
        // If cells need auto-wrapping (single group, no blank lines), distribute by col_count
        let rows_of_cells: Vec<Vec<(usize, usize)>> = if row_groups.len() > 1 {
            // Blank-line separated: each group is one row
            vec![cells]
        } else {
            // Auto-distribute cells into rows based on column count
            cells
                .chunks(col_count)
                .map(<[(usize, usize)]>::to_vec)
                .collect()
        };

        for (row_idx, row_cells) in rows_of_cells.into_iter().enumerate() {
            let is_header = has_header && group_idx == 0 && row_idx == 0 && row_groups.len() > 1;

            let mut row_block = Block::new("tableRow");
            if is_header {
                row_block.variant = Some("header");
            }

            let mut cell_blocks = Vec::with_capacity(row_cells.len());
            for (cell_start, cell_end) in row_cells {
                let mut cell_block = Block::new("tableCell");

                if cell_start < cell_end {
                    let cell_span = SourceSpan {
                        start: cell_start,
                        end: cell_end,
                    };
                    let (inlines, diags) = parse_span_as_inlines(cell_span, source, idx);
                    diagnostics.extend(diags);
                    cell_block.inlines = Some(inlines);
                    cell_block.location = Some(idx.location(&cell_span));
                } else {
                    cell_block.inlines = Some(vec![]);
                }

                cell_blocks.push(cell_block);
            }

            row_block.items = Some(cell_blocks);

            // Compute row location from first to last cell
            if let Some(first_cell) = row_block.items.as_ref().and_then(|items| items.first())
                && let Some(last_cell) = row_block.items.as_ref().and_then(|items| items.last())
                && let (Some(first_loc), Some(last_loc)) = (first_cell.location, last_cell.location)
            {
                row_block.location = Some([first_loc[0], last_loc[1]]);
            }

            all_rows.push(row_block);
        }
    }

    (all_rows, diagnostics)
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

        "table" => {
            if let Some(content_span) = raw.content_span {
                let (rows, diags) = parse_table_content(content_span, source, idx);
                diagnostics.extend(diags);
                block.items = Some(rows);
            } else {
                block.items = Some(vec![]);
            }
        }

        "tableRow" | "tableCell" | "break" | "heading" | "image" => {}

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
    let mut meta_attributes: SmallVec<[(&str, &str); 4]> = raw
        .named_attributes
        .iter()
        .filter(|(k, _)| !matches!(*k, "caption" | "id" | "style"))
        .copied()
        .collect();
    if let Some(lang) = promotion.language {
        meta_attributes.push(("language", lang));
    }
    if let Some(attr) = promotion.attribution {
        meta_attributes.push(("attribution", attr));
    }
    if let Some(cite) = promotion.citetitle {
        meta_attributes.push(("citetitle", cite));
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
        // Avoid cloning slug for the common case of first occurrence
        block.id = Some(Cow::Owned(
            if let Some(count) = id_registry.get_mut(&slug) {
                *count += 1;
                format!("{slug}_{count}")
            } else {
                id_registry.insert(slug.clone(), 1);
                slug
            },
        ));
    }

    // Handle content based on block type
    let content_diags = handle_block_content(
        &mut raw,
        &mut block,
        promotion.name,
        source,
        idx,
        id_registry,
    );
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
            attributes: SmallVec::new(),
        });
    }

    // Parse title span as inlines
    if let Some(title_span) = raw.title_span {
        let (title_inlines, title_diags) = parse_span_as_inlines(title_span, source, idx);
        diagnostics.extend(title_diags);
        heading.title = Some(title_inlines);
    }

    result_blocks.push(heading);

    // Parse body content as sibling blocks (not children)
    if let Some(content_span) = raw.content_span {
        let (child_blocks, block_diags) =
            parse_span_as_blocks(content_span, source, idx, id_registry);
        diagnostics.extend(block_diags);
        result_blocks.extend(child_blocks);
    }

    (result_blocks, diagnostics)
}

/// Transform a vector of `RawBlock`s into Blocks.
pub(super) fn transform_raw_blocks<'src>(
    raw_blocks: Vec<RawBlock<'src>>,
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let mut blocks = Vec::with_capacity(raw_blocks.len());
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
    use crate::lexer::lex;
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

    #[test]
    fn test_transform_simple_table() {
        let source = "|===\n|Cell 1 |Cell 2\n|Cell 3 |Cell 4\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "table");
        assert_eq!(blocks[0].form, Some("delimited"));
        assert_eq!(blocks[0].delimiter, Some("|==="));

        let rows = blocks[0].items.as_ref().expect("should have rows");
        // With no blank lines, cells are auto-distributed: 4 cells / 2 cols = 2 rows
        assert_eq!(rows.len(), 2, "expected 2 rows, got {}", rows.len());

        for row in rows {
            assert_eq!(row.name, "tableRow");
            let cells = row.items.as_ref().expect("row should have cells");
            assert_eq!(cells.len(), 2);
            for cell in cells {
                assert_eq!(cell.name, "tableCell");
                assert!(cell.inlines.is_some());
            }
        }
    }

    #[test]
    fn test_transform_table_with_header() {
        let source = "|===\n|Header 1 |Header 2\n\n|Cell 1 |Cell 2\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);

        let rows = blocks[0].items.as_ref().expect("should have rows");
        assert_eq!(rows.len(), 2);

        // First row should be header
        assert_eq!(rows[0].variant, Some("header"));
        // Second row should not be header
        assert!(rows[1].variant.is_none());
    }

    #[test]
    fn test_transform_table_cell_content_parsed_as_inlines() {
        let source = "|===\n|*bold* text |plain\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        let cells = rows[0].items.as_ref().expect("row should have cells");

        // First cell should have parsed inlines with bold
        let inlines = cells[0].inlines.as_ref().expect("cell should have inlines");
        assert!(
            inlines.len() >= 2,
            "expected at least strong + text, got {inlines:?}"
        );
    }

    #[test]
    fn test_transform_empty_table() {
        let source = "|===\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "table");
        let rows = blocks[0].items.as_ref().expect("should have items");
        assert!(rows.is_empty());
    }

    #[test]
    fn test_transform_table_one_cell_per_line() {
        let source = "|===\n|Cell A\n|Cell B\n\n|Cell C\n|Cell D\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        // Two blank-line-separated groups: each with 2 cells
        assert_eq!(rows.len(), 2);

        for row in rows {
            let cells = row.items.as_ref().expect("row should have cells");
            assert_eq!(cells.len(), 2);
        }
    }

    #[test]
    fn test_transform_table_auto_wrap_rows() {
        // 6 cells with 3 detected from first "row" (all cells, no blank lines)
        let source = "|===\n|A |B |C\n|D |E |F\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        assert_eq!(rows.len(), 2, "6 cells / 3 cols = 2 rows");

        for row in rows {
            let cells = row.items.as_ref().expect("row should have cells");
            assert_eq!(cells.len(), 3);
        }
    }
}

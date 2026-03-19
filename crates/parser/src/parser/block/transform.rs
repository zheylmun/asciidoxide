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

/// Parsed cell specifier (content before `|`).
///
/// Format: `[colspan][.rowspan][+][halign][valign][style]`
/// Examples: `2+`, `.3+`, `2.3+`, `>`, `.^`, `a`, `s`, `3*>s`
#[derive(Debug, Clone, Default)]
struct CellSpec {
    /// Number of columns this cell spans (default 1).
    colspan: usize,
    /// Number of rows this cell spans (default 1).
    rowspan: usize,
    /// Content style: `a` (asciidoc), `e` (emphasis), `l` (literal),
    /// `m` (monospace), `s` (strong), `h` (header).
    style: Option<char>,
    /// Horizontal alignment: `<` (left), `>` (right), `^` (center).
    halign: Option<char>,
    /// Vertical alignment: `.<` (top), `.>` (bottom), `.^` (middle).
    valign: Option<char>,
    /// Duplication factor (e.g., `3*` duplicates cell content into 3 cells).
    duplication: usize,
}

/// Parse a cell specifier string (content between previous cell end and `|`).
fn parse_cell_spec(spec: &str) -> CellSpec {
    let spec = spec.trim();
    if spec.is_empty() {
        return CellSpec {
            colspan: 1,
            rowspan: 1,
            duplication: 1,
            ..CellSpec::default()
        };
    }

    let mut result = CellSpec {
        colspan: 1,
        rowspan: 1,
        duplication: 1,
        ..CellSpec::default()
    };

    let bytes = spec.as_bytes();
    let mut i = 0;

    // Parse leading number (could be colspan, duplication, or just a number)
    let first_num = parse_number(bytes, &mut i);

    if i < bytes.len() && bytes[i] == b'.' {
        // colspan.rowspan pattern
        if let Some(n) = first_num {
            result.colspan = n;
        }
        i += 1; // skip '.'
        let row_num = parse_number(bytes, &mut i);
        if i < bytes.len() && bytes[i] == b'+' {
            // colspan.rowspan+
            result.rowspan = row_num.unwrap_or(1);
            i += 1;
        } else if let Some(n) = row_num {
            // Could be .N+ (rowspan only, no colspan prefix)
            // But we already consumed the dot after a number, so this is colspan.rowspan
            result.rowspan = n;
            // Check for + after
            if i < bytes.len() && bytes[i] == b'+' {
                i += 1;
            }
        } else {
            // Just a dot followed by alignment chars: .^ or .< or .>
            // The dot was a valign indicator
            // Reparse: first_num was colspan? No - back up.
            // Actually if first_num is set and we got `.` with no number after,
            // this could be `2.^` meaning colspan=2, valign=^
            if i < bytes.len() && matches!(bytes[i], b'^' | b'<' | b'>') {
                result.valign = Some(bytes[i] as char);
                i += 1;
            }
        }
    } else if i < bytes.len() && bytes[i] == b'+' {
        // N+ pattern: colspan only
        if let Some(n) = first_num {
            result.colspan = n;
        }
        i += 1;
    } else if i < bytes.len() && bytes[i] == b'*' {
        // N* pattern: duplication
        if let Some(n) = first_num {
            result.duplication = n;
        }
        i += 1;
    }

    // If we had no first_num and first char is '.', parse rowspan
    if first_num.is_none() && i == 0 && !bytes.is_empty() && bytes[0] == b'.' {
        i = 1; // skip '.'
        let row_num = parse_number(bytes, &mut i);
        if i < bytes.len() && bytes[i] == b'+' {
            result.rowspan = row_num.unwrap_or(1);
            i += 1;
        } else if let Some(_n) = row_num {
            // .N without + is valign position? No - .2+ is rowspan.
            // Just .N isn't standard, treat as nothing special
        } else if i < bytes.len() && matches!(bytes[i], b'^' | b'<' | b'>') {
            // .^ or .< or .> is vertical alignment
            result.valign = Some(bytes[i] as char);
            i += 1;
        }
    }

    // Parse remaining: alignment and style chars
    while i < bytes.len() {
        match bytes[i] {
            b'<' | b'>' | b'^' => {
                if result.halign.is_none() {
                    result.halign = Some(bytes[i] as char);
                }
            }
            b'a' | b'e' | b'l' | b'm' | b's' | b'h' | b'd' => {
                result.style = Some(bytes[i] as char);
            }
            _ => {}
        }
        i += 1;
    }

    result
}

/// Parse a decimal number from bytes starting at position `i`, advancing `i`.
fn parse_number(bytes: &[u8], i: &mut usize) -> Option<usize> {
    let start = *i;
    while *i < bytes.len() && bytes[*i].is_ascii_digit() {
        *i += 1;
    }
    if *i > start {
        std::str::from_utf8(&bytes[start..*i])
            .ok()
            .and_then(|s| s.parse().ok())
    } else {
        None
    }
}

/// Info about a parsed table cell.
#[derive(Debug, Clone)]
struct CellInfo {
    /// Content span (start, end) as byte offsets into source.
    content_start: usize,
    content_end: usize,
    /// Cell specifier parsed from content before the `|`.
    spec: CellSpec,
}

/// Find cells in table content, parsing cell specifiers.
///
/// In `AsciiDoc`, a cell specifier appears immediately before the `|` that starts
/// a cell. At the start of a line, text before the first `|` is treated as a
/// specifier. Between cells on the same line, text after a space following cell
/// content is treated as a specifier for the next cell.
fn find_cells(content: &str, base_offset: usize) -> Vec<CellInfo> {
    let mut cells = Vec::new();

    // Find all pipe positions that start cells
    let mut pipe_positions: Vec<usize> = Vec::new();
    let bytes = content.as_bytes();
    for (i, &b) in bytes.iter().enumerate() {
        if b == b'|' && (i == 0 || bytes[i - 1] != b'\\') {
            pipe_positions.push(i);
        }
    }

    for (pipe_idx, &pipe_pos) in pipe_positions.iter().enumerate() {
        // Extract specifier: text on the same line before this pipe
        let spec = extract_spec_before_pipe(content, pipe_pos);

        let cell_content_start = pipe_pos + 1;
        let cell_content_end = if pipe_idx + 1 < pipe_positions.len() {
            let next_pipe = pipe_positions[pipe_idx + 1];
            // Content ends before the next pipe's specifier
            let spec_len = spec_length_before_pipe(content, next_pipe);
            next_pipe - spec_len
        } else {
            content.len()
        };

        let cell_text = &content[cell_content_start..cell_content_end];
        let trimmed = cell_text.trim();

        let duplication = spec.duplication.max(1);
        for _ in 0..duplication {
            if trimmed.is_empty() {
                cells.push(CellInfo {
                    content_start: base_offset + cell_content_start,
                    content_end: base_offset + cell_content_start,
                    spec: spec.clone(),
                });
            } else {
                let leading_ws = cell_text.len() - cell_text.trim_start().len();
                let start = base_offset + cell_content_start + leading_ws;
                let end = start + trimmed.len();
                cells.push(CellInfo {
                    content_start: start,
                    content_end: end,
                    spec: spec.clone(),
                });
            }
        }
    }

    cells
}

/// Extract a cell specifier from text immediately before a pipe character.
///
/// Looks backward from the pipe position to find specifier characters.
/// A specifier is a short sequence of spec characters at the start of a line
/// or after whitespace.
fn extract_spec_before_pipe(content: &str, pipe_pos: usize) -> CellSpec {
    let default_spec = CellSpec {
        colspan: 1,
        rowspan: 1,
        duplication: 1,
        ..CellSpec::default()
    };

    if pipe_pos == 0 {
        return default_spec;
    }

    // Walk backward from pipe to find the spec text
    let bytes = content.as_bytes();
    let mut start = pipe_pos;
    while start > 0 {
        let prev = bytes[start - 1];
        if is_spec_char(prev) {
            start -= 1;
        } else {
            break;
        }
    }

    if start == pipe_pos {
        return default_spec;
    }

    // Spec must be at start of line or preceded by whitespace
    if start > 0 && !bytes[start - 1].is_ascii_whitespace() {
        return default_spec;
    }

    let spec_text = &content[start..pipe_pos];
    parse_cell_spec(spec_text)
}

/// Calculate the length of a specifier before a pipe position.
fn spec_length_before_pipe(content: &str, pipe_pos: usize) -> usize {
    if pipe_pos == 0 {
        return 0;
    }

    let bytes = content.as_bytes();
    let mut start = pipe_pos;
    while start > 0 && is_spec_char(bytes[start - 1]) {
        start -= 1;
    }

    // Spec must be at start of line or preceded by whitespace
    if start < pipe_pos && (start == 0 || bytes[start - 1].is_ascii_whitespace()) {
        pipe_pos - start
    } else {
        0
    }
}

/// Check if a byte is a valid cell specifier character.
fn is_spec_char(b: u8) -> bool {
    b.is_ascii_digit()
        || matches!(
            b,
            b'.' | b'+'
                | b'*'
                | b'<'
                | b'>'
                | b'^'
                | b'a'
                | b'e'
                | b'l'
                | b'm'
                | b's'
                | b'h'
                | b'd'
        )
}

/// Parse the `cols` attribute to determine column count.
///
/// Supported formats:
/// - `"1,2,3"` → 3 columns
/// - `"3*"` → 3 columns
/// - `"2*,1"` → 3 columns (2 from multiplier + 1)
/// - `"<,^,>"` → 3 columns (alignment-only specs)
/// - `".^~,3*"` → 4 columns
fn parse_cols_count(cols: &str) -> Option<usize> {
    if cols.is_empty() {
        return None;
    }

    let mut count = 0;
    for spec in cols.split(',') {
        let spec = spec.trim();
        if spec.is_empty() {
            count += 1;
            continue;
        }
        // Check for multiplier: N* pattern
        if let Some(star_pos) = spec.find('*') {
            if let Ok(n) = spec[..star_pos].trim().parse::<usize>() {
                count += n;
            } else {
                count += 1;
            }
        } else {
            count += 1;
        }
    }

    if count > 0 { Some(count) } else { None }
}

/// Table parsing options derived from block attributes and options.
struct TableOptions {
    /// Column count from `cols` attribute, if specified.
    col_count: Option<usize>,
    /// Whether `%header` option is set.
    has_header_option: bool,
    /// Whether `%footer` option is set.
    has_footer_option: bool,
}

/// Extract table options from a raw block's metadata.
fn extract_table_options(raw: &RawBlock<'_>) -> TableOptions {
    let col_count = raw
        .named_attributes
        .iter()
        .find(|(k, _)| *k == "cols")
        .and_then(|(_, v)| parse_cols_count(v));

    let has_header_option = raw.options.contains(&"header");
    let has_footer_option = raw.options.contains(&"footer");
    TableOptions {
        col_count,
        has_header_option,
        has_footer_option,
    }
}

/// Map cell style character to a style name string.
fn cell_style_name(ch: char) -> Option<&'static str> {
    match ch {
        'a' => Some("asciidoc"),
        'e' => Some("emphasis"),
        'l' => Some("literal"),
        'm' => Some("monospace"),
        's' => Some("strong"),
        'h' => Some("header"),
        'd' => Some("default"),
        _ => None,
    }
}

/// Map alignment character to alignment name.
fn halign_name(ch: char) -> Option<&'static str> {
    match ch {
        '<' => Some("left"),
        '>' => Some("right"),
        '^' => Some("center"),
        _ => None,
    }
}

/// Map vertical alignment character to alignment name.
fn valign_name(ch: char) -> Option<&'static str> {
    match ch {
        '<' => Some("top"),
        '>' => Some("bottom"),
        '^' => Some("middle"),
        _ => None,
    }
}

/// Split table content into row groups separated by blank lines.
///
/// Returns parallel vectors of group slices and their byte offsets into source.
fn split_row_groups(content: &str, base_offset: usize) -> (Vec<&str>, Vec<usize>) {
    let mut groups: Vec<&str> = Vec::new();
    let mut offsets: Vec<usize> = Vec::new();
    let mut current_start: Option<usize> = None;
    let mut offset: usize = 0;

    for line in content.split('\n') {
        if line.trim().is_empty() {
            if let Some(start) = current_start.take() {
                let end = offset.saturating_sub(1).max(start);
                if start < content.len() && start <= end {
                    groups.push(&content[start..end]);
                    offsets.push(base_offset + start);
                }
            }
        } else if current_start.is_none() {
            current_start = Some(offset);
        }
        offset += line.len() + 1;
    }

    if let Some(start) = current_start {
        let end = content.len();
        if start < end {
            groups.push(&content[start..end]);
            offsets.push(base_offset + start);
        }
    }

    (groups, offsets)
}

/// Parse table content into rows and cells.
///
/// Table content between `|===` delimiters is parsed as follows:
/// - `|` characters delimit cells, with optional specifiers before `|`
/// - Blank lines separate rows
/// - If no blank lines, cells are auto-distributed based on column count
/// - Column count is determined from `cols` attribute, else from first row
/// - `%header`/`%footer` options mark first/last rows
/// - Implicit header: first row followed by blank line (when no `%header`)
/// - Cell specifiers control colspan, rowspan, alignment, and content style
fn parse_table_content<'src>(
    content_span: SourceSpan,
    source: &'src str,
    idx: &SourceIndex,
    options: &TableOptions,
    id_registry: &mut HashMap<String, usize>,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let content = &source[content_span.start..content_span.end];
    let mut diagnostics = Vec::new();
    let mut all_rows: Vec<Block<'src>> = Vec::new();

    let (row_groups, row_group_offsets) = split_row_groups(content, content_span.start);

    if row_groups.is_empty() {
        return (all_rows, diagnostics);
    }

    // Detect implicit header: first group followed by a blank line (and no explicit %header)
    let has_implicit_header = !options.has_header_option && row_groups.len() > 1;

    // Determine column count
    let col_count = if let Some(c) = options.col_count {
        c
    } else {
        // Auto-detect from first line of first group
        let first_line = row_groups[0].split('\n').next().unwrap_or("");
        let first_line_cells = find_cells(first_line, row_group_offsets[0]);
        first_line_cells.len().max(1)
    };

    // Process each row group into rows
    let total_groups = row_groups.len();
    for (group_idx, (group, base_offset)) in
        row_groups.iter().zip(row_group_offsets.iter()).enumerate()
    {
        let cells = find_cells(group, *base_offset);

        if cells.is_empty() {
            continue;
        }

        // If we have blank-line-separated groups, each group is one row
        // Otherwise, auto-distribute cells into rows based on column count
        let rows_of_cells: Vec<Vec<CellInfo>> = if total_groups > 1 {
            vec![cells]
        } else {
            cells.chunks(col_count).map(<[CellInfo]>::to_vec).collect()
        };

        let total_rows_in_group = rows_of_cells.len();
        for (row_idx, row_cells) in rows_of_cells.into_iter().enumerate() {
            // Determine row variant
            let is_header = (options.has_header_option || has_implicit_header)
                && group_idx == 0
                && row_idx == 0;
            let is_footer = options.has_footer_option
                && group_idx == total_groups - 1
                && row_idx == total_rows_in_group - 1;

            let mut row_block = Block::new("tableRow");
            if is_header {
                row_block.variant = Some("header");
            } else if is_footer {
                row_block.variant = Some("footer");
            }

            let mut cell_blocks = Vec::with_capacity(row_cells.len());
            for cell_info in row_cells {
                let mut cell_block = Block::new("tableCell");

                // Apply cell specifier to block
                apply_cell_spec(&cell_info.spec, &mut cell_block);

                if cell_info.content_start < cell_info.content_end {
                    let cell_span = SourceSpan {
                        start: cell_info.content_start,
                        end: cell_info.content_end,
                    };

                    // Parse content based on cell style
                    if cell_info.spec.style == Some('a') {
                        // AsciiDoc cell: parse as blocks
                        let (blocks, diags) =
                            parse_span_as_blocks(cell_span, source, idx, id_registry);
                        diagnostics.extend(diags);
                        cell_block.blocks = Some(blocks);
                    } else {
                        let (inlines, diags) = parse_span_as_inlines(cell_span, source, idx);
                        diagnostics.extend(diags);
                        cell_block.inlines = Some(inlines);
                    }
                    cell_block.location = Some(idx.location(&cell_span));
                } else if cell_info.spec.style == Some('a') {
                    cell_block.blocks = Some(vec![]);
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

/// Apply a cell specifier to a cell Block, setting metadata attributes.
fn apply_cell_spec(spec: &CellSpec, cell: &mut Block<'_>) {
    let mut attrs: SmallVec<[(&str, &str); 4]> = SmallVec::new();

    if spec.colspan > 1 {
        // Store as a leaked string to get 'static lifetime — acceptable for small counts
        attrs.push(("colspan", leak_usize(spec.colspan)));
    }
    if spec.rowspan > 1 {
        attrs.push(("rowspan", leak_usize(spec.rowspan)));
    }
    if let Some(h) = spec.halign.and_then(halign_name) {
        attrs.push(("halign", h));
    }
    if let Some(v) = spec.valign.and_then(valign_name) {
        attrs.push(("valign", v));
    }

    if let Some(style_name) = spec.style.and_then(cell_style_name) {
        cell.style = Some(style_name);
    }

    if !attrs.is_empty() {
        cell.metadata = Some(BlockMetadata {
            roles: SmallVec::new(),
            options: SmallVec::new(),
            attributes: attrs,
        });
    }
}

/// Convert a usize to a &'static str. Uses a small set of pre-allocated strings
/// for common values, falling back to a leaked allocation for larger numbers.
fn leak_usize(n: usize) -> &'static str {
    match n {
        2 => "2",
        3 => "3",
        4 => "4",
        5 => "5",
        6 => "6",
        7 => "7",
        8 => "8",
        9 => "9",
        10 => "10",
        _ => Box::leak(n.to_string().into_boxed_str()),
    }
}

/// Handle content parsing based on block type.
fn handle_block_content<'src>(
    raw: &mut RawBlock<'src>,
    block: &mut Block<'src>,
    promoted_name: &str,
    source: &'src str,
    idx: &SourceIndex,
    id_registry: &mut HashMap<String, usize>,
    table_options: &TableOptions,
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
                let (rows, diags) =
                    parse_table_content(content_span, source, idx, table_options, id_registry);
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

    // Extract table options before roles/options are taken
    let table_options = if promotion.name == "table" {
        extract_table_options(&raw)
    } else {
        TableOptions {
            col_count: None,
            has_header_option: false,
            has_footer_option: false,
        }
    };

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
        &table_options,
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

    // Phase 2 tests: cols attribute, %header/%footer, cell specifiers, cell styles

    #[test]
    fn test_parse_cols_count() {
        assert_eq!(parse_cols_count("1,2,3"), Some(3));
        assert_eq!(parse_cols_count("3*"), Some(3));
        assert_eq!(parse_cols_count("2*,1"), Some(3));
        assert_eq!(parse_cols_count("<,^,>"), Some(3));
        assert_eq!(parse_cols_count(""), None);
        assert_eq!(parse_cols_count("1"), Some(1));
        assert_eq!(parse_cols_count("1,1,1,1"), Some(4));
    }

    #[test]
    fn test_table_cols_multiplier() {
        // cols="3*" means 3 columns, so 6 cells → 2 rows
        let source = "[cols=\"3*\"]\n|===\n|A |B |C |D |E |F\n|===\n";
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

    #[test]
    fn test_table_cols_multiplier_4() {
        // cols="4*" means 4 columns
        let source = "[cols=\"4*\"]\n|===\n|A |B |C |D |E |F |G |H\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        assert_eq!(rows.len(), 2, "8 cells / 4 cols = 2 rows");
        for row in rows {
            let cells = row.items.as_ref().expect("row should have cells");
            assert_eq!(cells.len(), 4);
        }
    }

    #[test]
    fn test_table_explicit_header_option() {
        // [%header] marks first row as header even without blank line
        let source = "[%header]\n|===\n|H1 |H2\n|C1 |C2\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].variant, Some("header"));
        assert!(rows[1].variant.is_none());
    }

    #[test]
    fn test_table_footer_option() {
        // [%footer] marks last row as footer
        let source = "[%footer]\n|===\n|C1 |C2\n|F1 |F2\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        assert_eq!(rows.len(), 2);
        assert!(rows[0].variant.is_none());
        assert_eq!(rows[1].variant, Some("footer"));
    }

    #[test]
    fn test_table_header_and_footer_options() {
        // [%header%footer] marks first as header, last as footer
        let source = "[%header%footer]\n|===\n|H1 |H2\n\n|C1 |C2\n\n|F1 |F2\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        assert_eq!(rows.len(), 3);
        assert_eq!(rows[0].variant, Some("header"));
        assert!(rows[1].variant.is_none());
        assert_eq!(rows[2].variant, Some("footer"));
    }

    #[test]
    fn test_parse_cell_spec_colspan() {
        let spec = parse_cell_spec("2+");
        assert_eq!(spec.colspan, 2);
        assert_eq!(spec.rowspan, 1);
    }

    #[test]
    fn test_parse_cell_spec_rowspan() {
        let spec = parse_cell_spec(".3+");
        assert_eq!(spec.colspan, 1);
        assert_eq!(spec.rowspan, 3);
    }

    #[test]
    fn test_parse_cell_spec_colspan_rowspan() {
        let spec = parse_cell_spec("2.3+");
        assert_eq!(spec.colspan, 2);
        assert_eq!(spec.rowspan, 3);
    }

    #[test]
    fn test_parse_cell_spec_style() {
        let spec = parse_cell_spec("a");
        assert_eq!(spec.style, Some('a'));

        let spec = parse_cell_spec("s");
        assert_eq!(spec.style, Some('s'));

        let spec = parse_cell_spec("e");
        assert_eq!(spec.style, Some('e'));
    }

    #[test]
    fn test_parse_cell_spec_halign() {
        let spec = parse_cell_spec(">");
        assert_eq!(spec.halign, Some('>'));

        let spec = parse_cell_spec("<");
        assert_eq!(spec.halign, Some('<'));

        let spec = parse_cell_spec("^");
        assert_eq!(spec.halign, Some('^'));
    }

    #[test]
    fn test_parse_cell_spec_duplication() {
        let spec = parse_cell_spec("3*");
        assert_eq!(spec.duplication, 3);
    }

    #[test]
    fn test_table_cell_colspan() {
        let source = "|===\n2+|spans two |normal\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        let cells = rows[0].items.as_ref().expect("should have cells");
        assert_eq!(cells.len(), 2);

        // First cell should have colspan=2
        let meta = cells[0]
            .metadata
            .as_ref()
            .expect("cell should have metadata");
        let colspan = meta
            .attributes
            .iter()
            .find(|(k, _)| *k == "colspan")
            .map(|(_, v)| *v);
        assert_eq!(colspan, Some("2"));
    }

    #[test]
    fn test_table_cell_style_asciidoc() {
        // a| means parse content as AsciiDoc (blocks)
        let source = "|===\na|Some *bold* paragraph |normal\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        let cells = rows[0].items.as_ref().expect("should have cells");

        // First cell should have style "asciidoc" and blocks (not inlines)
        assert_eq!(cells[0].style, Some("asciidoc"));
        assert!(
            cells[0].blocks.is_some(),
            "asciidoc cell should have blocks"
        );
        assert!(
            cells[0].inlines.is_none(),
            "asciidoc cell should not have inlines"
        );
    }

    #[test]
    fn test_table_cell_style_strong() {
        let source = "|===\ns|strong cell |normal\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        let cells = rows[0].items.as_ref().expect("should have cells");

        assert_eq!(cells[0].style, Some("strong"));
    }

    #[test]
    fn test_table_cell_halign() {
        let source = "|===\n>|right |normal\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        let cells = rows[0].items.as_ref().expect("should have cells");

        let meta = cells[0]
            .metadata
            .as_ref()
            .expect("cell should have metadata");
        let halign = meta
            .attributes
            .iter()
            .find(|(k, _)| *k == "halign")
            .map(|(_, v)| *v);
        assert_eq!(halign, Some("right"));
    }

    #[test]
    fn test_table_cell_duplication() {
        // 3*| duplicates empty cell 3 times
        let source = "[cols=\"3*\"]\n|===\n3*|same\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        assert_eq!(rows.len(), 1);
        let cells = rows[0].items.as_ref().expect("should have cells");
        assert_eq!(cells.len(), 3, "3* should produce 3 cells");
    }

    #[test]
    fn test_table_cols_comma_separated() {
        let source = "[cols=\"1,1,1\"]\n|===\n|a |b |c\n|d |e |f\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (raw_blocks, _) = super::super::boundary::parse_raw_blocks(&tokens, source, &idx);
        let (blocks, diags) = transform_raw_blocks(raw_blocks, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "table");

        let rows = blocks[0].items.as_ref().expect("should have rows");
        assert_eq!(rows.len(), 2, "should have 2 rows with cols=\"1,1,1\"");
        for row in rows {
            let cells = row.items.as_ref().expect("should have cells");
            assert_eq!(cells.len(), 3, "each row should have 3 cells");
        }
    }
}

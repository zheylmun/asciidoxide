//! Block/document parser: chumsky-based block structure detection.
//!
//! This module uses chumsky combinators for individual block parsers with
//! procedural orchestration for the main loop that handles metadata propagation.

mod attributes;
mod breaks;
mod chumsky_parser;
mod combinators;
mod comments;
mod delimited;
mod lists;
mod metadata;
mod paragraphs;
mod sections;

use super::{Spanned, content_span, strip_trailing_newline_index, strip_trailing_newlines};
use crate::asg::{Block, InlineNode};
use crate::diagnostic::ParseDiagnostic;
use crate::span::SourceIndex;
use crate::token::Token;

pub(super) use attributes::{HeaderResult, extract_header};

/// Block parse result type alias for readability.
type ParseResult<'src> = Option<(Block<'src>, usize, Vec<ParseDiagnostic>)>;

/// Try to skip pure non-content elements (newlines, comments).
///
/// Returns `Some(next_pos)` if something was skipped, `None` otherwise.
/// Note: Block attributes are handled separately to track pending attrs.
fn try_skip_comment(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    // Skip line comments (// ...).
    if let Some(next) = is_line_comment(tokens, i) {
        return Some(next);
    }

    // Skip block comments (////...////).
    if let Some(next) = try_skip_block_comment(tokens, i) {
        return Some(next);
    }

    None
}

/// Try to parse verbatim blocks (listing, literal, fenced code, passthrough).
fn try_verbatim_block<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> ParseResult<'src> {
    if let Some((block, next)) = try_listing(tokens, i, source, idx) {
        return Some((block, next, vec![]));
    }
    if let Some((block, next)) = try_literal(tokens, i, source, idx) {
        return Some((block, next, vec![]));
    }
    if let Some((block, next)) = try_fenced_code(tokens, i, source, idx) {
        return Some((block, next, vec![]));
    }
    if let Some((block, next)) = try_passthrough(tokens, i, source, idx) {
        return Some((block, next, vec![]));
    }
    None
}

/// Try to parse compound blocks (example, quote, sidebar, open).
fn try_compound_block<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
    pending_title: Option<Vec<InlineNode<'src>>>,
) -> ParseResult<'src> {
    if let Some((block, next, diags)) = try_example(tokens, i, source, idx, pending_title) {
        return Some((block, next, diags));
    }
    if let Some((block, next, diags)) = try_quote(tokens, i, source, idx) {
        return Some((block, next, diags));
    }
    if let Some((block, next, diags)) = try_sidebar(tokens, i, source, idx) {
        return Some((block, next, diags));
    }
    if let Some((block, next, diags)) = try_open(tokens, i, source, idx) {
        return Some((block, next, diags));
    }
    None
}

/// Try to parse structural blocks (sections, lists).
fn try_structural_block<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
    pending_attrs: Option<&BlockAttrs<'src>>,
) -> ParseResult<'src> {
    if let Some((block, next, diags)) = try_section(tokens, i, source, idx, pending_attrs) {
        return Some((block, next, diags));
    }
    if let Some((block, next, diags)) = try_list(tokens, i, source, idx) {
        return Some((block, next, diags));
    }
    None
}

/// Build block-level ASG nodes from a body token stream.
///
/// Scans tokens linearly, detecting delimited blocks (e.g., listing) before
/// falling back to paragraph collection. Blocks are separated by blank lines
/// (two or more consecutive `Newline` tokens).
pub(super) fn build_blocks<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let mut blocks = Vec::new();
    let mut diagnostics = Vec::new();
    let mut i = 0;
    let mut pending_title: Option<Vec<InlineNode<'src>>> = None;
    let mut pending_attrs: Option<BlockAttrs<'src>> = None;

    while i < tokens.len() {
        // Skip inter-block newlines.
        while i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
            i += 1;
        }
        if i >= tokens.len() {
            break;
        }

        // Skip comments.
        if let Some(next) = try_skip_comment(tokens, i) {
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }

        // Handle block attribute lines (e.g., [abstract], [source,ruby], [comment]).
        if let Some(attr_result) = is_block_attribute_line(tokens, i, source) {
            // If [comment], skip the following block entirely.
            if attr_result.attrs.is_comment() {
                pending_title = None;
                pending_attrs = None;
                i = skip_comment_block(tokens, attr_result.next, source, idx);
                continue;
            }
            // Store attributes for the next block.
            pending_attrs = Some(attr_result.attrs);
            i = attr_result.next;
            continue;
        }

        // Try block title (.Title).
        if let Some(title_result) = try_block_title(tokens, i, source, idx) {
            pending_title = Some(title_result.inlines);
            diagnostics.extend(title_result.diagnostics);
            i = title_result.next;
            continue;
        }

        // Try break blocks (thematic or page).
        if let Some((block, next)) = try_break(tokens, i, idx) {
            blocks.push(block);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }

        // Try discrete heading (section with [discrete] style).
        if let Some((block, next, diags)) =
            try_discrete_heading(tokens, i, source, idx, pending_attrs.as_ref())
        {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }

        // Try verbatim blocks.
        if let Some((block, next, diags)) = try_verbatim_block(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }

        // Try compound blocks.
        if let Some((block, next, diags)) =
            try_compound_block(tokens, i, source, idx, pending_title.take())
        {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_attrs = None;
            i = next;
            continue;
        }

        // Try structural blocks.
        if let Some((block, next, diags)) =
            try_structural_block(tokens, i, source, idx, pending_attrs.as_ref())
        {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }

        // Collect paragraph tokens until a blank line or delimited block.
        let para_end = find_paragraph_end(tokens, i);
        if i < para_end {
            let (block, diags) = make_paragraph(&tokens[i..para_end], source, idx);
            blocks.push(block);
            diagnostics.extend(diags);
        }
        pending_title = None;
        pending_attrs = None;
        i = para_end;
    }

    (blocks, diagnostics)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::asg::{InlineNode, Position};
    use crate::parser::parse_doc;

    #[test]
    fn doc_empty() {
        let (doc, diags) = parse_doc("");
        assert!(diags.is_empty());
        assert!(doc.blocks.is_empty());
        assert!(doc.header.is_none());
        assert!(doc.location.is_none());
    }

    #[test]
    fn doc_single_paragraph() {
        let (doc, diags) = parse_doc("hello world");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        assert_eq!(doc.blocks[0].name, "paragraph");
        let inlines = doc.blocks[0].inlines.as_ref().unwrap();
        assert_eq!(inlines.len(), 1);
        match &inlines[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "hello world"),
            _ => panic!("expected Text"),
        }
    }

    #[test]
    fn doc_sibling_paragraphs() {
        let (doc, diags) = parse_doc("para1\n\npara2");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 2);
        assert_eq!(doc.blocks[0].name, "paragraph");
        assert_eq!(doc.blocks[1].name, "paragraph");
    }

    #[test]
    fn doc_header_body() {
        let (doc, diags) = parse_doc("= Title\n\nbody");
        assert!(diags.is_empty());
        assert!(doc.header.is_some());
        let header = doc.header.as_ref().unwrap();
        assert_eq!(header.title.len(), 1);
        match &header.title[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "Title"),
            _ => panic!("expected Text"),
        }
        assert_eq!(doc.blocks.len(), 1);
        assert!(doc.attributes.is_some());
    }

    #[test]
    fn doc_body_only_no_attributes() {
        let (doc, diags) = parse_doc("just text");
        assert!(diags.is_empty());
        assert!(doc.attributes.is_none());
        assert!(doc.header.is_none());
    }

    #[test]
    fn doc_multiple_blank_lines() {
        let (doc, diags) = parse_doc("a\n\n\nb");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 2);
    }

    #[test]
    fn doc_listing_block() {
        let (doc, diags) = parse_doc("----\ndef main\n  puts 'hello'\nend\n----");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let block = &doc.blocks[0];
        assert_eq!(block.name, "listing");
        assert_eq!(block.form, Some("delimited"));
        assert_eq!(block.delimiter, Some("----"));
        let inlines = block.inlines.as_ref().unwrap();
        assert_eq!(inlines.len(), 1);
        match &inlines[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "def main\n  puts 'hello'\nend");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 2, col: 1 });
                assert_eq!(loc[1], Position { line: 4, col: 3 });
            }
            _ => panic!("expected Text node"),
        }
        let loc = block.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 5, col: 4 });
    }

    #[test]
    fn doc_unordered_list_single_item() {
        let (doc, diags) = parse_doc("* water");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let list = &doc.blocks[0];
        assert_eq!(list.name, "list");
        assert_eq!(list.variant, Some("unordered"));
        assert_eq!(list.marker, Some("*"));
        let items = list.items.as_ref().unwrap();
        assert_eq!(items.len(), 1);
        let item = &items[0];
        assert_eq!(item.name, "listItem");
        assert_eq!(item.marker, Some("*"));
        let principal = item.principal.as_ref().unwrap();
        assert_eq!(principal.len(), 1);
        match &principal[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "water");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 1, col: 3 });
                assert_eq!(loc[1], Position { line: 1, col: 7 });
            }
            _ => panic!("expected Text node"),
        }
        let loc = list.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 1, col: 7 });
    }

    #[test]
    fn doc_section_with_body() {
        let (doc, diags) = parse_doc("== Section Title\n\nparagraph");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let section = &doc.blocks[0];
        assert_eq!(section.name, "section");
        assert_eq!(section.level, Some(1));
        let title = section.title.as_ref().unwrap();
        assert_eq!(title.len(), 1);
        match &title[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "Section Title");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 1, col: 4 });
                assert_eq!(loc[1], Position { line: 1, col: 16 });
            }
            _ => panic!("expected Text node"),
        }
        let blocks = section.blocks.as_ref().unwrap();
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
        let loc = section.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 3, col: 9 });
    }

    #[test]
    fn doc_sidebar_with_list() {
        let (doc, diags) = parse_doc("****\n* phone\n* wallet\n* keys\n****");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let sidebar = &doc.blocks[0];
        assert_eq!(sidebar.name, "sidebar");
        assert_eq!(sidebar.form, Some("delimited"));
        assert_eq!(sidebar.delimiter, Some("****"));
        let blocks = sidebar.blocks.as_ref().unwrap();
        assert_eq!(blocks.len(), 1);
        let list = &blocks[0];
        assert_eq!(list.name, "list");
        assert_eq!(list.variant, Some("unordered"));
        let items = list.items.as_ref().unwrap();
        assert_eq!(items.len(), 3);
        assert_eq!(items[0].principal.as_ref().unwrap().len(), 1);
        let loc = sidebar.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 5, col: 4 });
    }

    #[test]
    fn doc_open_block() {
        let (doc, diags) = parse_doc("--\nhello\n--");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let open = &doc.blocks[0];
        assert_eq!(open.name, "open");
        assert_eq!(open.form, Some("delimited"));
        assert_eq!(open.delimiter, Some("--"));
        let blocks = open.blocks.as_ref().unwrap();
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
    }

    #[test]
    fn doc_body_only_attributes() {
        let (doc, diags) = parse_doc(":frog: Tanglefoot");
        assert!(diags.is_empty());
        assert!(doc.header.is_none());
        assert!(doc.blocks.is_empty());
        let attrs = doc.attributes.as_ref().expect("should have attributes");
        assert_eq!(
            attrs.get("frog"),
            Some(&crate::asg::AttributeValue::Single("Tanglefoot"))
        );
        let loc = doc.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 1, col: 17 });
    }

    #[test]
    fn doc_attribute_delete_bang_prefix() {
        let (doc, diags) = parse_doc(":frog: Tanglefoot\n:!frog:");
        assert!(diags.is_empty());
        assert!(doc.header.is_none());
        assert!(doc.blocks.is_empty());
        // Attribute was set then deleted, so no attributes remain.
        assert!(doc.attributes.is_none());
    }

    #[test]
    fn doc_attribute_delete_bang_suffix() {
        let (doc, diags) = parse_doc(":frog: Tanglefoot\n:frog!:");
        assert!(diags.is_empty());
        assert!(doc.header.is_none());
        assert!(doc.blocks.is_empty());
        // Attribute was set then deleted, so no attributes remain.
        assert!(doc.attributes.is_none());
    }

    #[test]
    fn doc_invalid_attr_name_becomes_paragraph() {
        // `:foo:bar: baz` has a colon in the name position - should be a paragraph
        let (doc, diags) = parse_doc(":foo:bar: baz");
        assert!(diags.is_empty());
        assert!(doc.attributes.is_none());
        assert_eq!(doc.blocks.len(), 1);
        assert_eq!(doc.blocks[0].name, "paragraph");
    }

    #[test]
    fn doc_thematic_break() {
        let (doc, diags) = parse_doc("'''");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let br = &doc.blocks[0];
        assert_eq!(br.name, "break");
        assert_eq!(br.variant, Some("thematic"));
        let loc = br.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 1, col: 3 });
    }

    #[test]
    fn doc_page_break() {
        let (doc, diags) = parse_doc("page 1\n\n<<<\n\npage 2");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 3);
        assert_eq!(doc.blocks[0].name, "paragraph");
        assert_eq!(doc.blocks[1].name, "break");
        assert_eq!(doc.blocks[1].variant, Some("page"));
        assert_eq!(doc.blocks[2].name, "paragraph");
    }

    #[test]
    fn doc_line_comment_skipped() {
        let (doc, diags) = parse_doc("first\n\n// comment\n\nsecond");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 2);
        assert_eq!(doc.blocks[0].name, "paragraph");
        assert_eq!(doc.blocks[1].name, "paragraph");
    }

    #[test]
    fn doc_block_comment_skipped() {
        let (doc, diags) = parse_doc("first\n\n////\ncomment\n////\n\nsecond");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 2);
        assert_eq!(doc.blocks[0].name, "paragraph");
        assert_eq!(doc.blocks[1].name, "paragraph");
    }

    #[test]
    fn doc_comment_style_paragraph_skipped() {
        let (doc, diags) = parse_doc("[comment]\nskip this\n\nkeep this");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        assert_eq!(doc.blocks[0].name, "paragraph");
    }

    #[test]
    fn doc_example_block() {
        let (doc, diags) = parse_doc("====\nThis is an example.\n====");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let example = &doc.blocks[0];
        assert_eq!(example.name, "example");
        assert_eq!(example.form, Some("delimited"));
        assert_eq!(example.delimiter, Some("===="));
        let blocks = example.blocks.as_ref().unwrap();
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
        let loc = example.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 3, col: 4 });
    }

    #[test]
    fn doc_example_block_with_title() {
        let (doc, diags) = parse_doc(".My Example\n====\ncontent\n====");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let example = &doc.blocks[0];
        assert_eq!(example.name, "example");
        let title = example.title.as_ref().unwrap();
        assert_eq!(title.len(), 1);
        match &title[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "My Example"),
            _ => panic!("expected Text"),
        }
    }

    #[test]
    fn doc_fenced_code_block() {
        let (doc, diags) = parse_doc("```\nputs 'hello'\n```");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let listing = &doc.blocks[0];
        assert_eq!(listing.name, "listing");
        assert_eq!(listing.form, Some("delimited"));
        assert_eq!(listing.delimiter, Some("```"));
        let inlines = listing.inlines.as_ref().unwrap();
        assert_eq!(inlines.len(), 1);
        match &inlines[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "puts 'hello'"),
            _ => panic!("expected Text node"),
        }
        let loc = listing.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 3, col: 3 });
    }

    #[test]
    fn doc_literal_block() {
        let (doc, diags) = parse_doc("....\nline one\n\nline two\n....");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let literal = &doc.blocks[0];
        assert_eq!(literal.name, "literal");
        assert_eq!(literal.form, Some("delimited"));
        assert_eq!(literal.delimiter, Some("...."));
        let inlines = literal.inlines.as_ref().unwrap();
        assert_eq!(inlines.len(), 1);
        match &inlines[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "line one\n\nline two");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 2, col: 1 });
                assert_eq!(loc[1], Position { line: 4, col: 8 });
            }
            _ => panic!("expected Text node"),
        }
        let loc = literal.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 5, col: 4 });
    }

    #[test]
    fn doc_quote_block() {
        let (doc, diags) = parse_doc("____\nA famous quote.\n____");
        assert!(diags.is_empty());
        assert_eq!(doc.blocks.len(), 1);
        let quote = &doc.blocks[0];
        assert_eq!(quote.name, "quote");
        assert_eq!(quote.form, Some("delimited"));
        assert_eq!(quote.delimiter, Some("____"));
        let blocks = quote.blocks.as_ref().unwrap();
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
        let inlines = blocks[0].inlines.as_ref().unwrap();
        assert_eq!(inlines.len(), 1);
        match &inlines[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "A famous quote.");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 2, col: 1 });
                assert_eq!(loc[1], Position { line: 2, col: 15 });
            }
            _ => panic!("expected Text node"),
        }
        let loc = quote.location.as_ref().unwrap();
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 3, col: 4 });
    }
}

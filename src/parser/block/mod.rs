//! Block/document parser: pure chumsky-based block structure detection.
//!
//! This module uses pure chumsky combinators for block parsing with a two-phase approach:
//! 1. Phase 2: Identify block structure and boundaries (`RawBlock`)
//! 2. Phase 3: Transform `RawBlock` to Block with inline parsing

mod attributes;
mod boundary;
mod raw_block;
mod transform;

use super::Spanned;
use crate::asg::Block;
use crate::diagnostic::ParseDiagnostic;
use crate::span::SourceIndex;

pub(super) use attributes::{HeaderResult, extract_header};

/// Build block-level ASG nodes from a body token stream.
///
/// Uses pure chumsky-based parsers with a two-phase approach:
/// 1. Phase 2: Identify block structure and boundaries (`RawBlock`)
/// 2. Phase 3: Transform `RawBlock` to Block with inline parsing
///
/// Blocks are separated by blank lines (two or more consecutive `Newline` tokens).
pub(super) fn build_blocks<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    boundary::build_blocks_pure(tokens, source, idx)
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
    fn doc_attribute_with_hyphens() {
        let (doc, diags) = parse_doc(":type-req-prefix: REQ");
        assert!(diags.is_empty());
        assert!(doc.header.is_none());
        assert!(doc.blocks.is_empty());
        let attrs = doc.attributes.as_ref().expect("should have attributes");
        assert_eq!(
            attrs.get("type-req-prefix"),
            Some(&crate::asg::AttributeValue::Single("REQ"))
        );
    }

    #[test]
    fn doc_attribute_with_underscores() {
        let (doc, diags) = parse_doc(":my_attr: value");
        assert!(diags.is_empty());
        let attrs = doc.attributes.as_ref().expect("should have attributes");
        assert_eq!(
            attrs.get("my_attr"),
            Some(&crate::asg::AttributeValue::Single("value"))
        );
    }

    #[test]
    fn doc_attribute_hyphen_delete_bang_prefix() {
        let (doc, diags) = parse_doc(":type-req-prefix: REQ\n:!type-req-prefix:");
        assert!(diags.is_empty());
        assert!(doc.attributes.is_none());
    }

    #[test]
    fn doc_attribute_hyphen_delete_bang_suffix() {
        let (doc, diags) = parse_doc(":type-req-prefix: REQ\n:type-req-prefix!:");
        assert!(diags.is_empty());
        assert!(doc.attributes.is_none());
    }

    #[test]
    fn doc_comment_between_body_attributes() {
        let (doc, diags) = parse_doc(":foo: bar\n// a comment\n:baz: qux");
        assert!(diags.is_empty());
        let attrs = doc.attributes.as_ref().expect("should have attributes");
        assert_eq!(
            attrs.get("foo"),
            Some(&crate::asg::AttributeValue::Single("bar"))
        );
        assert_eq!(
            attrs.get("baz"),
            Some(&crate::asg::AttributeValue::Single("qux"))
        );
    }

    #[test]
    fn doc_comment_between_header_attributes() {
        let (doc, diags) = parse_doc("= Title\n:foo: bar\n// a comment\n:baz: qux\n\nbody");
        assert!(diags.is_empty());
        assert!(doc.header.is_some());
        let attrs = doc.attributes.as_ref().expect("should have attributes");
        assert_eq!(
            attrs.get("foo"),
            Some(&crate::asg::AttributeValue::Single("bar"))
        );
        assert_eq!(
            attrs.get("baz"),
            Some(&crate::asg::AttributeValue::Single("qux"))
        );
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

//! Pure chumsky block parsers for phase 2 (block boundary identification).
//!
//! This module provides block parsing using only chumsky combinators.
//! Block content is captured as byte spans rather than parsed inline nodes,
//! deferring inline parsing to phase 3.

mod breaks;
mod compound;
mod lists;
mod macros;
mod metadata;
mod sections;
mod utility;
mod verbatim;

use chumsky::{input::ValueInput, prelude::*};

use super::raw_block::{PendingMetadata, RawBlock};
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

// Re-export from submodules
use breaks::{block_comment, line_comment, page_break, thematic_break};
use compound::{example_block, open_block, quote_block, sidebar_block};
use lists::{description_list, ordered_list, unordered_list};
use macros::image_block_macro;
use metadata::{block_anchor_line, block_attr_line, block_title_line};
use sections::{markdown_heading, section_heading, setext_heading};
use utility::{BlockExtra, blank_lines};
use verbatim::{fenced_code_block, listing_block, literal_block, passthrough_block};

// ---------------------------------------------------------------------------
// Paragraph Parser
// ---------------------------------------------------------------------------

/// Check if current position looks like a block interrupt pattern.
///
/// This checks for patterns that should end a paragraph:
/// - Block delimiters (`----`, `====`, `****`, `....`, `++++`, `--`)
/// - Section headings (`== Title`)
/// - List markers (`*`, `.` at line start followed by space)
/// - Block attribute lines (`[...]`)
/// - Block title lines (`.Title` - single dot followed by non-space)
/// - Thematic break (`'''`)
/// - Page break (`<<<`)
fn is_block_interrupt<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
) -> bool
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let before = inp.save();
    let result = check_block_interrupt_impl(inp);
    inp.rewind(before);
    result
}

/// Implementation of block interrupt checking (consumes tokens, caller must rewind).
fn check_block_interrupt_impl<'tokens, 'src: 'tokens, I>(
    inp: &mut chumsky::input::InputRef<'tokens, '_, I, BlockExtra<'tokens, 'src>>,
) -> bool
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    match inp.peek() {
        // End of input, block attribute line [...], or block anchor [[...]]
        None | Some(Token::LBracket) => true,

        // Section heading (== Title) - need 2+ equals followed by whitespace
        Some(Token::Eq) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Eq)) {
                inp.skip();
                count += 1;
            }
            count >= 2 && matches!(inp.peek(), Some(Token::Whitespace))
        }

        // Unordered list marker (* item) or sidebar delimiter (****)
        Some(Token::Star) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Star)) {
                inp.skip();
                count += 1;
            }
            // Single star + whitespace = list item
            // 4+ stars + newline/EOF = sidebar delimiter
            (count == 1 && matches!(inp.peek(), Some(Token::Whitespace)))
                || (count >= 4
                    && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline))))
        }

        // Ordered list marker (. item) or literal block (....)
        // Also block title (.Title)
        Some(Token::Dot) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Dot)) {
                inp.skip();
                count += 1;
            }
            // Single dot + whitespace = list item
            // Single dot + non-whitespace/non-newline = block title
            // 4+ dots + newline/EOF = literal delimiter
            if count == 1 {
                // Single dot: list item (. foo) or block title (.Title)
                // Not a title if followed by newline or EOF
                !matches!(inp.peek(), Some(Token::Newline) | None)
            } else {
                count >= 4 && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
            }
        }

        // Listing block (----) or open block (--)
        Some(Token::Hyphen) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Hyphen)) {
                inp.skip();
                count += 1;
            }
            // 2 hyphens + newline/EOF = open block
            // 4+ hyphens + newline/EOF = listing delimiter
            (count == 2 || count >= 4)
                && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
        }

        // Passthrough block (++++)
        Some(Token::Plus) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Plus)) {
                inp.skip();
                count += 1;
            }
            count >= 4 && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
        }

        // Quote block (____)
        Some(Token::Underscore) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Underscore)) {
                inp.skip();
                count += 1;
            }
            count >= 4 && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
        }

        // Thematic break (''')
        Some(Token::SingleQuote) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::SingleQuote)) {
                inp.skip();
                count += 1;
            }
            count >= 3 && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
        }

        // Comment (// or ////)
        Some(Token::Slash) => {
            inp.skip();
            matches!(inp.peek(), Some(Token::Slash))
        }

        // Page break (<<<)
        Some(Token::DoubleLeftAngle) => {
            inp.skip();
            matches!(inp.peek(), Some(Token::Text("<")))
        }

        // Fenced code block (```)
        Some(Token::Backtick) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Backtick)) {
                inp.skip();
                count += 1;
            }
            count >= 3
        }

        // Markdown heading (## Title)
        Some(Token::Hash) => {
            let mut count = 0;
            while matches!(inp.peek(), Some(Token::Hash)) {
                inp.skip();
                count += 1;
            }
            count >= 2 && matches!(inp.peek(), Some(Token::Whitespace))
        }

        _ => false,
    }
}

/// Parse a paragraph (fallback for unrecognized content).
///
/// A paragraph consists of one or more contiguous lines of text.
/// It ends at:
/// - A blank line (empty line / two newlines)
/// - A block delimiter or other block-starting pattern
/// - End of input
fn paragraph<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let start_cursor = inp.cursor();

        // Must have at least some content
        if inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)) {
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "empty paragraph",
            ));
        }

        // Consume lines until we hit a paragraph boundary
        loop {
            // Consume content on current line
            while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
            }

            // Check what's after the newline
            if inp.peek().is_none() {
                // End of input - paragraph ends here
                break;
            }

            // We're at a newline
            debug_assert!(matches!(inp.peek(), Some(Token::Newline)));

            // Peek ahead: is next line blank or a block interrupt?
            let before_newline = inp.save();
            inp.skip(); // consume the newline

            // Check for blank line (another newline immediately)
            if inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)) {
                // Blank line or EOF - paragraph ends before this newline
                inp.rewind(before_newline);
                break;
            }

            // Check for block interrupt pattern
            if is_block_interrupt(inp) {
                // Block interrupt - paragraph ends before this newline
                inp.rewind(before_newline);
                break;
            }

            // Continue with next line (newline is part of paragraph content)
            // We've already consumed the newline, continue the loop
        }

        let content_span: SourceSpan = inp.span_since(&start_cursor);

        // Consume trailing newline if present
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        if content_span.start >= content_span.end {
            return Err(Rich::custom(content_span, "empty paragraph"));
        }

        let mut block = RawBlock::new("paragraph");
        block.content_span = Some(content_span);
        block.location = Some(idx.location(&content_span));

        Ok(block)
    })
}

// ---------------------------------------------------------------------------
// Main Block Parser
// ---------------------------------------------------------------------------

/// Result type for the metadata/block parsing.
/// Either metadata to apply to next block, or a parsed block.
#[derive(Clone)]
#[allow(clippy::large_enum_variant)]
enum ParseItem<'src> {
    /// Metadata to apply to next block.
    Metadata(PendingMetadata<'src>),
    /// Skip this item (comment).
    Skip,
    /// A parsed block.
    Block(RawBlock<'src>),
}

/// Build the main block parser.
///
/// This parser handles:
/// - Metadata accumulation (attributes, titles)
/// - Block recognition (breaks, verbatim, compound, paragraph)
///
/// Note: Compound blocks store content as a span. Recursive parsing of
/// compound block content happens in phase 3.
pub(super) fn blocks_parser<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Vec<RawBlock<'src>>, BlockExtra<'tokens, 'src>>
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // Individual item parsers
    let item = choice((
        // Comments (skip) - block_comment must come before line_comment
        // because //// starts with // which would match line_comment
        block_comment().to(ParseItem::Skip),
        line_comment().to(ParseItem::Skip),
        // Metadata (accumulate)
        block_anchor_line(source).map(ParseItem::Metadata),
        block_attr_line(source).map(ParseItem::Metadata),
        block_title_line().map(ParseItem::Metadata),
        // Breaks
        thematic_break(idx).map(ParseItem::Block),
        page_break(idx).map(ParseItem::Block),
        // Section headings (AsciiDoc, Markdown, and setext)
        section_heading(source, idx).map(ParseItem::Block),
        markdown_heading(source, idx).map(ParseItem::Block),
        setext_heading(source, idx).map(ParseItem::Block),
        // Lists (must come before paragraph)
        description_list(source, idx).map(ParseItem::Block),
        unordered_list(source, idx).map(ParseItem::Block),
        ordered_list(source, idx).map(ParseItem::Block),
        // Verbatim blocks (must come before compound to avoid conflicts)
        listing_block(source, idx).map(ParseItem::Block),
        literal_block(source, idx).map(ParseItem::Block),
        passthrough_block(source, idx).map(ParseItem::Block),
        fenced_code_block(source, idx).map(ParseItem::Block),
        // Compound blocks (content parsed in phase 3)
        example_block(source, idx).map(ParseItem::Block),
        sidebar_block(source, idx).map(ParseItem::Block),
        quote_block(source, idx).map(ParseItem::Block),
        open_block(source, idx).map(ParseItem::Block),
        // Block macros
        image_block_macro(source, idx).map(ParseItem::Block),
        // Fallback paragraph
        paragraph(idx).map(ParseItem::Block),
    ));

    // Parse items separated by blank lines, fold metadata into blocks
    item.separated_by(blank_lines().or_not())
        .allow_leading()
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(fold_items)
}

/// Fold parse items, applying pending metadata to blocks.
fn fold_items(items: Vec<ParseItem<'_>>) -> Vec<RawBlock<'_>> {
    let mut blocks = Vec::new();
    let mut pending_meta: Option<PendingMetadata<'_>> = None;

    for item in items {
        match item {
            ParseItem::Skip => {
                // Comments clear pending metadata
                pending_meta = None;
            }
            ParseItem::Metadata(meta) => {
                // Merge with any existing pending metadata
                if let Some(existing) = pending_meta.take() {
                    pending_meta = Some(merge_metadata(existing, meta));
                } else {
                    pending_meta = Some(meta);
                }
            }
            ParseItem::Block(mut block) => {
                // Check if [comment] style - skip this block entirely
                if pending_meta
                    .as_ref()
                    .is_some_and(PendingMetadata::is_comment)
                {
                    pending_meta = None;
                    continue; // Skip this block
                }

                // Apply pending metadata to block
                if let Some(meta) = pending_meta.take() {
                    meta.apply_to(&mut block);
                }
                blocks.push(block);
            }
        }
    }

    blocks
}

/// Merge two pending metadata values.
fn merge_metadata<'src>(
    mut base: PendingMetadata<'src>,
    overlay: PendingMetadata<'src>,
) -> PendingMetadata<'src> {
    // Overlay takes precedence for single values
    if overlay.title_span.is_some() {
        base.title_span = overlay.title_span;
    }
    if overlay.id.is_some() {
        base.id = overlay.id;
    }
    if overlay.style.is_some() {
        base.style = overlay.style;
    }
    if overlay.reftext_span.is_some() {
        base.reftext_span = overlay.reftext_span;
    }
    // Merge collections
    base.roles.extend(overlay.roles);
    base.options.extend(overlay.options);
    if !overlay.positionals.is_empty() {
        base.positionals = overlay.positionals;
    }
    base.named_attributes.extend(overlay.named_attributes);
    base
}

// ---------------------------------------------------------------------------
// Entry Point
// ---------------------------------------------------------------------------

use super::Spanned;
use crate::asg::Block;
use crate::diagnostic::ParseDiagnostic;

/// Parse tokens into raw blocks (phase 2).
///
/// This identifies block structure and boundaries without parsing inline content.
pub(super) fn parse_raw_blocks<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<RawBlock<'src>>, Vec<ParseDiagnostic>) {
    if tokens.is_empty() {
        return (Vec::new(), Vec::new());
    }

    let last_span = tokens.last().unwrap().1;
    let eoi = SourceSpan {
        start: last_span.end,
        end: last_span.end,
    };
    let input = tokens.split_token_span(eoi);

    let parser = blocks_parser(source, idx);
    let (output, errors) = parser.parse(input).into_output_errors();

    let diagnostics: Vec<ParseDiagnostic> = errors
        .into_iter()
        .map(|e| ParseDiagnostic {
            span: *e.span(),
            message: e.to_string(),
            severity: crate::diagnostic::Severity::Error,
        })
        .collect();

    (output.unwrap_or_default(), diagnostics)
}

// ---------------------------------------------------------------------------
// Combined Entry Point (Phase 2 + Phase 3)
// ---------------------------------------------------------------------------

/// Build blocks from a token stream using pure chumsky parsing.
///
/// This combines Phase 2 (block boundary identification) and Phase 3
/// (`RawBlock` → Block transformation with inline parsing).
pub(super) fn build_blocks_pure<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    // Phase 2: Parse into RawBlocks
    let (raw_blocks, mut diagnostics) = parse_raw_blocks(tokens, source, idx);

    // Phase 3: Transform to Blocks with inline parsing
    let (blocks, transform_diags) =
        crate::parser::block::transform::transform_raw_blocks(raw_blocks, source, idx);
    diagnostics.extend(transform_diags);

    (blocks, diagnostics)
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use metadata::parse_attr_content;

    #[test]
    fn test_thematic_break() {
        let source = "'''\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "break");
        assert_eq!(blocks[0].variant, Some("thematic"));
    }

    #[test]
    fn test_page_break() {
        let source = "<<<\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "break");
        assert_eq!(blocks[0].variant, Some("page"));
    }

    #[test]
    fn test_paragraph() {
        let source = "hello world\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
        assert!(blocks[0].content_span.is_some());
    }

    #[test]
    fn test_multiline_paragraph() {
        let source = "line one\nline two\nline three\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(
            blocks.len(),
            1,
            "should be single paragraph, got {blocks:?}"
        );
        assert_eq!(blocks[0].name, "paragraph");

        let span = blocks[0].content_span.expect("should have content span");
        let content = &source[span.start..span.end];
        assert_eq!(content, "line one\nline two\nline three");
    }

    #[test]
    fn test_multiline_paragraph_ends_at_blank_line() {
        let source = "para one\nstill para one\n\npara two\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 2, "should be two paragraphs");

        let span1 = blocks[0].content_span.expect("first para should have span");
        assert_eq!(&source[span1.start..span1.end], "para one\nstill para one");

        let span2 = blocks[1]
            .content_span
            .expect("second para should have span");
        assert_eq!(&source[span2.start..span2.end], "para two");
    }

    #[test]
    fn test_multiline_paragraph_ends_at_block_delimiter() {
        let source = "paragraph text\ncontinued\n----\ncode\n----\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[0].name, "paragraph");
        assert_eq!(blocks[1].name, "listing");

        let span = blocks[0].content_span.expect("paragraph should have span");
        assert_eq!(&source[span.start..span.end], "paragraph text\ncontinued");
    }

    #[test]
    fn test_attr_parsing() {
        let content = "source#myid.role1.role2%opt1";
        let meta = parse_attr_content(content);

        assert_eq!(meta.style, Some("source"));
        assert_eq!(meta.id, Some("myid"));
        assert_eq!(meta.roles, vec!["role1", "role2"]);
        assert_eq!(meta.options, vec!["opt1"]);
    }

    #[test]
    fn test_literal_block_no_trailing_newline() {
        let source = "....\nline one\n\nline two\n....";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "literal");
    }

    #[test]
    fn test_block_comment_alone() {
        let source = "////\ncomment\n////\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 0, "block comment should be skipped");
    }

    #[test]
    fn test_block_comment_skipped() {
        let source = "first\n\n////\ncomment\n////\n\nsecond";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[0].name, "paragraph");
        assert_eq!(blocks[1].name, "paragraph");
    }

    #[test]
    fn test_listing_block() {
        let source = "----\ncode here\n----\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "listing");
        assert_eq!(blocks[0].form, Some("delimited"));
        assert_eq!(blocks[0].delimiter, Some("----"));
        let span = blocks[0].content_span.expect("should have content span");
        assert_eq!(&source[span.start..span.end], "code here");
    }

    #[test]
    fn test_multiple_blocks() {
        let source = "para1\n\n----\ncode\n----\n\npara2\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 3);
        assert_eq!(blocks[0].name, "paragraph");
        assert_eq!(blocks[1].name, "listing");
        assert_eq!(blocks[2].name, "paragraph");
    }

    #[test]
    fn test_example_block() {
        let source = "====\ninner paragraph\n====\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "example");
        assert_eq!(blocks[0].form, Some("delimited"));
        assert_eq!(blocks[0].delimiter, Some("===="));
        // Content stored as span (will be parsed in phase 3)
        let span = blocks[0].content_span.expect("should have content span");
        assert_eq!(&source[span.start..span.end], "inner paragraph");
    }

    #[test]
    fn test_sidebar_block() {
        let source = "****\nsidebar content\n****\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "sidebar");
        assert_eq!(blocks[0].form, Some("delimited"));
        let span = blocks[0].content_span.expect("should have content span");
        assert_eq!(&source[span.start..span.end], "sidebar content");
    }

    #[test]
    fn test_open_block() {
        let source = "--\nopen content\n--\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "open");
        assert_eq!(blocks[0].form, Some("delimited"));
        assert_eq!(blocks[0].delimiter, Some("--"));
        let span = blocks[0].content_span.expect("should have content span");
        assert_eq!(&source[span.start..span.end], "open content");
    }

    #[test]
    fn test_nested_example_in_sidebar() {
        // Tests that compound blocks capture their full content span
        // including nested delimiters (parsing nested structure happens in phase 3)
        let source = "====\n****\nnested\n****\n====\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "example");
        let span = blocks[0].content_span.expect("should have content span");
        // Content includes the nested sidebar block
        assert_eq!(&source[span.start..span.end], "****\nnested\n****");
    }

    #[test]
    fn test_section_heading() {
        let source = "== Section Title\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "section");
        assert_eq!(blocks[0].level, Some(1));
        let title_span = blocks[0].title_span.expect("should have title span");
        assert_eq!(&source[title_span.start..title_span.end], "Section Title");
    }

    #[test]
    fn test_section_level_2() {
        let source = "=== Subsection\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "section");
        assert_eq!(blocks[0].level, Some(2));
    }

    #[test]
    fn test_unordered_list_simple() {
        let source = "* item one\n* item two\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");
        assert_eq!(blocks[0].variant, Some("unordered"));

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].name, "listItem");
        assert_eq!(items[0].marker, Some("*"));

        let span = items[0].principal_span.expect("should have principal span");
        assert_eq!(&source[span.start..span.end], "item one");
    }

    #[test]
    fn test_ordered_list_simple() {
        let source = ". first\n. second\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");
        assert_eq!(blocks[0].variant, Some("ordered"));

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2);
        assert_eq!(items[0].marker, Some("."));
    }

    #[test]
    fn test_nested_unordered_list() {
        let source = "* outer\n** nested\n* back to outer\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2);

        // First item should have nested list in its blocks
        let nested = items[0]
            .blocks
            .as_ref()
            .expect("first item should have blocks");
        assert_eq!(nested.len(), 1);
        assert_eq!(nested[0].name, "list");
        assert_eq!(nested[0].variant, Some("unordered"));

        let nested_items = nested[0]
            .items
            .as_ref()
            .expect("nested list should have items");
        assert_eq!(nested_items.len(), 1);
        assert_eq!(nested_items[0].marker, Some("**"));
    }

    #[test]
    fn test_nested_unordered_list_3_levels() {
        let source = "* outer\n** middle\n*** inner\n** middle2\n* outer2\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2, "top-level list should have 2 items");

        // First top-level item should have a nested ** list
        let level2 = items[0]
            .blocks
            .as_ref()
            .expect("first item should have nested blocks");
        assert_eq!(level2.len(), 1);
        assert_eq!(level2[0].name, "list");
        assert_eq!(level2[0].variant, Some("unordered"));

        let level2_items = level2[0]
            .items
            .as_ref()
            .expect("level-2 list should have items");
        assert_eq!(level2_items.len(), 2, "level-2 list should have 2 items");
        assert_eq!(level2_items[0].marker, Some("**"));
        assert_eq!(level2_items[1].marker, Some("**"));

        // First level-2 item should have a nested *** list
        let level3 = level2_items[0]
            .blocks
            .as_ref()
            .expect("first level-2 item should have nested blocks");
        assert_eq!(level3.len(), 1);
        assert_eq!(level3[0].name, "list");
        assert_eq!(level3[0].variant, Some("unordered"));

        let level3_items = level3[0]
            .items
            .as_ref()
            .expect("level-3 list should have items");
        assert_eq!(level3_items.len(), 1);
        assert_eq!(level3_items[0].marker, Some("***"));
    }

    #[test]
    fn test_nested_ordered_list_3_levels() {
        let source = ". Foo\n.. Boo\n... Snoo\n. Blech\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");
        assert_eq!(blocks[0].variant, Some("ordered"));

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2, "top-level list should have 2 items");
        assert_eq!(items[0].marker, Some("."));
        assert_eq!(items[1].marker, Some("."));

        // First top-level item should have a nested .. list
        let level2 = items[0]
            .blocks
            .as_ref()
            .expect("first item should have nested blocks");
        assert_eq!(level2.len(), 1);
        assert_eq!(level2[0].name, "list");
        assert_eq!(level2[0].marker, Some(".."));

        let level2_items = level2[0]
            .items
            .as_ref()
            .expect("level-2 list should have items");
        assert_eq!(level2_items.len(), 1);
        assert_eq!(level2_items[0].marker, Some(".."));

        // The level-2 item should have a nested ... list
        let level3 = level2_items[0]
            .blocks
            .as_ref()
            .expect("level-2 item should have nested blocks");
        assert_eq!(level3.len(), 1);
        assert_eq!(level3[0].name, "list");
        assert_eq!(level3[0].marker, Some("..."));

        let level3_items = level3[0]
            .items
            .as_ref()
            .expect("level-3 list should have items");
        assert_eq!(level3_items.len(), 1);
        assert_eq!(level3_items[0].marker, Some("..."));
    }

    // Tests for combined entry point (Phase 2 + Phase 3)

    #[test]
    fn test_combined_paragraph_with_formatting() {
        let source = "hello *bold* world\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = build_blocks_pure(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "paragraph");
        assert!(blocks[0].inlines.is_some());
        // Should have parsed inlines with strong formatting
        let inlines = blocks[0].inlines.as_ref().unwrap();
        assert!(inlines.len() >= 2); // At least text + strong
    }

    #[test]
    fn test_combined_nested_compound() {
        let source = "====\ninner *bold* paragraph\n====\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = build_blocks_pure(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "example");

        // Should have child blocks with parsed inlines
        let child_blocks = blocks[0].blocks.as_ref().expect("should have child blocks");
        assert_eq!(child_blocks.len(), 1);
        assert_eq!(child_blocks[0].name, "paragraph");
        assert!(child_blocks[0].inlines.is_some());
    }

    #[test]
    fn test_combined_list_with_formatting() {
        let source = "* item *one*\n* item two\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = build_blocks_pure(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "list");

        let items = blocks[0].items.as_ref().expect("should have items");
        assert_eq!(items.len(), 2);

        // First item should have parsed principal with formatting
        let principal = items[0].principal.as_ref().expect("should have principal");
        assert!(!principal.is_empty());
    }
}

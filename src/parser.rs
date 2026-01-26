//! Parser that transforms a token stream into ASG nodes.
//!
//! The **inline parser** is chumsky-based (recursive, composable for adding
//! formatting variants). The **document/block parser** is procedural (a natural
//! fit for `AsciiDoc`'s line-oriented block structure).

use std::collections::HashMap;

use chumsky::{extra, input::ValueInput, prelude::*};

use crate::asg::{Block, Document, Header, InlineNode, SpanNode, TextNode};
use crate::lexer::lex;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// A token paired with its source span (re-stated here to avoid a name
/// conflict with [`chumsky::span::Spanned`]).
type Spanned<'a> = (Token<'a>, SourceSpan);

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse a full `AsciiDoc` document into its ASG.
#[must_use]
pub fn parse_doc(input: &str) -> Document<'_> {
    let tokens = lex(input);
    let idx = SourceIndex::new(input);

    if tokens.is_empty() {
        return Document {
            attributes: None,
            header: None,
            blocks: Vec::new(),
            location: None,
        };
    }

    let (header, body_start) = extract_header(&tokens, input, &idx);
    let body_tokens = &tokens[body_start..];
    let blocks = build_blocks(body_tokens, input, &idx);

    // Document location: first content token → last content token.
    let doc_span = content_span(&tokens);
    let location = doc_span.map(|s| idx.location(&s));

    Document {
        attributes: if header.is_some() {
            Some(HashMap::new())
        } else {
            None
        },
        header,
        blocks,
        location,
    }
}

/// Parse `AsciiDoc` inline content into inline nodes.
#[must_use]
pub fn parse_inlines(input: &str) -> Vec<InlineNode<'_>> {
    let tokens = lex(input);
    let idx = SourceIndex::new(input);
    let trimmed = strip_trailing_newlines(&tokens);
    run_inline_parser(trimmed, input, &idx)
}

// ---------------------------------------------------------------------------
// Inline parser (chumsky)
// ---------------------------------------------------------------------------

/// Build a recursive chumsky parser for inline content.
///
/// The parser produces `Vec<InlineNode>` from a token stream, using the source
/// string for zero-copy text slicing and the source index for locations.
fn inline_parser<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Vec<InlineNode<'src>>, extra::Default> + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    recursive(|inline_content| {
        // A text run: one or more tokens that are not `Star`.
        let text_run = any()
            .filter(|t: &Token| !matches!(t, Token::Star))
            .repeated()
            .at_least(1)
            .map_with(move |_toks, e| {
                let span: SourceSpan = e.span();
                InlineNode::Text(TextNode {
                    value: &source[span.start..span.end],
                    location: Some(idx.location(&span)),
                })
            });

        // Constrained strong: *inline_content*
        let strong = inline_content
            .delimited_by(just(Token::Star), just(Token::Star))
            .map_with(move |inlines: Vec<InlineNode<'src>>, e| {
                let span: SourceSpan = e.span();
                InlineNode::Span(SpanNode {
                    variant: "strong",
                    form: "constrained",
                    inlines,
                    location: Some(idx.location(&span)),
                })
            });

        choice((strong, text_run))
            .repeated()
            .at_least(1)
            .collect()
    })
}

/// Run the inline parser on a token sub-slice.
fn run_inline_parser<'tokens, 'src: 'tokens>(
    tokens: &'tokens [Spanned<'src>],
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> Vec<InlineNode<'src>> {
    if tokens.is_empty() {
        return Vec::new();
    }
    let last_span = tokens.last().expect("non-empty").1;
    let eoi = SourceSpan {
        start: last_span.end,
        end: last_span.end,
    };
    let input = tokens.split_token_span(eoi);
    inline_parser(source, idx)
        .parse(input)
        .into_output()
        .unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Document / block parser (procedural)
// ---------------------------------------------------------------------------

/// Detect a document header (`= Title`) at the start of the token stream.
///
/// Returns `(Some(header), first_body_token_index)` when a header is found,
/// or `(None, 0)` otherwise.
fn extract_header<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Option<Header<'src>>, usize) {
    // Header requires at least: Eq Whitespace <content>
    if tokens.len() >= 3
        && matches!(tokens[0].0, Token::Eq)
        && matches!(tokens[1].0, Token::Whitespace)
    {
        // Find the Newline (or end-of-tokens) that terminates the header line.
        let title_start = 2;
        let mut title_end = title_start;
        while title_end < tokens.len() && !matches!(tokens[title_end].0, Token::Newline) {
            title_end += 1;
        }

        if title_start < title_end {
            let title_tokens = &tokens[title_start..title_end];
            let title_inlines = run_inline_parser(title_tokens, source, idx);

            let header_start = tokens[0].1.start;
            let header_end = tokens[title_end - 1].1.end;
            let header_span = SourceSpan {
                start: header_start,
                end: header_end,
            };

            let header = Header {
                title: title_inlines,
                location: Some(idx.location(&header_span)),
            };

            // Advance past the terminating Newline (if present).
            let next = if title_end < tokens.len() {
                title_end + 1
            } else {
                title_end
            };

            return (Some(header), next);
        }
    }

    (None, 0)
}

/// Build block-level ASG nodes from a body token stream.
///
/// For the first pass this only produces paragraph blocks, split at blank
/// lines (two or more consecutive `Newline` tokens).
fn build_blocks<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> Vec<Block<'src>> {
    let groups = split_paragraphs(tokens);
    groups
        .into_iter()
        .map(|para_tokens| make_paragraph(para_tokens, source, idx))
        .collect()
}

/// Split a token stream into paragraph groups at blank lines.
///
/// A blank line is two or more consecutive `Newline` tokens. Each returned
/// slice contains the content tokens of one paragraph (no leading/trailing
/// newlines).
fn split_paragraphs<'a, 'src>(tokens: &'a [Spanned<'src>]) -> Vec<&'a [Spanned<'src>]> {
    let mut paragraphs = Vec::new();
    let mut i = 0;

    // Skip leading newlines.
    while i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
        i += 1;
    }

    let mut para_start = i;

    while i < tokens.len() {
        if matches!(tokens[i].0, Token::Newline) {
            let newline_start = i;

            // Count consecutive newlines.
            let mut newline_count = 0;
            while i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
                newline_count += 1;
                i += 1;
            }

            if newline_count >= 2 {
                // Blank line boundary — close the current paragraph.
                if para_start < newline_start {
                    paragraphs.push(&tokens[para_start..newline_start]);
                }
                para_start = i;
            }
            // A single newline is part of the paragraph (multi-line para).
        } else {
            i += 1;
        }
    }

    // Trailing paragraph (may end without a blank line).
    if para_start < tokens.len() {
        let end = strip_trailing_newline_index(tokens, para_start);
        if para_start < end {
            paragraphs.push(&tokens[para_start..end]);
        }
    }

    paragraphs
}

/// Build a paragraph `Block` from its content tokens.
fn make_paragraph<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> Block<'src> {
    let trimmed = strip_trailing_newlines(tokens);
    let inlines = run_inline_parser(trimmed, source, idx);

    let span = content_span(trimmed);
    let location = span.map(|s| idx.location(&s));

    Block {
        name: "paragraph",
        form: None,
        delimiter: None,
        title: None,
        level: None,
        variant: None,
        marker: None,
        inlines: Some(inlines),
        blocks: None,
        items: None,
        principal: None,
        location,
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Compute the overall `SourceSpan` covering all tokens in a slice,
/// or `None` if the slice is empty.
fn content_span(tokens: &[Spanned<'_>]) -> Option<SourceSpan> {
    let trimmed = strip_trailing_newlines(tokens);
    if trimmed.is_empty() {
        return None;
    }
    let start = trimmed.first().expect("non-empty").1.start;
    let end = trimmed.last().expect("non-empty").1.end;
    Some(SourceSpan { start, end })
}

/// Return a sub-slice with trailing `Newline` tokens removed.
fn strip_trailing_newlines<'a, 'src>(tokens: &'a [Spanned<'src>]) -> &'a [Spanned<'src>] {
    let end = strip_trailing_newline_index(tokens, 0);
    &tokens[..end]
}

/// Return the exclusive end index after stripping trailing newlines
/// starting the search from `from`.
fn strip_trailing_newline_index(tokens: &[Spanned<'_>], from: usize) -> usize {
    let mut end = tokens.len();
    while end > from && matches!(tokens[end - 1].0, Token::Newline) {
        end -= 1;
    }
    end
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::asg::Position;

    // -- Inline parsing tests -----------------------------------------------

    #[test]
    fn inline_plain_text() {
        let nodes = parse_inlines("hello");
        assert_eq!(nodes.len(), 1);
        match &nodes[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "hello");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 1, col: 1 });
                assert_eq!(loc[1], Position { line: 1, col: 5 });
            }
            InlineNode::Span(_) => panic!("expected Text node"),
        }
    }

    #[test]
    fn inline_strong() {
        let nodes = parse_inlines("*s*");
        assert_eq!(nodes.len(), 1);
        match &nodes[0] {
            InlineNode::Span(s) => {
                assert_eq!(s.variant, "strong");
                assert_eq!(s.form, "constrained");
                assert_eq!(s.inlines.len(), 1);
                match &s.inlines[0] {
                    InlineNode::Text(t) => assert_eq!(t.value, "s"),
                    InlineNode::Span(_) => panic!("expected inner Text"),
                }
                let loc = s.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 1, col: 1 });
                assert_eq!(loc[1], Position { line: 1, col: 3 });
            }
            InlineNode::Text(_) => panic!("expected Span node"),
        }
    }

    // -- Document parsing tests ---------------------------------------------

    #[test]
    fn doc_empty() {
        let doc = parse_doc("");
        assert!(doc.blocks.is_empty());
        assert!(doc.header.is_none());
        assert!(doc.location.is_none());
    }

    #[test]
    fn doc_single_paragraph() {
        let doc = parse_doc("hello world");
        assert_eq!(doc.blocks.len(), 1);
        assert_eq!(doc.blocks[0].name, "paragraph");
        let inlines = doc.blocks[0].inlines.as_ref().unwrap();
        assert_eq!(inlines.len(), 1);
        match &inlines[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "hello world"),
            InlineNode::Span(_) => panic!("expected Text"),
        }
    }

    #[test]
    fn doc_sibling_paragraphs() {
        let doc = parse_doc("para1\n\npara2");
        assert_eq!(doc.blocks.len(), 2);
        assert_eq!(doc.blocks[0].name, "paragraph");
        assert_eq!(doc.blocks[1].name, "paragraph");
    }

    #[test]
    fn doc_header_body() {
        let doc = parse_doc("= Title\n\nbody");
        assert!(doc.header.is_some());
        let header = doc.header.as_ref().unwrap();
        assert_eq!(header.title.len(), 1);
        match &header.title[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "Title"),
            InlineNode::Span(_) => panic!("expected Text"),
        }
        assert_eq!(doc.blocks.len(), 1);
        assert!(doc.attributes.is_some());
    }

    #[test]
    fn doc_body_only_no_attributes() {
        let doc = parse_doc("just text");
        assert!(doc.attributes.is_none());
        assert!(doc.header.is_none());
    }

    #[test]
    fn doc_multiple_blank_lines() {
        let doc = parse_doc("a\n\n\nb");
        assert_eq!(doc.blocks.len(), 2);
    }
}

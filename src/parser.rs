//! Parser that transforms a token stream into ASG nodes.
//!
//! The **inline parser** is chumsky-based (recursive, composable for adding
//! formatting variants). The **document/block parser** is procedural (a natural
//! fit for `AsciiDoc`'s line-oriented block structure).

use std::collections::HashMap;

use chumsky::{extra, input::ValueInput, prelude::*};

use crate::asg::{Block, Document, Header, InlineNode, SpanNode, TextNode};
use crate::diagnostic::{ParseDiagnostic, Severity};
use crate::lexer::lex;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// A token paired with its source span (re-stated here to avoid a name
/// conflict with [`chumsky::span::Spanned`]).
type Spanned<'a> = (Token<'a>, SourceSpan);

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse a full `AsciiDoc` document into its ASG and any diagnostics.
#[must_use]
pub fn parse_doc(input: &str) -> (Document<'_>, Vec<ParseDiagnostic>) {
    let tokens = lex(input);
    let idx = SourceIndex::new(input);

    if tokens.is_empty() {
        return (
            Document {
                attributes: None,
                header: None,
                blocks: Vec::new(),
                location: None,
            },
            Vec::new(),
        );
    }

    let (header, body_start, mut diagnostics) = extract_header(&tokens, input, &idx);
    let body_tokens = &tokens[body_start..];
    let (blocks, block_diags) = build_blocks(body_tokens, input, &idx);
    diagnostics.extend(block_diags);

    // Document location: first content token → last content token.
    let doc_span = content_span(&tokens);
    let location = doc_span.map(|s| idx.location(&s));

    (
        Document {
            attributes: if header.is_some() {
                Some(HashMap::new())
            } else {
                None
            },
            header,
            blocks,
            location,
        },
        diagnostics,
    )
}

/// Parse `AsciiDoc` inline content into inline nodes and any diagnostics.
#[must_use]
pub fn parse_inlines(input: &str) -> (Vec<InlineNode<'_>>, Vec<ParseDiagnostic>) {
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
///
/// The `star_as_text` fallback lives *outside* the recursive parser so that it
/// is only available at the top level — inside delimited spans (e.g., `*…*`)
/// a `Star` token is never consumed as literal text, allowing `delimited_by`
/// to match the closing delimiter.
fn inline_parser<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Vec<InlineNode<'src>>, extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>>
       + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // The recursive parser handles a single inline node (strong span or text
    // run). It does NOT include `star_as_text`, so a `Star` token inside a
    // delimited span remains available for the closing delimiter.
    let single_inline = recursive(|single_inline| {
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
        let inner_content = single_inline
            .repeated()
            .at_least(1)
            .collect::<Vec<InlineNode<'src>>>();

        let strong = inner_content
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
    });

    // Lone star fallback: a `*` that doesn't start a strong span is treated
    // as literal text. This is only available at the top level.
    let star_as_text = just(Token::Star).map_with(move |_tok, e| {
        let span: SourceSpan = e.span();
        InlineNode::Text(TextNode {
            value: &source[span.start..span.end],
            location: Some(idx.location(&span)),
        })
    });

    choice((single_inline, star_as_text))
        .repeated()
        .at_least(1)
        .collect()
}

/// Run the inline parser on a token sub-slice, returning nodes and diagnostics.
fn run_inline_parser<'tokens, 'src: 'tokens>(
    tokens: &'tokens [Spanned<'src>],
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> (Vec<InlineNode<'src>>, Vec<ParseDiagnostic>) {
    let Some(last) = tokens.last() else {
        return (Vec::new(), Vec::new());
    };
    let last_span = last.1;
    let eoi = SourceSpan {
        start: last_span.end,
        end: last_span.end,
    };
    let input = tokens.split_token_span(eoi);
    let (output, errors) = inline_parser(source, idx)
        .parse(input)
        .into_output_errors();

    let diagnostics = errors
        .into_iter()
        .map(|e| ParseDiagnostic {
            span: *e.span(),
            message: e.to_string(),
            severity: Severity::Error,
        })
        .collect();

    (output.unwrap_or_default(), diagnostics)
}

// ---------------------------------------------------------------------------
// Document / block parser (procedural)
// ---------------------------------------------------------------------------

/// Detect a document header (`= Title`) at the start of the token stream.
///
/// Returns `(Some(header), first_body_token_index, diagnostics)` when a header
/// is found, or `(None, 0, diagnostics)` otherwise.
fn extract_header<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Option<Header<'src>>, usize, Vec<ParseDiagnostic>) {
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
            let (title_inlines, diagnostics) = run_inline_parser(title_tokens, source, idx);

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

            return (Some(header), next, diagnostics);
        }
    }

    (None, 0, Vec::new())
}

/// Build block-level ASG nodes from a body token stream.
///
/// For the first pass this only produces paragraph blocks, split at blank
/// lines (two or more consecutive `Newline` tokens).
fn build_blocks<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let groups = split_paragraphs(tokens);
    let mut blocks = Vec::with_capacity(groups.len());
    let mut diagnostics = Vec::new();
    for para_tokens in groups {
        let (block, diags) = make_paragraph(para_tokens, source, idx);
        blocks.push(block);
        diagnostics.extend(diags);
    }
    (blocks, diagnostics)
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
) -> (Block<'src>, Vec<ParseDiagnostic>) {
    let trimmed = strip_trailing_newlines(tokens);
    let (inlines, diagnostics) = run_inline_parser(trimmed, source, idx);

    let span = content_span(trimmed);
    let location = span.map(|s| idx.location(&s));

    (
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
        },
        diagnostics,
    )
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Compute the overall `SourceSpan` covering all tokens in a slice,
/// or `None` if the slice is empty.
fn content_span(tokens: &[Spanned<'_>]) -> Option<SourceSpan> {
    let trimmed = strip_trailing_newlines(tokens);
    let start = trimmed.first()?.1.start;
    let end = trimmed.last()?.1.end;
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
        let (nodes, diags) = parse_inlines("hello");
        assert!(diags.is_empty());
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
        let (nodes, diags) = parse_inlines("*s*");
        assert!(diags.is_empty());
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

    #[test]
    fn inline_lone_star_is_text() {
        let (nodes, diags) = parse_inlines("*hello");
        assert!(diags.is_empty());
        assert_eq!(nodes.len(), 2);
        match &nodes[0] {
            InlineNode::Text(t) => assert_eq!(t.value, "*"),
            InlineNode::Span(_) => panic!("expected Text for lone star"),
        }
        match &nodes[1] {
            InlineNode::Text(t) => assert_eq!(t.value, "hello"),
            InlineNode::Span(_) => panic!("expected Text"),
        }
    }

    // -- Document parsing tests ---------------------------------------------

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
            InlineNode::Span(_) => panic!("expected Text"),
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
            InlineNode::Span(_) => panic!("expected Text"),
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
}

//! Inline literal and passthrough parsers.

use chumsky::{input::ValueInput, prelude::*};

use crate::asg::{InlineNode, RawNode, TextNode};
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

use super::ParseExtra;

/// Parsers for inline literal (plus delimiters): unconstrained (`++..++`)
/// and constrained (`+..+`).
///
/// Unlike code spans, inline literals produce text nodes with the content value
/// but location spanning the full delimited form.
///
/// Returns `(unconstrained, constrained)`.
pub(super) fn inline_literal_parsers<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> (
    impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
    impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
)
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // Content: any tokens except Plus
    let literal_content = any()
        .filter(|t: &Token| !matches!(t, Token::Plus))
        .repeated()
        .at_least(1);

    // Unconstrained: ++content++
    let unconstrained = just(Token::Plus)
        .then(just(Token::Plus))
        .ignore_then(literal_content)
        .then_ignore(just(Token::Plus).then(just(Token::Plus)))
        .map_with(move |_toks, e| {
            let span: SourceSpan = e.span();
            // Extract content between delimiters (skip first 2 and last 2 bytes)
            let content_start = span.start + 2;
            let content_end = span.end - 2;
            let value = &source[content_start..content_end];
            InlineNode::Text(TextNode {
                value,
                location: Some(idx.location(&span)),
            })
        });

    // Constrained: +content+
    let constrained = just(Token::Plus)
        .ignore_then(literal_content)
        .then_ignore(just(Token::Plus))
        .map_with(move |_toks, e| {
            let span: SourceSpan = e.span();
            // Extract content between delimiters (skip first and last byte)
            let content_start = span.start + 1;
            let content_end = span.end - 1;
            let value = &source[content_start..content_end];
            InlineNode::Text(TextNode {
                value,
                location: Some(idx.location(&span)),
            })
        });

    (unconstrained, constrained)
}

/// Parser for triple-plus passthrough (`+++..+++`).
///
/// Produces a raw node with the content passed through without processing.
pub(super) fn triple_plus_passthrough_parser<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // Content: any tokens except Plus sequences of 3+
    // We'll be greedy and stop at +++
    let raw_content = any()
        .filter(|t: &Token| !matches!(t, Token::Plus))
        .repeated()
        .at_least(1);

    just(Token::Plus)
        .then(just(Token::Plus))
        .then(just(Token::Plus))
        .ignore_then(raw_content)
        .then_ignore(just(Token::Plus).then(just(Token::Plus)).then(just(Token::Plus)))
        .map_with(move |_toks, e| {
            let span: SourceSpan = e.span();
            // Extract content between delimiters (skip first 3 and last 3 bytes)
            let content_start = span.start + 3;
            let content_end = span.end - 3;
            let value = &source[content_start..content_end];
            InlineNode::Raw(RawNode {
                value,
                location: Some(idx.location(&span)),
            })
        })
}

//! Inline span parsers for formatting (strong, emphasis, code, mark).

use chumsky::{input::ValueInput, prelude::*};

use crate::asg::{InlineNode, SpanNode, TextNode};
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

use super::ParseExtra;

/// Parser for escaped span delimiters: `\*`, `\_`, `` \` ``, `\#` → literal text.
pub(super) fn escaped_delimiter_parser<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let span_delim = choice((
        just(Token::Star),
        just(Token::Underscore),
        just(Token::Backtick),
        just(Token::Hash),
    ));
    just(Token::Backslash)
        .then(span_delim)
        .map_with(move |_toks, e| {
            let span: SourceSpan = e.span();
            InlineNode::Text(TextNode {
                value: &source[span.start + 1..span.end],
                location: Some(idx.location(&span)),
            })
        })
}

/// Parsers for inline code spans: unconstrained (``` ``code`` ```) and
/// constrained (`` `code` ``). Code content is verbatim (no nested formatting).
///
/// Returns `(unconstrained, constrained)` — unconstrained must be tried first
/// so that double-backtick delimiters are not consumed as two single backticks.
///
/// The constrained parser also accepts `BacktickEscaped` as an opener to handle
/// escaped unconstrained delimiters (backslash followed by double backticks).
pub(super) fn code_span_parsers<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> (
    impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
    impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
)
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let code_content = any()
        .filter(|t: &Token| !matches!(t, Token::Backtick | Token::BacktickEscaped))
        .repeated()
        .at_least(1)
        .map_with(move |_toks, e| {
            let span: SourceSpan = e.span();
            vec![InlineNode::Text(TextNode {
                value: &source[span.start..span.end],
                location: Some(idx.location(&span)),
            })]
        });

    let unconstrained = code_content
        .delimited_by(
            just(Token::Backtick).then(just(Token::Backtick)),
            just(Token::Backtick).then(just(Token::Backtick)),
        )
        .map_with(move |inlines: Vec<InlineNode<'src>>, e| {
            let span: SourceSpan = e.span();
            InlineNode::Span(SpanNode {
                variant: "code",
                form: "unconstrained",
                inlines,
                location: Some(idx.location(&span)),
            })
        });

    // Constrained opener: Backtick or BacktickEscaped (for escaped unconstrained)
    let constrained_opener = choice((just(Token::Backtick), just(Token::BacktickEscaped)));

    let constrained = constrained_opener
        .ignore_then(code_content)
        .then_ignore(just(Token::Backtick))
        .map_with(move |inlines: Vec<InlineNode<'src>>, e| {
            let span: SourceSpan = e.span();
            InlineNode::Span(SpanNode {
                variant: "code",
                form: "constrained",
                inlines,
                location: Some(idx.location(&span)),
            })
        });

    (unconstrained, constrained)
}

/// Parsers for inline strong spans: unconstrained (`**strong**`) and
/// constrained (`*strong*`). Strong content supports nested formatting.
///
/// Returns `(unconstrained, constrained)` — unconstrained must be tried first
/// so that double-star delimiters are not consumed as two single stars.
///
/// The constrained parser also accepts `StarEscaped` as an opener to handle
/// escaped unconstrained delimiters (`\**`).
pub(super) fn strong_span_parsers<'tokens, 'src: 'tokens, I, P>(
    inner: P,
    idx: &'tokens SourceIndex,
) -> (
    impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
    impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
)
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
    P: Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
{
    // For unconstrained (**), use negative lookahead to stop before **.
    // This prevents the inner parser from consuming potential closing delimiters.
    let not_double_star = just(Token::Star)
        .then(just(Token::Star))
        .not()
        .rewind();

    let inner_unconstrained = not_double_star
        .ignore_then(inner.clone())
        .repeated()
        .at_least(1)
        .collect::<Vec<InlineNode<'src>>>();

    let unconstrained = inner_unconstrained
        .delimited_by(
            just(Token::Star).then(just(Token::Star)),
            just(Token::Star).then(just(Token::Star)),
        )
        .map_with(move |inlines: Vec<InlineNode<'src>>, e| {
            let span: SourceSpan = e.span();
            InlineNode::Span(SpanNode {
                variant: "strong",
                form: "unconstrained",
                inlines,
                location: Some(idx.location(&span)),
            })
        });

    // For constrained (*), use negative lookahead to stop before single *.
    // This prevents exponential backtracking when parsing alternating spans.
    let not_star = just(Token::Star).not().rewind();

    let inner_constrained = not_star
        .ignore_then(inner)
        .repeated()
        .at_least(1)
        .collect::<Vec<InlineNode<'src>>>();

    // Constrained opener: Star or StarEscaped (for escaped unconstrained)
    let constrained_opener = choice((just(Token::Star), just(Token::StarEscaped)));

    let constrained = constrained_opener
        .ignore_then(inner_constrained)
        .then_ignore(just(Token::Star))
        .map_with(move |inlines: Vec<InlineNode<'src>>, e| {
            let span: SourceSpan = e.span();
            InlineNode::Span(SpanNode {
                variant: "strong",
                form: "constrained",
                inlines,
                location: Some(idx.location(&span)),
            })
        });

    (unconstrained, constrained)
}

/// Parsers for inline emphasis spans: unconstrained (`__emphasis__`) and
/// constrained (`_emphasis_`). Emphasis content supports nested formatting.
///
/// Returns `(unconstrained, constrained)` — unconstrained must be tried first
/// so that double-underscore delimiters are not consumed as two single underscores.
///
/// The constrained parser also accepts `UnderscoreEscaped` as an opener to handle
/// escaped unconstrained delimiters (`\__`).
pub(super) fn emphasis_span_parsers<'tokens, 'src: 'tokens, I, P>(
    inner: P,
    idx: &'tokens SourceIndex,
) -> (
    impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
    impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
)
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
    P: Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
{
    // For unconstrained (__), use negative lookahead to stop before __.
    let not_double_underscore = just(Token::Underscore)
        .then(just(Token::Underscore))
        .not()
        .rewind();

    let inner_unconstrained = not_double_underscore
        .ignore_then(inner.clone())
        .repeated()
        .at_least(1)
        .collect::<Vec<InlineNode<'src>>>();

    let unconstrained = inner_unconstrained
        .delimited_by(
            just(Token::Underscore).then(just(Token::Underscore)),
            just(Token::Underscore).then(just(Token::Underscore)),
        )
        .map_with(move |inlines: Vec<InlineNode<'src>>, e| {
            let span: SourceSpan = e.span();
            InlineNode::Span(SpanNode {
                variant: "emphasis",
                form: "unconstrained",
                inlines,
                location: Some(idx.location(&span)),
            })
        });

    // For constrained (_), use negative lookahead to stop before single _.
    let not_underscore = just(Token::Underscore).not().rewind();

    let inner_constrained = not_underscore
        .ignore_then(inner)
        .repeated()
        .at_least(1)
        .collect::<Vec<InlineNode<'src>>>();

    // Constrained opener: Underscore or UnderscoreEscaped (for escaped unconstrained)
    let constrained_opener = choice((just(Token::Underscore), just(Token::UnderscoreEscaped)));

    let constrained = constrained_opener
        .ignore_then(inner_constrained)
        .then_ignore(just(Token::Underscore))
        .map_with(move |inlines: Vec<InlineNode<'src>>, e| {
            let span: SourceSpan = e.span();
            InlineNode::Span(SpanNode {
                variant: "emphasis",
                form: "constrained",
                inlines,
                location: Some(idx.location(&span)),
            })
        });

    (unconstrained, constrained)
}

/// Parsers for inline mark spans: unconstrained (`##mark##`) and
/// constrained (`#mark#`). Mark content supports nested formatting.
///
/// Returns `(unconstrained, constrained)` — unconstrained must be tried first
/// so that double-hash delimiters are not consumed as two single hashes.
///
/// The constrained parser also accepts `HashEscaped` as an opener to handle
/// escaped unconstrained delimiters (`\##`).
pub(super) fn mark_span_parsers<'tokens, 'src: 'tokens, I, P>(
    inner: P,
    idx: &'tokens SourceIndex,
) -> (
    impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
    impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
)
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
    P: Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens,
{
    // For unconstrained (##), use negative lookahead to stop before ##.
    let not_double_hash = just(Token::Hash).then(just(Token::Hash)).not().rewind();

    let inner_unconstrained = not_double_hash
        .ignore_then(inner.clone())
        .repeated()
        .at_least(1)
        .collect::<Vec<InlineNode<'src>>>();

    let unconstrained = inner_unconstrained
        .delimited_by(
            just(Token::Hash).then(just(Token::Hash)),
            just(Token::Hash).then(just(Token::Hash)),
        )
        .map_with(move |inlines: Vec<InlineNode<'src>>, e| {
            let span: SourceSpan = e.span();
            InlineNode::Span(SpanNode {
                variant: "mark",
                form: "unconstrained",
                inlines,
                location: Some(idx.location(&span)),
            })
        });

    // For constrained (#), use negative lookahead to stop before single #.
    let not_hash = just(Token::Hash).not().rewind();

    let inner_constrained = not_hash
        .ignore_then(inner)
        .repeated()
        .at_least(1)
        .collect::<Vec<InlineNode<'src>>>();

    // Constrained opener: Hash or HashEscaped (for escaped unconstrained)
    let constrained_opener = choice((just(Token::Hash), just(Token::HashEscaped)));

    let constrained = constrained_opener
        .ignore_then(inner_constrained)
        .then_ignore(just(Token::Hash))
        .map_with(move |inlines: Vec<InlineNode<'src>>, e| {
            let span: SourceSpan = e.span();
            InlineNode::Span(SpanNode {
                variant: "mark",
                form: "constrained",
                inlines,
                location: Some(idx.location(&span)),
            })
        });

    (unconstrained, constrained)
}

//! Inline parser: chumsky-based formatting parser + procedural macro detection.

use chumsky::{extra, input::ValueInput, prelude::*};

use super::Spanned;
use crate::asg::{InlineNode, RefNode, SpanNode, TextNode};
use crate::diagnostic::{ParseDiagnostic, Severity};
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Shorthand for the chumsky error type used by all inline parsers.
type ParseExtra<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>;

/// Parser for escaped span delimiters: `\*`, `\_`, `` \` ``, `\#` → literal text.
fn escaped_delimiter_parser<'tokens, 'src: 'tokens, I>(
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
fn code_span_parsers<'tokens, 'src: 'tokens, I>(
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
        .filter(|t: &Token| !matches!(t, Token::Backtick))
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

    let constrained = code_content
        .delimited_by(just(Token::Backtick), just(Token::Backtick))
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
fn strong_span_parsers<'tokens, 'src: 'tokens, I, P>(
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
    let inner_content = inner
        .repeated()
        .at_least(1)
        .collect::<Vec<InlineNode<'src>>>();

    let unconstrained = inner_content
        .clone()
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

    let constrained = inner_content
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

    (unconstrained, constrained)
}

/// Parsers for inline emphasis spans: unconstrained (`__emphasis__`) and
/// constrained (`_emphasis_`). Emphasis content supports nested formatting.
///
/// Returns `(unconstrained, constrained)` — unconstrained must be tried first
/// so that double-underscore delimiters are not consumed as two single underscores.
fn emphasis_span_parsers<'tokens, 'src: 'tokens, I, P>(
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
    let inner_content = inner
        .repeated()
        .at_least(1)
        .collect::<Vec<InlineNode<'src>>>();

    let unconstrained = inner_content
        .clone()
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

    let constrained = inner_content
        .delimited_by(just(Token::Underscore), just(Token::Underscore))
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
fn mark_span_parsers<'tokens, 'src: 'tokens, I, P>(
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
    let inner_content = inner
        .repeated()
        .at_least(1)
        .collect::<Vec<InlineNode<'src>>>();

    let unconstrained = inner_content
        .clone()
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

    let constrained = inner_content
        .delimited_by(just(Token::Hash), just(Token::Hash))
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

/// Parser that treats a single token as literal text.
fn token_as_text<'tokens, 'src: 'tokens, I>(
    token: Token<'src>,
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(token).map_with(move |_tok, e| {
        let span: SourceSpan = e.span();
        InlineNode::Text(TextNode {
            value: &source[span.start..span.end],
            location: Some(idx.location(&span)),
        })
    })
}

/// Build the recursive inline parser that recognises spans and delimiters.
///
/// Returns a single-node parser. Delimiter tokens (`*`, `` ` ``, `_`, `#`)
/// that do not open a span are **not** consumed here — they are handled by
/// top-level fallbacks so that closing delimiters remain available inside
/// nested spans.
///
/// `pre_parsed` holds inline nodes for `Placeholder` tokens injected by the
/// macro detection pass. When no macros are present the slice is empty.
fn single_inline_parser<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
    pre_parsed: &'tokens [InlineNode<'src>],
) -> impl Parser<'tokens, I, InlineNode<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    recursive(|single_inline| {
        let text_run = any()
            .filter(|t: &Token| {
                !matches!(
                    t,
                    Token::Star
                        | Token::Backslash
                        | Token::Backtick
                        | Token::Underscore
                        | Token::Hash
                        | Token::Placeholder(_)
                )
            })
            .repeated()
            .at_least(1)
            .map_with(move |_toks, e| {
                let span: SourceSpan = e.span();
                InlineNode::Text(TextNode {
                    value: &source[span.start..span.end],
                    location: Some(idx.location(&span)),
                })
            });

        let escaped = escaped_delimiter_parser(source, idx);

        let (strong_unconstrained, strong_constrained) =
            strong_span_parsers(single_inline.clone(), idx);
        let (code_unconstrained, code_constrained) = code_span_parsers(source, idx);
        let (emphasis_unconstrained, emphasis_constrained) =
            emphasis_span_parsers(single_inline.clone(), idx);
        let (mark_unconstrained, mark_constrained) = mark_span_parsers(single_inline, idx);

        let backslash_as_text = token_as_text(Token::Backslash, source, idx);

        // Match a Placeholder token and return the pre-parsed inline node.
        let placeholder = any()
            .filter(|t: &Token| matches!(t, Token::Placeholder(_)))
            .map(move |tok| match tok {
                Token::Placeholder(i) => pre_parsed[i].clone(),
                _ => unreachable!(),
            });

        choice((
            strong_unconstrained,
            strong_constrained,
            code_unconstrained,
            code_constrained,
            emphasis_unconstrained,
            emphasis_constrained,
            mark_unconstrained,
            mark_constrained,
            placeholder,
            escaped,
            text_run,
            backslash_as_text,
        ))
    })
}

/// Build a recursive chumsky parser for inline content.
///
/// The parser produces `Vec<InlineNode>` from a token stream, using the source
/// string for zero-copy text slicing and the source index for locations.
///
/// Delimiter-as-text fallbacks live *outside* the recursive parser so that
/// delimiter tokens inside spans remain available for closing delimiters.
fn inline_parser<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
    pre_parsed: &'tokens [InlineNode<'src>],
) -> impl Parser<'tokens, I, Vec<InlineNode<'src>>, ParseExtra<'tokens, 'src>> + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let single_inline = single_inline_parser(source, idx, pre_parsed);

    let star_as_text = token_as_text(Token::Star, source, idx);
    let backtick_as_text = token_as_text(Token::Backtick, source, idx);
    let underscore_as_text = token_as_text(Token::Underscore, source, idx);
    let hash_as_text = token_as_text(Token::Hash, source, idx);

    // Catch-all recovery: if all grammar branches fail on a token, consume
    // it as a text node. This emits the original parse error as a diagnostic
    // and lets `.repeated()` continue with the next token.
    let catch_all = any().map_with(move |_tok, e| {
        let span: SourceSpan = e.span();
        InlineNode::Text(TextNode {
            value: &source[span.start..span.end],
            location: Some(idx.location(&span)),
        })
    });

    choice((
        single_inline,
        star_as_text,
        backtick_as_text,
        underscore_as_text,
        hash_as_text,
    ))
    .recover_with(via_parser(catch_all))
    .repeated()
    .at_least(1)
    .collect()
}

/// Run the chumsky inline parser on a token sub-slice without text merging.
fn run_chumsky_inline<'tokens, 'src: 'tokens>(
    tokens: &'tokens [Spanned<'src>],
    source: &'src str,
    idx: &'tokens SourceIndex,
    pre_parsed: &'tokens [InlineNode<'src>],
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
    let (output, errors) = inline_parser(source, idx, pre_parsed)
        .parse(input)
        .into_output_errors();

    let diagnostics: Vec<ParseDiagnostic> = errors
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
// Inline macro detection (procedural, runs before chumsky)
// ---------------------------------------------------------------------------

/// A detected inline macro (link, xref, or bare URL) in the token stream.
struct MacroMatch<'a> {
    /// Reference variant (`"link"` or `"xref"`).
    variant: &'static str,
    /// Target URL or path (zero-copy slice of source).
    target: &'a str,
    /// Token index of the first token in the macro.
    tok_start: usize,
    /// Token index past the last token in the macro.
    tok_end: usize,
    /// Token range of bracket content, if any (start inclusive, end exclusive).
    content_tok_range: Option<(usize, usize)>,
    /// Byte offset of the macro start in source.
    byte_start: usize,
    /// Byte offset of the macro end in source.
    byte_end: usize,
}

/// Scan the token stream for inline macros (link, xref, bare URL).
fn find_inline_macros<'src>(tokens: &[Spanned<'src>], source: &'src str) -> Vec<MacroMatch<'src>> {
    let mut matches = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        if let Some(m) = try_named_macro(tokens, i, source, "link", "link")
            .or_else(|| try_named_macro(tokens, i, source, "xref", "xref"))
            .or_else(|| try_bare_url(tokens, i, source))
        {
            let next = m.tok_end;
            matches.push(m);
            i = next;
        } else {
            i += 1;
        }
    }
    matches
}

/// Try to match a named inline macro (`link:target[content]` or
/// `xref:target[content]`) starting at token position `i`.
fn try_named_macro<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    macro_name: &str,
    variant: &'static str,
) -> Option<MacroMatch<'src>> {
    if i + 2 >= tokens.len() {
        return None;
    }
    let is_match = matches!(&tokens[i].0, Token::Text(s) if *s == macro_name);
    if !is_match || !matches!(tokens[i + 1].0, Token::Colon) {
        return None;
    }

    // Scan target: tokens after Colon up to LBracket.
    let target_start = i + 2;
    let mut j = target_start;
    while j < tokens.len()
        && !matches!(
            tokens[j].0,
            Token::LBracket | Token::Whitespace | Token::Newline
        )
    {
        j += 1;
    }
    if j >= tokens.len() || !matches!(tokens[j].0, Token::LBracket) || j == target_start {
        return None;
    }

    let target_byte_start = tokens[target_start].1.start;
    let target_byte_end = tokens[j - 1].1.end;
    let raw_target = &source[target_byte_start..target_byte_end];
    let target = if variant == "xref" {
        raw_target.strip_suffix(".adoc").unwrap_or(raw_target)
    } else {
        raw_target
    };

    // Find matching RBracket.
    let content_start = j + 1;
    let mut k = content_start;
    let mut depth: u32 = 1;
    while k < tokens.len() {
        match tokens[k].0 {
            Token::LBracket => depth += 1,
            Token::RBracket => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            _ => {}
        }
        k += 1;
    }
    if depth != 0 {
        return None;
    }

    // k is at the closing RBracket.
    Some(MacroMatch {
        variant,
        target,
        tok_start: i,
        tok_end: k + 1,
        content_tok_range: Some((content_start, k)),
        byte_start: tokens[i].1.start,
        byte_end: tokens[k].1.end,
    })
}

/// Try to match a bare URL (`https://...` or `http://...`) starting at
/// token position `i`.
fn try_bare_url<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
) -> Option<MacroMatch<'src>> {
    if i + 4 >= tokens.len() {
        return None;
    }
    let is_scheme = matches!(&tokens[i].0, Token::Text(s) if *s == "https" || *s == "http");
    if !is_scheme
        || !matches!(tokens[i + 1].0, Token::Colon)
        || !matches!(tokens[i + 2].0, Token::Slash)
        || !matches!(tokens[i + 3].0, Token::Slash)
    {
        return None;
    }

    // URL body: consume until whitespace, newline, or end.
    let mut j = i + 4;
    while j < tokens.len()
        && !matches!(
            tokens[j].0,
            Token::Whitespace | Token::Newline | Token::LBracket | Token::RBracket
        )
    {
        j += 1;
    }
    if j <= i + 4 {
        return None;
    }

    let byte_start = tokens[i].1.start;
    let byte_end = tokens[j - 1].1.end;
    let url = &source[byte_start..byte_end];

    Some(MacroMatch {
        variant: "link",
        target: url,
        tok_start: i,
        tok_end: j,
        content_tok_range: None,
        byte_start,
        byte_end,
    })
}

/// Run the inline parser on a token sub-slice, returning nodes and diagnostics.
///
/// Detects inline macros (link, xref, bare URL) procedurally, replaces
/// each macro's token range with a single `Placeholder` token, then runs
/// the chumsky parser on the unified stream so that span delimiters can
/// match across macro boundaries. Adjacent text nodes are merged.
pub(super) fn run_inline_parser<'tokens, 'src: 'tokens>(
    tokens: &'tokens [Spanned<'src>],
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> (Vec<InlineNode<'src>>, Vec<ParseDiagnostic>) {
    if tokens.is_empty() {
        return (Vec::new(), Vec::new());
    }

    let macros = find_inline_macros(tokens, source);

    if macros.is_empty() {
        let empty: &[InlineNode<'src>] = &[];
        let (nodes, diagnostics) = run_chumsky_inline(tokens, source, idx, empty);
        return (merge_text_nodes(nodes, source), diagnostics);
    }

    // Pre-parse each macro into an InlineNode and build a unified token
    // stream with Placeholder tokens replacing macro token ranges.
    let mut pre_parsed: Vec<InlineNode<'src>> = Vec::with_capacity(macros.len());
    let mut diagnostics = Vec::new();

    let empty: &[InlineNode<'src>] = &[];
    for m in &macros {
        let ref_inlines = if let Some((cs, ce)) = m.content_tok_range {
            let content_tokens = &tokens[cs..ce];
            let (nodes, diags) = run_chumsky_inline(content_tokens, source, idx, empty);
            diagnostics.extend(diags);
            nodes
        } else {
            // Bare URL: display text is the URL itself.
            let url_span = SourceSpan {
                start: m.byte_start,
                end: m.byte_end,
            };
            vec![InlineNode::Text(TextNode {
                value: m.target,
                location: Some(idx.location(&url_span)),
            })]
        };

        let macro_span = SourceSpan {
            start: m.byte_start,
            end: m.byte_end,
        };
        pre_parsed.push(InlineNode::Ref(RefNode {
            variant: m.variant,
            target: m.target,
            inlines: ref_inlines,
            location: Some(idx.location(&macro_span)),
        }));
    }

    // Build unified stream: copy non-macro tokens, replace each macro range
    // with a single Placeholder token.
    let mut unified: Vec<Spanned<'src>> = Vec::with_capacity(tokens.len());
    let mut pos = 0;
    for (i, m) in macros.iter().enumerate() {
        // Copy tokens before this macro.
        unified.extend_from_slice(&tokens[pos..m.tok_start]);
        // Insert placeholder with the macro's byte span.
        unified.push((
            Token::Placeholder(i),
            SourceSpan {
                start: m.byte_start,
                end: m.byte_end,
            },
        ));
        pos = m.tok_end;
    }
    // Copy tokens after the last macro.
    unified.extend_from_slice(&tokens[pos..]);

    let (nodes, chumsky_diags) = run_chumsky_inline(&unified, source, idx, &pre_parsed);
    diagnostics.extend(chumsky_diags);

    (merge_text_nodes(nodes, source), diagnostics)
}

/// Merge adjacent text nodes whose values are contiguous in the source.
///
/// After parsing, escaped delimiters and fallback text nodes may produce
/// multiple adjacent `TextNode`s that represent a single logical text run
/// (e.g., `\*not bold*` → escaped `*` + text run `not bold` + lone star `*`).
/// This function merges them into a single `TextNode` whose value is a single
/// slice of the source and whose location spans the full range.
///
/// Span nodes have their `inlines` recursively merged.
fn merge_text_nodes<'a>(nodes: Vec<InlineNode<'a>>, source: &'a str) -> Vec<InlineNode<'a>> {
    let source_base = source.as_ptr() as usize;
    let mut result: Vec<InlineNode<'a>> = Vec::with_capacity(nodes.len());

    for node in nodes {
        // Recursively merge inside span and ref nodes.
        let node = match node {
            InlineNode::Span(mut s) => {
                s.inlines = merge_text_nodes(s.inlines, source);
                InlineNode::Span(s)
            }
            InlineNode::Ref(mut r) => {
                r.inlines = merge_text_nodes(r.inlines, source);
                InlineNode::Ref(r)
            }
            InlineNode::Text(t) => InlineNode::Text(t),
        };

        // Try to merge with the previous text node if values are contiguous.
        let merged = if let InlineNode::Text(curr) = &node {
            if let Some(InlineNode::Text(prev)) = result.last_mut() {
                let prev_offset = prev.value.as_ptr() as usize - source_base;
                let curr_offset = curr.value.as_ptr() as usize - source_base;
                if prev_offset + prev.value.len() == curr_offset {
                    prev.value = &source[prev_offset..curr_offset + curr.value.len()];
                    if let (Some(prev_loc), Some(curr_loc)) = (&mut prev.location, &curr.location) {
                        prev_loc[1] = curr_loc[1].clone();
                    }
                    true
                } else {
                    false
                }
            } else {
                false
            }
        } else {
            false
        };

        if !merged {
            result.push(node);
        }
    }

    result
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::asg::{InlineNode, Position};
    use crate::parser::parse_inlines;

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
            _ => panic!("expected Text node"),
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
                    _ => panic!("expected inner Text"),
                }
                let loc = s.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 1, col: 1 });
                assert_eq!(loc[1], Position { line: 1, col: 3 });
            }
            _ => panic!("expected Span node"),
        }
    }

    #[test]
    fn inline_lone_star_is_text() {
        let (nodes, diags) = parse_inlines("*hello");
        assert!(diags.is_empty());
        assert_eq!(nodes.len(), 1);
        match &nodes[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "*hello");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 1, col: 1 });
                assert_eq!(loc[1], Position { line: 1, col: 6 });
            }
            _ => panic!("expected Text for lone star"),
        }
    }
}

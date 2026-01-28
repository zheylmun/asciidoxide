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
) -> impl Parser<'tokens, I, Vec<InlineNode<'src>>, ParseExtra<'tokens, 'src>> + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    // The recursive parser handles a single inline node (strong span, escaped
    // delimiter, text run, or literal backslash). It does NOT include
    // `star_as_text`, so a `Star` token inside a delimited span remains
    // available for the closing delimiter.
    let single_inline = recursive(|single_inline| {
        let text_run = any()
            .filter(|t: &Token| !matches!(t, Token::Star | Token::Backslash | Token::Backtick))
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

        let (code_unconstrained, code_constrained) = code_span_parsers(source, idx);

        let backslash_as_text = just(Token::Backslash).map_with(move |_tok, e| {
            let span: SourceSpan = e.span();
            InlineNode::Text(TextNode {
                value: &source[span.start..span.end],
                location: Some(idx.location(&span)),
            })
        });

        choice((
            strong,
            code_unconstrained,
            code_constrained,
            escaped,
            text_run,
            backslash_as_text,
        ))
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

    // Lone backtick fallback: a `` ` `` that doesn't start a code span is
    // treated as literal text. This is only available at the top level.
    let backtick_as_text = just(Token::Backtick).map_with(move |_tok, e| {
        let span: SourceSpan = e.span();
        InlineNode::Text(TextNode {
            value: &source[span.start..span.end],
            location: Some(idx.location(&span)),
        })
    });

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

    choice((single_inline, star_as_text, backtick_as_text))
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
    let (output, errors) = inline_parser(source, idx).parse(input).into_output_errors();

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
/// Detects inline macros (link, xref, bare URL) procedurally, parses
/// non-macro segments and bracket content through the chumsky inline parser,
/// and merges adjacent text nodes.
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
        let (nodes, diagnostics) = run_chumsky_inline(tokens, source, idx);
        return (merge_text_nodes(nodes, source), diagnostics);
    }

    let mut result = Vec::new();
    let mut diagnostics = Vec::new();
    let mut pos = 0;

    for m in &macros {
        // Parse tokens before this macro with chumsky.
        if pos < m.tok_start {
            let segment = &tokens[pos..m.tok_start];
            let (nodes, diags) = run_chumsky_inline(segment, source, idx);
            result.extend(nodes);
            diagnostics.extend(diags);
        }

        // Parse bracket content (if any) with chumsky.
        let ref_inlines = if let Some((cs, ce)) = m.content_tok_range {
            let content_tokens = &tokens[cs..ce];
            let (nodes, diags) = run_chumsky_inline(content_tokens, source, idx);
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
        result.push(InlineNode::Ref(RefNode {
            variant: m.variant,
            target: m.target,
            inlines: ref_inlines,
            location: Some(idx.location(&macro_span)),
        }));

        pos = m.tok_end;
    }

    // Parse tokens after the last macro.
    if pos < tokens.len() {
        let segment = &tokens[pos..];
        let (nodes, diags) = run_chumsky_inline(segment, source, idx);
        result.extend(nodes);
        diagnostics.extend(diags);
    }

    (merge_text_nodes(result, source), diagnostics)
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

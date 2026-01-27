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

/// Result of header extraction from the token stream.
struct HeaderResult<'src> {
    header: Option<Header<'src>>,
    /// Index of the first body token (past the header and its attributes).
    body_start: usize,
    diagnostics: Vec<ParseDiagnostic>,
    /// Document attributes parsed from `:key: value` lines below the title.
    /// `Some` when a header is present (even if no attribute entries exist).
    attributes: Option<HashMap<&'src str, &'src str>>,
}

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

    let HeaderResult {
        header,
        body_start,
        mut diagnostics,
        attributes,
    } = extract_header(&tokens, input, &idx);
    let body_tokens = &tokens[body_start..];
    let (blocks, block_diags) = build_blocks(body_tokens, input, &idx);
    diagnostics.extend(block_diags);

    // Document location: first content token → last content token.
    let doc_span = content_span(&tokens);
    let location = doc_span.map(|s| idx.location(&s));

    (
        Document {
            attributes,
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
    // The recursive parser handles a single inline node (strong span, escaped
    // delimiter, text run, or literal backslash). It does NOT include
    // `star_as_text`, so a `Star` token inside a delimited span remains
    // available for the closing delimiter.
    let single_inline = recursive(|single_inline| {
        // A text run: one or more tokens that are not `Star` or `Backslash`.
        // Backslash is excluded so the `escaped` combinator gets priority.
        let text_run = any()
            .filter(|t: &Token| !matches!(t, Token::Star | Token::Backslash))
            .repeated()
            .at_least(1)
            .map_with(move |_toks, e| {
                let span: SourceSpan = e.span();
                InlineNode::Text(TextNode {
                    value: &source[span.start..span.end],
                    location: Some(idx.location(&span)),
                })
            });

        // Escaped span delimiter: \* \_ \` \# → literal delimiter text.
        // The backslash is consumed; the value starts after it.
        let span_delim = choice((
            just(Token::Star),
            just(Token::Underscore),
            just(Token::Backtick),
            just(Token::Hash),
        ));
        let escaped = just(Token::Backslash)
            .then(span_delim)
            .map_with(move |_toks, e| {
                let span: SourceSpan = e.span();
                InlineNode::Text(TextNode {
                    value: &source[span.start + 1..span.end],
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

        // Literal backslash: a `\` not followed by a span delimiter.
        let backslash_as_text = just(Token::Backslash).map_with(move |_tok, e| {
            let span: SourceSpan = e.span();
            InlineNode::Text(TextNode {
                value: &source[span.start..span.end],
                location: Some(idx.location(&span)),
            })
        });

        choice((strong, escaped, text_run, backslash_as_text))
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

    choice((single_inline, star_as_text))
        .recover_with(via_parser(catch_all))
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

    let nodes = merge_text_nodes(output.unwrap_or_default(), source);
    (nodes, diagnostics)
}

// ---------------------------------------------------------------------------
// Document / block parser (procedural)
// ---------------------------------------------------------------------------

/// Detect a document header (`= Title`) at the start of the token stream.
///
/// When a header is found, `attributes` is `Some(map)` (possibly empty).
/// Attribute entry lines (`:key: value`) immediately following the title are
/// parsed into the map.
fn extract_header<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> HeaderResult<'src> {
    // Header requires at least: Eq Whitespace <content>
    if tokens.len() >= 3
        && matches!(tokens[0].0, Token::Eq)
        && matches!(tokens[1].0, Token::Whitespace)
    {
        // Find the Newline (or end-of-tokens) that terminates the title line.
        let title_start = 2;
        let mut title_end = title_start;
        while title_end < tokens.len() && !matches!(tokens[title_end].0, Token::Newline) {
            title_end += 1;
        }

        if title_start < title_end {
            let title_tokens = &tokens[title_start..title_end];
            let (title_inlines, diagnostics) = run_inline_parser(title_tokens, source, idx);

            let header_start = tokens[0].1.start;
            let mut span_end = tokens[title_end - 1].1.end;

            // Advance past the title's terminating Newline (if present).
            let mut i = if title_end < tokens.len() {
                title_end + 1
            } else {
                title_end
            };

            // Parse attribute entry lines: `:key: value` or `:key:`
            let mut attributes = HashMap::new();
            while i + 2 < tokens.len()
                && matches!(tokens[i].0, Token::Colon)
                && matches!(tokens[i + 1].0, Token::Text(_))
                && matches!(tokens[i + 2].0, Token::Colon)
            {
                let key = match &tokens[i + 1].0 {
                    Token::Text(s) => *s,
                    _ => unreachable!(),
                };

                // Skip optional whitespace after second colon.
                let mut val_start = i + 3;
                if val_start < tokens.len()
                    && matches!(tokens[val_start].0, Token::Whitespace)
                {
                    val_start += 1;
                }

                // Scan to Newline or end-of-tokens.
                let mut line_end = val_start;
                while line_end < tokens.len()
                    && !matches!(tokens[line_end].0, Token::Newline)
                {
                    line_end += 1;
                }

                // Extract value via source slicing (zero-copy).
                let value = if val_start < line_end {
                    let start = tokens[val_start].1.start;
                    let end = tokens[line_end - 1].1.end;
                    &source[start..end]
                } else {
                    ""
                };

                attributes.insert(key, value);

                // Update header span end to last content token on this line.
                let last_content = if line_end > val_start {
                    line_end - 1
                } else {
                    i + 2
                };
                span_end = tokens[last_content].1.end;

                // Advance past Newline (if present).
                i = if line_end < tokens.len() {
                    line_end + 1
                } else {
                    line_end
                };
            }

            let header_span = SourceSpan {
                start: header_start,
                end: span_end,
            };

            let header = Header {
                title: title_inlines,
                location: Some(idx.location(&header_span)),
            };

            return HeaderResult {
                header: Some(header),
                body_start: i,
                diagnostics,
                attributes: Some(attributes),
            };
        }
    }

    HeaderResult {
        header: None,
        body_start: 0,
        diagnostics: Vec::new(),
        attributes: None,
    }
}

/// Build block-level ASG nodes from a body token stream.
///
/// Scans tokens linearly, detecting delimited blocks (e.g., listing) before
/// falling back to paragraph collection. Blocks are separated by blank lines
/// (two or more consecutive `Newline` tokens).
fn build_blocks<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    let mut blocks = Vec::new();
    let mut diagnostics = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        // Skip inter-block newlines.
        while i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
            i += 1;
        }
        if i >= tokens.len() {
            break;
        }

        // Try delimited listing block.
        if let Some((block, next)) = try_listing(tokens, i, source, idx) {
            blocks.push(block);
            i = next;
            continue;
        }

        // Try delimited sidebar block.
        if let Some((block, next, diags)) = try_sidebar(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            i = next;
            continue;
        }

        // Try section heading.
        if let Some((block, next, diags)) = try_section(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            i = next;
            continue;
        }

        // Try unordered list.
        if let Some((block, next, diags)) = try_list(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
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
        i = para_end;
    }

    (blocks, diagnostics)
}

/// Find the end of a paragraph starting at `start`.
///
/// Stops at a blank line (2+ consecutive `Newline` tokens), at a line that
/// starts a listing delimiter, or at the end of the token stream. Returns
/// the index of the first token past the paragraph content (trailing
/// newlines are excluded from the paragraph).
fn find_paragraph_end(tokens: &[Spanned<'_>], start: usize) -> usize {
    let mut i = start;
    while i < tokens.len() {
        if matches!(tokens[i].0, Token::Newline) {
            let nl_start = i;
            let mut nl_count = 0;
            while i < tokens.len() && matches!(tokens[i].0, Token::Newline) {
                nl_count += 1;
                i += 1;
            }
            if nl_count >= 2 {
                // Blank line — end paragraph before the newlines.
                return nl_start;
            }
            // After a single newline, check if the next line starts a new block.
            if is_listing_delimiter(tokens, i).is_some()
                || is_sidebar_delimiter(tokens, i).is_some()
                || is_list_item(tokens, i)
                || is_section_heading(tokens, i).is_some()
            {
                return nl_start;
            }
        } else {
            i += 1;
        }
    }
    // Reached end of tokens — strip trailing newlines.
    strip_trailing_newline_index(tokens, start)
}

/// Check whether position `i` starts a listing delimiter line.
///
/// A listing delimiter is 4 or more consecutive `Hyphen` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
fn is_listing_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 3 >= tokens.len() {
        return None;
    }
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Hyphen) {
        j += 1;
    }
    if j - i < 4 {
        return None;
    }
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        j += 1;
    }
    Some(j)
}

/// Try to parse a delimited listing block starting at position `i`.
///
/// Returns `Some((block, next_index))` if a complete listing block (opening
/// **and** matching closing delimiter) is found, or `None` otherwise.
fn try_listing<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    let content_start = is_listing_delimiter(tokens, i)?;

    // Count opening hyphens to match against the closing delimiter.
    let mut delim_end_tok = i;
    while delim_end_tok < tokens.len() && matches!(tokens[delim_end_tok].0, Token::Hyphen) {
        delim_end_tok += 1;
    }
    let open_hyphen_count = delim_end_tok - i;
    let delimiter = &source[tokens[i].1.start..tokens[delim_end_tok - 1].1.end];

    // Scan line-by-line for a matching closing delimiter.
    let mut j = content_start;
    loop {
        if j >= tokens.len() {
            // No closing delimiter found.
            return None;
        }
        // Check for closing delimiter at start of this line.
        if let Some(after_close) = is_listing_delimiter(tokens, j) {
            let mut k = j;
            while k < tokens.len() && matches!(tokens[k].0, Token::Hyphen) {
                k += 1;
            }
            if k - j == open_hyphen_count {
                // Matching closing delimiter found.

                // Content tokens are content_start..before the Newline preceding
                // the closing delimiter.
                let content_end = if j > content_start
                    && matches!(tokens[j - 1].0, Token::Newline)
                {
                    j - 1
                } else {
                    j
                };

                let (value, content_location) = if content_start < content_end {
                    let start_byte = tokens[content_start].1.start;
                    let end_byte = tokens[content_end - 1].1.end;
                    let span = SourceSpan {
                        start: start_byte,
                        end: end_byte,
                    };
                    (&source[start_byte..end_byte], Some(idx.location(&span)))
                } else {
                    ("", None)
                };

                // Block location: opening delimiter through closing delimiter.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k - 1].1.end,
                };

                let text_node = InlineNode::Text(TextNode {
                    value,
                    location: content_location,
                });

                let block = Block {
                    name: "listing",
                    form: Some("delimited"),
                    delimiter: Some(delimiter),
                    title: None,
                    level: None,
                    variant: None,
                    marker: None,
                    inlines: Some(vec![text_node]),
                    blocks: None,
                    items: None,
                    principal: None,
                    location: Some(idx.location(&block_span)),
                };

                return Some((block, after_close));
            }
        }
        // Advance to the next line.
        while j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
            j += 1;
        }
        if j < tokens.len() {
            j += 1;
        }
    }
}

/// Check whether position `i` starts a sidebar delimiter line.
///
/// A sidebar delimiter is 4 or more consecutive `Star` tokens with nothing
/// else on the line (followed by `Newline` or end-of-tokens). Returns the
/// index past the delimiter (past the `Newline` if present).
fn is_sidebar_delimiter(tokens: &[Spanned<'_>], i: usize) -> Option<usize> {
    if i + 3 >= tokens.len() {
        return None;
    }
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Star) {
        j += 1;
    }
    if j - i < 4 {
        return None;
    }
    // Must be followed by Newline or be at end-of-tokens.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }
    if j < tokens.len() {
        j += 1;
    }
    Some(j)
}

/// Try to parse a delimited sidebar block starting at position `i`.
///
/// A sidebar uses `****` delimiters and has a compound content model — its
/// content is recursively parsed through [`build_blocks`]. Returns `None` if
/// no complete sidebar (opening **and** matching closing delimiter) is found.
fn try_sidebar<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    let content_start = is_sidebar_delimiter(tokens, i)?;

    // Count opening stars to match against the closing delimiter.
    let mut delim_end_tok = i;
    while delim_end_tok < tokens.len() && matches!(tokens[delim_end_tok].0, Token::Star) {
        delim_end_tok += 1;
    }
    let open_star_count = delim_end_tok - i;
    let delimiter = &source[tokens[i].1.start..tokens[delim_end_tok - 1].1.end];

    // Scan line-by-line for a matching closing delimiter.
    let mut j = content_start;
    loop {
        if j >= tokens.len() {
            return None;
        }
        if let Some(after_close) = is_sidebar_delimiter(tokens, j) {
            let mut k = j;
            while k < tokens.len() && matches!(tokens[k].0, Token::Star) {
                k += 1;
            }
            if k - j == open_star_count {
                // Matching closing delimiter found.

                // Content tokens: exclude the Newline before the closing delimiter.
                let content_end = if j > content_start
                    && matches!(tokens[j - 1].0, Token::Newline)
                {
                    j - 1
                } else {
                    j
                };

                // Recursively parse content as blocks.
                let content_tokens = &tokens[content_start..content_end];
                let (body_blocks, body_diags) = build_blocks(content_tokens, source, idx);

                // Block location: opening delimiter through closing delimiter.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k - 1].1.end,
                };

                return Some((
                    Block {
                        name: "sidebar",
                        form: Some("delimited"),
                        delimiter: Some(delimiter),
                        title: None,
                        level: None,
                        variant: None,
                        marker: None,
                        inlines: None,
                        blocks: Some(body_blocks),
                        items: None,
                        principal: None,
                        location: Some(idx.location(&block_span)),
                    },
                    after_close,
                    body_diags,
                ));
            }
        }
        // Advance to the next line.
        while j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
            j += 1;
        }
        if j < tokens.len() {
            j += 1;
        }
    }
}

/// Check whether position `i` starts a section heading (2+ `Eq` followed by
/// `Whitespace`). Returns `(level, title_start_index)` where level is the
/// number of `Eq` tokens minus one.
fn is_section_heading(tokens: &[Spanned<'_>], i: usize) -> Option<(usize, usize)> {
    let mut j = i;
    while j < tokens.len() && matches!(tokens[j].0, Token::Eq) {
        j += 1;
    }
    let eq_count = j - i;
    if eq_count < 2 {
        return None;
    }
    if j >= tokens.len() || !matches!(tokens[j].0, Token::Whitespace) {
        return None;
    }
    Some((eq_count - 1, j + 1))
}

/// Try to parse a section starting at position `i`.
///
/// A section heading is 2+ `Eq` tokens followed by `Whitespace` and a title.
/// The section's body is all subsequent content (processed recursively through
/// [`build_blocks`]).
fn try_section<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    let (level, title_start) = is_section_heading(tokens, i)?;

    // Find end of title line.
    let mut title_end = title_start;
    while title_end < tokens.len() && !matches!(tokens[title_end].0, Token::Newline) {
        title_end += 1;
    }

    if title_start >= title_end {
        return None;
    }

    // Parse title through inline parser.
    let title_tokens = &tokens[title_start..title_end];
    let (title_inlines, mut diagnostics) = run_inline_parser(title_tokens, source, idx);

    // Body starts after the title's Newline.
    let body_start = if title_end < tokens.len() {
        title_end + 1
    } else {
        title_end
    };

    // The section consumes all remaining tokens.
    // TODO: Handle section nesting (stop at a heading of equal or higher level).
    let section_end = tokens.len();

    // Build body blocks.
    let body_tokens = &tokens[body_start..section_end];
    let (body_blocks, body_diags) = build_blocks(body_tokens, source, idx);
    diagnostics.extend(body_diags);

    // Section location: from first Eq to last content token.
    let section_tokens = &tokens[i..section_end];
    let section_span = content_span(section_tokens);
    let section_location = section_span.map(|s| idx.location(&s));

    Some((
        Block {
            name: "section",
            form: None,
            delimiter: None,
            title: Some(title_inlines),
            level: Some(level),
            variant: None,
            marker: None,
            inlines: None,
            blocks: Some(body_blocks),
            items: None,
            principal: None,
            location: section_location,
        },
        section_end,
        diagnostics,
    ))
}

/// Check whether position `i` starts an unordered list item (`Star Whitespace`).
fn is_list_item(tokens: &[Spanned<'_>], i: usize) -> bool {
    i + 1 < tokens.len()
        && matches!(tokens[i].0, Token::Star)
        && matches!(tokens[i + 1].0, Token::Whitespace)
}

/// Try to parse an unordered list starting at position `i`.
///
/// Collects consecutive list items (each starting with `Star Whitespace`) into
/// a single `list` block. Returns `None` if position `i` does not start a list
/// item.
fn try_list<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    if !is_list_item(tokens, i) {
        return None;
    }

    let mut items = Vec::new();
    let mut diagnostics = Vec::new();
    let mut j = i;
    let list_span_start = tokens[i].1.start;
    let mut list_span_end = tokens[i].1.end;

    while is_list_item(tokens, j) {
        let marker = &source[tokens[j].1.start..tokens[j].1.end];
        let item_start = tokens[j].1.start;

        // Content starts after Star Whitespace.
        let content_start = j + 2;
        let mut content_end = content_start;
        while content_end < tokens.len() && !matches!(tokens[content_end].0, Token::Newline) {
            content_end += 1;
        }

        // Parse principal content through the inline parser.
        let content_tokens = &tokens[content_start..content_end];
        let (principal, diags) = run_inline_parser(content_tokens, source, idx);
        diagnostics.extend(diags);

        // Item location: from the marker to the last content token.
        let item_end = if content_end > content_start {
            tokens[content_end - 1].1.end
        } else {
            tokens[j + 1].1.end
        };
        let item_span = SourceSpan {
            start: item_start,
            end: item_end,
        };

        items.push(Block {
            name: "listItem",
            form: None,
            delimiter: None,
            title: None,
            level: None,
            variant: None,
            marker: Some(marker),
            inlines: None,
            blocks: None,
            items: None,
            principal: Some(principal),
            location: Some(idx.location(&item_span)),
        });

        list_span_end = item_end;

        // Advance past the Newline (if present).
        j = if content_end < tokens.len() {
            content_end + 1
        } else {
            content_end
        };
    }

    let list_span = SourceSpan {
        start: list_span_start,
        end: list_span_end,
    };
    let marker = items[0].marker;

    Some((
        Block {
            name: "list",
            form: None,
            delimiter: None,
            title: None,
            level: None,
            variant: Some("unordered"),
            marker,
            inlines: None,
            blocks: None,
            items: Some(items),
            principal: None,
            location: Some(idx.location(&list_span)),
        },
        j,
        diagnostics,
    ))
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
        // Recursively merge inside span nodes.
        let node = match node {
            InlineNode::Span(mut s) => {
                s.inlines = merge_text_nodes(s.inlines, source);
                InlineNode::Span(s)
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
        assert_eq!(nodes.len(), 1);
        match &nodes[0] {
            InlineNode::Text(t) => {
                assert_eq!(t.value, "*hello");
                let loc = t.location.as_ref().unwrap();
                assert_eq!(loc[0], Position { line: 1, col: 1 });
                assert_eq!(loc[1], Position { line: 1, col: 6 });
            }
            InlineNode::Span(_) => panic!("expected Text for lone star"),
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
            InlineNode::Span(_) => panic!("expected Text node"),
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
            InlineNode::Span(_) => panic!("expected Text node"),
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
            InlineNode::Span(_) => panic!("expected Text node"),
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
}

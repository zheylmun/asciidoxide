//! Chumsky-based block parser implementation.
//!
//! This module provides block parsing using chumsky combinators. Block parsing
//! presents unique challenges compared to inline parsing:
//!
//! - **Delimiter matching**: Opening and closing delimiters must have the same count
//! - **Line-oriented**: Delimiters must be on their own lines
//! - **Recursive content**: Compound blocks contain nested blocks
//! - **Metadata propagation**: Block attributes and titles apply to the following block
//!
//! The approach uses chumsky for individual block parsers while maintaining
//! procedural orchestration for the main block loop that handles metadata.

use chumsky::{input::ValueInput, prelude::*};

use super::combinators::ParseExtra;
use super::Spanned;
use crate::asg::{Block, InlineNode, TextNode};
use crate::diagnostic::ParseDiagnostic;
use crate::parser::inline::run_inline_parser;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

// ---------------------------------------------------------------------------
// Helper Combinators
// ---------------------------------------------------------------------------

/// Parser that matches a newline or end of input.
fn line_end<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, (), ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    choice((just(Token::Newline).ignored(), end().rewind().ignored()))
}

/// Parser that matches n+ consecutive tokens of a given type, followed by line end.
/// Returns the count of matched tokens.
fn delimiter_run<'tokens, 'src: 'tokens, I>(
    token: Token<'src>,
    min_count: usize,
) -> impl Parser<'tokens, I, usize, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(token)
        .repeated()
        .at_least(min_count)
        .count()
        .then_ignore(line_end())
}

/// Parser that matches exactly n consecutive tokens of a given type, followed by line end.
fn exact_delimiter<'tokens, 'src: 'tokens, I>(
    token: Token<'src>,
    count: usize,
) -> impl Parser<'tokens, I, (), ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(token)
        .repeated()
        .exactly(count)
        .ignored()
        .then_ignore(line_end())
}

// ---------------------------------------------------------------------------
// Break Parsers
// ---------------------------------------------------------------------------

/// Thematic break parser (`'''`).
fn thematic_break<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Block<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::SingleQuote)
        .then(just(Token::SingleQuote))
        .then(just(Token::SingleQuote))
        .then_ignore(line_end())
        .map_with(move |_, e| {
            let span: SourceSpan = e.span();
            let mut block = Block::new("break");
            block.variant = Some("thematic");
            block.location = Some(idx.location(&span));
            block
        })
}

/// Page break parser (`<<<`).
///
/// Note: Due to chumsky's token matching semantics with borrowed strings,
/// we use `filter` instead of `just` for the `Text("<")` token.
fn page_break<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Block<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::DoubleLeftAngle)
        .then(any().filter(|t: &Token| matches!(t, Token::Text(s) if *s == "<")))
        .then_ignore(line_end())
        .map_with(move |_: (Token<'src>, Token<'src>), e| {
            let span: SourceSpan = e.span();
            let mut block = Block::new("break");
            block.variant = Some("page");
            block.location = Some(idx.location(&span));
            block
        })
}

/// Combined break parser.
#[allow(dead_code)]
fn break_parser<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Block<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    choice((thematic_break(idx), page_break(idx)))
}

// ---------------------------------------------------------------------------
// Comment Parsers (return Option<Block> - None means skip)
// ---------------------------------------------------------------------------

/// Line comment parser (`// ...`).
fn line_comment<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Option<Block<'src>>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::Slash)
        .then(just(Token::Slash))
        .then(
            any()
                .filter(|t: &Token| !matches!(t, Token::Newline))
                .repeated(),
        )
        .then_ignore(line_end())
        .to(None)
}

/// Block comment parser (`//// ... ////`).
fn block_comment<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Option<Block<'src>>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    delimiter_run(Token::Slash, 4)
        .then(
            any()
                .filter(|t: &Token| !matches!(t, Token::Slash))
                .or(just(Token::Slash).then_ignore(
                    any().filter(|t: &Token| !matches!(t, Token::Slash)).rewind(),
                ))
                .repeated(),
        )
        .then_ignore(delimiter_run(Token::Slash, 4))
        .to(None)
}

// ---------------------------------------------------------------------------
// Verbatim Block Parsers
// ---------------------------------------------------------------------------

/// Information about a delimiter for verbatim blocks.
#[derive(Clone)]
struct DelimiterInfo<'src> {
    /// The delimiter character repeated (e.g., "----").
    delimiter: &'src str,
    /// Number of delimiter tokens.
    count: usize,
    /// Position after the opening delimiter line.
    content_start: usize,
}

/// Check if tokens starting at `i` form a delimiter line of at least `min_count` tokens.
/// Returns delimiter info if valid.
fn check_delimiter_at<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    delimiter_token: &Token<'_>,
    min_count: usize,
    source: &'src str,
) -> Option<DelimiterInfo<'src>> {
    if i >= tokens.len() {
        return None;
    }

    // Count consecutive delimiter tokens.
    let mut j = i;
    while j < tokens.len() && std::mem::discriminant(&tokens[j].0) == std::mem::discriminant(delimiter_token) {
        j += 1;
    }
    let count = j - i;

    if count < min_count {
        return None;
    }

    // Must be followed by Newline or EOF.
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }

    let delimiter = &source[tokens[i].1.start..tokens[j - 1].1.end];
    let content_start = if j < tokens.len() { j + 1 } else { j };

    Some(DelimiterInfo {
        delimiter,
        count,
        content_start,
    })
}

/// Parse a verbatim block (listing, literal, passthrough, fenced code).
///
/// Verbatim blocks have a simple content model - the content between delimiters
/// is captured as a single text node without inline parsing.
fn parse_verbatim_block<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
    delimiter_token: &Token<'src>,
    min_count: usize,
    block_name: &'static str,
) -> Option<(Block<'src>, usize)> {
    let open_info = check_delimiter_at(tokens, i, delimiter_token, min_count, source)?;

    // Scan for matching closing delimiter.
    let mut j = open_info.content_start;
    loop {
        if j >= tokens.len() {
            return None;
        }

        // Check if at start of a line (after newline or at content_start).
        let at_line_start = j == open_info.content_start
            || (j > 0 && matches!(tokens[j - 1].0, Token::Newline));

        if at_line_start
            && let Some(close_info) = check_delimiter_at(tokens, j, delimiter_token, min_count, source)
            && close_info.count == open_info.count
        {
            // Found matching closing delimiter.

            // Content is between opening delimiter and closing delimiter.
            // Exclude the newline before closing delimiter.
            let content_end = if j > open_info.content_start
                && matches!(tokens[j - 1].0, Token::Newline)
            {
                j - 1
            } else {
                j
            };

            // Build inlines from content.
            let inlines = if open_info.content_start < content_end {
                let start_byte = tokens[open_info.content_start].1.start;
                let end_byte = tokens[content_end - 1].1.end;
                let span = SourceSpan {
                    start: start_byte,
                    end: end_byte,
                };
                vec![InlineNode::Text(TextNode {
                    value: &source[start_byte..end_byte],
                    location: Some(idx.location(&span)),
                })]
            } else {
                vec![]
            };

            // Block location spans from opening to closing delimiter.
            let block_span = SourceSpan {
                start: tokens[i].1.start,
                end: tokens[j + close_info.count - 1].1.end,
            };

            let mut block = Block::new(block_name);
            block.form = Some("delimited");
            block.delimiter = Some(open_info.delimiter);
            block.inlines = Some(inlines);
            block.location = Some(idx.location(&block_span));

            return Some((block, close_info.content_start));
        }

        j += 1;
    }
}

/// Try to parse a listing block (`----`).
pub(super) fn try_listing_chumsky<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    parse_verbatim_block(tokens, i, source, idx, &Token::Hyphen, 4, "listing")
}

/// Try to parse a literal block (`....`).
pub(super) fn try_literal_chumsky<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    parse_verbatim_block(tokens, i, source, idx, &Token::Dot, 4, "literal")
}

/// Try to parse a passthrough block (`++++`).
pub(super) fn try_passthrough_chumsky<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    parse_verbatim_block(tokens, i, source, idx, &Token::Plus, 4, "pass")
}

/// Try to parse a fenced code block (`` ``` ``).
pub(super) fn try_fenced_code_chumsky<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize)> {
    parse_verbatim_block(tokens, i, source, idx, &Token::Backtick, 3, "listing")
}

// ---------------------------------------------------------------------------
// Compound Block Parsers
// ---------------------------------------------------------------------------

/// Parse a compound block (example, quote, sidebar, open).
///
/// Compound blocks have a recursive content model - the content between
/// delimiters is parsed as nested blocks.
fn parse_compound_block<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
    delimiter_token: &Token<'src>,
    min_count: usize,
    block_name: &'static str,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    let open_info = check_delimiter_at(tokens, i, delimiter_token, min_count, source)?;

    // Scan for matching closing delimiter.
    let mut j = open_info.content_start;
    loop {
        if j >= tokens.len() {
            return None;
        }

        // Check if at start of a line.
        let at_line_start = j == open_info.content_start
            || (j > 0 && matches!(tokens[j - 1].0, Token::Newline));

        if at_line_start
            && let Some(close_info) = check_delimiter_at(tokens, j, delimiter_token, min_count, source)
            && close_info.count == open_info.count
        {
            // Found matching closing delimiter.

            // Content is between opening delimiter and closing delimiter.
            let content_end = if j > open_info.content_start
                && matches!(tokens[j - 1].0, Token::Newline)
            {
                j - 1
            } else {
                j
            };

            // Recursively parse content as blocks.
            let content_tokens = &tokens[open_info.content_start..content_end];
            let (body_blocks, body_diags) = super::build_blocks(content_tokens, source, idx);

            // Block location spans from opening to closing delimiter.
            let block_span = SourceSpan {
                start: tokens[i].1.start,
                end: tokens[j + close_info.count - 1].1.end,
            };

            let mut block = Block::new(block_name);
            block.form = Some("delimited");
            block.delimiter = Some(open_info.delimiter);
            block.blocks = Some(body_blocks);
            block.location = Some(idx.location(&block_span));

            return Some((block, close_info.content_start, body_diags));
        }

        j += 1;
    }
}

/// Try to parse an example block (`====`).
pub(super) fn try_example_chumsky<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
    title: Option<Vec<InlineNode<'src>>>,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    let (mut block, next, diags) =
        parse_compound_block(tokens, i, source, idx, &Token::Eq, 4, "example")?;
    block.title = title;
    Some((block, next, diags))
}

/// Try to parse a quote block (`____`).
pub(super) fn try_quote_chumsky<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    parse_compound_block(tokens, i, source, idx, &Token::Underscore, 4, "quote")
}

/// Try to parse a sidebar block (`****`).
pub(super) fn try_sidebar_chumsky<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    parse_compound_block(tokens, i, source, idx, &Token::Star, 4, "sidebar")
}

/// Try to parse an open block (`--`).
///
/// Open blocks use exactly 2 hyphens (not 4+), so they don't conflict with listing blocks.
pub(super) fn try_open_chumsky<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    idx: &SourceIndex,
) -> Option<(Block<'src>, usize, Vec<ParseDiagnostic>)> {
    // Open block must be exactly 2 hyphens.
    if i + 1 >= tokens.len() {
        return None;
    }
    if !matches!(tokens[i].0, Token::Hyphen) || !matches!(tokens[i + 1].0, Token::Hyphen) {
        return None;
    }
    // Third token must NOT be a hyphen (otherwise it would be a listing delimiter).
    if i + 2 < tokens.len() && matches!(tokens[i + 2].0, Token::Hyphen) {
        return None;
    }
    // Must be followed by Newline or EOF.
    let j = i + 2;
    if j < tokens.len() && !matches!(tokens[j].0, Token::Newline) {
        return None;
    }

    let delimiter = &source[tokens[i].1.start..tokens[i + 1].1.end];
    let content_start = if j < tokens.len() { j + 1 } else { j };

    // Scan for matching closing delimiter.
    let mut k = content_start;
    loop {
        if k >= tokens.len() {
            return None;
        }

        // Check if at start of a line.
        let at_line_start = k == content_start || (k > 0 && matches!(tokens[k - 1].0, Token::Newline));

        if at_line_start
            && k + 1 < tokens.len()
            && matches!(tokens[k].0, Token::Hyphen)
            && matches!(tokens[k + 1].0, Token::Hyphen)
            && (k + 2 >= tokens.len() || !matches!(tokens[k + 2].0, Token::Hyphen))
        {
            // Check for line ending.
            let after_delim = k + 2;
            if after_delim >= tokens.len() || matches!(tokens[after_delim].0, Token::Newline) {
                // Found matching closing delimiter.

                let content_end = if k > content_start && matches!(tokens[k - 1].0, Token::Newline) {
                    k - 1
                } else {
                    k
                };

                // Recursively parse content as blocks.
                let content_tokens = &tokens[content_start..content_end];
                let (body_blocks, body_diags) = super::build_blocks(content_tokens, source, idx);

                // Block location.
                let block_span = SourceSpan {
                    start: tokens[i].1.start,
                    end: tokens[k + 1].1.end,
                };

                let mut block = Block::new("open");
                block.form = Some("delimited");
                block.delimiter = Some(delimiter);
                block.blocks = Some(body_blocks);
                block.location = Some(idx.location(&block_span));

                let after_close = if after_delim < tokens.len() {
                    after_delim + 1
                } else {
                    after_delim
                };
                return Some((block, after_close, body_diags));
            }
        }

        k += 1;
    }
}

// ---------------------------------------------------------------------------
// Paragraph Parser
// ---------------------------------------------------------------------------

/// Check if a position starts a block delimiter that would interrupt a paragraph.
fn is_block_delimiter(tokens: &[Spanned<'_>], i: usize) -> bool {
    if i >= tokens.len() {
        return false;
    }

    // Check for various delimiter types.
    check_delimiter_at(tokens, i, &Token::Hyphen, 4, "").is_some()
        || check_delimiter_at(tokens, i, &Token::Dot, 4, "").is_some()
        || check_delimiter_at(tokens, i, &Token::Plus, 4, "").is_some()
        || check_delimiter_at(tokens, i, &Token::Backtick, 3, "").is_some()
        || check_delimiter_at(tokens, i, &Token::Eq, 4, "").is_some()
        || check_delimiter_at(tokens, i, &Token::Underscore, 4, "").is_some()
        || check_delimiter_at(tokens, i, &Token::Star, 4, "").is_some()
        || check_delimiter_at(tokens, i, &Token::Slash, 4, "").is_some()
        || super::breaks::is_thematic_break(tokens, i).is_some()
        || super::breaks::is_page_break(tokens, i).is_some()
        || super::comments::is_line_comment(tokens, i).is_some()
        || super::lists::is_list_item(tokens, i)
        || super::sections::is_section_heading(tokens, i).is_some()
        // Open block (exactly 2 hyphens).
        || (i + 1 < tokens.len()
            && matches!(tokens[i].0, Token::Hyphen)
            && matches!(tokens[i + 1].0, Token::Hyphen)
            && (i + 2 >= tokens.len() || !matches!(tokens[i + 2].0, Token::Hyphen))
            && (i + 2 >= tokens.len() || matches!(tokens[i + 2].0, Token::Newline)))
}

/// Find the end of a paragraph.
pub(super) fn find_paragraph_end_chumsky(tokens: &[Spanned<'_>], start: usize) -> usize {
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
                // Blank line ends paragraph.
                return nl_start;
            }
            // Check if next line starts a block.
            if is_block_delimiter(tokens, i) {
                return nl_start;
            }
        } else {
            i += 1;
        }
    }
    // Strip trailing newlines.
    super::strip_trailing_newline_index(tokens, start)
}

/// Build a paragraph block.
pub(super) fn make_paragraph_chumsky<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Block<'src>, Vec<ParseDiagnostic>) {
    let trimmed = super::strip_trailing_newlines(tokens);
    let (inlines, diagnostics) = run_inline_parser(trimmed, source, idx);

    let span = super::content_span(trimmed);
    let location = span.map(|s| idx.location(&s));

    let mut paragraph = Block::new("paragraph");
    paragraph.inlines = Some(inlines);
    paragraph.location = location;

    (paragraph, diagnostics)
}

// ---------------------------------------------------------------------------
// Main Block Parser (Chumsky-based with procedural orchestration)
// ---------------------------------------------------------------------------

/// Build blocks from a token stream using chumsky-based parsers.
///
/// This function uses individual chumsky-style parsers for each block type
/// while maintaining procedural orchestration for the main loop. This hybrid
/// approach handles `AsciiDoc`'s unique requirements:
///
/// - Metadata propagation (block attributes/titles apply to following block)
/// - Delimiter matching (opening and closing counts must match)
/// - Line-oriented constraints (delimiters must be on their own lines)
#[allow(clippy::too_many_lines)]
pub(super) fn build_blocks_chumsky<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    use super::metadata::{BlockAttrs, is_block_attribute_line, skip_comment_block};
    use super::sections::try_discrete_heading;

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

        // Skip line comments.
        if let Some(next) = super::comments::is_line_comment(tokens, i) {
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }

        // Skip block comments.
        if let Some(next) = super::comments::try_skip_block_comment(tokens, i) {
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

        // Try block title.
        if let Some(title_result) = super::metadata::try_block_title(tokens, i, source, idx) {
            pending_title = Some(title_result.inlines);
            diagnostics.extend(title_result.diagnostics);
            i = title_result.next;
            continue;
        }

        // Try break blocks.
        if let Some((block, next)) = super::breaks::try_break(tokens, i, idx) {
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
        if let Some((block, next)) = try_listing_chumsky(tokens, i, source, idx) {
            blocks.push(block);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }
        if let Some((block, next)) = try_literal_chumsky(tokens, i, source, idx) {
            blocks.push(block);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }
        if let Some((block, next)) = try_fenced_code_chumsky(tokens, i, source, idx) {
            blocks.push(block);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }
        if let Some((block, next)) = try_passthrough_chumsky(tokens, i, source, idx) {
            blocks.push(block);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }

        // Try compound blocks.
        if let Some((block, next, diags)) = try_example_chumsky(tokens, i, source, idx, pending_title.take()) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_attrs = None;
            i = next;
            continue;
        }
        if let Some((block, next, diags)) = try_quote_chumsky(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }
        if let Some((block, next, diags)) = try_sidebar_chumsky(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }
        if let Some((block, next, diags)) = try_open_chumsky(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }

        // Try sections.
        if let Some((block, next, diags)) = super::sections::try_section(tokens, i, source, idx, pending_attrs.as_ref()) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }

        // Try lists.
        if let Some((block, next, diags)) = super::lists::try_list(tokens, i, source, idx) {
            blocks.push(block);
            diagnostics.extend(diags);
            pending_title = None;
            pending_attrs = None;
            i = next;
            continue;
        }

        // Fall back to paragraph.
        let para_end = find_paragraph_end_chumsky(tokens, i);
        if i < para_end {
            let (block, diags) = make_paragraph_chumsky(&tokens[i..para_end], source, idx);
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
    use super::*;

    #[test]
    fn test_thematic_break_parser() {
        use crate::lexer::lex;

        let source = "'''\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let eoi = SourceSpan {
            start: source.len(),
            end: source.len(),
        };
        let input = tokens.as_slice().split_token_span(eoi);

        let parser = thematic_break(&idx);
        let result = parser.parse(input);

        assert!(result.has_output());
        let block = result.into_output().unwrap();
        assert_eq!(block.name, "break");
        assert_eq!(block.variant, Some("thematic"));
    }

    #[test]
    fn test_line_comment_parser() {
        use crate::lexer::lex;

        let source = "// comment\n";
        let tokens = lex(source);

        let eoi = SourceSpan {
            start: source.len(),
            end: source.len(),
        };
        let input = tokens.as_slice().split_token_span(eoi);

        let result = line_comment().parse(input);

        assert!(result.has_output());
        assert!(result.into_output().unwrap().is_none()); // Comments return None
    }

    #[test]
    fn test_listing_block() {
        use crate::lexer::lex;

        let source = "----\ncode\n----\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let result = try_listing_chumsky(&tokens, 0, source, &idx);

        assert!(result.is_some());
        let (block, _next) = result.unwrap();
        assert_eq!(block.name, "listing");
        assert_eq!(block.form, Some("delimited"));
        assert_eq!(block.delimiter, Some("----"));
    }

    #[test]
    fn test_example_block() {
        use crate::lexer::lex;

        let source = "====\nAn example.\n====\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let result = try_example_chumsky(&tokens, 0, source, &idx, None);

        assert!(result.is_some());
        let (block, _next, _diags) = result.unwrap();
        assert_eq!(block.name, "example");
        assert_eq!(block.form, Some("delimited"));
        assert_eq!(block.delimiter, Some("===="));
        assert!(block.blocks.is_some());
    }

    #[test]
    fn test_open_block() {
        use crate::lexer::lex;

        let source = "--\nhello\n--\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let result = try_open_chumsky(&tokens, 0, source, &idx);

        assert!(result.is_some());
        let (block, _next, _diags) = result.unwrap();
        assert_eq!(block.name, "open");
        assert_eq!(block.form, Some("delimited"));
        assert_eq!(block.delimiter, Some("--"));
    }

    #[test]
    fn test_build_blocks_chumsky() {
        use crate::lexer::lex;

        let source = "para1\n\npara2";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = build_blocks_chumsky(&tokens, source, &idx);

        assert!(diags.is_empty());
        assert_eq!(blocks.len(), 2);
        assert_eq!(blocks[0].name, "paragraph");
        assert_eq!(blocks[1].name, "paragraph");
    }
}

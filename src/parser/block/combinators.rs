//! Block parser combinators and infrastructure for chumsky-based parsing.
//!
//! This module provides the foundation for block-level parsing using chumsky,
//! including type aliases, helper combinators for line-oriented parsing, and
//! utilities for building block ASG nodes.

use chumsky::{extra, input::ValueInput, prelude::*};

use crate::asg::Block;
use crate::diagnostic::ParseDiagnostic;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Shorthand for the chumsky error type used by all block parsers.
pub(super) type ParseExtra<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, SourceSpan>>;

/// Result type for block parsing: a block with its associated diagnostics.
pub(super) type BlockResult<'src> = (Block<'src>, Vec<ParseDiagnostic>);

/// Parser that matches a line ending (newline or end of input).
///
/// `AsciiDoc` block delimiters must be followed by a line ending to be valid.
/// This combinator enforces that constraint.
pub(super) fn line_ending<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, (), ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    choice((
        just(Token::Newline).ignored(),
        end().rewind().ignored(), // Accept EOF without consuming
    ))
}

/// Parser that matches n or more consecutive tokens of the same type,
/// followed by a line ending. Returns the count of matched tokens.
///
/// Used for delimiter lines like `----` (4+ hyphens), `====` (4+ equals), etc.
pub(super) fn delimiter_line<'tokens, 'src: 'tokens, I>(
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
        .then_ignore(line_ending())
}

/// Parser that matches exactly n consecutive tokens of the same type,
/// followed by a line ending.
///
/// Used for delimiters that must be an exact count, like open blocks (`--`).
pub(super) fn exact_delimiter_line<'tokens, 'src: 'tokens, I>(
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
        .then_ignore(line_ending())
}

/// Check if a token matches the expected delimiter type.
fn token_matches_delimiter(tok: &Token<'_>, delimiter: &Token<'_>) -> bool {
    std::mem::discriminant(tok) == std::mem::discriminant(delimiter)
}

/// Parser that consumes all tokens until encountering a line that matches
/// the given delimiter pattern. Returns the consumed tokens and the delimiter count.
///
/// The delimiter line itself is consumed but not included in the returned content.
pub(super) fn content_until_delimiter<'tokens, 'src: 'tokens, I>(
    delimiter_token: Token<'src>,
    min_count: usize,
) -> impl Parser<'tokens, I, (Vec<(Token<'src>, SourceSpan)>, usize), ParseExtra<'tokens, 'src>>
       + Clone
       + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let delim_for_line = delimiter_token.clone();
    let delim_for_check = delimiter_token;

    // We need to collect tokens until we hit a matching delimiter line.
    // A delimiter line is: at start of line, n+ delimiter tokens, then newline/EOF.
    any()
        .map_with(|tok, e| (tok, e.span()))
        .repeated()
        .collect::<Vec<_>>()
        .then(delimiter_line(delim_for_line, min_count))
        .try_map(move |(tokens, close_count), _span| {
            // Find where the closing delimiter starts in the collected tokens
            // by scanning backwards from the end
            let mut content_end = tokens.len();
            let mut delim_count = 0;

            // Count backwards to find delimiter tokens at the end
            for (tok, _) in tokens.iter().rev() {
                if token_matches_delimiter(tok, &delim_for_check) {
                    delim_count += 1;
                } else {
                    break;
                }
            }

            if delim_count >= min_count {
                content_end = tokens.len() - delim_count;
            }

            // Also strip trailing newline before delimiter if present
            if content_end > 0
                && let Some((Token::Newline, _)) = tokens.get(content_end - 1)
            {
                content_end -= 1;
            }

            Ok((tokens[..content_end].to_vec(), close_count))
        })
}

/// Skip inter-block whitespace (newlines only, not horizontal whitespace).
///
/// Blocks are separated by blank lines. This combinator skips any number
/// of consecutive newlines.
pub(super) fn skip_blank_lines<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, (), ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::Newline).repeated().ignored()
}

/// Create a block with the given name and compute its location from a span.
pub(super) fn make_block<'src>(name: &'static str, span: SourceSpan, idx: &SourceIndex) -> Block<'src> {
    let mut block = Block::new(name);
    block.location = Some(idx.location(&span));
    block
}

/// Extract the source slice for a span of tokens.
pub(super) fn source_slice<'src>(
    tokens: &[(Token<'src>, SourceSpan)],
    source: &'src str,
) -> Option<&'src str> {
    let start = tokens.first()?.1.start;
    let end = tokens.last()?.1.end;
    Some(&source[start..end])
}

/// Compute the combined span covering all tokens in a slice.
pub(super) fn combined_span(tokens: &[(Token<'_>, SourceSpan)]) -> Option<SourceSpan> {
    let start = tokens.first()?.1.start;
    let end = tokens.last()?.1.end;
    Some(SourceSpan { start, end })
}

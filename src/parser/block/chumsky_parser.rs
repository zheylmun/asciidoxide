//! Chumsky-based block parser implementation.
//!
//! This module provides block parsing using chumsky combinators, following
//! the same patterns as the inline parser.

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

/// Parser that matches n+ consecutive tokens followed by line end.
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

/// Skip newlines between blocks.
fn skip_newlines<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, (), ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    just(Token::Newline).repeated().ignored()
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

// ---------------------------------------------------------------------------
// Comment Parsers (return Option to indicate skip)
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
/// Note: This is simplified and may need adjustment for proper delimiter matching.
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

/// Generic verbatim block parser for listing, literal, passthrough, fenced code.
fn verbatim_block<'tokens, 'src: 'tokens, I>(
    delimiter_token: Token<'src>,
    min_count: usize,
    block_name: &'static str,
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Block<'src>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    let delim_tok = delimiter_token.clone();
    let delim_tok2 = delimiter_token.clone();

    // Opening delimiter
    delimiter_run(delimiter_token, min_count)
        .then(
            // Content: anything until closing delimiter
            any()
                .map_with(|tok, e| (tok, e.span()))
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then(delimiter_run(delim_tok, min_count))
        .try_map(move |((open_count, content), close_count), span| {
            if open_count != close_count {
                return Err(Rich::custom(span, "delimiter count mismatch"));
            }

            // Build delimiter string
            let delimiter_str = std::iter::repeat(match &delim_tok2 {
                Token::Hyphen => "-",
                Token::Dot => ".",
                Token::Backtick => "`",
                Token::Plus => "+",
                _ => "",
            })
            .take(open_count)
            .collect::<String>();

            let _ = (source, idx, block_name, delimiter_str, content);
            // This is getting complex - defer to procedural for now
            Err(Rich::custom(span, "verbatim block parsing deferred"))
        })
}

// ---------------------------------------------------------------------------
// Main Block Parser
// ---------------------------------------------------------------------------

/// Combined block parser using chumsky choice.
#[allow(dead_code)]
fn block_parser<'tokens, 'src: 'tokens, I>(
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, Option<Block<'src>>, ParseExtra<'tokens, 'src>> + Clone + 'tokens
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    choice((
        // Comments (return None to skip)
        line_comment(),
        block_comment(),
        // Breaks (return Some)
        thematic_break(idx).map(Some),
        page_break(idx).map(Some),
        // TODO: Add more block types
    ))
}

/// Build blocks from a token stream using chumsky.
///
/// Note: This is a partial implementation. For full block parsing with
/// metadata handling and compound blocks, we fall back to the procedural
/// implementation for now.
#[allow(dead_code)]
pub(super) fn build_blocks_chumsky<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
    idx: &SourceIndex,
) -> (Vec<Block<'src>>, Vec<ParseDiagnostic>) {
    // For now, delegate to the procedural implementation
    // The chumsky parsers above demonstrate the pattern but aren't fully integrated
    super::build_blocks(tokens, source, idx)
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

        // Use a helper function to avoid explicit type annotation
        fn parse_line_comment<'a>(
            input: impl chumsky::input::ValueInput<'a, Token = Token<'a>, Span = SourceSpan>,
        ) -> chumsky::ParseResult<Option<Block<'a>>, Rich<'a, Token<'a>, SourceSpan>> {
            line_comment().parse(input)
        }

        let result = parse_line_comment(input);

        assert!(result.has_output());
        assert!(result.into_output().unwrap().is_none()); // Comments return None
    }
}

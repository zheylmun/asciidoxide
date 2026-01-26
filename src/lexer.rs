//! Chumsky-based lexer for `AsciiDoc` source text.
//!
//! Produces a flat sequence of atomic [`Token`]s paired with [`SourceSpan`]s.
//! All context-sensitive disambiguation (e.g., `*` as bold vs. list marker)
//! is deferred to the parser.

use chumsky::{extra, prelude::*};

use crate::span::SourceSpan;
use crate::token::Token;

/// A single token paired with its source span.
pub type Spanned<'a> = (Token<'a>, SourceSpan);

/// Characters that are lexed as individual punctuation tokens rather than
/// being absorbed into [`Token::Text`] runs.
const SPECIAL: &str = "=*_`#^~+-.:!/\\|,\"'<>[]{}";

/// Lex an `AsciiDoc` source string into a sequence of tokens with spans.
///
/// The lexer is infallible: every byte of input maps to a token, so this
/// always returns a complete token stream.
#[must_use]
pub fn lex(input: &str) -> Vec<Spanned<'_>> {
    lexer()
        .parse(input)
        .into_output()
        .expect("infallible lexer produced no output")
}

/// Build the chumsky lexer parser.
fn lexer<'src>() -> impl Parser<'src, &'src str, Vec<(Token<'src>, SourceSpan)>, extra::Default> {
    let newline = text::newline().to(Token::Newline);

    let whitespace = one_of(" \t").repeated().at_least(1).to(Token::Whitespace);

    let punct_a = choice((
        just('=').to(Token::Eq),
        just('*').to(Token::Star),
        just('_').to(Token::Underscore),
        just('`').to(Token::Backtick),
        just('#').to(Token::Hash),
        just('^').to(Token::Caret),
        just('~').to(Token::Tilde),
        just('+').to(Token::Plus),
        just('-').to(Token::Hyphen),
        just('.').to(Token::Dot),
        just(':').to(Token::Colon),
        just('!').to(Token::Bang),
    ));

    let punct_b = choice((
        just('/').to(Token::Slash),
        just('\\').to(Token::Backslash),
        just('|').to(Token::Pipe),
        just(',').to(Token::Comma),
        just('"').to(Token::DoubleQuote),
        just('\'').to(Token::SingleQuote),
        just('[').to(Token::LBracket),
        just(']').to(Token::RBracket),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
    ));

    let double_angle = choice((
        just("<<").to(Token::DoubleLeftAngle),
        just(">>").to(Token::DoubleRightAngle),
    ));

    // Lone < or > (not part of << or >>) become single-char text.
    let lone_angle = one_of("<>").to_slice().map(Token::Text);

    let text = none_of(SPECIAL)
        .filter(|c: &char| !c.is_ascii_whitespace())
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Text);

    let token = choice((
        newline,
        whitespace,
        double_angle,
        punct_a,
        punct_b,
        lone_angle,
        text,
    ));

    token
        .map_with(|tok, e| (tok, SourceSpan::from(e.span())))
        .repeated()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: extract just the token variants (discard spans).
    fn tokens(input: &str) -> Vec<Token<'_>> {
        lex(input).into_iter().map(|(t, _)| t).collect()
    }

    #[test]
    fn empty_input() {
        assert!(tokens("").is_empty());
    }

    #[test]
    fn single_word() {
        assert_eq!(tokens("hello"), vec![Token::Text("hello")]);
    }

    #[test]
    fn punctuation_individual() {
        assert_eq!(
            tokens("*bold*"),
            vec![Token::Star, Token::Text("bold"), Token::Star]
        );
    }

    #[test]
    fn whitespace_grouping() {
        assert_eq!(
            tokens("a  b"),
            vec![Token::Text("a"), Token::Whitespace, Token::Text("b")]
        );
    }

    #[test]
    fn newline_lf() {
        assert_eq!(
            tokens("a\nb"),
            vec![Token::Text("a"), Token::Newline, Token::Text("b")]
        );
    }

    #[test]
    fn newline_crlf() {
        assert_eq!(
            tokens("a\r\nb"),
            vec![Token::Text("a"), Token::Newline, Token::Text("b")]
        );
    }

    #[test]
    fn mixed_content() {
        let toks = tokens("= Title\n\nparagraph");
        assert_eq!(
            toks,
            vec![
                Token::Eq,
                Token::Whitespace,
                Token::Text("Title"),
                Token::Newline,
                Token::Newline,
                Token::Text("paragraph"),
            ]
        );
    }

    #[test]
    fn all_punctuation() {
        let input = "=*_`#^~+-.:!/\\|,\"'<<>>[]{}";
        let toks = tokens(input);
        assert_eq!(
            toks,
            vec![
                Token::Eq,
                Token::Star,
                Token::Underscore,
                Token::Backtick,
                Token::Hash,
                Token::Caret,
                Token::Tilde,
                Token::Plus,
                Token::Hyphen,
                Token::Dot,
                Token::Colon,
                Token::Bang,
                Token::Slash,
                Token::Backslash,
                Token::Pipe,
                Token::Comma,
                Token::DoubleQuote,
                Token::SingleQuote,
                Token::DoubleLeftAngle,
                Token::DoubleRightAngle,
                Token::LBracket,
                Token::RBracket,
                Token::LBrace,
                Token::RBrace,
            ]
        );
    }

    #[test]
    fn text_absorbs_non_special() {
        // Letters, digits, and non-AsciiDoc punctuation merge into Text
        assert_eq!(tokens("foo123@bar%"), vec![Token::Text("foo123@bar%")]);
    }

    #[test]
    fn lone_angles_are_text() {
        // Single < and > have no AsciiDoc meaning; only << and >> do.
        // They become individual Text slices since they break text runs.
        assert_eq!(
            tokens("a<b>c"),
            vec![
                Token::Text("a"),
                Token::Text("<"),
                Token::Text("b"),
                Token::Text(">"),
                Token::Text("c"),
            ]
        );
    }

    #[test]
    fn double_angles_are_tokens() {
        assert_eq!(
            tokens("<<ref>>"),
            vec![
                Token::DoubleLeftAngle,
                Token::Text("ref"),
                Token::DoubleRightAngle,
            ]
        );
    }

    #[test]
    fn spans_are_correct() {
        let toks = lex("ab\n*");
        assert_eq!(
            toks[0],
            (Token::Text("ab"), SourceSpan { start: 0, end: 2 })
        );
        assert_eq!(toks[1], (Token::Newline, SourceSpan { start: 2, end: 3 }));
        assert_eq!(toks[2], (Token::Star, SourceSpan { start: 3, end: 4 }));
    }

    #[test]
    fn tabs_are_whitespace() {
        assert_eq!(
            tokens("\thello"),
            vec![Token::Whitespace, Token::Text("hello")]
        );
    }
}

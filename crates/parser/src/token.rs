//! Lexical tokens for the `AsciiDoc` lexer.
//!
//! Tokens represent the atomic lexical units of an `AsciiDoc` document.
//! Each AsciiDoc-significant punctuation character gets its own variant;
//! the parser determines context-sensitive meaning (e.g., `Star` as bold
//! delimiter vs. list marker).
//!
//! Span information (source location and extent) is tracked externally
//! by chumsky's span machinery rather than stored in the token itself.

/// A lexical token produced by the `AsciiDoc` lexer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token<'a> {
    // --- Whitespace & structure ---
    /// A line feed (`\n`) or carriage return + line feed (`\r\n`).
    Newline,

    /// One or more horizontal whitespace characters (spaces or tabs).
    Whitespace,

    // --- Single punctuation characters ---
    /// `=`
    Eq,

    /// `*`
    Star,

    /// `_`
    Underscore,

    /// `` ` ``
    Backtick,

    /// `#`
    Hash,

    /// `^`
    Caret,

    /// `~`
    Tilde,

    /// `+`
    Plus,

    /// `-`
    Hyphen,

    /// `.`
    Dot,

    /// `:`
    Colon,

    /// `!`
    Bang,

    /// `/`
    Slash,

    /// `\`
    Backslash,

    /// `|`
    Pipe,

    /// `,`
    Comma,

    /// `"`
    DoubleQuote,

    /// `'`
    SingleQuote,

    /// `<<`
    DoubleLeftAngle,

    /// `>>`
    DoubleRightAngle,

    /// `[`
    LBracket,

    /// `]`
    RBracket,

    /// `{`
    LBrace,

    /// `}`
    RBrace,

    // --- Text content ---
    /// A run of characters that are not `AsciiDoc`-significant punctuation,
    /// whitespace, or newlines. References a slice of the input.
    Text(&'a str),

    // --- Internal ---
    /// A placeholder for a pre-parsed inline macro (link, xref, bare URL).
    ///
    /// The `usize` is an index into the pre-parsed node table. This variant
    /// is never produced by the lexer — it is injected by the inline parser
    /// to let chumsky match span delimiters across macro boundaries.
    Placeholder(usize),

    /// An escaped star delimiter (`\**` → `StarEscaped *`).
    ///
    /// This variant is injected during inline preprocessing when an escaped
    /// unconstrained delimiter is detected. It acts like a `Star` for
    /// constrained span matching but does not combine with an adjacent `Star`
    /// to form an unconstrained delimiter.
    StarEscaped,

    /// An escaped underscore delimiter (`\__` → `UnderscoreEscaped _`).
    UnderscoreEscaped,

    /// An escaped backtick delimiter (`` \`` `` → `BacktickEscaped \``).
    BacktickEscaped,

    /// An escaped hash delimiter (`\##` → `HashEscaped #`).
    HashEscaped,

    /// A star that should be treated as text only (from escaped unconstrained).
    ///
    /// After `\**`, the second star becomes `StarAsText` which renders as `*`
    /// but cannot open a span (it's consumed as content text).
    StarAsText,

    /// An underscore that should be treated as text only (from escaped unconstrained).
    UnderscoreAsText,

    /// A backtick that should be treated as text only (from escaped unconstrained).
    BacktickAsText,

    /// A hash that should be treated as text only (from escaped unconstrained).
    HashAsText,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Newline => write!(f, "newline"),
            Token::Whitespace => write!(f, "whitespace"),
            Token::Eq => write!(f, "'='"),
            Token::Star | Token::StarEscaped | Token::StarAsText => write!(f, "'*'"),
            Token::Underscore | Token::UnderscoreEscaped | Token::UnderscoreAsText => {
                write!(f, "'_'")
            }
            Token::Backtick | Token::BacktickEscaped | Token::BacktickAsText => write!(f, "'`'"),
            Token::Hash | Token::HashEscaped | Token::HashAsText => write!(f, "'#'"),
            Token::Caret => write!(f, "'^'"),
            Token::Tilde => write!(f, "'~'"),
            Token::Plus => write!(f, "'+'"),
            Token::Hyphen => write!(f, "'-'"),
            Token::Dot => write!(f, "'.'"),
            Token::Colon => write!(f, "':'"),
            Token::Bang => write!(f, "'!'"),
            Token::Slash => write!(f, "'/'"),
            Token::Backslash => write!(f, "'\\'"),
            Token::Pipe => write!(f, "'|'"),
            Token::Comma => write!(f, "','"),
            Token::DoubleQuote => write!(f, "'\"'"),
            Token::SingleQuote => write!(f, "'''"),
            Token::DoubleLeftAngle => write!(f, "'<<'"),
            Token::DoubleRightAngle => write!(f, "'>>'"),
            Token::LBracket => write!(f, "'['"),
            Token::RBracket => write!(f, "']'"),
            Token::LBrace => write!(f, "'{{'"),
            Token::RBrace => write!(f, "'}}'"),
            Token::Text(s) => write!(f, "{s}"),
            Token::Placeholder(i) => write!(f, "<placeholder:{i}>"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn display_newline() {
        assert_eq!(Token::Newline.to_string(), "newline");
    }

    #[test]
    fn display_whitespace() {
        assert_eq!(Token::Whitespace.to_string(), "whitespace");
    }

    #[test]
    fn display_single_punct() {
        assert_eq!(Token::Eq.to_string(), "'='");
        assert_eq!(Token::Caret.to_string(), "'^'");
        assert_eq!(Token::Tilde.to_string(), "'~'");
        assert_eq!(Token::Plus.to_string(), "'+'");
        assert_eq!(Token::Hyphen.to_string(), "'-'");
        assert_eq!(Token::Dot.to_string(), "'.'");
        assert_eq!(Token::Colon.to_string(), "':'");
        assert_eq!(Token::Bang.to_string(), "'!'");
        assert_eq!(Token::Slash.to_string(), "'/'");
        assert_eq!(Token::Backslash.to_string(), "'\\'");
        assert_eq!(Token::Pipe.to_string(), "'|'");
        assert_eq!(Token::Comma.to_string(), "','");
        assert_eq!(Token::DoubleQuote.to_string(), "'\"'");
        assert_eq!(Token::SingleQuote.to_string(), "'''");
        assert_eq!(Token::LBracket.to_string(), "'['");
        assert_eq!(Token::RBracket.to_string(), "']'");
        assert_eq!(Token::LBrace.to_string(), "'{'");
        assert_eq!(Token::RBrace.to_string(), "'}'");
    }

    #[test]
    fn display_double_angles() {
        assert_eq!(Token::DoubleLeftAngle.to_string(), "'<<'");
        assert_eq!(Token::DoubleRightAngle.to_string(), "'>>'");
    }

    #[test]
    fn display_star_variants() {
        assert_eq!(Token::Star.to_string(), "'*'");
        assert_eq!(Token::StarEscaped.to_string(), "'*'");
        assert_eq!(Token::StarAsText.to_string(), "'*'");
    }

    #[test]
    fn display_underscore_variants() {
        assert_eq!(Token::Underscore.to_string(), "'_'");
        assert_eq!(Token::UnderscoreEscaped.to_string(), "'_'");
        assert_eq!(Token::UnderscoreAsText.to_string(), "'_'");
    }

    #[test]
    fn display_backtick_variants() {
        assert_eq!(Token::Backtick.to_string(), "'`'");
        assert_eq!(Token::BacktickEscaped.to_string(), "'`'");
        assert_eq!(Token::BacktickAsText.to_string(), "'`'");
    }

    #[test]
    fn display_hash_variants() {
        assert_eq!(Token::Hash.to_string(), "'#'");
        assert_eq!(Token::HashEscaped.to_string(), "'#'");
        assert_eq!(Token::HashAsText.to_string(), "'#'");
    }

    #[test]
    fn display_text() {
        assert_eq!(Token::Text("hello").to_string(), "hello");
    }

    #[test]
    fn display_placeholder() {
        assert_eq!(Token::Placeholder(42).to_string(), "<placeholder:42>");
    }
}

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
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Newline => write!(f, "newline"),
            Token::Whitespace => write!(f, "whitespace"),
            Token::Eq => write!(f, "'='"),
            Token::Star => write!(f, "'*'"),
            Token::Underscore => write!(f, "'_'"),
            Token::Backtick => write!(f, "'`'"),
            Token::Hash => write!(f, "'#'"),
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
        }
    }
}

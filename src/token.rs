//! Lexical tokens for the `AsciiDoc` lexer.
//!
//! Tokens represent the atomic lexical units of an `AsciiDoc` document.
//! Span information (source location and text) is tracked externally by
//! chumsky's span machinery rather than stored in the token itself.

/// A lexical token produced by the `AsciiDoc` lexer.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token<'a> {
    // --- Whitespace & structure ---
    /// A line feed (`\n`) or carriage return + line feed (`\r\n`).
    Newline,

    /// One or more horizontal whitespace characters (spaces or tabs).
    Whitespace,

    // --- Block delimiters ---
    /// A line of 4+ repeated delimiter characters (`----`, `****`, `====`, `....`, `++++`, `____`, `~~~~`, `////`).
    /// The character and count are preserved for delimiter matching.
    BlockDelimiter(char, usize),

    // --- Heading ---
    /// One or more `=` characters at the start of a line, followed by a space.
    /// The count determines the section level (1 = document title, 2 = section level 1, etc.).
    SectionMarker(usize),

    // --- List markers ---
    /// One or more `*` characters as an unordered list marker.
    UnorderedListMarker(usize),

    /// One or more `.` characters as an ordered list marker.
    OrderedListMarker(usize),

    /// A `-` character as an unordered list marker.
    HyphenListMarker,

    /// A description list marker (`::`, `:::`, or `::::`).
    DescriptionListMarker(usize),

    // --- Inline formatting marks ---
    /// Constrained strong (`*`).
    Star,

    /// Unconstrained strong (`**`).
    DoubleStar,

    /// Constrained emphasis (`_`).
    Underscore,

    /// Unconstrained emphasis (`__`).
    DoubleUnderscore,

    /// Constrained monospace (`` ` ``).
    Backtick,

    /// Unconstrained monospace (` `` `).
    DoubleBacktick,

    /// Constrained highlight (`#`).
    Hash,

    /// Unconstrained highlight (`##`).
    DoubleHash,

    /// Superscript marker (`^`).
    Caret,

    /// Subscript marker (`~`).
    Tilde,

    // --- Brackets & delimiters ---
    /// `[`
    LeftBracket,

    /// `]`
    RightBracket,

    /// `{`
    LeftBrace,

    /// `}`
    RightBrace,

    /// `<<`
    DoubleLeftAngle,

    /// `>>`
    DoubleRightAngle,

    // --- Attribute entries ---
    /// An attribute entry at the start of a line (`:name:` or `:name: value`).
    /// Contains the attribute name.
    AttributeEntry(&'a str),

    /// An attribute unset entry (`:!name:` or `:name!:`).
    /// Contains the attribute name.
    AttributeUnset(&'a str),

    // --- Macros ---
    /// A block macro name followed by `::` (e.g., `image::`, `include::`).
    /// Contains the macro name.
    BlockMacro(&'a str),

    /// An inline macro name followed by `:` (e.g., `link:`, `xref:`).
    /// Contains the macro name.
    InlineMacro(&'a str),

    // --- Special block-level markers ---
    /// A block title line (`.Title`).
    BlockTitle(&'a str),

    /// Page break (`<<<`).
    PageBreak,

    /// Thematic break (`'''` or `---`).
    ThematicBreak,

    /// List continuation (`+` on its own line).
    ListContinuation,

    /// Hard line break (`+` at end of line before newline, or ` +\n`).
    HardLineBreak,

    /// A line comment prefix (`//`).
    LineComment,

    // --- Passthrough ---
    /// Single plus passthrough (`+`).
    SinglePlus,

    /// Double plus passthrough (`++`).
    DoublePlus,

    /// Triple plus passthrough (`+++`).
    TriplePlus,

    // --- Escape ---
    /// Backslash escape (`\`).
    Backslash,

    // --- Table ---
    /// Pipe character for table cells (`|`).
    Pipe,

    // --- Punctuation used in attribute lists ---
    /// Comma separator.
    Comma,

    /// Equals sign (used in named attributes `key=value`).
    Equals,

    /// Double quote for quoted attribute values.
    DoubleQuote,

    /// Single quote for quoted attribute values.
    SingleQuote,

    // --- Text content ---
    /// A run of text content that is not a recognized marker.
    /// References a slice of the input.
    Text(&'a str),
}

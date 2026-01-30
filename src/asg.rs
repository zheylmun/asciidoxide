//! Abstract Semantic Graph (ASG) types produced by the parser.

use std::borrow::Cow;
use std::collections::HashMap;

/// An attribute value that may be single-line or multiline.
///
/// Stores references to source slices; allocation only happens when
/// [`resolve()`](Self::resolve) is called on multiline values.
#[derive(Debug, Clone, PartialEq)]
pub enum AttributeValue<'a> {
    /// A single-line value (zero-copy slice from source).
    Single(&'a str),
    /// A multiline value with continuation lines.
    ///
    /// Each element is a line segment; when resolved, segments are joined
    /// with a single space (backslash and leading whitespace removed).
    Multiline(Vec<&'a str>),
}

impl<'a> AttributeValue<'a> {
    /// Resolve the attribute value to a string.
    ///
    /// For single-line values, returns a borrowed reference (no allocation).
    /// For multiline values, joins segments with spaces and returns an owned string.
    #[must_use]
    pub fn resolve(&self) -> Cow<'a, str> {
        match self {
            Self::Single(s) => Cow::Borrowed(s),
            Self::Multiline(segments) => Cow::Owned(segments.join(" ")),
        }
    }

    /// Returns the value as a `&str` if it's a single-line value.
    ///
    /// Returns `None` for multiline values (use [`resolve()`](Self::resolve) instead).
    #[must_use]
    pub fn as_str(&self) -> Option<&'a str> {
        match self {
            Self::Single(s) => Some(s),
            Self::Multiline(_) => None,
        }
    }

    /// Returns `true` if this is a multiline value.
    #[must_use]
    pub fn is_multiline(&self) -> bool {
        matches!(self, Self::Multiline(_))
    }
}

/// Block-level metadata (roles, options, element attributes).
#[derive(Debug, Clone, PartialEq, Default)]
pub struct BlockMetadata<'a> {
    /// Role classes (e.g., from `[.role1.role2]`).
    pub roles: Vec<&'a str>,
    /// Options (e.g., from `[%option]`).
    pub options: Vec<&'a str>,
    /// Element attributes (e.g., `name=value`).
    pub attributes: HashMap<&'a str, &'a str>,
}

/// A 1-based source position (line and column).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    /// 1-based line number.
    pub line: usize,
    /// 1-based column number.
    pub col: usize,
}

/// A source location as a `[start, end]` pair of positions.
pub type Location = [Position; 2];

/// The root document node of an ASG.
#[derive(Debug, Clone, PartialEq)]
pub struct Document<'a> {
    /// Document-level attributes.
    pub attributes: Option<HashMap<&'a str, AttributeValue<'a>>>,
    /// Document header (title, authors, etc.).
    pub header: Option<Header<'a>>,
    /// Top-level blocks in the document body.
    pub blocks: Vec<Block<'a>>,
    /// Source location of the document.
    pub location: Option<Location>,
}

/// A document header containing a title and optional metadata.
#[derive(Debug, Clone, PartialEq)]
pub struct Header<'a> {
    /// Title as inline nodes.
    pub title: Vec<InlineNode<'a>>,
    /// Source location of the header.
    pub location: Option<Location>,
}

/// A block-level ASG node.
#[derive(Debug, Clone, PartialEq)]
pub struct Block<'a> {
    /// Block name (e.g., `"paragraph"`, `"section"`, `"list"`, `"listing"`, `"sidebar"`).
    pub name: &'static str,
    /// Structural form (e.g., `"delimited"`).
    pub form: Option<&'static str>,
    /// Delimiter string for delimited blocks.
    pub delimiter: Option<&'a str>,
    /// Block or section ID (e.g., from `[[id]]` or `[#id]`).
    pub id: Option<&'a str>,
    /// Block style (e.g., `"appendix"`, `"discrete"`, `"source"`, `"abstract"`).
    pub style: Option<&'a str>,
    /// Reference text for cross-references (e.g., from `[[id,reftext]]`), as inline nodes.
    pub reftext: Option<Vec<InlineNode<'a>>>,
    /// Block metadata (roles, options, attributes).
    pub metadata: Option<BlockMetadata<'a>>,
    /// Section title as inline nodes.
    pub title: Option<Vec<InlineNode<'a>>>,
    /// Section level (0-5).
    pub level: Option<usize>,
    /// List variant (e.g., `"unordered"`).
    pub variant: Option<&'static str>,
    /// List or list-item marker (e.g., `"*"`).
    pub marker: Option<&'a str>,
    /// Inline content for leaf blocks (paragraph, listing, etc.).
    pub inlines: Option<Vec<InlineNode<'a>>>,
    /// Child blocks for compound blocks (section, sidebar, etc.).
    pub blocks: Option<Vec<Block<'a>>>,
    /// List items.
    pub items: Option<Vec<Block<'a>>>,
    /// Principal content of a list item.
    pub principal: Option<Vec<InlineNode<'a>>>,
    /// Source location.
    pub location: Option<Location>,
}

/// An inline ASG node — text, formatting span, reference, or raw passthrough.
#[derive(Debug, Clone, PartialEq)]
pub enum InlineNode<'a> {
    /// A text string node.
    Text(TextNode<'a>),
    /// A formatting span node.
    Span(SpanNode<'a>),
    /// An inline reference (link or cross-reference).
    Ref(RefNode<'a>),
    /// A raw passthrough node (content that bypasses processing).
    Raw(RawNode<'a>),
}

/// A leaf text node in the ASG.
#[derive(Debug, Clone, PartialEq)]
pub struct TextNode<'a> {
    /// The text value, borrowed from the input.
    pub value: &'a str,
    /// Source location.
    pub location: Option<Location>,
}

/// An inline formatting span (e.g., strong, emphasis).
#[derive(Debug, Clone, PartialEq)]
pub struct SpanNode<'a> {
    /// Span variant (e.g., `"strong"`, `"emphasis"`, `"code"`, `"mark"`).
    pub variant: &'static str,
    /// Span form (`"constrained"` or `"unconstrained"`).
    pub form: &'static str,
    /// Child inline nodes.
    pub inlines: Vec<InlineNode<'a>>,
    /// Source location.
    pub location: Option<Location>,
}

/// An inline reference (link or cross-reference).
#[derive(Debug, Clone, PartialEq)]
pub struct RefNode<'a> {
    /// Reference variant (`"link"` or `"xref"`).
    pub variant: &'static str,
    /// Reference target URL or path.
    pub target: &'a str,
    /// Child inline nodes (display text).
    pub inlines: Vec<InlineNode<'a>>,
    /// Source location.
    pub location: Option<Location>,
}

/// A raw passthrough node (content that bypasses processing).
///
/// Used for triple-plus passthrough (`+++..+++`) and `pass:[]` macros.
/// The content is passed through without any substitutions.
#[derive(Debug, Clone, PartialEq)]
pub struct RawNode<'a> {
    /// The raw value, borrowed from the input.
    pub value: &'a str,
    /// Source location.
    pub location: Option<Location>,
}

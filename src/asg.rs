//! Abstract Semantic Graph (ASG) types produced by the parser.

use std::collections::HashMap;

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
    pub attributes: Option<HashMap<&'a str, &'a str>>,
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

/// An inline ASG node — text, formatting span, or reference.
#[derive(Debug, Clone, PartialEq)]
pub enum InlineNode<'a> {
    /// A text string node.
    Text(TextNode<'a>),
    /// A formatting span node.
    Span(SpanNode<'a>),
    /// An inline reference (link or cross-reference).
    Ref(RefNode<'a>),
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

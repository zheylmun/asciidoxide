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
    /// A multiline value with backslash continuation lines.
    ///
    /// Each element is a line segment; when resolved, segments are joined
    /// with a single space (backslash and leading whitespace removed).
    Multiline(Vec<&'a str>),
    /// A multiline value with legacy `+` continuation lines.
    ///
    /// Each element is a line segment; when resolved, segments are directly
    /// concatenated (trailing whitespace preserved, no separator added).
    MultilineLegacy(Vec<&'a str>),
    /// A value with attribute references already resolved (owned string).
    Resolved(String),
}

impl<'a> AttributeValue<'a> {
    /// Resolve the attribute value to a string.
    ///
    /// For single-line values, returns a borrowed reference (no allocation).
    /// For backslash multiline values, joins segments with spaces and returns an owned string.
    /// For legacy `+` multiline values, concatenates segments directly (no separator).
    #[must_use]
    pub fn resolve(&self) -> Cow<'a, str> {
        match self {
            Self::Single(s) => Cow::Borrowed(s),
            Self::Multiline(segments) => Cow::Owned(segments.join(" ")),
            Self::MultilineLegacy(segments) => Cow::Owned(segments.concat()),
            Self::Resolved(s) => Cow::Owned(s.clone()),
        }
    }

    /// Returns the value as a `&str` if it's a single-line value.
    ///
    /// Returns `None` for multiline or resolved values
    /// (use [`resolve()`](Self::resolve) instead).
    #[must_use]
    pub fn as_str(&self) -> Option<&'a str> {
        match self {
            Self::Single(s) => Some(s),
            Self::Multiline(_) | Self::MultilineLegacy(_) | Self::Resolved(_) => None,
        }
    }

    /// Returns `true` if this is a multiline value.
    #[must_use]
    pub fn is_multiline(&self) -> bool {
        matches!(self, Self::Multiline(_) | Self::MultilineLegacy(_))
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

/// A document author.
#[derive(Debug, Clone, PartialEq)]
pub struct Author<'a> {
    /// Full name (e.g., `"Doc Writer"`).
    pub fullname: &'a str,
    /// Initials derived from name parts (e.g., `"DW"`).
    pub initials: String,
    /// First name.
    pub firstname: &'a str,
    /// Middle name (if present).
    pub middlename: Option<&'a str>,
    /// Last name (if present).
    pub lastname: Option<&'a str>,
    /// Email address (from `<email>` syntax).
    pub address: Option<&'a str>,
}

/// A document header containing a title and optional metadata.
#[derive(Debug, Clone, PartialEq)]
pub struct Header<'a> {
    /// Title as inline nodes.
    pub title: Vec<InlineNode<'a>>,
    /// Document authors.
    pub authors: Option<Vec<Author<'a>>>,
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
    /// Uses `Cow` to support both borrowed explicit IDs and owned auto-generated IDs.
    pub id: Option<Cow<'a, str>>,
    /// Block style (e.g., `"appendix"`, `"discrete"`, `"source"`, `"abstract"`).
    pub style: Option<&'a str>,
    /// Target for block macros (e.g., image path).
    pub target: Option<&'a str>,
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
    /// Description list item terms (each term is a group of inline nodes).
    pub terms: Option<Vec<Vec<InlineNode<'a>>>>,
    /// Source location.
    pub location: Option<Location>,
}

impl Block<'_> {
    /// Creates a new block with the given name and all optional fields set to `None`.
    #[must_use]
    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            form: None,
            delimiter: None,
            id: None,
            style: None,
            target: None,
            reftext: None,
            metadata: None,
            title: None,
            level: None,
            variant: None,
            marker: None,
            inlines: None,
            blocks: None,
            items: None,
            principal: None,
            terms: None,
            location: None,
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    // ── AttributeValue::resolve ──────────────────────────────────────

    #[test]
    fn resolve_single() {
        let v = AttributeValue::Single("hello");
        assert_eq!(v.resolve(), "hello");
        assert!(matches!(v.resolve(), Cow::Borrowed(_)));
    }

    #[test]
    fn resolve_multiline() {
        let v = AttributeValue::Multiline(vec!["hello", "world"]);
        assert_eq!(v.resolve(), "hello world");
    }

    #[test]
    fn resolve_multiline_legacy() {
        let v = AttributeValue::MultilineLegacy(vec!["hello", "world"]);
        assert_eq!(v.resolve(), "helloworld");
    }

    #[test]
    fn resolve_resolved() {
        let v = AttributeValue::Resolved("already done".to_string());
        assert_eq!(v.resolve(), "already done");
    }

    // ── AttributeValue::as_str ───────────────────────────────────────

    #[test]
    fn as_str_single() {
        let v = AttributeValue::Single("val");
        assert_eq!(v.as_str(), Some("val"));
    }

    #[test]
    fn as_str_multiline_returns_none() {
        let v = AttributeValue::Multiline(vec!["a", "b"]);
        assert_eq!(v.as_str(), None);
    }

    #[test]
    fn as_str_multiline_legacy_returns_none() {
        let v = AttributeValue::MultilineLegacy(vec!["a", "b"]);
        assert_eq!(v.as_str(), None);
    }

    #[test]
    fn as_str_resolved_returns_none() {
        let v = AttributeValue::Resolved("x".to_string());
        assert_eq!(v.as_str(), None);
    }

    // ── AttributeValue::is_multiline ─────────────────────────────────

    #[test]
    fn is_multiline_single() {
        assert!(!AttributeValue::Single("x").is_multiline());
    }

    #[test]
    fn is_multiline_multiline() {
        assert!(AttributeValue::Multiline(vec!["a"]).is_multiline());
    }

    #[test]
    fn is_multiline_multiline_legacy() {
        assert!(AttributeValue::MultilineLegacy(vec!["a"]).is_multiline());
    }

    #[test]
    fn is_multiline_resolved() {
        assert!(!AttributeValue::Resolved("x".to_string()).is_multiline());
    }

    // ── BlockMetadata default ────────────────────────────────────────

    #[test]
    fn block_metadata_default() {
        let m = BlockMetadata::default();
        assert!(m.roles.is_empty());
        assert!(m.options.is_empty());
        assert!(m.attributes.is_empty());
    }

    // ── Block::new ───────────────────────────────────────────────────

    #[test]
    fn block_new_sets_name() {
        let b = Block::new("paragraph");
        assert_eq!(b.name, "paragraph");
    }

    #[test]
    fn block_new_all_fields_none() {
        let b = Block::new("paragraph");
        assert!(b.form.is_none());
        assert!(b.delimiter.is_none());
        assert!(b.id.is_none());
        assert!(b.style.is_none());
        assert!(b.target.is_none());
        assert!(b.reftext.is_none());
        assert!(b.metadata.is_none());
        assert!(b.title.is_none());
        assert!(b.level.is_none());
        assert!(b.variant.is_none());
        assert!(b.marker.is_none());
        assert!(b.inlines.is_none());
        assert!(b.blocks.is_none());
        assert!(b.items.is_none());
        assert!(b.principal.is_none());
        assert!(b.terms.is_none());
        assert!(b.location.is_none());
    }
}

#![doc = include_str!("../README.md")]
#![deny(missing_docs, unsafe_code)]

use serde::Serialize;
use std::collections::HashMap;

/// A 1-based source position (line and column).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Position {
    /// 1-based line number.
    pub line: usize,
    /// 1-based column number.
    pub col: usize,
}

/// A source location as a `[start, end]` pair of positions.
pub type Location = [Position; 2];

/// The root document node of an ASG.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Document<'a> {
    /// Node name, always `"document"`.
    pub name: &'static str,
    /// Node type, always `"block"`.
    #[serde(rename = "type")]
    pub node_type: &'static str,
    /// Document-level attributes.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attributes: Option<HashMap<&'a str, &'a str>>,
    /// Document header (title, authors, etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub header: Option<Header<'a>>,
    /// Top-level blocks in the document body.
    pub blocks: Vec<Block<'a>>,
    /// Source location of the document.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<Location>,
}

/// A document header containing a title and optional metadata.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Header<'a> {
    /// Title as inline nodes.
    pub title: Vec<InlineNode<'a>>,
    /// Source location of the header.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<Location>,
}

/// A block-level ASG node.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Block<'a> {
    /// Block name (e.g., `"paragraph"`, `"section"`, `"list"`, `"listing"`, `"sidebar"`).
    pub name: &'static str,
    /// Node type, always `"block"`.
    #[serde(rename = "type")]
    pub node_type: &'static str,
    /// Structural form (e.g., `"delimited"`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub form: Option<&'static str>,
    /// Delimiter string for delimited blocks.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub delimiter: Option<&'a str>,
    /// Section title as inline nodes.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<Vec<InlineNode<'a>>>,
    /// Section level (0-5).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub level: Option<usize>,
    /// List variant (e.g., `"unordered"`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub variant: Option<&'static str>,
    /// List or list-item marker (e.g., `"*"`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub marker: Option<&'a str>,
    /// Inline content for leaf blocks (paragraph, listing, etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inlines: Option<Vec<InlineNode<'a>>>,
    /// Child blocks for compound blocks (section, sidebar, etc.).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub blocks: Option<Vec<Block<'a>>>,
    /// List items.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub items: Option<Vec<Block<'a>>>,
    /// Principal content of a list item.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub principal: Option<Vec<InlineNode<'a>>>,
    /// Source location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<Location>,
}

/// An inline ASG node — either a text string or a formatting span.
#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(untagged)]
pub enum InlineNode<'a> {
    /// A text string node (`type: "string"`).
    Text(TextNode<'a>),
    /// A formatting span node (`type: "inline"`).
    Span(SpanNode<'a>),
}

/// A leaf text node in the ASG.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct TextNode<'a> {
    /// Node name, typically `"text"`.
    pub name: &'static str,
    /// Node type, always `"string"`.
    #[serde(rename = "type")]
    pub node_type: &'static str,
    /// The text value, borrowed from the input.
    pub value: &'a str,
    /// Source location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<Location>,
}

/// An inline formatting span (e.g., strong, emphasis).
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SpanNode<'a> {
    /// Node name, typically `"span"`.
    pub name: &'static str,
    /// Node type, always `"inline"`.
    #[serde(rename = "type")]
    pub node_type: &'static str,
    /// Span variant (e.g., `"strong"`, `"emphasis"`, `"code"`, `"mark"`).
    pub variant: &'static str,
    /// Span form (`"constrained"` or `"unconstrained"`).
    pub form: &'static str,
    /// Child inline nodes.
    pub inlines: Vec<InlineNode<'a>>,
    /// Source location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<Location>,
}

/// Parse an AsciiDoc document and return its ASG.
pub fn parse_document(_input: &str) -> Document<'_> {
    Document {
        name: "document",
        node_type: "block",
        attributes: None,
        header: None,
        blocks: Vec::new(),
        location: None,
    }
}

/// Parse AsciiDoc inline content and return a list of inline nodes.
pub fn parse_inline(_input: &str) -> Vec<InlineNode<'_>> {
    Vec::new()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}

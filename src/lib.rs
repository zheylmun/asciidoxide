#![doc = include_str!("../README.md")]
#![deny(missing_docs, unsafe_code)]

pub mod asg;
mod span;
mod token;

use asg::{Document, InlineNode};

/// Parse an `AsciiDoc` document and return its ASG.
#[must_use]
pub fn parse_document(_input: &str) -> Document<'_> {
    Document {
        attributes: None,
        header: None,
        blocks: Vec::new(),
        location: None,
    }
}

/// Parse `AsciiDoc` inline content and return a list of inline nodes.
#[must_use]
pub fn parse_inline(_input: &str) -> Vec<InlineNode<'_>> {
    Vec::new()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}

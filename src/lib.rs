#![doc = include_str!("../README.md")]
#![deny(missing_docs, unsafe_code)]

pub mod asg;
mod lexer;
mod parser;
mod span;
mod token;

use asg::{Document, InlineNode};

/// Parse an `AsciiDoc` document and return its ASG.
#[must_use]
pub fn parse_document(input: &str) -> Document<'_> {
    parser::parse_doc(input)
}

/// Parse `AsciiDoc` inline content and return a list of inline nodes.
#[must_use]
pub fn parse_inline(input: &str) -> Vec<InlineNode<'_>> {
    parser::parse_inlines(input)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}

#![doc = include_str!("../README.md")]
#![deny(missing_docs, unsafe_code)]

pub mod asg;
pub mod diagnostic;
mod lexer;
mod parser;
mod span;
mod token;

use asg::{Document, InlineNode};
use diagnostic::ParseDiagnostic;

/// Parse an `AsciiDoc` document and return its ASG along with any diagnostics.
#[must_use]
pub fn parse_document(input: &str) -> (Document<'_>, Vec<ParseDiagnostic>) {
    parser::parse_doc(input)
}

/// Parse `AsciiDoc` inline content and return a list of inline nodes along
/// with any diagnostics.
#[must_use]
pub fn parse_inline(input: &str) -> (Vec<InlineNode<'_>>, Vec<ParseDiagnostic>) {
    parser::parse_inlines(input)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}

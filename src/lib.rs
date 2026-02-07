#![doc = include_str!("../README.md")]
#![deny(missing_docs, unsafe_code)]

pub mod asg;
pub mod diagnostic;
mod lexer;
mod parser;
pub mod preprocess;
pub mod span;
mod token;

use asg::{Document, InlineNode};
use diagnostic::ParseDiagnostic;

/// Parse an `AsciiDoc` document and return its ASG along with any diagnostics.
///
/// This function does **not** evaluate conditional preprocessing directives
/// (`ifdef`, `ifndef`, `ifeval`). To support conditional content, use
/// [`preprocess::preprocess`] first:
///
/// ```
/// use std::collections::HashMap;
/// use asciidoxide::{preprocess, parse_document};
///
/// let input = "ifdef::debug[]\nDebug mode\nendif::[]";
/// let attrs = HashMap::from([("debug".to_string(), "true".to_string())]);
///
/// // Step 1: Preprocess
/// let result = preprocess::preprocess(input, &attrs);
///
/// // Step 2: Parse the preprocessed content
/// let (doc, diags) = parse_document(&result.content);
/// ```
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

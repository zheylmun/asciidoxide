//! High performance [AsciiDoc](https://asciidoc.org) lexer and parser library
//! written in safe Rust.
//!
//! This crate provides a zero-copy, spec-compliant AsciiDoc parser that
//! produces an Abstract Semantic Graph (ASG). It uses
//! [chumsky](https://github.com/zesterer/chumsky) for lexing and parsing.
//!
//! # Usage
//!
//! The two main entry points are [`parse_document`] for full documents and
//! [`parse_inline`] for inline content:
//!
//! ```
//! let input = "= My Document\n\nA paragraph with *bold* text.";
//! let (doc, diagnostics) = asciidoxide_parser::parse_document(input);
//! ```
//!
//! # Preprocessing
//!
//! Conditional directives (`ifdef`, `ifndef`, `ifeval`) are handled by the
//! [`preprocess`] module, which should be run before parsing:
//!
//! ```
//! use std::collections::HashMap;
//! use asciidoxide_parser::{preprocess, parse_document};
//!
//! let input = "ifdef::debug[]\nDebug mode\nendif::[]";
//! let attrs = HashMap::from([("debug".to_string(), "true".to_string())]);
//!
//! let result = preprocess::preprocess(input, &attrs);
//! let (doc, diags) = parse_document(&result.content);
//! ```
//!
//! # Architecture
//!
//! Parsing proceeds in three phases:
//!
//! 1. **Preprocess** ([`preprocess`]): Evaluates conditional directives against
//!    document attributes. Returns `Cow<str>` (zero-copy when no directives
//!    are found).
//!
//! 2. **Block boundary detection**: Chumsky combinators identify block structure
//!    from the token stream, producing intermediate representations with
//!    byte-offset spans.
//!
//! 3. **Transform and inline parsing**: Converts block intermediates into ASG
//!    [`asg::Block`] nodes, parsing inline content (formatting spans, macros,
//!    passthroughs) recursively.

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
/// use asciidoxide_parser::{preprocess, parse_document};
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

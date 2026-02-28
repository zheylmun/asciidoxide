//! Parser that transforms a token stream into ASG nodes.
//!
//! Both the **inline parser** and **block parser** are chumsky-based. The inline
//! parser uses recursive combinators for formatting. The block parser uses
//! chumsky's `custom()` combinator for complex patterns like delimiter matching,
//! which suits `AsciiDoc`'s line-oriented block structure.

mod block;
mod inline;

pub(crate) use block::BlockExtra;

use crate::asg::{Document, InlineNode};
use crate::diagnostic::ParseDiagnostic;
use crate::lexer::lex;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// A token paired with its source span (re-stated here to avoid a name
/// conflict with [`chumsky::span::Spanned`]).
type Spanned<'a> = (Token<'a>, SourceSpan);

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Parse a full `AsciiDoc` document into its ASG and any diagnostics.
#[must_use]
pub fn parse_doc(input: &str) -> (Document<'_>, Vec<ParseDiagnostic>) {
    let tokens = lex(input);
    let idx = SourceIndex::new(input);

    if tokens.is_empty() {
        return (
            Document {
                attributes: None,
                header: None,
                blocks: Vec::new(),
                location: None,
            },
            Vec::new(),
        );
    }

    let block::HeaderResult {
        header,
        body_start,
        mut diagnostics,
        attributes,
    } = block::extract_header(&tokens, input, &idx);
    let body_tokens = &tokens[body_start..];
    let (blocks, block_diags) = block::build_blocks(body_tokens, input, &idx);
    diagnostics.extend(block_diags);

    // Document location: first content token → last content token.
    let doc_span = content_span(&tokens);
    let location = doc_span.map(|s| idx.location(&s));

    (
        Document {
            attributes,
            header,
            blocks,
            location,
        },
        diagnostics,
    )
}

/// Parse `AsciiDoc` inline content into inline nodes and any diagnostics.
#[must_use]
pub fn parse_inlines(input: &str) -> (Vec<InlineNode<'_>>, Vec<ParseDiagnostic>) {
    let tokens = lex(input);
    let idx = SourceIndex::new(input);
    let trimmed = strip_trailing_newlines(&tokens);
    inline::run_inline_parser(trimmed, input, &idx)
}

// ---------------------------------------------------------------------------
// Helpers (shared with sub-modules)
// ---------------------------------------------------------------------------

/// Compute the overall `SourceSpan` covering all tokens in a slice,
/// or `None` if the slice is empty.
pub(super) fn content_span(tokens: &[Spanned<'_>]) -> Option<SourceSpan> {
    let trimmed = strip_trailing_newlines(tokens);
    let start = trimmed.first()?.1.start;
    let end = trimmed.last()?.1.end;
    Some(SourceSpan { start, end })
}

/// Return a sub-slice with trailing `Newline` tokens removed.
pub(super) fn strip_trailing_newlines<'a, 'src>(
    tokens: &'a [Spanned<'src>],
) -> &'a [Spanned<'src>] {
    let end = strip_trailing_newline_index(tokens, 0);
    &tokens[..end]
}

/// Return the exclusive end index after stripping trailing newlines
/// starting the search from `from`.
pub(super) fn strip_trailing_newline_index(tokens: &[Spanned<'_>], from: usize) -> usize {
    let mut end = tokens.len();
    while end > from && matches!(tokens[end - 1].0, Token::Newline) {
        end -= 1;
    }
    end
}

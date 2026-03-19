//! Table block boundary parser.
//!
//! Recognizes `|===` delimiters and captures the content span between them.
//! The content is parsed into rows and cells during phase 3 (transform).

use chumsky::{input::ValueInput, prelude::*};

use super::utility::BlockExtra;
use crate::parser::block::raw_block::RawBlock;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Parse a table block delimited by `|===`.
///
/// Tables use `|` followed by 3+ `=` as their delimiter. The content between
/// the opening and closing delimiters is captured as a span for phase 3
/// to parse into rows and cells.
pub(super) fn table_block<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Check for opening pipe
        if !matches!(inp.peek(), Some(Token::Pipe)) {
            return Err(Rich::custom(inp.span_since(&start_cursor), "expected pipe"));
        }
        inp.skip();

        // Count equals signs (need 3+)
        let mut eq_count = 0;
        while matches!(inp.peek(), Some(Token::Eq)) {
            inp.skip();
            eq_count += 1;
        }

        if eq_count < 3 {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "need at least 3 equals for table delimiter",
            ));
        }

        // Must be followed by newline or EOF
        if inp.peek().is_some() && !matches!(inp.peek(), Some(Token::Newline)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected newline after table delimiter",
            ));
        }

        let delimiter_span: SourceSpan = inp.span_since(&start_cursor);

        // Consume newline if present
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        // Capture content start
        let content_cursor = inp.cursor();
        let content_start_span: SourceSpan = inp.span_since(&content_cursor);
        let content_start = content_start_span.start;

        // Scan for matching closing delimiter
        let mut at_line_start = true;
        loop {
            if at_line_start {
                let close_cursor = inp.cursor();
                let close_start_span: SourceSpan = inp.span_since(&close_cursor);
                let content_end = close_start_span.start;

                // Check for |===
                if matches!(inp.peek(), Some(Token::Pipe)) {
                    inp.skip();
                    let mut close_eq_count = 0;
                    while matches!(inp.peek(), Some(Token::Eq)) {
                        inp.skip();
                        close_eq_count += 1;
                    }

                    if close_eq_count == eq_count
                        && (inp.peek().is_none() || matches!(inp.peek(), Some(Token::Newline)))
                    {
                        // Found matching closer
                        let block_end_span: SourceSpan = inp.span_since(&start_cursor);
                        if matches!(inp.peek(), Some(Token::Newline)) {
                            inp.skip();
                        }

                        let delimiter = &source[delimiter_span.start..delimiter_span.end];

                        // Adjust content_end to exclude the preceding newline
                        let actual_content_end = if content_end > content_start {
                            content_end - 1
                        } else {
                            content_end
                        };

                        let mut block = RawBlock::new("table");
                        block.form = Some("delimited");
                        block.delimiter = Some(delimiter);
                        if content_start < actual_content_end {
                            block.content_span = Some(SourceSpan {
                                start: content_start,
                                end: actual_content_end,
                            });
                        }
                        block.location = Some(idx.location(&block_end_span));

                        return Ok(block);
                    }
                    // Not a closing delimiter, continue scanning
                }
            }

            // Check for end of input (unclosed block)
            if inp.peek().is_none() {
                inp.rewind(before);
                return Err(Rich::custom(
                    inp.span_since(&start_cursor),
                    "unclosed table block",
                ));
            }

            // Skip token
            if matches!(inp.peek(), Some(Token::Newline)) {
                inp.skip();
                at_line_start = true;
            } else {
                inp.skip();
                at_line_start = false;
            }
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::lexer::lex;
    use crate::parser::block::boundary::parse_raw_blocks;
    use crate::span::SourceIndex;

    #[test]
    fn test_simple_table() {
        let source = "|===\n|Cell 1 |Cell 2\n|Cell 3 |Cell 4\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "table");
        assert_eq!(blocks[0].form, Some("delimited"));
        assert_eq!(blocks[0].delimiter, Some("|==="));
        let span = blocks[0].content_span.expect("should have content span");
        assert_eq!(
            &source[span.start..span.end],
            "|Cell 1 |Cell 2\n|Cell 3 |Cell 4"
        );
    }

    #[test]
    fn test_empty_table() {
        let source = "|===\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "table");
        assert!(blocks[0].content_span.is_none());
    }

    #[test]
    fn test_table_with_header() {
        let source = "|===\n|Header 1 |Header 2\n\n|Cell 1 |Cell 2\n|===\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "table");
        let span = blocks[0].content_span.expect("should have content span");
        assert_eq!(
            &source[span.start..span.end],
            "|Header 1 |Header 2\n\n|Cell 1 |Cell 2"
        );
    }

    #[test]
    fn test_table_interrupts_paragraph() {
        let source = "before\n\n|===\n|Cell\n|===\n\nafter\n";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 3);
        assert_eq!(blocks[0].name, "paragraph");
        assert_eq!(blocks[1].name, "table");
        assert_eq!(blocks[2].name, "paragraph");
    }

    #[test]
    fn test_table_no_trailing_newline() {
        let source = "|===\n|Cell 1\n|===";
        let tokens = lex(source);
        let idx = SourceIndex::new(source);

        let (blocks, diags) = parse_raw_blocks(&tokens, source, &idx);

        assert!(diags.is_empty(), "diagnostics: {diags:?}");
        assert_eq!(blocks.len(), 1);
        assert_eq!(blocks[0].name, "table");
    }
}

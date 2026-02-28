//! Block macro parsers.

use chumsky::{input::ValueInput, prelude::*};

use super::utility::BlockExtra;
use crate::parser::block::raw_block::RawBlock;
use crate::span::{SourceIndex, SourceSpan};
use crate::token::Token;

/// Parse an `image::target[attrlist]` block macro.
///
/// Produces a `RawBlock` with `name="image"`, `form="macro"`, `target="path"`.
pub(super) fn image_block_macro<'tokens, 'src: 'tokens, I>(
    source: &'src str,
    idx: &'tokens SourceIndex,
) -> impl Parser<'tokens, I, RawBlock<'src>, BlockExtra<'tokens, 'src>> + Clone
where
    I: ValueInput<'tokens, Token = Token<'src>, Span = SourceSpan>,
{
    custom(move |inp| {
        let before = inp.save();
        let start_cursor = inp.cursor();

        // Match "image" text
        if let Some(Token::Text("image")) = inp.peek() {
            inp.skip();
        } else {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected 'image'",
            ));
        }

        // Match `::`
        if !matches!(inp.peek(), Some(Token::Colon)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected ':' after 'image'",
            ));
        }
        inp.skip();
        if !matches!(inp.peek(), Some(Token::Colon)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected '::' after 'image'",
            ));
        }
        inp.skip();

        // Capture target (everything until `[`)
        let target_cursor = inp.cursor();
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::LBracket | Token::Newline))
        {
            inp.skip();
        }
        let target_span: SourceSpan = inp.span_since(&target_cursor);

        if target_span.start >= target_span.end {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "image macro target cannot be empty",
            ));
        }

        // Match `[`
        if !matches!(inp.peek(), Some(Token::LBracket)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected '[' after image target",
            ));
        }
        inp.skip();

        // Consume attrlist until `]` (we ignore the content for now)
        while inp.peek().is_some() && !matches!(inp.peek(), Some(Token::RBracket | Token::Newline))
        {
            inp.skip();
        }

        // Match `]`
        if !matches!(inp.peek(), Some(Token::RBracket)) {
            inp.rewind(before);
            return Err(Rich::custom(
                inp.span_since(&start_cursor),
                "expected ']' to close image macro",
            ));
        }
        inp.skip();

        let block_span: SourceSpan = inp.span_since(&start_cursor);

        // Consume trailing newline
        if matches!(inp.peek(), Some(Token::Newline)) {
            inp.skip();
        }

        let target = &source[target_span.start..target_span.end];
        let mut block = RawBlock::new("image");
        block.form = Some("macro");
        block.target = Some(target);
        block.location = Some(idx.location(&block_span));

        Ok(block)
    })
}

//! Source location tracking for the parser pipeline.
//!
//! [`SourceSpan`] is a lightweight, `Copy` span that stores byte offsets into
//! the source text. It implements [`chumsky::span::Span`], bridging the
//! lexer/parser layers without any wrapper types.
//!
//! [`SourceIndex`] pre-computes line-start byte offsets so that a
//! [`SourceSpan`] can be cheaply converted into the 1-based `[Position; 2]`
//! locations used by the ASG.
//!
//! # Typical flow
//!
//! 1. The **lexer** consumes `&str` and produces `(Token, SourceSpan)` pairs
//!    via chumsky's `map_with` combinator.
//! 2. The **parser** propagates spans through `map_with` as it builds ASG
//!    nodes.
//! 3. At the end of a parse, a [`SourceIndex`] built from the original input
//!    converts each [`SourceSpan`] into an ASG [`Location`].

use std::fmt;
use std::ops::Range;

use crate::asg::{Location, Position};

// ---------------------------------------------------------------------------
// SourceSpan
// ---------------------------------------------------------------------------

/// A byte-offset span into the source text.
///
/// This type is intentionally minimal — just a start and end offset — so that
/// it can be `Copy` and cheaply threaded through the parser pipeline. Both
/// chumsky accepts it directly via its [`Span`](chumsky::span::Span) impl.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SourceSpan {
    /// Inclusive start byte offset.
    pub start: usize,
    /// Exclusive end byte offset.
    pub end: usize,
}

impl fmt::Debug for SourceSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl fmt::Display for SourceSpan {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<Range<usize>> for SourceSpan {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }
}

impl From<SourceSpan> for Range<usize> {
    fn from(span: SourceSpan) -> Self {
        span.start..span.end
    }
}

impl From<chumsky::span::SimpleSpan> for SourceSpan {
    fn from(span: chumsky::span::SimpleSpan) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

// -- chumsky::span::Span ----------------------------------------------------

impl chumsky::span::Span for SourceSpan {
    type Context = ();
    type Offset = usize;

    fn new(_context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

// ---------------------------------------------------------------------------
// SourceIndex
// ---------------------------------------------------------------------------

/// Pre-computed line-start index for converting byte offsets to 1-based
/// line/column positions.
///
/// Build one from the source text before (or after) parsing, then call
/// [`location`](Self::location) to turn any [`SourceSpan`] into an ASG
/// [`Location`].
#[derive(Debug, Clone)]
pub struct SourceIndex {
    /// Byte offset of the first character on each line.
    /// `line_starts[0]` is always `0`.
    line_starts: Vec<usize>,
    /// 0-based character column for each byte offset.
    ///
    /// Multi-byte characters map all their bytes to the same column.
    /// Length is `source.len() + 1` (extra slot for end-of-input).
    byte_to_col: Vec<usize>,
}

impl SourceIndex {
    /// Build a line-start index from the source text.
    #[must_use]
    pub fn new(source: &str) -> Self {
        let mut line_starts = vec![0];
        let mut byte_to_col = vec![0; source.len() + 1];
        let mut col: usize = 0;

        for (i, ch) in source.char_indices() {
            for slot in byte_to_col.iter_mut().skip(i).take(ch.len_utf8()) {
                *slot = col;
            }
            if ch == '\n' {
                line_starts.push(i + 1);
                col = 0;
            } else {
                col += 1;
            }
        }
        byte_to_col[source.len()] = col;

        Self {
            line_starts,
            byte_to_col,
        }
    }

    /// Convert a byte offset to a 1-based [`Position`].
    ///
    /// The column value counts Unicode characters from the start of the line
    /// (1-based).
    #[must_use]
    pub fn position(&self, byte_offset: usize) -> Position {
        let line = self
            .line_starts
            .partition_point(|&start| start <= byte_offset)
            - 1;
        let col = self.byte_to_col[byte_offset];
        Position {
            line: line + 1,
            col: col + 1,
        }
    }

    /// Convert a [`SourceSpan`] to an ASG [`Location`].
    ///
    /// The end position is **inclusive** (points to the last byte of the span),
    /// matching the convention used by the `AsciiDoc` TCK.
    #[must_use]
    pub fn location(&self, span: &SourceSpan) -> Location {
        let end = if span.end > span.start {
            span.end - 1
        } else {
            span.start
        };
        [self.position(span.start), self.position(end)]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_line() {
        let src = "hello";
        let idx = SourceIndex::new(src);
        assert_eq!(idx.position(0), Position { line: 1, col: 1 });
        assert_eq!(idx.position(4), Position { line: 1, col: 5 });
    }

    #[test]
    fn multiple_lines() {
        let src = "ab\ncd\nef";
        let idx = SourceIndex::new(src);
        // Line 1: bytes 0..2 ("ab")
        assert_eq!(idx.position(0), Position { line: 1, col: 1 });
        assert_eq!(idx.position(1), Position { line: 1, col: 2 });
        // Line 2: bytes 3..4 ("cd")
        assert_eq!(idx.position(3), Position { line: 2, col: 1 });
        assert_eq!(idx.position(4), Position { line: 2, col: 2 });
        // Line 3: bytes 6..7 ("ef")
        assert_eq!(idx.position(6), Position { line: 3, col: 1 });
        assert_eq!(idx.position(7), Position { line: 3, col: 2 });
    }

    #[test]
    fn span_to_location() {
        let src = "ab\ncd";
        let idx = SourceIndex::new(src);
        // Span 0..4 covers bytes a(0) b(1) \n(2) c(3).
        // Inclusive end is byte 3 → line 2, col 1.
        let span = SourceSpan { start: 0, end: 4 };
        let loc = idx.location(&span);
        assert_eq!(loc[0], Position { line: 1, col: 1 });
        assert_eq!(loc[1], Position { line: 2, col: 1 });
    }

    #[test]
    fn empty_source() {
        let idx = SourceIndex::new("");
        assert_eq!(idx.position(0), Position { line: 1, col: 1 });
    }

    #[test]
    fn trailing_newline() {
        let src = "a\n";
        let idx = SourceIndex::new(src);
        assert_eq!(idx.position(0), Position { line: 1, col: 1 });
        assert_eq!(idx.position(2), Position { line: 2, col: 1 });
    }
}

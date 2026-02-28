//! Parse diagnostics produced during ASG construction.
//!
//! The parser crate produces structured diagnostic data — it does **not**
//! render diagnostics. Consumers can use [`ParseDiagnostic::span`] to build
//! their own error reports (e.g., via `ariadne`).

use crate::span::SourceSpan;

/// Severity level for a parse diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// A warning — the parser recovered but the output may not match intent.
    Warning,
    /// An error — the parser could not fully interpret the input.
    Error,
}

/// A diagnostic emitted during parsing.
///
/// Contains enough information for consumers to render error reports
/// (e.g., via `ariadne`). [`SourceSpan`] exposes public `start`/`end` byte
/// offsets and converts to `Range<usize>` via [`From`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseDiagnostic {
    /// The source span where the issue was detected.
    pub span: SourceSpan,
    /// Human-readable description of the issue.
    pub message: String,
    /// Severity level.
    pub severity: Severity,
}

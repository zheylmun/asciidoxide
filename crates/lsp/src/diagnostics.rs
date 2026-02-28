use asciidoxide_parser::diagnostic::{ParseDiagnostic, Severity};
use asciidoxide_parser::span::SourceIndex;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Convert an asciidoxide `ParseDiagnostic` to an LSP `Diagnostic`.
///
/// Uses the `SourceIndex` to convert byte-offset spans into 0-based
/// line/character positions expected by the LSP protocol.
#[allow(clippy::cast_possible_truncation)]
fn to_lsp_diagnostic(diag: &ParseDiagnostic, source_index: &SourceIndex<'_>) -> Diagnostic {
    let start = source_index.position(diag.span.start);
    let end = source_index.position(diag.span.end);

    // asciidoxide positions are 1-based; LSP positions are 0-based
    let range = Range::new(
        Position::new(start.line as u32 - 1, start.col as u32 - 1),
        Position::new(end.line as u32 - 1, end.col as u32 - 1),
    );

    let severity = match diag.severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
    };

    Diagnostic {
        range,
        severity: Some(severity),
        source: Some("asciidoc".to_string()),
        message: diag.message.clone(),
        ..Diagnostic::default()
    }
}

/// Convert a slice of `ParseDiagnostic`s to LSP `Diagnostic`s.
pub(crate) fn to_lsp_diagnostics(
    diagnostics: &[ParseDiagnostic],
    source_index: &SourceIndex<'_>,
) -> Vec<Diagnostic> {
    diagnostics
        .iter()
        .map(|d| to_lsp_diagnostic(d, source_index))
        .collect()
}

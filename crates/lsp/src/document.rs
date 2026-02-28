use asciidoxide_parser::asg::Document;
use asciidoxide_parser::diagnostic::ParseDiagnostic;
use asciidoxide_parser::span::SourceIndex;
use self_cell::self_cell;
use tower_lsp::lsp_types::Diagnostic;

use crate::diagnostics::to_lsp_diagnostics;

self_cell! {
    struct ParsedDocument {
        owner: String,

        #[not_covariant]
        dependent: Document,
    }
}

pub struct DocumentState {
    parsed: ParsedDocument,
    diagnostics: Vec<ParseDiagnostic>,
    version: i32,
}

impl DocumentState {
    #[must_use]
    pub fn new(version: i32, source: String) -> Self {
        let mut diagnostics = Vec::new();
        let parsed = ParsedDocument::new(source, |s| {
            let (doc, diags) = asciidoxide_parser::parse_document(s);
            diagnostics = diags;
            doc
        });
        Self {
            parsed,
            diagnostics,
            version,
        }
    }

    pub fn reparse(&mut self, version: i32, source: String) {
        let mut diagnostics = Vec::new();
        let parsed = ParsedDocument::new(source, |s| {
            let (doc, diags) = asciidoxide_parser::parse_document(s);
            diagnostics = diags;
            doc
        });
        self.parsed = parsed;
        self.diagnostics = diagnostics;
        self.version = version;
    }

    #[must_use]
    pub fn version(&self) -> i32 {
        self.version
    }

    /// Build LSP diagnostics on demand, constructing a temporary `SourceIndex`.
    #[must_use]
    pub fn lsp_diagnostics(&self) -> Vec<Diagnostic> {
        let idx = SourceIndex::new(self.parsed.borrow_owner());
        to_lsp_diagnostics(&self.diagnostics, &idx)
    }
}

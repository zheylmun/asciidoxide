use asciidoxide_parser::asg::Document;
use tower_lsp::lsp_types::{DocumentSymbol, SymbolKind};

use crate::util::{asg_location_to_lsp_range, inlines_to_text, zero_range};

fn blocks_to_symbols(blocks: &[asciidoxide_parser::asg::Block]) -> Vec<DocumentSymbol> {
    let mut symbols = Vec::new();
    for block in blocks {
        if block.name != "section" {
            continue;
        }
        let name = block
            .title
            .as_ref()
            .map(|t| inlines_to_text(t))
            .unwrap_or_default();
        let detail = block.id.as_deref().map(String::from);
        let range = block
            .location
            .as_ref()
            .map_or_else(zero_range, asg_location_to_lsp_range);
        let children = block
            .blocks
            .as_ref()
            .map(|b| blocks_to_symbols(b))
            .unwrap_or_default();

        #[allow(deprecated)]
        symbols.push(DocumentSymbol {
            name,
            detail,
            kind: SymbolKind::STRING,
            range,
            selection_range: range,
            children: if children.is_empty() {
                None
            } else {
                Some(children)
            },
            tags: None,
            deprecated: None,
        });
    }
    symbols
}

pub(crate) fn build_document_symbols(doc: &Document) -> Vec<DocumentSymbol> {
    blocks_to_symbols(&doc.blocks)
}

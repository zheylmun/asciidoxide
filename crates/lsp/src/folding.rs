use asciidoxide_parser::asg::{Block, Document};
use tower_lsp::lsp_types::{FoldingRange, FoldingRangeKind};

fn collect_folding_ranges(blocks: &[Block], ranges: &mut Vec<FoldingRange>) {
    for block in blocks {
        let foldable = matches!(
            block.name,
            "section"
                | "listing"
                | "literal"
                | "pass"
                | "verse"
                | "quote"
                | "example"
                | "sidebar"
                | "open"
                | "list"
                | "dlist"
        );

        if foldable
            && let Some(loc) = &block.location
            && loc[0].line != loc[1].line
        {
            #[allow(clippy::cast_possible_truncation)]
            ranges.push(FoldingRange {
                start_line: (loc[0].line - 1) as u32,
                start_character: None,
                end_line: (loc[1].line - 1) as u32,
                end_character: None,
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: None,
            });
        }

        if let Some(children) = &block.blocks {
            collect_folding_ranges(children, ranges);
        }
        if let Some(items) = &block.items {
            collect_folding_ranges(items, ranges);
        }
    }
}

pub(crate) fn build_folding_ranges(doc: &Document) -> Vec<FoldingRange> {
    let mut ranges = Vec::new();
    collect_folding_ranges(&doc.blocks, &mut ranges);
    ranges
}

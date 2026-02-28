use asciidoxide_parser::asg::{Block, Document, InlineNode, Location};
use tower_lsp::lsp_types::{Position as LspPosition, Range};

use crate::util::asg_location_to_lsp_range;

/// Check whether an ASG location (1-based, end-inclusive) contains a given LSP position (0-based).
fn location_contains(loc: &Location, pos: LspPosition) -> bool {
    let line = pos.line as usize + 1;
    let col = pos.character as usize + 1;

    let start = &loc[0];
    let end = &loc[1];

    if line < start.line || line > end.line {
        return false;
    }
    if line == start.line && col < start.col {
        return false;
    }
    if line == end.line && col > end.col {
        return false;
    }
    true
}

/// Find the xref target at the given cursor position by walking inline nodes.
fn find_xref_in_inlines<'a>(inlines: &[InlineNode<'a>], pos: LspPosition) -> Option<&'a str> {
    for node in inlines {
        match node {
            InlineNode::Ref(r) if r.variant == "xref" => {
                if let Some(loc) = &r.location
                    && location_contains(loc, pos)
                {
                    return Some(r.target);
                }
                if let Some(target) = find_xref_in_inlines(&r.inlines, pos) {
                    return Some(target);
                }
            }
            InlineNode::Span(s) => {
                if let Some(target) = find_xref_in_inlines(&s.inlines, pos) {
                    return Some(target);
                }
            }
            InlineNode::Ref(r) => {
                if let Some(target) = find_xref_in_inlines(&r.inlines, pos) {
                    return Some(target);
                }
            }
            InlineNode::Text(_) | InlineNode::Raw(_) => {}
        }
    }
    None
}

/// Find the xref target at the given cursor position by walking all blocks.
fn find_xref_in_blocks<'a>(blocks: &[Block<'a>], pos: LspPosition) -> Option<&'a str> {
    for block in blocks {
        if let Some(inlines) = &block.inlines
            && let Some(target) = find_xref_in_inlines(inlines, pos)
        {
            return Some(target);
        }
        if let Some(title) = &block.title
            && let Some(target) = find_xref_in_inlines(title, pos)
        {
            return Some(target);
        }
        if let Some(principal) = &block.principal
            && let Some(target) = find_xref_in_inlines(principal, pos)
        {
            return Some(target);
        }
        if let Some(terms) = &block.terms {
            for term in terms {
                if let Some(target) = find_xref_in_inlines(term, pos) {
                    return Some(target);
                }
            }
        }
        if let Some(children) = &block.blocks
            && let Some(target) = find_xref_in_blocks(children, pos)
        {
            return Some(target);
        }
        if let Some(items) = &block.items
            && let Some(target) = find_xref_in_blocks(items, pos)
        {
            return Some(target);
        }
    }
    None
}

/// Find the location of a block with the given ID.
fn find_block_by_id<'a>(blocks: &'a [Block], id: &str) -> Option<&'a Location> {
    for block in blocks {
        if let Some(block_id) = &block.id
            && block_id.as_ref() == id
        {
            return block.location.as_ref();
        }
        if let Some(children) = &block.blocks
            && let Some(loc) = find_block_by_id(children, id)
        {
            return Some(loc);
        }
        if let Some(items) = &block.items
            && let Some(loc) = find_block_by_id(items, id)
        {
            return Some(loc);
        }
    }
    None
}

/// Find the definition target for a cross-reference at the given cursor position.
///
/// Returns the LSP range of the block whose ID matches the xref target, or `None`
/// if the cursor is not on an xref or the target ID is not found.
pub(crate) fn find_definition(doc: &Document<'_>, position: LspPosition) -> Option<Range> {
    let target = find_xref_in_blocks(&doc.blocks, position)?;
    let loc = find_block_by_id(&doc.blocks, target)?;
    Some(asg_location_to_lsp_range(loc))
}

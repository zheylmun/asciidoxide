use asciidoxide_parser::asg::{Block, Document, InlineNode};
use tower_lsp::lsp_types::{
    Hover, HoverContents, MarkupContent, MarkupKind, Position as LspPosition,
};

use crate::util::{asg_location_to_lsp_range, inlines_to_text, location_contains};

/// Information about an inline element at the cursor.
enum InlineHit<'a> {
    Xref {
        target: &'a str,
        location: Option<tower_lsp::lsp_types::Range>,
    },
    Link {
        target: &'a str,
        location: Option<tower_lsp::lsp_types::Range>,
    },
    Span {
        variant: &'static str,
        form: &'static str,
        location: Option<tower_lsp::lsp_types::Range>,
    },
}

/// Search inline nodes for the most specific element at the cursor.
fn find_inline_hit<'a>(inlines: &[InlineNode<'a>], pos: LspPosition) -> Option<InlineHit<'a>> {
    for node in inlines {
        match node {
            InlineNode::Ref(r) => {
                // Recurse into children first for more specific hits
                if let Some(hit) = find_inline_hit(&r.inlines, pos) {
                    return Some(hit);
                }
                if let Some(loc) = &r.location
                    && location_contains(loc, pos)
                {
                    let range = Some(asg_location_to_lsp_range(loc));
                    return Some(if r.variant == "xref" {
                        InlineHit::Xref {
                            target: r.target,
                            location: range,
                        }
                    } else {
                        InlineHit::Link {
                            target: r.target,
                            location: range,
                        }
                    });
                }
            }
            InlineNode::Span(s) => {
                // Recurse into children first for more specific hits
                if let Some(hit) = find_inline_hit(&s.inlines, pos) {
                    return Some(hit);
                }
                if let Some(loc) = &s.location
                    && location_contains(loc, pos)
                {
                    return Some(InlineHit::Span {
                        variant: s.variant,
                        form: s.form,
                        location: Some(asg_location_to_lsp_range(loc)),
                    });
                }
            }
            InlineNode::Text(_) | InlineNode::Raw(_) => {}
        }
    }
    None
}

/// Search all inline fields of a block for a hit.
fn find_inline_hit_in_block<'a>(block: &Block<'a>, pos: LspPosition) -> Option<InlineHit<'a>> {
    for inlines in [&block.inlines, &block.title, &block.principal] {
        if let Some(inlines) = inlines
            && let Some(hit) = find_inline_hit(inlines, pos)
        {
            return Some(hit);
        }
    }
    if let Some(terms) = &block.terms {
        for term in terms {
            if let Some(hit) = find_inline_hit(term, pos) {
                return Some(hit);
            }
        }
    }
    None
}

/// Resolve an xref target to a block's title and type.
fn resolve_xref(blocks: &[Block<'_>], target: &str) -> Option<(String, &'static str)> {
    for block in blocks {
        if let Some(id) = &block.id
            && id.as_ref() == target
        {
            let title = block
                .title
                .as_ref()
                .map(|t| inlines_to_text(t))
                .unwrap_or_default();
            return Some((title, block.name));
        }
        if let Some(children) = &block.blocks
            && let Some(result) = resolve_xref(children, target)
        {
            return Some(result);
        }
        if let Some(items) = &block.items
            && let Some(result) = resolve_xref(items, target)
        {
            return Some(result);
        }
    }
    None
}

/// Check if a block is "hover-worthy" (not a plain paragraph without ID).
fn is_hover_worthy(block: &Block) -> bool {
    block.id.is_some() || block.name == "section" || !matches!(block.name, "paragraph" | "preamble")
}

/// Format hover content for a section block.
fn format_section(block: &Block) -> String {
    let level = block.level.unwrap_or(0);
    let title = block
        .title
        .as_ref()
        .map(|t| inlines_to_text(t))
        .unwrap_or_default();
    let mut parts = vec![format!("**Section** (level {level})")];
    if !title.is_empty() {
        parts.push(format!("Title: {title}"));
    }
    if let Some(id) = &block.id {
        parts.push(format!("ID: `{id}`"));
    }
    if let Some(meta) = &block.metadata {
        if !meta.roles.is_empty() {
            let roles: Vec<&str> = meta.roles.iter().copied().collect();
            parts.push(format!("Roles: {}", roles.join(", ")));
        }
        if !meta.options.is_empty() {
            let opts: Vec<&str> = meta.options.iter().copied().collect();
            parts.push(format!("Options: {}", opts.join(", ")));
        }
    }
    parts.join("\n\n")
}

/// Format hover content for a notable block.
fn format_block(block: &Block) -> String {
    let mut parts = vec![format!("**{}**", capitalize(block.name))];
    if let Some(id) = &block.id {
        parts.push(format!("ID: `{id}`"));
    }
    if let Some(style) = block.style {
        parts.push(format!("Style: {style}"));
    }
    if let Some(meta) = &block.metadata {
        for &(key, value) in &meta.attributes {
            if key == "language" {
                parts.push(format!("Language: `{value}`"));
            }
        }
    }
    if let Some(title_inlines) = &block.title {
        let title = inlines_to_text(title_inlines);
        if !title.is_empty() {
            parts.push(format!("Title: {title}"));
        }
    }
    parts.join("\n\n")
}

fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().to_string() + c.as_str(),
    }
}

/// Walk blocks depth-first, returning hover info for the most specific element at the cursor.
fn find_hover_in_blocks(
    blocks: &[Block<'_>],
    all_blocks: &[Block<'_>],
    pos: LspPosition,
) -> Option<Hover> {
    for block in blocks {
        // First check inline content for the most specific hits
        if let Some(hit) = find_inline_hit_in_block(block, pos) {
            return Some(inline_hit_to_hover(&hit, all_blocks));
        }

        // Recurse into child blocks
        if let Some(children) = &block.blocks
            && let Some(hover) = find_hover_in_blocks(children, all_blocks, pos)
        {
            return Some(hover);
        }
        if let Some(items) = &block.items
            && let Some(hover) = find_hover_in_blocks(items, all_blocks, pos)
        {
            return Some(hover);
        }

        // Fall back to the block itself if cursor is within it
        if let Some(loc) = &block.location
            && location_contains(loc, pos)
            && is_hover_worthy(block)
        {
            let value = if block.name == "section" {
                format_section(block)
            } else {
                format_block(block)
            };
            // Sections are large; omit range. For other notable blocks, include range.
            let range = if block.name == "section" {
                None
            } else {
                Some(asg_location_to_lsp_range(loc))
            };
            return Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value,
                }),
                range,
            });
        }
    }
    None
}

/// Convert an inline hit to a Hover response.
fn inline_hit_to_hover(hit: &InlineHit<'_>, all_blocks: &[Block<'_>]) -> Hover {
    let (value, range) = match *hit {
        InlineHit::Xref { target, location } => {
            let resolved = resolve_xref(all_blocks, target);
            let value = if let Some((title, block_type)) = resolved {
                if title.is_empty() {
                    format!("**Cross-reference**\n\nTarget: `{target}` ({block_type})")
                } else {
                    format!(
                        "**Cross-reference**\n\nTarget: `{target}` ({block_type})\n\nTitle: {title}"
                    )
                }
            } else {
                format!("**Cross-reference**\n\nTarget: `{target}`\n\nTarget not found in document")
            };
            (value, location)
        }
        InlineHit::Link { target, location } => (format!("**Link**\n\nTarget: {target}"), location),
        InlineHit::Span {
            variant,
            form,
            location,
        } => (format!("**{}** ({})", capitalize(variant), form), location),
    };
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range,
    }
}

/// Build hover information for the element at the given cursor position.
pub(crate) fn build_hover(doc: &Document<'_>, position: LspPosition) -> Option<Hover> {
    find_hover_in_blocks(&doc.blocks, &doc.blocks, position)
}

#[cfg(test)]
mod tests {
    use super::*;
    use asciidoxide_parser::parse_document;
    use tower_lsp::lsp_types::Position;

    fn hover_at(source: &str, line: u32, character: u32) -> Option<Hover> {
        let (doc, _) = parse_document(source);
        build_hover(&doc, Position { line, character })
    }

    fn hover_value(hover: &Hover) -> &str {
        match &hover.contents {
            HoverContents::Markup(m) => &m.value,
            _ => panic!("expected markup content"),
        }
    }

    #[test]
    fn section_hover() {
        let source = "== My Section\n\nSome text here.\n";
        let hover = hover_at(source, 0, 3).expect("should have hover");
        let value = hover_value(&hover);
        assert!(value.contains("**Section**"), "got: {value}");
        assert!(value.contains("level 1"), "got: {value}");
        assert!(value.contains("My Section"), "got: {value}");
        assert!(hover.range.is_none(), "sections should omit range");
    }

    #[test]
    fn xref_resolved() {
        // Parser uses xref:target[] macro syntax (not <<target>>)
        let source = "[[my-target]]\n== Target Section\n\nSee xref:my-target[here].\n";
        // xref is on line 3 (0-based), starts at col 4
        let hover = hover_at(source, 3, 8).expect("should have hover");
        let value = hover_value(&hover);
        assert!(value.contains("**Cross-reference**"), "got: {value}");
        assert!(value.contains("my-target"), "got: {value}");
        assert!(value.contains("Target Section"), "got: {value}");
    }

    #[test]
    fn xref_unresolved() {
        let source = "See xref:nonexistent[link].\n";
        let hover = hover_at(source, 0, 8).expect("should have hover");
        let value = hover_value(&hover);
        assert!(value.contains("not found"), "got: {value}");
    }

    #[test]
    fn link_hover() {
        let source = "Visit https://example.com for info.\n";
        let hover = hover_at(source, 0, 10).expect("should have hover");
        let value = hover_value(&hover);
        assert!(value.contains("**Link**"), "got: {value}");
        assert!(value.contains("https://example.com"), "got: {value}");
    }

    #[test]
    fn span_hover() {
        let source = "This is *bold text* here.\n";
        let hover = hover_at(source, 0, 10).expect("should have hover");
        let value = hover_value(&hover);
        assert!(value.contains("**Strong**"), "got: {value}");
        assert!(value.contains("constrained"), "got: {value}");
    }

    #[test]
    fn plain_text_no_hover() {
        let source = "Just a plain paragraph.\n";
        let hover = hover_at(source, 0, 5);
        assert!(hover.is_none(), "plain text should not hover");
    }

    #[test]
    fn inline_priority_over_block() {
        // Bold text inside a section — hovering on the bold should show span, not section
        let source = "== Section\n\nSome *bold* text.\n";
        let hover = hover_at(source, 2, 6).expect("should have hover");
        let value = hover_value(&hover);
        assert!(
            value.contains("**Strong**"),
            "inline should take priority, got: {value}"
        );
    }
}

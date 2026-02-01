//! Raw block types for phase 2 (block boundary identification).
//!
//! These types store content as byte spans rather than parsed inline nodes,
//! allowing block structure to be identified before inline parsing.

use crate::asg::Location;
use crate::span::SourceSpan;

/// A raw block with unparsed content spans.
///
/// Phase 2 produces `RawBlock` values. Phase 3 transforms them to `Block`
/// by parsing inline content for basic-content-model blocks.
#[derive(Debug, Clone)]
pub(super) struct RawBlock<'src> {
    /// Block name (e.g., `"paragraph"`, `"section"`, `"listing"`).
    pub(super) name: &'static str,
    /// Structural form (e.g., `"delimited"`).
    pub(super) form: Option<&'static str>,
    /// Delimiter string for delimited blocks.
    pub(super) delimiter: Option<&'src str>,
    /// Block or section ID.
    pub(super) id: Option<&'src str>,
    /// Block style (e.g., `"source"`, `"abstract"`).
    pub(super) style: Option<&'src str>,
    /// Roles from block attributes.
    pub(super) roles: Vec<&'src str>,
    /// Options from block attributes.
    pub(super) options: Vec<&'src str>,

    // --- Content fields (exactly one of these is Some based on content model) ---
    /// Title content span (to be parsed as inlines in phase 3).
    pub(super) title_span: Option<SourceSpan>,
    /// Content span for basic/verbatim blocks (to be parsed as inlines in phase 3).
    pub(super) content_span: Option<SourceSpan>,
    /// Child blocks for compound content model.
    pub(super) blocks: Option<Vec<RawBlock<'src>>>,
    /// List items.
    pub(super) items: Option<Vec<RawBlock<'src>>>,
    /// Principal content span for list items.
    pub(super) principal_span: Option<SourceSpan>,
    /// Reftext span for cross-references.
    pub(super) reftext_span: Option<SourceSpan>,

    // --- Type-specific fields ---
    /// Section level (0-5).
    pub(super) level: Option<usize>,
    /// List/break variant.
    pub(super) variant: Option<&'static str>,
    /// List item marker.
    pub(super) marker: Option<&'src str>,

    /// For sections: location of just the heading line (for discrete heading conversion).
    pub(super) heading_line_location: Option<Location>,

    /// Source location.
    pub(super) location: Option<Location>,
}

impl RawBlock<'_> {
    /// Create a new raw block with the given name.
    pub(super) fn new(name: &'static str) -> Self {
        Self {
            name,
            form: None,
            delimiter: None,
            id: None,
            style: None,
            roles: Vec::new(),
            options: Vec::new(),
            title_span: None,
            content_span: None,
            blocks: None,
            items: None,
            principal_span: None,
            reftext_span: None,
            level: None,
            variant: None,
            marker: None,
            heading_line_location: None,
            location: None,
        }
    }
}

/// Pending block metadata parsed from attribute and title lines.
///
/// This is threaded forward through the parser to apply to the next block.
#[derive(Debug, Clone, Default)]
pub(super) struct PendingMetadata<'src> {
    /// Title span from `.Title` line.
    pub(super) title_span: Option<SourceSpan>,
    /// Block ID from `[#id]` or `[[id]]`.
    pub(super) id: Option<&'src str>,
    /// Block style from first positional attribute.
    pub(super) style: Option<&'src str>,
    /// Roles from `.role` shorthand.
    pub(super) roles: Vec<&'src str>,
    /// Options from `%option` shorthand.
    pub(super) options: Vec<&'src str>,
    /// Reftext span from `[[id,reftext]]`.
    pub(super) reftext_span: Option<SourceSpan>,
}

impl<'src> PendingMetadata<'src> {
    /// Returns true if this is a comment block (should be skipped).
    pub(super) fn is_comment(&self) -> bool {
        self.style == Some("comment")
    }

    /// Apply metadata to a raw block, consuming self.
    pub(super) fn apply_to(self, block: &mut RawBlock<'src>) {
        if block.title_span.is_none() {
            block.title_span = self.title_span;
        }
        if block.id.is_none() {
            block.id = self.id;
        }
        if block.style.is_none() {
            block.style = self.style;
        }
        if block.reftext_span.is_none() {
            block.reftext_span = self.reftext_span;
        }
        block.roles.extend(self.roles);
        block.options.extend(self.options);
    }
}

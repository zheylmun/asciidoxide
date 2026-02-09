//! Raw block types for phase 2 (block boundary identification).
//!
//! These types store content as byte spans rather than parsed inline nodes,
//! allowing block structure to be identified before inline parsing.

use smallvec::SmallVec;

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
    pub(super) roles: SmallVec<[&'src str; 2]>,
    /// Options from block attributes.
    pub(super) options: SmallVec<[&'src str; 2]>,
    /// Positional attributes from block attribute line (all comma-separated values).
    pub(super) positionals: SmallVec<[&'src str; 4]>,
    /// Named attributes from block attribute line (key=value pairs).
    pub(super) named_attributes: SmallVec<[(&'src str, &'src str); 4]>,

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
    /// Term spans for description list items (each is parsed as inlines).
    pub(super) term_spans: SmallVec<[SourceSpan; 2]>,
    /// Reftext span for cross-references.
    pub(super) reftext_span: Option<SourceSpan>,

    // --- Type-specific fields ---
    /// Section level (0-5).
    pub(super) level: Option<usize>,
    /// List/break variant.
    pub(super) variant: Option<&'static str>,
    /// List item marker.
    pub(super) marker: Option<&'src str>,

    /// Target for block macros (e.g., `image::target[...]`).
    pub(super) target: Option<&'src str>,

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
            roles: SmallVec::new(),
            options: SmallVec::new(),
            positionals: SmallVec::new(),
            named_attributes: SmallVec::new(),
            title_span: None,
            content_span: None,
            blocks: None,
            items: None,
            principal_span: None,
            term_spans: SmallVec::new(),
            reftext_span: None,
            level: None,
            variant: None,
            marker: None,
            target: None,
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
    pub(super) roles: SmallVec<[&'src str; 2]>,
    /// Options from `%option` shorthand.
    pub(super) options: SmallVec<[&'src str; 2]>,
    /// Positional attributes (all comma-separated values).
    pub(super) positionals: SmallVec<[&'src str; 4]>,
    /// Named attributes (key=value pairs).
    pub(super) named_attributes: SmallVec<[(&'src str, &'src str); 4]>,
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
        if block.positionals.is_empty() {
            block.positionals = self.positionals;
        }
        if block.named_attributes.is_empty() {
            block.named_attributes = self.named_attributes;
        } else {
            block.named_attributes.extend(self.named_attributes);
        }
    }
}

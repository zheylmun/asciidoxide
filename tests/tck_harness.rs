//! Integration test that runs the official TCK harness against our parser.
//!
//! This test has two modes:
//! 1. Normal mode: Runs the Node.js TCK harness, pointing to itself as the adapter
//! 2. Adapter mode: When `TCK_ADAPTER_MODE=1` is set, acts as the CLI adapter
//!
//! The harness invokes this same binary with the env var set to get parser output.

use asciidoxide::asg;
use asciidoxide::{parse_document, parse_inline};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::path::Path;
use std::process::Command;

fn main() {
    // Check if we're in adapter mode
    if std::env::var("TCK_ADAPTER_MODE").is_ok() {
        run_adapter();
    } else {
        run_harness();
    }
}

// =============================================================================
// Harness runner mode
// =============================================================================

fn run_harness() {
    let exe_path = std::env::current_exe().expect("Failed to get current executable path");
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let tck_dir = Path::new(manifest_dir).join("language_repositories/asciidoc_tck");

    // Build the adapter command that sets the env var and runs this binary
    // We use a shell wrapper to set the environment variable
    let adapter_command = format!("TCK_ADAPTER_MODE=1 {}", exe_path.display());

    // Run the TCK harness
    let output = Command::new("node")
        .args([
            "harness/bin/asciidoc-tck.js",
            "cli",
            "--adapter-command",
            &adapter_command,
        ])
        .current_dir(&tck_dir)
        .output()
        .expect("Failed to run TCK harness - is Node.js installed?");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    // Print output
    if !stdout.is_empty() {
        println!("{stdout}");
    }
    if !stderr.is_empty() {
        eprintln!("{stderr}");
    }

    if !output.status.success() {
        std::process::exit(1);
    }
}

// =============================================================================
// Adapter mode - reads JSON from stdin, outputs ASG JSON to stdout
// =============================================================================

fn run_adapter() {
    if let Err(e) = adapter_main() {
        eprintln!("Adapter error: {e}");
        std::process::exit(1);
    }
}

fn adapter_main() -> Result<(), Box<dyn std::error::Error>> {
    // Read JSON from stdin
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;

    let request: TckRequest = serde_json::from_str(&input)?;

    // Parse based on type
    let output = match request.parse_type.as_str() {
        "block" => {
            let (doc, _diagnostics) = parse_document(&request.contents);
            let json_doc = JsonDocument::from(&doc);
            serde_json::to_string(&json_doc)?
        }
        "inline" => {
            let (inlines, _diagnostics) = parse_inline(&request.contents);
            let json_inlines: Vec<_> = inlines.iter().map(JsonInlineNode::from).collect();
            serde_json::to_string(&json_inlines)?
        }
        other => {
            return Err(format!("Unknown parse type: {other}").into());
        }
    };

    // Write JSON to stdout
    io::stdout().write_all(output.as_bytes())?;
    io::stdout().flush()?;

    Ok(())
}

// =============================================================================
// TCK Protocol types
// =============================================================================

#[derive(Deserialize)]
struct TckRequest {
    contents: String,
    #[allow(dead_code)]
    path: String,
    #[serde(rename = "type")]
    parse_type: String,
}

// =============================================================================
// JSON serialization types (mirror of ASG with TCK-required fields)
// =============================================================================

#[derive(Serialize)]
struct JsonDocument<'a> {
    name: &'static str,
    #[serde(rename = "type")]
    node_type: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    attributes: Option<HashMap<String, String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    header: Option<JsonHeader<'a>>,
    blocks: Vec<JsonBlock<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<JsonLocation>,
}

impl<'a> From<&asg::Document<'a>> for JsonDocument<'a> {
    fn from(doc: &asg::Document<'a>) -> Self {
        Self {
            name: "document",
            node_type: "block",
            attributes: doc.attributes.as_ref().map(|attrs| {
                attrs
                    .iter()
                    .map(|(k, v)| ((*k).to_string(), v.resolve().into_owned()))
                    .collect()
            }),
            header: doc.header.as_ref().map(JsonHeader::from),
            blocks: doc.blocks.iter().map(JsonBlock::from).collect(),
            location: doc.location.as_ref().map(convert_location),
        }
    }
}

#[derive(Serialize)]
struct JsonAuthor<'a> {
    fullname: &'a str,
    initials: String,
    firstname: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    middlename: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    lastname: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    address: Option<&'a str>,
}

impl<'a> From<&asg::Author<'a>> for JsonAuthor<'a> {
    fn from(a: &asg::Author<'a>) -> Self {
        Self {
            fullname: a.fullname,
            initials: a.initials.clone(),
            firstname: a.firstname,
            middlename: a.middlename,
            lastname: a.lastname,
            address: a.address,
        }
    }
}

#[derive(Serialize)]
struct JsonHeader<'a> {
    title: Vec<JsonInlineNode<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    authors: Option<Vec<JsonAuthor<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<JsonLocation>,
}

impl<'a> From<&asg::Header<'a>> for JsonHeader<'a> {
    fn from(h: &asg::Header<'a>) -> Self {
        Self {
            title: h.title.iter().map(JsonInlineNode::from).collect(),
            authors: h
                .authors
                .as_ref()
                .map(|a| a.iter().map(JsonAuthor::from).collect()),
            location: h.location.as_ref().map(convert_location),
        }
    }
}

#[derive(Serialize)]
struct JsonBlockMetadata<'a> {
    #[serde(skip_serializing_if = "Vec::is_empty")]
    roles: Vec<&'a str>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    options: Vec<&'a str>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    attributes: HashMap<&'a str, &'a str>,
}

impl<'a> From<&asg::BlockMetadata<'a>> for JsonBlockMetadata<'a> {
    fn from(m: &asg::BlockMetadata<'a>) -> Self {
        Self {
            roles: m.roles.to_vec(),
            options: m.options.to_vec(),
            attributes: m.attributes.iter().copied().collect(),
        }
    }
}

#[derive(Serialize)]
struct JsonBlock<'a> {
    name: &'static str,
    #[serde(rename = "type")]
    node_type: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    form: Option<&'static str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    delimiter: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    target: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    style: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    reftext: Option<Vec<JsonInlineNode<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    metadata: Option<JsonBlockMetadata<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    title: Option<Vec<JsonInlineNode<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    level: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    variant: Option<&'static str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    marker: Option<&'a str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    inlines: Option<Vec<JsonInlineNode<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    blocks: Option<Vec<JsonBlock<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    items: Option<Vec<JsonBlock<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    principal: Option<Vec<JsonInlineNode<'a>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    terms: Option<Vec<Vec<JsonInlineNode<'a>>>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<JsonLocation>,
}

impl<'a> From<&asg::Block<'a>> for JsonBlock<'a> {
    fn from(b: &asg::Block<'a>) -> Self {
        Self {
            name: b.name,
            node_type: "block",
            form: b.form,
            delimiter: b.delimiter,
            id: b.id.as_ref().map(ToString::to_string),
            target: b.target,
            style: b.style,
            reftext: b
                .reftext
                .as_ref()
                .map(|v| v.iter().map(JsonInlineNode::from).collect()),
            metadata: b.metadata.as_ref().map(JsonBlockMetadata::from),
            title: b
                .title
                .as_ref()
                .map(|v| v.iter().map(JsonInlineNode::from).collect()),
            level: b.level,
            variant: b.variant,
            marker: b.marker,
            inlines: b
                .inlines
                .as_ref()
                .map(|v| v.iter().map(JsonInlineNode::from).collect()),
            blocks: b
                .blocks
                .as_ref()
                .map(|v| v.iter().map(JsonBlock::from).collect()),
            items: b
                .items
                .as_ref()
                .map(|v| v.iter().map(JsonBlock::from).collect()),
            principal: b
                .principal
                .as_ref()
                .map(|v| v.iter().map(JsonInlineNode::from).collect()),
            terms: b.terms.as_ref().map(|term_groups| {
                term_groups
                    .iter()
                    .map(|group| group.iter().map(JsonInlineNode::from).collect())
                    .collect()
            }),
            location: b.location.as_ref().map(convert_location),
        }
    }
}

#[derive(Serialize)]
#[serde(untagged)]
enum JsonInlineNode<'a> {
    Text(JsonTextNode<'a>),
    Span(JsonSpanNode<'a>),
    Ref(JsonRefNode<'a>),
    Raw(JsonRawNode<'a>),
}

impl<'a> From<&asg::InlineNode<'a>> for JsonInlineNode<'a> {
    fn from(node: &asg::InlineNode<'a>) -> Self {
        match node {
            asg::InlineNode::Text(t) => Self::Text(JsonTextNode::from(t)),
            asg::InlineNode::Span(s) => Self::Span(JsonSpanNode::from(s)),
            asg::InlineNode::Ref(r) => Self::Ref(JsonRefNode::from(r)),
            asg::InlineNode::Raw(r) => Self::Raw(JsonRawNode::from(r)),
        }
    }
}

#[derive(Serialize)]
struct JsonTextNode<'a> {
    name: &'static str,
    #[serde(rename = "type")]
    node_type: &'static str,
    value: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<JsonLocation>,
}

impl<'a> From<&asg::TextNode<'a>> for JsonTextNode<'a> {
    fn from(t: &asg::TextNode<'a>) -> Self {
        Self {
            name: "text",
            node_type: "string",
            value: t.value,
            location: t.location.as_ref().map(convert_location),
        }
    }
}

#[derive(Serialize)]
struct JsonSpanNode<'a> {
    name: &'static str,
    #[serde(rename = "type")]
    node_type: &'static str,
    variant: &'static str,
    form: &'static str,
    inlines: Vec<JsonInlineNode<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<JsonLocation>,
}

impl<'a> From<&asg::SpanNode<'a>> for JsonSpanNode<'a> {
    fn from(s: &asg::SpanNode<'a>) -> Self {
        Self {
            name: "span",
            node_type: "inline",
            variant: s.variant,
            form: s.form,
            inlines: s.inlines.iter().map(JsonInlineNode::from).collect(),
            location: s.location.as_ref().map(convert_location),
        }
    }
}

#[derive(Serialize)]
struct JsonRefNode<'a> {
    name: &'static str,
    #[serde(rename = "type")]
    node_type: &'static str,
    variant: &'static str,
    target: &'a str,
    inlines: Vec<JsonInlineNode<'a>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<JsonLocation>,
}

impl<'a> From<&asg::RefNode<'a>> for JsonRefNode<'a> {
    fn from(r: &asg::RefNode<'a>) -> Self {
        Self {
            name: "ref",
            node_type: "inline",
            variant: r.variant,
            target: r.target,
            inlines: r.inlines.iter().map(JsonInlineNode::from).collect(),
            location: r.location.as_ref().map(convert_location),
        }
    }
}

#[derive(Serialize)]
struct JsonRawNode<'a> {
    name: &'static str,
    #[serde(rename = "type")]
    node_type: &'static str,
    value: &'a str,
    #[serde(skip_serializing_if = "Option::is_none")]
    location: Option<JsonLocation>,
}

impl<'a> From<&asg::RawNode<'a>> for JsonRawNode<'a> {
    fn from(r: &asg::RawNode<'a>) -> Self {
        Self {
            name: "raw",
            node_type: "string",
            value: r.value,
            location: r.location.as_ref().map(convert_location),
        }
    }
}

#[derive(Serialize)]
struct JsonPosition {
    line: usize,
    col: usize,
}

type JsonLocation = [JsonPosition; 2];

impl From<&asg::Position> for JsonPosition {
    fn from(p: &asg::Position) -> Self {
        Self {
            line: p.line,
            col: p.col,
        }
    }
}

fn convert_location(loc: &asg::Location) -> JsonLocation {
    [JsonPosition::from(&loc[0]), JsonPosition::from(&loc[1])]
}

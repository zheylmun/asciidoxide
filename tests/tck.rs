//! Integration tests that validate parser output against TCK fixtures.
//!
//! Reads test cases from `language_repositories/asciidoc_tck/tests/` and compares
//! the parser's ASG output against expected JSON, replicating the logic of the
//! TCK's JavaScript test harness.
//!
//! Serializable mirror types (`Json*`) convert from the library's zero-copy ASG
//! types via `From` impls, injecting the constant `name`/`type` fields that the
//! TCK JSON format requires.

use asciidoxide::asg;
use asciidoxide::{parse_document, parse_inline};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

// --- JSON mirror types ---

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

// --- Config ---

#[derive(Default, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TckConfig {
    name: Option<String>,
    #[serde(default)]
    trim_trailing_whitespace: bool,
    #[serde(default)]
    ensure_trailing_newline: bool,
    #[serde(default)]
    skip: bool,
}

// --- Fixture ---

enum TckCategory {
    Block,
    Inline,
}

struct TckFixture {
    name: String,
    relative_path: String,
    category: TckCategory,
    input: String,
    expected: Value,
    expected_without_locations: Value,
    skip: bool,
}

// --- Discovery ---

fn tck_tests_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("language_repositories/asciidoc_tck/tests")
}

fn discover_fixtures() -> Vec<TckFixture> {
    let base = tck_tests_dir();
    let mut fixtures = Vec::new();
    discover_recursive(&base, &base, &mut fixtures);
    fixtures
}

fn discover_recursive(dir: &Path, base: &Path, fixtures: &mut Vec<TckFixture>) {
    let mut entries: Vec<_> = fs::read_dir(dir)
        .unwrap_or_else(|e| panic!("failed to read TCK test directory {}: {e}", dir.display()))
        .filter_map(Result::ok)
        .collect();
    entries.sort_by_key(fs::DirEntry::file_name);

    // Process files before directories (matches JS harness behavior)
    let (files, dirs): (Vec<_>, Vec<_>) = entries
        .iter()
        .partition(|e| e.file_type().map(|ft| ft.is_file()).unwrap_or(false));

    for entry in &files {
        let path = entry.path();
        if let Some(name) = path.file_name().and_then(|n| n.to_str())
            && name.ends_with("-input.adoc")
        {
            fixtures.push(load_fixture(&path, base));
        }
    }

    for entry in &dirs {
        let path = entry.path();
        if path.is_dir() {
            discover_recursive(&path, base, fixtures);
        }
    }
}

fn load_fixture(input_path: &Path, base: &Path) -> TckFixture {
    let stem = input_path.file_name().unwrap().to_str().unwrap();
    let basename = &stem[..stem.len() - "-input.adoc".len()];
    let dir = input_path.parent().unwrap();

    let output_path = dir.join(format!("{basename}-output.json"));
    let config_path = dir.join(format!("{basename}-config.json"));

    // Determine category from relative path
    let rel = input_path.strip_prefix(base).unwrap();
    let category = if rel.starts_with("inline") {
        TckCategory::Inline
    } else {
        TckCategory::Block
    };

    // Load config
    let config: TckConfig = if config_path.exists() {
        let data = fs::read_to_string(&config_path).unwrap();
        serde_json::from_str(&data).unwrap()
    } else {
        TckConfig::default()
    };

    // Load and preprocess input
    let raw_input = fs::read_to_string(input_path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", input_path.display()));
    let input = preprocess_input(&raw_input, &config);

    // Load expected output (with and without locations)
    let expected_str = fs::read_to_string(&output_path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", output_path.display()));
    let expected: Value = serde_json::from_str(&expected_str).unwrap();
    let mut expected_without_locations = expected.clone();
    strip_locations(&mut expected_without_locations);

    let name = config.name.unwrap_or_else(|| basename.replace('-', " "));
    let relative_path = rel.to_string_lossy().to_string();

    TckFixture {
        name,
        relative_path,
        category,
        input,
        expected,
        expected_without_locations,
        skip: config.skip,
    }
}

// --- Preprocessing ---

fn preprocess_input(input: &str, config: &TckConfig) -> String {
    if config.trim_trailing_whitespace {
        input.trim_end().to_string()
    } else if config.ensure_trailing_newline {
        if input.ends_with('\n') {
            input.to_string()
        } else {
            format!("{input}\n")
        }
    } else {
        // Default: strip trailing newline
        input.strip_suffix('\n').unwrap_or(input).to_string()
    }
}

// --- Location stripping ---

fn strip_locations(value: &mut Value) {
    match value {
        Value::Object(map) => {
            map.remove("location");
            for v in map.values_mut() {
                strip_locations(v);
            }
        }
        Value::Array(arr) => {
            for v in arr.iter_mut() {
                strip_locations(v);
            }
        }
        _ => {}
    }
}

// --- ASG defaults population (replicates test-framework.js populateASGDefaults) ---

fn populate_asg_defaults(node: &mut Value) {
    let Some(obj) = node.as_object_mut() else {
        return;
    };

    // Only process block nodes
    if obj.get("type").and_then(|v| v.as_str()) != Some("block") {
        return;
    }

    // Populate metadata defaults
    if let Some(metadata) = obj.get_mut("metadata").and_then(|v| v.as_object_mut()) {
        metadata
            .entry("attributes")
            .or_insert_with(|| serde_json::json!({}));
        metadata
            .entry("options")
            .or_insert_with(|| serde_json::json!([]));
    }

    // Skip for macros, breaks, headings, and list items
    let form = obj
        .get("form")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let name = obj
        .get("name")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();

    if form == "macro" || name == "break" || name == "heading" || name == "listItem" {
        return;
    }

    // Leaf blocks: default inlines to []
    if matches!(
        name.as_str(),
        "listing" | "literal" | "pass" | "stem" | "paragraph" | "verse"
    ) {
        obj.entry("inlines")
            .or_insert_with(|| serde_json::json!([]));
    }
    // List types: recurse into items
    else if matches!(name.as_str(), "list" | "dlist") {
        if let Some(items) = obj.get_mut("items").and_then(|v| v.as_array_mut()) {
            for item in items.iter_mut() {
                populate_asg_defaults(item);
            }
        }
    }
    // Container blocks: default blocks to [], recurse
    else {
        let blocks = obj.entry("blocks").or_insert_with(|| serde_json::json!([]));
        if let Some(arr) = blocks.as_array_mut() {
            for block in arr.iter_mut() {
                populate_asg_defaults(block);
            }
        }
    }
}

// --- Not-yet-implemented features ---

/// Test fixtures that exercise features the parser does not yet support.
/// Remove entries from this list as the parser gains capabilities.
const UNSUPPORTED: &[&str] = &[];

fn is_unsupported(path: &str) -> bool {
    UNSUPPORTED.iter().any(|prefix| path.contains(prefix))
}

// --- Superset comparison ---

/// Check that `actual` is a superset of `expected` — i.e., `actual` contains
/// all fields and values in `expected`, but may have additional fields.
/// This allows the parser to produce extra information (auto-generated IDs,
/// attributes, etc.) without breaking fixtures that don't test for them.
/// Returns true if the value is an empty default (empty object or empty array).
fn is_empty_default(value: &Value) -> bool {
    match value {
        Value::Object(obj) => obj.is_empty(),
        Value::Array(arr) => arr.is_empty(),
        _ => false,
    }
}

fn is_superset(actual: &Value, expected: &Value) -> bool {
    match (actual, expected) {
        (Value::Object(actual_obj), Value::Object(expected_obj)) => {
            // Every key in expected must exist in actual and match.
            // Exception: if expected value is an empty object or array and
            // actual doesn't have the key, treat as matching (optional defaults).
            expected_obj.iter().all(|(key, expected_val)| {
                if let Some(actual_val) = actual_obj.get(key) {
                    is_superset(actual_val, expected_val)
                } else {
                    is_empty_default(expected_val)
                }
            })
        }
        (Value::Array(actual_arr), Value::Array(expected_arr)) => {
            // Arrays must have same length and each element must match
            actual_arr.len() == expected_arr.len()
                && actual_arr
                    .iter()
                    .zip(expected_arr.iter())
                    .all(|(a, e)| is_superset(a, e))
        }
        _ => actual == expected,
    }
}

/// Strips extra fields from `actual` that are not in `expected` for display.
fn strip_extra_fields(actual: &Value, expected: &Value) -> Value {
    match (actual, expected) {
        (Value::Object(actual_obj), Value::Object(expected_obj)) => {
            let mut filtered = serde_json::Map::new();
            for (key, actual_val) in actual_obj {
                if let Some(expected_val) = expected_obj.get(key) {
                    filtered.insert(key.clone(), strip_extra_fields(actual_val, expected_val));
                }
            }
            // Also include any keys missing from actual that expected has
            for (key, _) in expected_obj {
                if !actual_obj.contains_key(key) {
                    filtered.insert(key.clone(), Value::Null);
                }
            }
            Value::Object(filtered)
        }
        (Value::Array(actual_arr), Value::Array(expected_arr)) => Value::Array(
            actual_arr
                .iter()
                .zip(expected_arr.iter())
                .map(|(a, e)| strip_extra_fields(a, e))
                .collect(),
        ),
        _ => actual.clone(),
    }
}

// --- Test runners ---

fn run_block_fixture(fixture: &TckFixture) -> Result<(), String> {
    let (document, _diagnostics) = parse_document(&fixture.input);
    let json_doc = JsonDocument::from(&document);
    let mut actual = serde_json::to_value(&json_doc).map_err(|e| e.to_string())?;

    let has_locations = actual.get("location").is_some();
    let mut expected = if has_locations {
        fixture.expected.clone()
    } else {
        fixture.expected_without_locations.clone()
    };

    populate_asg_defaults(&mut actual);
    populate_asg_defaults(&mut expected);

    if is_superset(&actual, &expected) {
        Ok(())
    } else {
        // Show only the fields expected cares about for clearer diffs
        let filtered_actual = strip_extra_fields(&actual, &expected);
        Err(format!(
            "[{}] {}\nExpected:\n{}\nActual (filtered to expected fields):\n{}",
            fixture.relative_path,
            fixture.name,
            serde_json::to_string_pretty(&expected).unwrap(),
            serde_json::to_string_pretty(&filtered_actual).unwrap(),
        ))
    }
}

fn run_inline_fixture(fixture: &TckFixture) -> Result<(), String> {
    let (inlines, _diagnostics) = parse_inline(&fixture.input);
    let json_inlines: Vec<_> = inlines.iter().map(JsonInlineNode::from).collect();
    let actual = serde_json::to_value(&json_inlines).map_err(|e| e.to_string())?;

    let has_locations = actual
        .as_array()
        .and_then(|arr| arr.first())
        .and_then(|node| node.get("location"))
        .is_some();
    let expected = if has_locations {
        &fixture.expected
    } else {
        &fixture.expected_without_locations
    };

    if actual == *expected {
        Ok(())
    } else {
        Err(format!(
            "[{}] {}\nExpected:\n{}\nActual:\n{}",
            fixture.relative_path,
            fixture.name,
            serde_json::to_string_pretty(expected).unwrap(),
            serde_json::to_string_pretty(&actual).unwrap(),
        ))
    }
}

// --- Test entry points ---

#[test]
fn tck_block_tests() {
    let fixtures = discover_fixtures();
    let mut failures = Vec::new();
    for fixture in &fixtures {
        if fixture.skip
            || !matches!(fixture.category, TckCategory::Block)
            || is_unsupported(&fixture.relative_path)
        {
            continue;
        }
        if let Err(e) = run_block_fixture(fixture) {
            failures.push(e);
        }
    }
    assert!(failures.is_empty(), "\n{}", failures.join("\n\n"));
}

#[test]
fn tck_inline_tests() {
    let fixtures = discover_fixtures();
    let mut failures = Vec::new();
    for fixture in &fixtures {
        if fixture.skip
            || !matches!(fixture.category, TckCategory::Inline)
            || is_unsupported(&fixture.relative_path)
        {
            continue;
        }
        if let Err(e) = run_inline_fixture(fixture) {
            failures.push(e);
        }
    }
    assert!(failures.is_empty(), "\n{}", failures.join("\n\n"));
}

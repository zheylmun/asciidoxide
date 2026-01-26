//! Integration tests that validate parser output against TCK fixtures.
//!
//! Reads test cases from `language_repositories/asciidoc_tck/tests/` and compares
//! the parser's ASG output against expected JSON, replicating the logic of the
//! TCK's JavaScript test harness.

use asciidoc::{parse_document, parse_inline};
use serde::Deserialize;
use serde_json::Value;
use std::fs;
use std::path::{Path, PathBuf};

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
        .filter_map(|e| e.ok())
        .collect();
    entries.sort_by_key(|e| e.file_name());

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
    let obj = match node.as_object_mut() {
        Some(o) => o,
        None => return,
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

    // Skip for macros, breaks, and headings
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

    if form == "macro" || name == "break" || name == "heading" {
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

// --- Test runners ---

fn run_block_fixture(fixture: &TckFixture) -> Result<(), String> {
    let document = parse_document(&fixture.input);
    let mut actual = serde_json::to_value(&document).map_err(|e| e.to_string())?;

    let has_locations = actual.get("location").is_some();
    let mut expected = if has_locations {
        fixture.expected.clone()
    } else {
        fixture.expected_without_locations.clone()
    };

    populate_asg_defaults(&mut actual);
    populate_asg_defaults(&mut expected);

    if actual != expected {
        Err(format!(
            "[{}] {}\nExpected:\n{}\nActual:\n{}",
            fixture.relative_path,
            fixture.name,
            serde_json::to_string_pretty(&expected).unwrap(),
            serde_json::to_string_pretty(&actual).unwrap(),
        ))
    } else {
        Ok(())
    }
}

fn run_inline_fixture(fixture: &TckFixture) -> Result<(), String> {
    let inlines = parse_inline(&fixture.input);
    let actual = serde_json::to_value(&inlines).map_err(|e| e.to_string())?;

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

    if actual != *expected {
        Err(format!(
            "[{}] {}\nExpected:\n{}\nActual:\n{}",
            fixture.relative_path,
            fixture.name,
            serde_json::to_string_pretty(expected).unwrap(),
            serde_json::to_string_pretty(&actual).unwrap(),
        ))
    } else {
        Ok(())
    }
}

// --- Test entry points ---

#[test]
fn tck_block_tests() {
    let fixtures = discover_fixtures();
    let mut failures = Vec::new();
    for fixture in &fixtures {
        if fixture.skip || !matches!(fixture.category, TckCategory::Block) {
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
        if fixture.skip || !matches!(fixture.category, TckCategory::Inline) {
            continue;
        }
        if let Err(e) = run_inline_fixture(fixture) {
            failures.push(e);
        }
    }
    assert!(failures.is_empty(), "\n{}", failures.join("\n\n"));
}

//! Performance benchmarks for the `AsciiDoc` parser.
//!
//! Benchmarks inline parsing, block parsing, and batch processing of TCK files.

use asciidoxide::{parse_document, parse_inline};
use criterion::{BenchmarkId, Criterion, Throughput, black_box, criterion_group, criterion_main};
use std::fs;
use std::path::Path;

/// Recursively collect .adoc files from a directory.
fn collect_adoc_files(dir: &Path, inputs: &mut Vec<(String, String)>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.filter_map(Result::ok) {
            let path = entry.path();
            if path.is_dir() {
                collect_adoc_files(&path, inputs);
            } else if path.extension().is_some_and(|ext| ext == "adoc") {
                let name = path.file_stem().unwrap().to_string_lossy().to_string();
                if let Ok(content) = fs::read_to_string(&path) {
                    inputs.push((name, content));
                }
            }
        }
    }
}

/// Load TCK input files for batch benchmarking.
fn load_tck_inputs() -> Vec<(String, String)> {
    let tck_tests_dir =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("language_repositories/asciidoc_tck/tests");

    let mut inputs = Vec::new();
    collect_adoc_files(&tck_tests_dir, &mut inputs);
    inputs
}

/// Benchmark parsing many small TCK files in sequence.
fn bench_tck_batch(c: &mut Criterion) {
    let tck_inputs = load_tck_inputs();

    if tck_inputs.is_empty() {
        eprintln!("Warning: No TCK inputs found. Run `git submodule update --init --recursive`");
        return;
    }

    let total_bytes: usize = tck_inputs.iter().map(|(_, c)| c.len()).sum();

    let mut group = c.benchmark_group("tck_batch");
    group.throughput(Throughput::Bytes(total_bytes as u64));

    group.bench_function(
        BenchmarkId::new("parse_document", format!("{}_files", tck_inputs.len())),
        |b| {
            b.iter(|| {
                for (_, content) in &tck_inputs {
                    let _ = parse_document(black_box(content));
                }
            });
        },
    );

    group.bench_function(
        BenchmarkId::new("parse_inline", format!("{}_files", tck_inputs.len())),
        |b| {
            b.iter(|| {
                for (_, content) in &tck_inputs {
                    let _ = parse_inline(black_box(content));
                }
            });
        },
    );

    group.finish();
}

/// Benchmark inline parsing with various complexity levels.
fn bench_inline_complexity(c: &mut Criterion) {
    let long_100 = "word ".repeat(100);
    let long_1000 = "word ".repeat(1000);

    let inputs: Vec<(&str, &str)> = vec![
        ("plain_text", "This is plain text without any formatting."),
        ("single_strong", "This has *bold* text."),
        (
            "nested_spans",
            "This has *bold with _emphasis_ inside* text.",
        ),
        (
            "deeply_nested",
            "This has *bold with _emphasis with `code` inside_ inside* text.",
        ),
        (
            "multiple_spans",
            "This has *bold* and _emphasis_ and `code` and #mark# all together.",
        ),
        ("long_text_100", &long_100),
        ("long_text_1000", &long_1000),
    ];

    let mut group = c.benchmark_group("inline_complexity");

    for (name, content) in inputs {
        group.throughput(Throughput::Bytes(content.len() as u64));
        group.bench_with_input(BenchmarkId::new("parse_inline", name), &content, |b, c| {
            b.iter(|| parse_inline(black_box(c)));
        });
    }

    group.finish();
}

/// Benchmark block parsing with various structures.
fn bench_block_structures(c: &mut Criterion) {
    let paragraph = "This is a simple paragraph.\n";
    let section = "= Title\n\nParagraph content.\n";
    let listing = "----\ncode block\n----\n";
    let nested_list = "* Item 1\n** Nested 1\n** Nested 2\n* Item 2\n";
    let complex_doc = r"= Document Title

== Section One

This is the first paragraph.

.Listing Title
----
code example
----

== Section Two

* List item 1
* List item 2
** Nested item

****
Sidebar content
****
";

    let inputs = [
        ("single_paragraph", paragraph),
        ("section_with_paragraph", section),
        ("listing_block", listing),
        ("nested_list", nested_list),
        ("complex_document", complex_doc),
    ];

    let mut group = c.benchmark_group("block_structures");

    for (name, content) in inputs {
        group.throughput(Throughput::Bytes(content.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("parse_document", name),
            &content,
            |b, c| {
                b.iter(|| parse_document(black_box(c)));
            },
        );
    }

    group.finish();
}

/// Benchmark scaling behavior with increasing document size.
fn bench_scaling(c: &mut Criterion) {
    let base_paragraph = "This is a paragraph with *bold* and _emphasis_ text.\n\n";

    let mut group = c.benchmark_group("scaling");

    for size in [10, 50, 100, 500] {
        let content = base_paragraph.repeat(size);
        group.throughput(Throughput::Bytes(content.len() as u64));
        group.bench_with_input(BenchmarkId::new("paragraphs", size), &content, |b, c| {
            b.iter(|| parse_document(black_box(c)));
        });
    }

    group.finish();
}

/// Benchmark worst-case inline parsing patterns.
fn bench_inline_edge_cases(c: &mut Criterion) {
    // Patterns that might cause backtracking
    let unclosed_strong = "*not closed ".repeat(10);
    let mixed_unclosed = "*a _b `c #d ".repeat(10);
    let many_escapes = r"\*not\* \*bold\* ".repeat(50);
    let alternating = "*a* b *c* d *e* f ".repeat(50);

    let inputs: Vec<(&str, String)> = vec![
        ("unclosed_strong_10x", unclosed_strong),
        ("mixed_unclosed_10x", mixed_unclosed),
        ("many_escapes_50x", many_escapes),
        ("alternating_spans_50x", alternating),
    ];

    let mut group = c.benchmark_group("inline_edge_cases");

    for (name, content) in &inputs {
        group.throughput(Throughput::Bytes(content.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("parse_inline", name),
            content.as_str(),
            |b, c| {
                b.iter(|| parse_inline(black_box(c)));
            },
        );
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_tck_batch,
    bench_inline_complexity,
    bench_block_structures,
    bench_scaling,
    bench_inline_edge_cases,
);

criterion_main!(benches);

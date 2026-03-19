# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

```bash
cargo build                                        # Build workspace
cargo test                                         # All tests (178 unit + 2 TCK + 149 harness + 2 doc)
cargo test --lib                                   # Unit tests only (fast)
cargo test -p asciidoxide-parser doc_single        # Single test by name
cargo fmt --check                                  # Check formatting (must pass)
cargo clippy --all-targets                         # Pedantic lints via workspace config (must be clean)
```

TCK harness tests require Node.js 18+ and submodules: `git submodule update --init --recursive`

## Architecture

**asciidoxide** is a workspace for AsciiDoc tooling in Rust.

- **`crates/parser/`** (`asciidoxide-parser`): Zero-copy, spec-compliant AsciiDoc parser producing an Abstract Semantic Graph (ASG).
- **`crates/lsp/`** (`asciidoxide-lsp`): Language Server Protocol server built on the parser. Provides real-time diagnostics via `tower-lsp`. Binary: `asciidoxide-lsp`.
- **`editors/zed/`**: Zed editor extension (WASM cdylib, workspace member but excluded from `default-members`). Discovers `asciidoxide-lsp` in PATH. Build with `cargo build -p asciidoc-zed-lsp --target wasm32-wasip1`.

### Three-Phase Parsing Pipeline

1. **Preprocess** (`src/preprocess/`): Evaluates conditional directives (`ifdef`, `ifndef`, `ifeval`) against document attributes. Returns `Cow<str>` (zero-copy when no directives found).

2. **Block Boundary Detection** (`src/parser/block/boundary/`): Chumsky combinators identify block structure from the token stream, producing `RawBlock` intermediates with byte-offset spans.

3. **Transform + Inline Parsing** (`src/parser/block/transform.rs`, `src/parser/inline/`): Converts `RawBlock` → ASG `Block`, parsing inline content (formatting spans, macros, passthroughs) recursively.

**Public API** (`src/lib.rs`):
- `parse_document(input) -> (Document, Vec<ParseDiagnostic>)`
- `parse_inline(input) -> (Vec<InlineNode>, Vec<ParseDiagnostic>)`

### Span and Location Tracking

- `SourceSpan`: Lightweight `Copy` byte-offset pair (start, end-exclusive) implementing chumsky's `Span` trait.
- `SourceIndex`: Pre-computed line-start offsets; converts spans to 1-based `Location` (line/col). End positions are **inclusive** (TCK convention).
- `lex_with_offset(input, base_offset)`: Shifts token spans by `base_offset` so nested parsing produces global positions directly—avoids per-substring `SourceIndex` allocation and recursive offset traversal.

### Key Patterns

- **Zero-copy**: ASG borrows `&str` slices from source. `Cow<str>` for IDs (borrowed when explicit, owned when auto-generated).
- **SmallVec**: Used in `BlockMetadata` for roles/options/attributes (typically 0–3 items).
- **`merge_text_nodes`**: Relies on pointer contiguity (`source.as_ptr()`)—all text slices must originate from the same source string.
- **Infallible lexer**: Every byte maps to a token; disambiguation deferred to parser.
- **Inline macro preprocessing**: `find_inline_macros()` converts macros to `Token::Placeholder(index)` before parsing; the inline parser matches placeholders by index.

## Testing

- **Unit tests** (178): In-module `#[cfg(test)]` blocks covering lexer, parser, spans, preprocessing.
- **TCK integration** (`tests/tck.rs`): Loads fixtures from `language_repositories/asciidoc_tck/tests/`, compares serialized ASG JSON via superset matching.
- **TCK harness** (`tests/tck_harness.rs`): Runs official Node.js harness; validates exact ASG structure and location values. Dual-mode binary (harness runner + adapter via `TCK_ADAPTER_MODE=1`).
- **Benchmarks** (`benches/parser.rs`): Criterion + CodSpeed for inline complexity, block structures, scaling, and TCK batch throughput.

## Code Quality

- `#![deny(missing_docs, unsafe_code)]` enforced on the library.
- `cargo clippy --all-targets` must be clean (pedantic lints configured in workspace `Cargo.toml`).
- `cargo fmt --check` must pass.

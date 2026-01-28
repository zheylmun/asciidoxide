# asciidoc

High performance [AsciiDoc](https://asciidoc.org) lexer and parser library written in safe Rust.
The project aims to provide a zero-copy, spec-compliant `AsciiDoc` parser.
The library uses [chumsky](https://github.com/zesterer/chumsky) for lexing and parsing,
with errors provided by [ariadne](https://github.com/zesterer/ariadne).
No additional features beyond lexing and parsing are planned for this crate.

## Building

```bash
cargo build
```

## Testing

This project has two test suites:

1. **Rust test suite** — Unit tests and integration tests using local TCK fixtures
2. **TCK harness validation** — Integration with the official Eclipse `AsciiDoc` TCK harness

### Rust Test Suite

The Rust test suite includes unit tests for the lexer, parser, and span utilities, plus integration tests that validate parser output against TCK fixture files.

```bash
# Run all tests (unit + integration)
cargo test

# Run only unit tests (faster)
cargo test --lib

# Run a specific test by name
cargo test doc_single_paragraph

# Run tests with output shown
cargo test -- --nocapture
```

The integration tests in `tests/tck.rs` load fixture pairs from `language_repositories/asciidoc_tck/tests/` and compare the parser's ASG output against expected JSON. Features not yet implemented are listed in the `UNSUPPORTED` array and skipped during testing.

### TCK Harness Validation (Eclipse Foundation)

The [`AsciiDoc` Technology Compatibility Kit (TCK)](https://github.com/asciidoc/asciidoc-tck) is an official test suite maintained by the Eclipse Foundation to verify parser compliance with the `AsciiDoc` Language specification.

This project includes integration with the TCK's Node.js harness, which invokes our parser as an external adapter and validates output against the official fixtures.

#### Prerequisites

1. **Node.js 18+** — Required to run the TCK harness
   ```bash
   node --version  # Should be v18.0.0 or higher
   ```

2. **Initialize git submodules** — The TCK and language spec are git submodules
   ```bash
   git submodule update --init --recursive
   ```

3. **Install TCK dependencies**
   ```bash
   cd language_repositories/asciidoc_tck
   npm ci
   cd ../..
   ```

#### Running the TCK Harness

```bash
cargo test --test tck_harness
```

This command:
1. Builds the test binary which can act as a TCK adapter
2. Invokes the official Node.js TCK harness (`harness/bin/asciidoc-tck.js`)
3. The harness calls back into the test binary for each fixture
4. Results are displayed showing which tests pass (✔) or fail (✖)

#### Interpreting Results

The TCK harness output shows a tree of test results:

```
▶ tests
  ▶ inline
    ▶ span
      ▶ strong
        ✔ constrained single word (4ms)
        ✔ unconstrained (4ms)
      ✔ strong (50ms)
    ✔ span (200ms)
  ▶ block
    ▶ paragraph
      ✔ single line (4ms)
    ▶ section
      ✖ section with style (4ms)  # Parser doesn't support this yet
```

- **✔** — Test passed; parser output matches expected ASG
- **✖** — Test failed; see error details for the difference

Failed tests indicate features the parser hasn't implemented yet. The Rust test suite's `UNSUPPORTED` list tracks these gaps.

#### Manual Adapter Testing

You can also test the adapter protocol directly:

```bash
# Build the test binary
cargo test --test tck_harness --no-run

# Find the binary path (shown in cargo output)
# Then test it manually:
echo '{"contents":"*bold*","path":"test","type":"inline"}' | \
  TCK_ADAPTER_MODE=1 ./target/debug/deps/tck_harness-*
```

## Project Structure

```
src/
├── lib.rs          # Library entry points: parse_document, parse_inline
├── asg.rs          # Abstract Semantic Graph types (Document, Block, InlineNode, etc.)
├── lexer.rs        # Token definitions and lexer
├── parser/         # Block and inline parsers
└── span.rs         # Source location tracking

tests/
├── tck.rs          # Rust integration tests against TCK fixtures
└── tck_harness.rs  # TCK harness integration (runs official Node.js harness)

language_repositories/
├── asciidoc_lang/  # AsciiDoc Language specification (git submodule)
└── asciidoc_tck/   # Technology Compatibility Kit (git submodule)
```

## Acknowledgments

This project would not be possible without the work of the `AsciiDoc` community:

- [`AsciiDoc` Language Specification](https://github.com/asciidoc/asciidoc-lang) — The official language specification maintained by the Eclipse Foundation. This project aims to be a compliant implementation of this spec.
- [`AsciiDoc` Technology Compatibility Kit (TCK)](https://github.com/asciidoc/asciidoc-tck) — The official test suite for verifying parser compliance. Used extensively to validate this parser's output.
- [Asciidoctor](https://github.com/asciidoctor/asciidoctor) — The reference `AsciiDoc` processor. Its comprehensive test suite and documentation have been invaluable for understanding `AsciiDoc` semantics.

Thank you to all the contributors to these projects for making `AsciiDoc` an awesome, open, well-specified document format.

## License

Licensed under either of [Apache License, Version 2.0](LICENSE-APACHE) or [MIT license](LICENSE-MIT) at your option.

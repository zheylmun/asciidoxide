# asciidoc

High performance [Asciidoc](https://asciidoc.org) lexer and parser library written in safe rust.
The project aims to provide a zero-copy, spec-compliant Asciidoc parser.
The library uses [chumsky](https://github.com/zesterer/chumsky) for lexing and parsing,
with errors provided by [ariadne](https://github.com/zesterer/ariadne).
No additional features beyond lexing and parsing are planned for this crate.

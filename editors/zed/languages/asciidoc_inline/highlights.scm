; Inline code / monospace and passthroughs
[
  (monospace)
  (passthrough)
] @markup.raw

; Bold / strong
(emphasis) @markup.strong

; Italic
(ltalic) @markup.italic

; Highlighted text
(highlight) @markup.italic

; URLs and emails
[
  (link_url)
  (email)
] @markup.link.url

(uri_label) @markup.link.label

; Brackets
[
  "["
  "]"
  "{"
  "}"
  "<<"
  ">>"
] @punctuation.bracket

":" @punctuation.delimiter

; Replacements ({attribute-name})
(replacement) @string.special

; Cross-references
(xref
  (reftext) @markup.link.url)

(xref
  (id) @markup.link.url .)

(xref
  (id) @markup.link.label
  (reftext) @markup.link.url)

; Macro names and index terms
[
  (macro_name)
  "((("
  ")))"
  "(("
  "))"
] @keyword

; Inline macros (link:, image:, etc.)
(inline_macro
  (target)? @markup.link.url
  (attr)? @label)

; Stem macros (latexmath:, asciimath:)
(stem_macro
  (target)? @label
  (attr)? @markup.raw)

; Footnotes
(footnote
  (target)? @label
  (attr) @attribute)

; Index terms
(term) @attribute

; ID assignments
(id_assignment) @label

; Escaped sequences
(escaped_sequence) @string.escape

; Super escape
(super_escape) @string.special

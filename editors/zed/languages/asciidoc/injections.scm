; Inject inline grammar into block macro targets
((block_macro
  (block_macro_name)
  (target) @content)
  (#set! "language" "asciidoc_inline"))

; Inject inline grammar into table cells
((table_cell
  (table_cell_content) @content)
  (#set! "language" "asciidoc_inline"))

; Inject inline grammar into paragraphs
((paragraph) @content
  (#set! "language" "asciidoc_inline"))

; Inject inline grammar into lines (section titles, list items, etc.)
((line) @content
  (#set! "language" "asciidoc_inline"))

; Inject comment language into block comments
((block_comment
  (body) @content)
  (#set! "language" "comment"))

; Inject programming languages into listing blocks via [source,lang] attribute
((section_block
  (element_attr
    (element_attr_marker)
    (attr_value) @language
    (element_attr_marker))
  (listing_block
    (listing_block_start_marker)
    (listing_block_body) @content
    (listing_block_end_marker)))
  (#any-of? @language
    "a2s" "barcode" "blockdiag" "bpmn" "bytefield" "d2" "dbml" "diagrams" "ditaa" "dpic" "erd"
    "gnuplot" "graphviz" "lilypond" "meme" "mermaid" "msc" "nomnoml" "pikchr" "plantuml" "shaape"
    "smcat" "structurizr" "svgbob" "symbolator" "syntrax" "tikz" "umlet" "vega" "wavedrom"))

; Inject latex into passthrough blocks with [latexmath] attribute
((section_block
  (element_attr
    (element_attr_marker)
    (attr_value) @_attr
    (element_attr_marker))
  (passthrough_block
    (passthrough_block_marker)
    (listing_block_body) @content
    (passthrough_block_marker)))
  (#any-of? @_attr "latexmath")
  (#set! "language" "latex"))

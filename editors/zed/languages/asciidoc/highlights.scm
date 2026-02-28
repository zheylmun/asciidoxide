; Document title (= Title)
(document_title) @markup.heading.1

; Section headings (== through ======)
(title1) @markup.heading.2
(title2) @markup.heading.3
(title3) @markup.heading.4
(title4) @markup.heading.5
(title5) @markup.heading.6

; Author info
(email) @markup.link.url

(author_line
  ";" @punctuation.delimiter)

(revision_line
  "," @punctuation.delimiter
  ":" @punctuation.delimiter)

[
  (firstname)
  (middlename)
  (lastname)
] @attribute

(revnumber) @number
(revdate) @string.special
(revremark) @string

; Tables
(table_block_marker) @punctuation.special
(table_cell_attr) @attribute

(table_cell
  "|" @punctuation.special)

; Block delimiters and markers
[
  (breaks)
  (hard_wrap)
  (quoted_block_md_marker)
  (quoted_paragraph_marker)
  (open_block_marker)
  (listing_block_start_marker)
  (listing_block_end_marker)
  (literal_block_marker)
  (passthrough_block_marker)
  (quoted_block_marker)
  (ntable_block_marker)
  (callout_marker)
] @punctuation.special

(ntable_cell
  "!" @punctuation.special)

; Checklists
(checked_list_marker_unchecked) @markup.list.unchecked
(checked_list_marker_checked) @markup.list.checked

; List markers
[
  (list_marker_star)
  (list_marker_hyphen)
  (list_marker_dot)
  (list_marker_digit)
  (list_marker_geek)
  (list_marker_alpha)
] @markup.list

; Comments
[
  (line_comment)
  (block_comment)
] @comment

; Document attributes
[
  (document_attr_marker)
  (element_attr_marker)
] @punctuation.delimiter

(document_attr
  (attr_name) @property)

(element_attr
  (attr_value) @attribute)

; Block titles
(block_title
  (block_title_marker) @punctuation.special) @attribute

; Indented blocks (literal/code)
(ident_block) @markup.raw.block

; Callout list markers
(callout_list_marker) @punctuation.special

; Block macros (image::, include::, etc.)
(block_macro
  (block_macro_name) @keyword
  "::" @punctuation.delimiter
  (target)? @markup.link
  "[" @punctuation.bracket
  "]" @punctuation.bracket)

(attribute_name) @attribute
(attribute_value) @variable.parameter

; Admonitions
(admonition
  (admonition_important) @comment.error
  ":" @comment.error)

(admonition
  (admonition_warning) @comment.warning
  ":" @comment.warning)

(admonition
  (admonition_caution) @comment.warning
  ":" @comment.warning)

(admonition
  (admonition_note) @comment.note
  ":" @comment.note)

(admonition
  (admonition_tip) @comment.note
  ":" @comment.note)

; Admonition blocks with delimiters (NOTE/TIP)
((section_block
  (element_attr
    (element_attr_marker) @comment.note
    (attr_value) @attribute @comment.note
    (element_attr_marker) @comment.note)
  (delimited_block
    (delimited_block_start_marker) @comment.note
    (delimited_block_end_marker) @comment.note))
  (#any-of? @attribute "NOTE" "TIP"))

; Admonition blocks with delimiters (CAUTION/WARNING)
((section_block
  (element_attr
    (element_attr_marker) @comment.warning
    (attr_value) @attribute @comment.warning
    (element_attr_marker) @comment.warning)
  (delimited_block
    (delimited_block_start_marker) @comment.warning
    (delimited_block_end_marker) @comment.warning))
  (#any-of? @attribute "CAUTION" "WARNING"))

; Admonition blocks with delimiters (IMPORTANT)
((section_block
  (element_attr
    (element_attr_marker) @comment.error
    (attr_value) @attribute @comment.error
    (element_attr_marker) @comment.error)
  (delimited_block
    (delimited_block_start_marker) @comment.error
    (delimited_block_end_marker) @comment.error))
  (#eq? @attribute "IMPORTANT"))

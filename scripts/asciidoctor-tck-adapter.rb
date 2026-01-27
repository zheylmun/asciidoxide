#!/usr/bin/env ruby
# frozen_string_literal: true

# Asciidoctor TCK CLI Adapter
#
# Reads JSON from stdin: {"contents": "...", "path": "...", "type": "block"|"inline"}
# Parses with Asciidoctor, converts AST to ASG JSON, writes to stdout.
#
# Usage:
#   echo '{"contents":"*bold*","type":"inline"}' | ruby scripts/asciidoctor-tck-adapter.rb

require 'json'
require 'asciidoctor'

# Sentinel characters for marking inline span boundaries.
# These are embedded in the converter output so that after all quote substitutions
# run, we can reconstruct the nested inline ASG tree from the flat string.
MARKER_OPEN  = "\x01"
MARKER_CLOSE = "\x02"

# ---------------------------------------------------------------------------
# Monkey-patch: inject 'form' attribute (constrained vs unconstrained)
#
# Asciidoctor's Inline node does not store whether the matched quote was
# constrained or unconstrained. We override convert_quoted_text to add
# a 'form' key to the node's attributes hash so our converter can read it.
# ---------------------------------------------------------------------------
module Asciidoctor
  module Substitutors
    def convert_quoted_text match, type, scope
      if scope == :constrained
        if (is_unescaped = match[0].start_with?(RS))
          unescaped_attrs = %([#{match[2]}])
        end
        if is_unescaped
          %(#{unescaped_attrs}#{Inline.new(self, :quoted, match[3], type: type, attributes: { 'form' => 'constrained' }).convert})
        else
          if (attrlist = match[2])
            id = (attributes = parse_quoted_text_attributes(attrlist))['id']
            type = :unquoted if type == :mark
          end
          attributes = (attributes || {}).merge('form' => 'constrained')
          %(#{match[1]}#{Inline.new(self, :quoted, match[3], type: type, id: id, attributes: attributes).convert})
        end
      else
        if (attrlist = match[1])
          id = (attributes = parse_quoted_text_attributes(attrlist))['id']
          type = :unquoted if type == :mark
        end
        attributes = (attributes || {}).merge('form' => 'unconstrained')
        Inline.new(self, :quoted, match[2], type: type, id: id, attributes: attributes).convert
      end
    end
  end
end

# ---------------------------------------------------------------------------
# Custom Converter — outputs sentinel-wrapped markers for inline nodes
# ---------------------------------------------------------------------------
class AsgConverter < Asciidoctor::Converter::Base
  register_for 'asg'

  def convert(node, transform = nil, opts = nil)
    transform ||= node.node_name
    if respond_to?(transform, true)
      send(transform, node)
    else
      ''
    end
  end

  # Block-level stubs (we walk the tree manually; these are only called
  # if Asciidoctor internally triggers conversion on a block node)
  def document(node);       '' end
  def embedded(node);       '' end
  def paragraph(node);      node.content end
  def section(node);        '' end
  def listing(node);        '' end
  def sidebar(node);        '' end
  def open(node);           '' end
  def example(node);        '' end
  def literal(node);        '' end
  def admonition(node);     '' end
  def quote(node);          '' end
  def verse(node);          '' end
  def pass(node);           '' end
  def preamble(node);       '' end
  def dlist(node);          '' end
  def olist(node);          '' end
  def ulist(node);          '' end
  def colist(node);         '' end
  def table(node);          '' end
  def toc(node);            '' end
  def image(node);          '' end
  def audio(node);          '' end
  def video(node);          '' end
  def thematic_break(node); '' end
  def page_break(node);     '' end
  def stem(node);           '' end
  def floating_title(node); '' end

  # Inline converters -------------------------------------------------------

  def inline_quoted(node)
    variant = case node.type
              when :emphasis   then 'emphasis'
              when :strong     then 'strong'
              when :monospaced then 'code'
              when :mark       then 'mark'
              when :superscript then 'superscript'
              when :subscript  then 'subscript'
              when :double     then return %(\u201c#{node.text}\u201d)
              when :single     then return %(\u2018#{node.text}\u2019)
              when :unquoted   then return node.text
              else node.type.to_s
              end
    form = node.attributes&.[]('form') || 'constrained'
    "#{MARKER_OPEN}OPEN:#{variant}:#{form}#{MARKER_CLOSE}#{node.text}#{MARKER_OPEN}CLOSE#{MARKER_CLOSE}"
  end

  def inline_anchor(node);   node.text || node.target || '' end
  def inline_break(node);    node.text || '' end
  def inline_button(node);   node.text || '' end
  def inline_callout(node);  node.text || '' end
  def inline_footnote(node); node.text || '' end
  def inline_image(node);    '' end
  def inline_indexterm(node); node.text || '' end
  def inline_kbd(node);      node.text || '' end
  def inline_menu(node);     node.text || '' end
end

# ---------------------------------------------------------------------------
# Parse sentinel-annotated text into an array of ASG inline nodes
# ---------------------------------------------------------------------------
OPEN_RE = /\A#{Regexp.escape(MARKER_OPEN)}OPEN:([^:]+):([^#{Regexp.escape(MARKER_CLOSE)}]+)#{Regexp.escape(MARKER_CLOSE)}/
CLOSE_TOKEN = "#{MARKER_OPEN}CLOSE#{MARKER_CLOSE}"

def extract_inlines(text)
  return [] if text.nil? || text.empty?

  # Stack: each entry is [span_info_or_nil, children_array]
  stack = [[nil, []]]
  buf = String.new
  i = 0

  while i < text.length
    if text[i] == MARKER_OPEN
      rest = text[i..]
      if (m = rest.match(OPEN_RE))
        flush_text(stack, buf)
        buf = String.new
        stack << [{ 'variant' => m[1], 'form' => m[2] }, []]
        i += m[0].length
      elsif rest.start_with?(CLOSE_TOKEN)
        flush_text(stack, buf)
        buf = String.new
        span_info, children = stack.pop
        span = {
          'name'    => 'span',
          'type'    => 'inline',
          'variant' => span_info['variant'],
          'form'    => span_info['form'],
          'inlines' => children,
        }
        stack.last[1] << span
        i += CLOSE_TOKEN.length
      else
        buf << text[i]
        i += 1
      end
    else
      buf << text[i]
      i += 1
    end
  end

  flush_text(stack, buf)
  stack.last[1]
end

def flush_text(stack, buf)
  return if buf.empty?
  stack.last[1] << {
    'name'  => 'text',
    'type'  => 'string',
    'value' => decode_entities(buf),
  }
end

def decode_entities(text)
  text
    .gsub('&lt;', '<')
    .gsub('&gt;', '>')
    .gsub('&amp;', '&')
end

# ---------------------------------------------------------------------------
# Walk the Asciidoctor block tree → ASG
# ---------------------------------------------------------------------------

# Compute default attributes from a blank document so we can identify
# user-defined attributes by exclusion.
DEFAULT_ATTR_KEYS = Asciidoctor.load('', backend: 'asg', safe: :unsafe).attributes.keys.to_set

# Attributes auto-generated by Asciidoctor from document structure (not from
# explicit attribute entries in the header). These must be excluded from the
# ASG attributes hash since they are not user-defined.
AUTO_GENERATED_ATTRS = Set.new(%w[
  doctitle docfile docdir docname docdate doctime docdatetime
  localdate localtime localdatetime
  author firstname lastname middlename email authorinitials authors
  revdate revnumber revremark
])

def walk_block(node)
  case node.context
  when :document
    walk_document(node)
  when :section
    walk_section(node)
  when :paragraph
    walk_paragraph(node)
  when :listing
    walk_listing(node)
  when :sidebar
    walk_sidebar(node)
  when :ulist
    walk_ulist(node)
  when :olist
    walk_olist(node)
  when :literal
    walk_literal(node)
  when :example
    walk_example(node)
  when :open
    walk_open(node)
  when :admonition
    walk_admonition(node)
  when :preamble
    walk_preamble(node)
  else
    { 'name' => node.context.to_s, 'type' => 'block' }
  end
end

def walk_document(node)
  result = { 'name' => 'document', 'type' => 'block' }

  # Collect user-defined attributes
  user_attrs = {}
  node.attributes.each do |key, val|
    next if key.is_a?(Integer)
    next if DEFAULT_ATTR_KEYS.include?(key)
    next if AUTO_GENERATED_ATTRS.include?(key.to_s)
    user_attrs[key.to_s] = val.to_s
  end

  if node.header?
    result['attributes'] = user_attrs
    header = {}
    title_text = node.doctitle
    if title_text
      header['title'] = [{ 'name' => 'text', 'type' => 'string', 'value' => decode_entities(title_text) }]
    end
    result['header'] = header
  end

  result['blocks'] = node.blocks.map { |b| walk_block(b) }
  result
end

def walk_section(node)
  result = { 'name' => 'section', 'type' => 'block' }
  title = node.title
  if title
    result['title'] = extract_inlines(title)
  end
  result['level'] = node.level
  result['blocks'] = node.blocks.map { |b| walk_block(b) }
  result
end

def walk_paragraph(node)
  content = node.content # triggers inline substitutions via our converter
  result = { 'name' => 'paragraph', 'type' => 'block' }
  result['inlines'] = extract_inlines(content)
  result
end

def walk_listing(node)
  result = { 'name' => 'listing', 'type' => 'block' }
  result['form'] = 'delimited'
  result['delimiter'] = node.attributes['delimiter'] || '----'
  result['inlines'] = [{
    'name'  => 'text',
    'type'  => 'string',
    'value' => node.source,
  }]
  result
end

def walk_sidebar(node)
  result = { 'name' => 'sidebar', 'type' => 'block' }
  result['form'] = 'delimited'
  result['delimiter'] = '****'
  result['blocks'] = node.blocks.map { |b| walk_block(b) }
  result
end

def walk_ulist(node)
  result = { 'name' => 'list', 'type' => 'block' }
  result['variant'] = 'unordered'
  result['marker'] = node.items.first&.marker || '*'
  result['items'] = node.items.map { |item| walk_list_item(item) }
  result
end

def walk_olist(node)
  result = { 'name' => 'list', 'type' => 'block' }
  result['variant'] = 'ordered'
  result['marker'] = node.items.first&.marker || '.'
  result['items'] = node.items.map { |item| walk_list_item(item) }
  result
end

def walk_list_item(item)
  result = { 'name' => 'listItem', 'type' => 'block' }
  result['marker'] = item.marker
  content = item.text # triggers inline subs
  result['principal'] = extract_inlines(content) if content
  if item.blocks && !item.blocks.empty?
    result['blocks'] = item.blocks.map { |b| walk_block(b) }
  end
  result
end

def walk_literal(node)
  result = { 'name' => 'literal', 'type' => 'block' }
  result['form'] = 'delimited'
  result['delimiter'] = node.attributes['delimiter'] || '....'
  result['inlines'] = [{
    'name'  => 'text',
    'type'  => 'string',
    'value' => node.source,
  }]
  result
end

def walk_example(node)
  result = { 'name' => 'example', 'type' => 'block' }
  result['form'] = 'delimited'
  result['delimiter'] = '===='
  result['blocks'] = node.blocks.map { |b| walk_block(b) }
  result
end

def walk_open(node)
  result = { 'name' => 'open', 'type' => 'block' }
  result['form'] = 'delimited'
  result['delimiter'] = '--'
  result['blocks'] = node.blocks.map { |b| walk_block(b) }
  result
end

def walk_admonition(node)
  result = { 'name' => node.attributes['name'] || 'admonition', 'type' => 'block' }
  result['blocks'] = node.blocks.map { |b| walk_block(b) }
  result
end

def walk_preamble(node)
  # Preamble is transparent — just emit its child blocks
  node.blocks.map { |b| walk_block(b) }
end

# ---------------------------------------------------------------------------
# Main: read stdin, parse, output ASG JSON
# ---------------------------------------------------------------------------
input = JSON.parse($stdin.read)
contents = input['contents']
type     = input['type']

doc = Asciidoctor.load(contents, backend: 'asg', safe: :unsafe, header_footer: false)

if type == 'inline'
  # For inline tests, the content is a single line/paragraph of inline markup.
  # Parse it as a document, extract the paragraph's inline content.
  para = doc.blocks.first
  if para && para.context == :paragraph
    content = para.content
    result = extract_inlines(content)
    $stdout.puts JSON.generate(result)
  else
    $stdout.puts '[]'
  end
else
  result = walk_block(doc)
  $stdout.puts JSON.generate(result)
end

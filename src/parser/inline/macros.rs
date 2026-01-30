//! Inline macro detection (link, xref, pass, bare URL).
//!
//! Procedural detection runs before chumsky parsing to identify macros
//! and replace their token ranges with placeholder tokens.

use super::Spanned;
use crate::token::Token;

/// The type of inline macro detected.
pub(super) enum MacroType {
    /// A reference macro (`link:` or `xref:`).
    Ref { variant: &'static str },
    /// A pass macro (`pass:[]`) producing raw content.
    Pass,
}

/// A detected inline macro (link, xref, bare URL, or pass) in the token stream.
pub(super) struct MacroMatch<'a> {
    /// The type of macro.
    pub(super) macro_type: MacroType,
    /// Target URL or path (zero-copy slice of source). Empty for pass macros.
    pub(super) target: &'a str,
    /// Token index of the first token in the macro.
    pub(super) tok_start: usize,
    /// Token index past the last token in the macro.
    pub(super) tok_end: usize,
    /// Token range of bracket content, if any (start inclusive, end exclusive).
    pub(super) content_tok_range: Option<(usize, usize)>,
    /// Byte offset of the macro start in source.
    pub(super) byte_start: usize,
    /// Byte offset of the macro end in source.
    pub(super) byte_end: usize,
}

/// Scan the token stream for inline macros (link, xref, bare URL, pass).
pub(super) fn find_inline_macros<'src>(
    tokens: &[Spanned<'src>],
    source: &'src str,
) -> Vec<MacroMatch<'src>> {
    let mut matches = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        if let Some(m) = try_named_macro(tokens, i, source, "link", "link")
            .or_else(|| try_named_macro(tokens, i, source, "xref", "xref"))
            .or_else(|| try_pass_macro(tokens, i, source))
            .or_else(|| try_bare_url(tokens, i, source))
        {
            let next = m.tok_end;
            matches.push(m);
            i = next;
        } else {
            i += 1;
        }
    }
    matches
}

/// Try to match a named inline macro (`link:target[content]` or
/// `xref:target[content]`) starting at token position `i`.
fn try_named_macro<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
    macro_name: &str,
    variant: &'static str,
) -> Option<MacroMatch<'src>> {
    if i + 2 >= tokens.len() {
        return None;
    }
    let is_match = matches!(&tokens[i].0, Token::Text(s) if *s == macro_name);
    if !is_match || !matches!(tokens[i + 1].0, Token::Colon) {
        return None;
    }

    // Scan target: tokens after Colon up to LBracket.
    let target_start = i + 2;
    let mut j = target_start;
    while j < tokens.len()
        && !matches!(
            tokens[j].0,
            Token::LBracket | Token::Whitespace | Token::Newline
        )
    {
        j += 1;
    }
    if j >= tokens.len() || !matches!(tokens[j].0, Token::LBracket) || j == target_start {
        return None;
    }

    let target_byte_start = tokens[target_start].1.start;
    let target_byte_end = tokens[j - 1].1.end;
    let raw_target = &source[target_byte_start..target_byte_end];
    let target = if variant == "xref" {
        raw_target.strip_suffix(".adoc").unwrap_or(raw_target)
    } else {
        raw_target
    };

    // Find matching RBracket.
    let content_start = j + 1;
    let mut k = content_start;
    let mut depth: u32 = 1;
    while k < tokens.len() {
        match tokens[k].0 {
            Token::LBracket => depth += 1,
            Token::RBracket => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            _ => {}
        }
        k += 1;
    }
    if depth != 0 {
        return None;
    }

    // k is at the closing RBracket.
    Some(MacroMatch {
        macro_type: MacroType::Ref { variant },
        target,
        tok_start: i,
        tok_end: k + 1,
        content_tok_range: Some((content_start, k)),
        byte_start: tokens[i].1.start,
        byte_end: tokens[k].1.end,
    })
}

/// Try to match a pass macro (`pass:[content]`) starting at token position `i`.
fn try_pass_macro<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
) -> Option<MacroMatch<'src>> {
    if i + 2 >= tokens.len() {
        return None;
    }
    let is_match = matches!(&tokens[i].0, Token::Text(s) if *s == "pass");
    if !is_match
        || !matches!(tokens[i + 1].0, Token::Colon)
        || !matches!(tokens[i + 2].0, Token::LBracket)
    {
        return None;
    }

    // Find matching RBracket.
    let content_start = i + 3;
    let mut k = content_start;
    let mut depth: u32 = 1;
    while k < tokens.len() {
        match tokens[k].0 {
            Token::LBracket => depth += 1,
            Token::RBracket => {
                depth -= 1;
                if depth == 0 {
                    break;
                }
            }
            _ => {}
        }
        k += 1;
    }
    if depth != 0 {
        return None;
    }

    // k is at the closing RBracket.
    // Extract raw content between brackets.
    let content_byte_start = if content_start < k {
        tokens[content_start].1.start
    } else {
        // Empty content
        tokens[k].1.start
    };
    let content_byte_end = if content_start < k {
        tokens[k - 1].1.end
    } else {
        tokens[k].1.start
    };
    let raw_content = &source[content_byte_start..content_byte_end];

    Some(MacroMatch {
        macro_type: MacroType::Pass,
        target: raw_content, // Use target field to store raw content
        tok_start: i,
        tok_end: k + 1,
        content_tok_range: None, // Content is not parsed as inlines
        byte_start: tokens[i].1.start,
        byte_end: tokens[k].1.end,
    })
}

/// Try to match a bare URL (`https://...` or `http://...`) starting at
/// token position `i`.
fn try_bare_url<'src>(
    tokens: &[Spanned<'src>],
    i: usize,
    source: &'src str,
) -> Option<MacroMatch<'src>> {
    if i + 4 >= tokens.len() {
        return None;
    }
    let is_scheme = matches!(&tokens[i].0, Token::Text(s) if *s == "https" || *s == "http");
    if !is_scheme
        || !matches!(tokens[i + 1].0, Token::Colon)
        || !matches!(tokens[i + 2].0, Token::Slash)
        || !matches!(tokens[i + 3].0, Token::Slash)
    {
        return None;
    }

    // URL body: consume until whitespace, newline, or end.
    let mut j = i + 4;
    while j < tokens.len()
        && !matches!(
            tokens[j].0,
            Token::Whitespace | Token::Newline | Token::LBracket | Token::RBracket
        )
    {
        j += 1;
    }
    if j <= i + 4 {
        return None;
    }

    let byte_start = tokens[i].1.start;
    let byte_end = tokens[j - 1].1.end;
    let url = &source[byte_start..byte_end];

    Some(MacroMatch {
        macro_type: MacroType::Ref { variant: "link" },
        target: url,
        tok_start: i,
        tok_end: j,
        content_tok_range: None,
        byte_start,
        byte_end,
    })
}

/// Preprocess the token stream to handle escaped unconstrained delimiters.
///
/// When `\**`, `\__`, `\`​`` ``, or `\##` is encountered, this replaces the three
/// tokens with two special tokens:
/// 1. An escaped opener (e.g., `StarEscaped`) for the first delimiter
/// 2. A text-only variant (e.g., `StarAsText`) for the second delimiter
///
/// The escaped opener can start a constrained span, while the text-only variant
/// will be consumed as content text (it cannot open a nested span).
///
/// This implements the `AsciiDoc` spec rule that "a backslash may cause a different
/// markup sequence to be matched".
///
/// Returns a new token vector with the transformations applied.
pub(super) fn preprocess_escaped_unconstrained<'src>(
    tokens: &[Spanned<'src>],
) -> Vec<Spanned<'src>> {
    let mut result = Vec::with_capacity(tokens.len());
    let mut i = 0;

    while i < tokens.len() {
        // Check for escaped unconstrained delimiter: \** \__ \`` \##
        if i + 2 < tokens.len() && matches!(tokens[i].0, Token::Backslash) {
            let (t1, t2) = (&tokens[i + 1].0, &tokens[i + 2].0);

            let tokens_pair = match (t1, t2) {
                (Token::Star, Token::Star) => Some((Token::StarEscaped, Token::StarAsText)),
                (Token::Underscore, Token::Underscore) => {
                    Some((Token::UnderscoreEscaped, Token::UnderscoreAsText))
                }
                (Token::Backtick, Token::Backtick) => {
                    Some((Token::BacktickEscaped, Token::BacktickAsText))
                }
                (Token::Hash, Token::Hash) => Some((Token::HashEscaped, Token::HashAsText)),
                _ => None,
            };

            if let Some((escaped, as_text)) = tokens_pair {
                // Replace backslash + first delimiter with escaped opener.
                result.push((escaped, tokens[i + 1].1));
                // Replace second delimiter with text-only variant.
                result.push((as_text, tokens[i + 2].1));
                i += 3;
                continue;
            }
        }
        result.push(tokens[i].clone());
        i += 1;
    }

    result
}

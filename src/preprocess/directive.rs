//! Directive parsing for `AsciiDoc` preprocessing.
//!
//! This module handles parsing of conditional directive lines (`ifdef`, `ifndef`,
//! `ifeval`, `endif`) that control conditional inclusion during preprocessing.

/// How multiple attributes are combined in a conditional check.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Combinator {
    /// All attributes must be set (attr1+attr2).
    And,
    /// Any attribute must be set (attr1,attr2).
    Or,
}

/// A parsed preprocessing directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Directive<'a> {
    /// `ifdef::attr[]` or `ifdef::attr[content]`
    Ifdef {
        /// Attribute names to check.
        attributes: Vec<&'a str>,
        /// How to combine multiple attributes.
        combinator: Combinator,
        /// Inline content for single-line form, if present.
        inline_content: Option<&'a str>,
    },
    /// `ifndef::attr[]` or `ifndef::attr[content]`
    Ifndef {
        /// Attribute names to check.
        attributes: Vec<&'a str>,
        /// How to combine multiple attributes.
        combinator: Combinator,
        /// Inline content for single-line form, if present.
        inline_content: Option<&'a str>,
    },
    /// `ifeval::[expression]`
    Ifeval {
        /// The expression to evaluate.
        expression: &'a str,
    },
    /// `endif::[]` or `endif::attr[]`
    Endif {
        /// Optional attribute name for clarity.
        attribute: Option<&'a str>,
    },
    /// An escaped directive (backslash-prefixed), e.g., `\ifdef::attr[]`.
    Escaped(&'a str),
}

/// Try to parse a line as a preprocessing directive.
///
/// Returns `None` if the line is not a directive.
#[must_use]
pub fn parse_directive(line: &str) -> Option<Directive<'_>> {
    let trimmed = line.trim_start();

    // Check for escaped directives
    if let Some(rest) = trimmed.strip_prefix('\\') {
        if is_directive_start(rest) {
            return Some(Directive::Escaped(rest));
        }
        return None;
    }

    // Try parsing each directive type
    if let Some(d) = try_parse_ifdef(trimmed) {
        return Some(d);
    }
    if let Some(d) = try_parse_ifndef(trimmed) {
        return Some(d);
    }
    if let Some(d) = try_parse_ifeval(trimmed) {
        return Some(d);
    }
    if let Some(d) = try_parse_endif(trimmed) {
        return Some(d);
    }

    None
}

/// Check if a string starts with a directive keyword.
fn is_directive_start(s: &str) -> bool {
    s.starts_with("ifdef::")
        || s.starts_with("ifndef::")
        || s.starts_with("ifeval::")
        || s.starts_with("endif::")
}

/// Try to parse an `ifdef::` directive.
fn try_parse_ifdef(line: &str) -> Option<Directive<'_>> {
    let rest = line.strip_prefix("ifdef::")?;
    let (attributes, combinator, inline_content) = parse_conditional_body(rest)?;
    Some(Directive::Ifdef {
        attributes,
        combinator,
        inline_content,
    })
}

/// Try to parse an `ifndef::` directive.
fn try_parse_ifndef(line: &str) -> Option<Directive<'_>> {
    let rest = line.strip_prefix("ifndef::")?;
    let (attributes, combinator, inline_content) = parse_conditional_body(rest)?;
    Some(Directive::Ifndef {
        attributes,
        combinator,
        inline_content,
    })
}

/// Parse the body of an ifdef/ifndef directive: `attr[content]` or `attr1,attr2[]`
fn parse_conditional_body(s: &str) -> Option<(Vec<&str>, Combinator, Option<&str>)> {
    // Find the opening bracket
    let bracket_pos = s.find('[')?;
    let attr_part = &s[..bracket_pos];
    let rest = &s[bracket_pos + 1..];

    // Find the closing bracket
    let close_pos = rest.rfind(']')?;
    let content = &rest[..close_pos];

    // Parse attributes and determine combinator
    let (attributes, combinator) = parse_attribute_list(attr_part)?;

    // Inline content is present if non-empty
    let inline_content = if content.is_empty() {
        None
    } else {
        Some(content)
    };

    Some((attributes, combinator, inline_content))
}

/// Parse an attribute list like `attr`, `attr1,attr2`, or `attr1+attr2`.
fn parse_attribute_list(s: &str) -> Option<(Vec<&str>, Combinator)> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }

    // Check for AND combinator (+)
    if s.contains('+') {
        let attrs: Vec<&str> = s
            .split('+')
            .map(str::trim)
            .filter(|a| !a.is_empty())
            .collect();
        if attrs.is_empty() {
            return None;
        }
        return Some((attrs, Combinator::And));
    }

    // Check for OR combinator (,)
    if s.contains(',') {
        let attrs: Vec<&str> = s
            .split(',')
            .map(str::trim)
            .filter(|a| !a.is_empty())
            .collect();
        if attrs.is_empty() {
            return None;
        }
        return Some((attrs, Combinator::Or));
    }

    // Single attribute
    Some((vec![s], Combinator::Or))
}

/// Try to parse an `ifeval::` directive.
fn try_parse_ifeval(line: &str) -> Option<Directive<'_>> {
    let rest = line.strip_prefix("ifeval::[")?;
    let close_pos = rest.rfind(']')?;
    let expression = &rest[..close_pos];
    Some(Directive::Ifeval { expression })
}

/// Try to parse an `endif::` directive.
fn try_parse_endif(line: &str) -> Option<Directive<'_>> {
    let rest = line.strip_prefix("endif::")?;

    // Find the brackets
    let bracket_pos = rest.find('[')?;
    let attr_part = &rest[..bracket_pos];
    let rest_after = &rest[bracket_pos + 1..];

    // Must end with ]
    if !rest_after.starts_with(']') && !rest_after.ends_with(']') {
        // Check if there's a closing bracket
        rest_after.find(']')?;
    }

    let attribute = if attr_part.trim().is_empty() {
        None
    } else {
        Some(attr_part.trim())
    };

    Some(Directive::Endif { attribute })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_ifdef_block_form() {
        let d = parse_directive("ifdef::foo[]").unwrap();
        assert_eq!(
            d,
            Directive::Ifdef {
                attributes: vec!["foo"],
                combinator: Combinator::Or,
                inline_content: None,
            }
        );
    }

    #[test]
    fn parse_ifdef_inline_form() {
        let d = parse_directive("ifdef::foo[some content]").unwrap();
        assert_eq!(
            d,
            Directive::Ifdef {
                attributes: vec!["foo"],
                combinator: Combinator::Or,
                inline_content: Some("some content"),
            }
        );
    }

    #[test]
    fn parse_ifdef_multi_attr_or() {
        let d = parse_directive("ifdef::a,b,c[]").unwrap();
        assert_eq!(
            d,
            Directive::Ifdef {
                attributes: vec!["a", "b", "c"],
                combinator: Combinator::Or,
                inline_content: None,
            }
        );
    }

    #[test]
    fn parse_ifdef_multi_attr_and() {
        let d = parse_directive("ifdef::a+b[]").unwrap();
        assert_eq!(
            d,
            Directive::Ifdef {
                attributes: vec!["a", "b"],
                combinator: Combinator::And,
                inline_content: None,
            }
        );
    }

    #[test]
    fn parse_ifndef_block_form() {
        let d = parse_directive("ifndef::bar[]").unwrap();
        assert_eq!(
            d,
            Directive::Ifndef {
                attributes: vec!["bar"],
                combinator: Combinator::Or,
                inline_content: None,
            }
        );
    }

    #[test]
    fn parse_ifeval() {
        let d = parse_directive("ifeval::[{level} > 2]").unwrap();
        assert_eq!(
            d,
            Directive::Ifeval {
                expression: "{level} > 2"
            }
        );
    }

    #[test]
    fn parse_endif_empty() {
        let d = parse_directive("endif::[]").unwrap();
        assert_eq!(d, Directive::Endif { attribute: None });
    }

    #[test]
    fn parse_endif_with_attr() {
        let d = parse_directive("endif::foo[]").unwrap();
        assert_eq!(
            d,
            Directive::Endif {
                attribute: Some("foo")
            }
        );
    }

    #[test]
    fn parse_escaped_directive() {
        let d = parse_directive("\\ifdef::foo[]").unwrap();
        assert_eq!(d, Directive::Escaped("ifdef::foo[]"));
    }

    #[test]
    fn parse_non_directive() {
        assert!(parse_directive("just a regular line").is_none());
        assert!(parse_directive("= A heading").is_none());
        assert!(parse_directive("").is_none());
    }

    #[test]
    fn parse_with_leading_whitespace() {
        let d = parse_directive("  ifdef::foo[]").unwrap();
        assert_eq!(
            d,
            Directive::Ifdef {
                attributes: vec!["foo"],
                combinator: Combinator::Or,
                inline_content: None,
            }
        );
    }
}

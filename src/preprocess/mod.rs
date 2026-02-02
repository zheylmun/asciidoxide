//! Phase 1 preprocessing for `AsciiDoc` documents.
//!
//! This module handles preprocessing directives that are evaluated line-by-line
//! before block parsing. Currently supported:
//!
//! - `ifdef::attr[]` / `ifdef::attr[content]` — include if attribute is set
//! - `ifndef::attr[]` / `ifndef::attr[content]` — include if attribute is NOT set
//! - `ifeval::[expression]` — include if expression evaluates to true
//! - `endif::[]` — close a conditional block
//!
//! Escaped directives (`\ifdef::attr[]`) are emitted without the backslash.
//!
//! # Example
//!
//! ```
//! use std::collections::HashMap;
//! use asciidoxide::preprocess::preprocess;
//!
//! let input = "ifdef::debug[]\nDebug mode enabled\nendif::[]";
//! let attrs = HashMap::from([("debug".to_string(), "true".to_string())]);
//! let result = preprocess(input, &attrs);
//! assert_eq!(result.content, "Debug mode enabled\n");
//! ```

mod conditional;
mod directive;
mod expression;

use std::borrow::Cow;
use std::collections::HashMap;

use crate::span::SourceSpan;

pub use directive::{Combinator, Directive};
pub use expression::ExprError;

/// A diagnostic emitted during preprocessing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocessDiagnostic {
    /// The source span where the issue was detected.
    pub span: SourceSpan,
    /// Human-readable description of the issue.
    pub message: String,
    /// Severity level.
    pub severity: PreprocessSeverity,
}

/// Severity level for preprocessing diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PreprocessSeverity {
    /// A warning — preprocessing recovered but output may not match intent.
    Warning,
    /// An error — preprocessing could not interpret a directive.
    Error,
}

/// Result of preprocessing an `AsciiDoc` document.
#[derive(Debug)]
pub struct PreprocessResult<'a> {
    /// The preprocessed content.
    ///
    /// - `Cow::Borrowed` if no directives were processed (zero-copy)
    /// - `Cow::Owned` if content was modified
    pub content: Cow<'a, str>,
    /// Diagnostics emitted during preprocessing.
    pub diagnostics: Vec<PreprocessDiagnostic>,
}

/// Internal state for the preprocessor.
struct PreprocessorState<'a> {
    /// Document attributes for evaluating conditions.
    attributes: &'a HashMap<String, String>,
    /// Stack of conditional frames for tracking nested conditionals.
    condition_stack: Vec<ConditionalFrame>,
    /// Output buffer.
    output: String,
    /// Accumulated diagnostics.
    diagnostics: Vec<PreprocessDiagnostic>,
    /// Current byte offset in the input.
    current_offset: usize,
}

/// A frame on the conditional stack.
#[derive(Debug)]
struct ConditionalFrame {
    /// Whether this conditional's condition was true.
    is_active: bool,
    /// Byte offset where the conditional started.
    start_offset: usize,
    /// The line number (1-based) where the conditional started.
    start_line: usize,
}

/// Preprocess an `AsciiDoc` document, evaluating conditional directives.
///
/// # Arguments
///
/// * `input` - The raw `AsciiDoc` source text
/// * `attributes` - Document attributes for evaluating conditions
///
/// # Returns
///
/// A [`PreprocessResult`] containing the preprocessed content and any diagnostics.
#[must_use]
#[allow(clippy::implicit_hasher)]
pub fn preprocess<'a>(
    input: &'a str,
    attributes: &HashMap<String, String>,
) -> PreprocessResult<'a> {
    // Quick check: if no directive patterns, return borrowed input
    if !contains_directive_pattern(input) {
        return PreprocessResult {
            content: Cow::Borrowed(input),
            diagnostics: Vec::new(),
        };
    }

    let mut state = PreprocessorState {
        attributes,
        condition_stack: Vec::new(),
        output: String::with_capacity(input.len()),
        diagnostics: Vec::new(),
        current_offset: 0,
    };

    for (line_num, line) in input.lines().enumerate() {
        let line_start = state.current_offset;
        let line_end = line_start + line.len();

        process_line(&mut state, line, line_num + 1, line_start);

        // Advance past the line and newline (if present)
        state.current_offset = line_end;
        if state.current_offset < input.len() {
            state.current_offset += 1; // Skip the newline
        }
    }

    // Warn about unclosed conditionals
    for frame in &state.condition_stack {
        state.diagnostics.push(PreprocessDiagnostic {
            span: SourceSpan {
                start: frame.start_offset,
                end: frame.start_offset + 1,
            },
            message: format!(
                "Unclosed conditional directive starting at line {}",
                frame.start_line
            ),
            severity: PreprocessSeverity::Warning,
        });
    }

    PreprocessResult {
        content: Cow::Owned(state.output),
        diagnostics: state.diagnostics,
    }
}

/// Quick check for directive patterns to enable zero-copy fast path.
fn contains_directive_pattern(input: &str) -> bool {
    input.contains("ifdef::")
        || input.contains("ifndef::")
        || input.contains("ifeval::")
        || input.contains("endif::")
        || input.contains("\\ifdef::")
        || input.contains("\\ifndef::")
        || input.contains("\\ifeval::")
        || input.contains("\\endif::")
}

/// Process a single line.
fn process_line(state: &mut PreprocessorState<'_>, line: &str, line_num: usize, line_start: usize) {
    // Try to parse as a directive
    if let Some(directive) = directive::parse_directive(line) {
        match directive {
            Directive::Escaped(content) => {
                // Emit without the backslash, but only if we're in active context
                if is_active(state) {
                    state.output.push_str(content);
                    state.output.push('\n');
                }
            }
            Directive::Ifdef {
                attributes,
                combinator,
                inline_content,
            } => {
                handle_ifdef(
                    state,
                    &attributes,
                    combinator,
                    inline_content,
                    line_num,
                    line_start,
                );
            }
            Directive::Ifndef {
                attributes,
                combinator,
                inline_content,
            } => {
                handle_ifndef(
                    state,
                    &attributes,
                    combinator,
                    inline_content,
                    line_num,
                    line_start,
                );
            }
            Directive::Ifeval { expression } => {
                handle_ifeval(state, expression, line_num, line_start);
            }
            Directive::Endif { attribute: _ } => {
                handle_endif(state, line_start);
            }
        }
    } else if is_active(state) {
        // Not a directive, emit if in active context
        state.output.push_str(line);
        state.output.push('\n');
    }
}

/// Check if we're in an active context (all conditions on the stack are true).
fn is_active(state: &PreprocessorState<'_>) -> bool {
    state.condition_stack.iter().all(|frame| frame.is_active)
}

/// Handle an `ifdef` directive.
fn handle_ifdef(
    state: &mut PreprocessorState<'_>,
    attributes: &[&str],
    combinator: Combinator,
    inline_content: Option<&str>,
    line_num: usize,
    line_start: usize,
) {
    let condition = conditional::evaluate_ifdef(attributes, combinator, state.attributes);

    // The condition is only truly active if we're already in an active context
    let is_active = is_active(state) && condition;

    if let Some(content) = inline_content {
        // Single-line form: emit content if active
        if is_active {
            state.output.push_str(content);
            state.output.push('\n');
        }
    } else {
        // Block form: push to stack
        state.condition_stack.push(ConditionalFrame {
            is_active,
            start_offset: line_start,
            start_line: line_num,
        });
    }
}

/// Handle an `ifndef` directive.
fn handle_ifndef(
    state: &mut PreprocessorState<'_>,
    attributes: &[&str],
    combinator: Combinator,
    inline_content: Option<&str>,
    line_num: usize,
    line_start: usize,
) {
    let condition = conditional::evaluate_ifndef(attributes, combinator, state.attributes);

    // The condition is only truly active if we're already in an active context
    let is_active = is_active(state) && condition;

    if let Some(content) = inline_content {
        // Single-line form: emit content if active
        if is_active {
            state.output.push_str(content);
            state.output.push('\n');
        }
    } else {
        // Block form: push to stack
        state.condition_stack.push(ConditionalFrame {
            is_active,
            start_offset: line_start,
            start_line: line_num,
        });
    }
}

/// Handle an `ifeval` directive.
fn handle_ifeval(
    state: &mut PreprocessorState<'_>,
    expression: &str,
    line_num: usize,
    line_start: usize,
) {
    let condition = match expression::evaluate_expression(expression, state.attributes) {
        Ok(result) => result,
        Err(e) => {
            state.diagnostics.push(PreprocessDiagnostic {
                span: SourceSpan {
                    start: line_start,
                    end: line_start + expression.len(),
                },
                message: format!("Invalid ifeval expression: {e:?}"),
                severity: PreprocessSeverity::Error,
            });
            false
        }
    };

    // The condition is only truly active if we're already in an active context
    let is_active = is_active(state) && condition;

    state.condition_stack.push(ConditionalFrame {
        is_active,
        start_offset: line_start,
        start_line: line_num,
    });
}

/// Handle an `endif` directive.
fn handle_endif(state: &mut PreprocessorState<'_>, line_start: usize) {
    if state.condition_stack.pop().is_none() {
        state.diagnostics.push(PreprocessDiagnostic {
            span: SourceSpan {
                start: line_start,
                end: line_start + 8, // "endif::[]".len() approximately
            },
            message: "Unmatched endif directive".to_string(),
            severity: PreprocessSeverity::Warning,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn attrs(pairs: &[(&str, &str)]) -> HashMap<String, String> {
        pairs
            .iter()
            .map(|(k, v)| ((*k).to_string(), (*v).to_string()))
            .collect()
    }

    #[test]
    fn no_directives_returns_borrowed() {
        let input = "Just some text\nwith multiple lines";
        let result = preprocess(input, &HashMap::new());
        assert!(matches!(result.content, Cow::Borrowed(_)));
        assert_eq!(result.content, input);
    }

    #[test]
    fn ifdef_present() {
        let input = "ifdef::foo[]\nincluded\nendif::[]";
        let result = preprocess(input, &attrs(&[("foo", "bar")]));
        assert_eq!(result.content, "included\n");
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn ifdef_absent() {
        let input = "ifdef::foo[]\nskipped\nendif::[]";
        let result = preprocess(input, &HashMap::new());
        assert_eq!(result.content, "");
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn ifdef_inline_present() {
        let input = "ifdef::foo[inline content]";
        let result = preprocess(input, &attrs(&[("foo", "")]));
        assert_eq!(result.content, "inline content\n");
    }

    #[test]
    fn ifdef_inline_absent() {
        let input = "ifdef::foo[inline content]";
        let result = preprocess(input, &HashMap::new());
        assert_eq!(result.content, "");
    }

    #[test]
    fn ifndef_absent() {
        let input = "ifndef::foo[]\nincluded\nendif::[]";
        let result = preprocess(input, &HashMap::new());
        assert_eq!(result.content, "included\n");
    }

    #[test]
    fn ifndef_present() {
        let input = "ifndef::foo[]\nskipped\nendif::[]";
        let result = preprocess(input, &attrs(&[("foo", "")]));
        assert_eq!(result.content, "");
    }

    #[test]
    fn ifdef_or_any_present() {
        let input = "ifdef::a,b[]\nincluded\nendif::[]";
        let result = preprocess(input, &attrs(&[("b", "")]));
        assert_eq!(result.content, "included\n");
    }

    #[test]
    fn ifdef_and_all_present() {
        let input = "ifdef::a+b[]\nincluded\nendif::[]";
        let result = preprocess(input, &attrs(&[("a", ""), ("b", "")]));
        assert_eq!(result.content, "included\n");
    }

    #[test]
    fn ifdef_and_some_missing() {
        let input = "ifdef::a+b[]\nskipped\nendif::[]";
        let result = preprocess(input, &attrs(&[("a", "")]));
        assert_eq!(result.content, "");
    }

    #[test]
    fn nested_conditionals_both_true() {
        let input = "ifdef::a[]\nouter\nifdef::b[]\ninner\nendif::[]\nendif::[]";
        let result = preprocess(input, &attrs(&[("a", ""), ("b", "")]));
        assert_eq!(result.content, "outer\ninner\n");
    }

    #[test]
    fn nested_conditionals_inner_false() {
        let input = "ifdef::a[]\nouter\nifdef::b[]\ninner\nendif::[]\nendif::[]";
        let result = preprocess(input, &attrs(&[("a", "")]));
        assert_eq!(result.content, "outer\n");
    }

    #[test]
    fn nested_conditionals_outer_false() {
        let input = "ifdef::a[]\nouter\nifdef::b[]\ninner\nendif::[]\nendif::[]";
        let result = preprocess(input, &attrs(&[("b", "")]));
        assert_eq!(result.content, "");
    }

    #[test]
    fn ifeval_numeric_true() {
        let input = "ifeval::[{level} > 2]\nincluded\nendif::[]";
        let result = preprocess(input, &attrs(&[("level", "3")]));
        assert_eq!(result.content, "included\n");
    }

    #[test]
    fn ifeval_numeric_false() {
        let input = "ifeval::[{level} > 5]\nskipped\nendif::[]";
        let result = preprocess(input, &attrs(&[("level", "3")]));
        assert_eq!(result.content, "");
    }

    #[test]
    fn ifeval_string_comparison() {
        let input = "ifeval::[{env} == \"production\"]\nincluded\nendif::[]";
        let result = preprocess(input, &attrs(&[("env", "production")]));
        assert_eq!(result.content, "included\n");
    }

    #[test]
    fn escaped_directive() {
        let input = "\\ifdef::foo[]";
        let result = preprocess(input, &HashMap::new());
        assert_eq!(result.content, "ifdef::foo[]\n");
    }

    #[test]
    fn escaped_directive_in_inactive_context() {
        let input = "ifdef::missing[]\n\\ifdef::foo[]\nendif::[]";
        let result = preprocess(input, &HashMap::new());
        // In inactive context, escaped directive is not emitted
        assert_eq!(result.content, "");
    }

    #[test]
    fn mixed_content() {
        let input = "before\nifdef::foo[]\nmiddle\nendif::[]\nafter";
        let result = preprocess(input, &attrs(&[("foo", "")]));
        assert_eq!(result.content, "before\nmiddle\nafter\n");
    }

    #[test]
    fn unclosed_conditional_warning() {
        let input = "ifdef::foo[]\nno endif";
        let result = preprocess(input, &attrs(&[("foo", "")]));
        assert_eq!(result.content, "no endif\n");
        assert_eq!(result.diagnostics.len(), 1);
        assert!(matches!(
            result.diagnostics[0].severity,
            PreprocessSeverity::Warning
        ));
        assert!(result.diagnostics[0].message.contains("Unclosed"));
    }

    #[test]
    fn unmatched_endif_warning() {
        let input = "endif::[]";
        let result = preprocess(input, &HashMap::new());
        assert_eq!(result.diagnostics.len(), 1);
        assert!(result.diagnostics[0].message.contains("Unmatched"));
    }

    #[test]
    fn multiple_endifs() {
        let input = "ifdef::a[]\nifdef::b[]\ninner\nendif::[]\nendif::[]";
        let result = preprocess(input, &attrs(&[("a", ""), ("b", "")]));
        assert_eq!(result.content, "inner\n");
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn ifdef_with_trailing_content() {
        let input = "before\nifdef::foo[]\nincluded\nendif::[]\nafter";
        let result = preprocess(input, &attrs(&[("foo", "")]));
        assert_eq!(result.content, "before\nincluded\nafter\n");
    }
}

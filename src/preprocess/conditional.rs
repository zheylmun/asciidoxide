//! Conditional evaluation for `ifdef` and `ifndef` directives.
//!
//! This module handles the logic of evaluating whether attributes are set
//! and combining multiple attribute checks with AND/OR logic.

use std::collections::HashMap;

use super::directive::Combinator;

/// Evaluate an `ifdef` condition.
///
/// Returns `true` if the condition is satisfied (content should be included).
///
/// - `Combinator::Or`: returns `true` if ANY attribute is set
/// - `Combinator::And`: returns `true` if ALL attributes are set
#[must_use]
pub fn evaluate_ifdef(
    attributes: &[&str],
    combinator: Combinator,
    doc_attributes: &HashMap<String, String>,
) -> bool {
    match combinator {
        Combinator::Or => attributes
            .iter()
            .any(|attr| doc_attributes.contains_key(*attr)),
        Combinator::And => attributes
            .iter()
            .all(|attr| doc_attributes.contains_key(*attr)),
    }
}

/// Evaluate an `ifndef` condition.
///
/// Returns `true` if the condition is satisfied (content should be included).
///
/// - `Combinator::Or`: returns `true` if ANY attribute is NOT set
/// - `Combinator::And`: returns `true` if ALL attributes are NOT set
#[must_use]
pub fn evaluate_ifndef(
    attributes: &[&str],
    combinator: Combinator,
    doc_attributes: &HashMap<String, String>,
) -> bool {
    match combinator {
        Combinator::Or => attributes
            .iter()
            .any(|attr| !doc_attributes.contains_key(*attr)),
        Combinator::And => attributes
            .iter()
            .all(|attr| !doc_attributes.contains_key(*attr)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_attrs(pairs: &[(&str, &str)]) -> HashMap<String, String> {
        pairs
            .iter()
            .map(|(k, v)| ((*k).to_string(), (*v).to_string()))
            .collect()
    }

    #[test]
    fn ifdef_single_present() {
        let attrs = make_attrs(&[("foo", "bar")]);
        assert!(evaluate_ifdef(&["foo"], Combinator::Or, &attrs));
    }

    #[test]
    fn ifdef_single_absent() {
        let attrs = HashMap::new();
        assert!(!evaluate_ifdef(&["foo"], Combinator::Or, &attrs));
    }

    #[test]
    fn ifdef_or_any_present() {
        let attrs = make_attrs(&[("b", "")]);
        assert!(evaluate_ifdef(&["a", "b", "c"], Combinator::Or, &attrs));
    }

    #[test]
    fn ifdef_or_none_present() {
        let attrs = make_attrs(&[("x", "")]);
        assert!(!evaluate_ifdef(&["a", "b", "c"], Combinator::Or, &attrs));
    }

    #[test]
    fn ifdef_and_all_present() {
        let attrs = make_attrs(&[("a", ""), ("b", "")]);
        assert!(evaluate_ifdef(&["a", "b"], Combinator::And, &attrs));
    }

    #[test]
    fn ifdef_and_some_present() {
        let attrs = make_attrs(&[("a", "")]);
        assert!(!evaluate_ifdef(&["a", "b"], Combinator::And, &attrs));
    }

    #[test]
    fn ifndef_single_absent() {
        let attrs = HashMap::new();
        assert!(evaluate_ifndef(&["foo"], Combinator::Or, &attrs));
    }

    #[test]
    fn ifndef_single_present() {
        let attrs = make_attrs(&[("foo", "bar")]);
        assert!(!evaluate_ifndef(&["foo"], Combinator::Or, &attrs));
    }

    #[test]
    fn ifndef_or_any_absent() {
        let attrs = make_attrs(&[("a", ""), ("b", "")]);
        // "c" is absent, so OR returns true
        assert!(evaluate_ifndef(&["a", "b", "c"], Combinator::Or, &attrs));
    }

    #[test]
    fn ifndef_or_all_present() {
        let attrs = make_attrs(&[("a", ""), ("b", ""), ("c", "")]);
        // All present, so no absent → OR returns false
        assert!(!evaluate_ifndef(&["a", "b", "c"], Combinator::Or, &attrs));
    }

    #[test]
    fn ifndef_and_all_absent() {
        let attrs = HashMap::new();
        assert!(evaluate_ifndef(&["a", "b"], Combinator::And, &attrs));
    }

    #[test]
    fn ifndef_and_some_present() {
        let attrs = make_attrs(&[("a", "")]);
        // "a" is present, so not all are absent → AND returns false
        assert!(!evaluate_ifndef(&["a", "b"], Combinator::And, &attrs));
    }

    #[test]
    fn empty_value_counts_as_set() {
        // An attribute with an empty string value is still "set"
        let attrs = make_attrs(&[("foo", "")]);
        assert!(evaluate_ifdef(&["foo"], Combinator::Or, &attrs));
        assert!(!evaluate_ifndef(&["foo"], Combinator::Or, &attrs));
    }
}

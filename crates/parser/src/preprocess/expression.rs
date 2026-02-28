//! Expression parsing and evaluation for `ifeval` directives.
//!
//! Supports expressions of the form `{attr} operator value` or `value operator value`.
//!
//! Operators: `==`, `!=`, `<`, `<=`, `>`, `>=`
//! Values: attribute references (`{attr}`), numbers, quoted strings, booleans, nil

use std::collections::HashMap;

/// Error type for expression evaluation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprError {
    /// The expression syntax is invalid.
    InvalidSyntax(String),
    /// An unknown operator was used.
    UnknownOperator(String),
    /// Type mismatch in comparison.
    TypeMismatch(String),
    /// Unresolved attribute reference.
    UnresolvedAttribute(String),
}

/// A value in an ifeval expression.
#[derive(Debug, Clone, PartialEq)]
enum Value {
    /// A string value (from quotes or attribute).
    String(String),
    /// A numeric value.
    Number(f64),
    /// A boolean value.
    Boolean(bool),
    /// The nil/null value.
    Nil,
}

/// Comparison operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Operator {
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}

/// Evaluate an ifeval expression.
///
/// # Arguments
///
/// * `expr` - The expression string (e.g., `{level} > 2`)
/// * `attributes` - The document attributes for resolving `{attr}` references
///
/// # Returns
///
/// `Ok(true)` if the condition is satisfied, `Ok(false)` otherwise.
/// `Err` if the expression is malformed.
pub fn evaluate_expression(
    expr: &str,
    attributes: &HashMap<String, String>,
) -> Result<bool, ExprError> {
    let expr = expr.trim();

    // Find the operator
    let (lhs_str, op, rhs_str) = parse_expression_parts(expr)?;

    // Parse values
    let lhs = parse_value(lhs_str.trim(), attributes);
    let rhs = parse_value(rhs_str.trim(), attributes);

    // Compare
    compare_values(&lhs, op, &rhs)
}

/// Parse an expression into left-hand side, operator, and right-hand side.
fn parse_expression_parts(expr: &str) -> Result<(&str, Operator, &str), ExprError> {
    // Try two-character operators first
    for (op_str, op) in &[
        ("==", Operator::Equal),
        ("!=", Operator::NotEqual),
        ("<=", Operator::LessEqual),
        (">=", Operator::GreaterEqual),
    ] {
        if let Some(pos) = expr.find(op_str) {
            let lhs = &expr[..pos];
            let rhs = &expr[pos + 2..];
            return Ok((lhs, *op, rhs));
        }
    }

    // Try single-character operators (must check after two-char to avoid false matches)
    for (op_str, op) in &[("<", Operator::LessThan), (">", Operator::GreaterThan)] {
        if let Some(pos) = find_standalone_operator(expr, op_str) {
            let lhs = &expr[..pos];
            let rhs = &expr[pos + 1..];
            return Ok((lhs, *op, rhs));
        }
    }

    Err(ExprError::InvalidSyntax(format!(
        "No operator found in expression: {expr}"
    )))
}

/// Find a single-character operator that isn't part of `<=` or `>=`.
fn find_standalone_operator(expr: &str, op: &str) -> Option<usize> {
    let mut pos = 0;
    while let Some(idx) = expr[pos..].find(op) {
        let abs_pos = pos + idx;
        // Check it's not part of <= or >=
        let next_char = expr.as_bytes().get(abs_pos + 1);
        if next_char != Some(&b'=') {
            return Some(abs_pos);
        }
        pos = abs_pos + 1;
    }
    None
}

/// Parse a value string into a `Value`.
fn parse_value(s: &str, attributes: &HashMap<String, String>) -> Value {
    let s = s.trim();

    // Check for attribute reference: {attr}
    if s.starts_with('{') && s.ends_with('}') {
        let attr_name = &s[1..s.len() - 1];
        return resolve_attribute(attr_name, attributes);
    }

    // Check for quoted string
    if (s.starts_with('"') && s.ends_with('"')) || (s.starts_with('\'') && s.ends_with('\'')) {
        let inner = &s[1..s.len() - 1];
        return Value::String(inner.to_string());
    }

    // Check for boolean
    if s.eq_ignore_ascii_case("true") {
        return Value::Boolean(true);
    }
    if s.eq_ignore_ascii_case("false") {
        return Value::Boolean(false);
    }

    // Check for nil
    if s.eq_ignore_ascii_case("nil") || s.eq_ignore_ascii_case("null") {
        return Value::Nil;
    }

    // Try parsing as number
    if let Ok(n) = s.parse::<f64>() {
        return Value::Number(n);
    }

    // Unquoted string - treat as string literal
    Value::String(s.to_string())
}

/// Resolve an attribute reference to a value.
fn resolve_attribute(name: &str, attributes: &HashMap<String, String>) -> Value {
    match attributes.get(name) {
        Some(val) => {
            // Try to interpret the value as a typed value
            if val.eq_ignore_ascii_case("true") {
                Value::Boolean(true)
            } else if val.eq_ignore_ascii_case("false") {
                Value::Boolean(false)
            } else if let Ok(n) = val.parse::<f64>() {
                Value::Number(n)
            } else {
                Value::String(val.clone())
            }
        }
        None => {
            // Unset attribute is nil
            Value::Nil
        }
    }
}

/// Compare two values with an operator.
fn compare_values(lhs: &Value, op: Operator, rhs: &Value) -> Result<bool, ExprError> {
    match (lhs, rhs) {
        // Number comparisons
        (Value::Number(a), Value::Number(b)) => Ok(compare_numbers(*a, op, *b)),

        // String comparisons
        (Value::String(a), Value::String(b)) => Ok(compare_strings(a, op, b)),

        // Boolean comparisons (only == and !=)
        (Value::Boolean(a), Value::Boolean(b)) => match op {
            Operator::Equal => Ok(a == b),
            Operator::NotEqual => Ok(a != b),
            _ => Err(ExprError::TypeMismatch(
                "Booleans can only be compared with == or !=".to_string(),
            )),
        },

        // Nil comparisons (only == and !=)
        (Value::Nil, Value::Nil) => match op {
            Operator::Equal => Ok(true),
            Operator::NotEqual => Ok(false),
            _ => Err(ExprError::TypeMismatch(
                "nil can only be compared with == or !=".to_string(),
            )),
        },
        (Value::Nil, _) | (_, Value::Nil) => match op {
            Operator::Equal => Ok(false),
            Operator::NotEqual => Ok(true),
            _ => Err(ExprError::TypeMismatch(
                "nil can only be compared with == or !=".to_string(),
            )),
        },

        // Cross-type comparisons
        (Value::Number(_), Value::String(s)) | (Value::String(s), Value::Number(_)) => {
            // Try coercing string to number
            if let Ok(n) = s.parse::<f64>() {
                let (a, b) = match (lhs, rhs) {
                    (Value::Number(num), Value::String(_)) => (*num, n),
                    (Value::String(_), Value::Number(num)) => (n, *num),
                    _ => unreachable!(),
                };
                Ok(compare_numbers(a, op, b))
            } else {
                // String doesn't parse as number - compare as strings
                let (a, b) = match (lhs, rhs) {
                    (Value::Number(num), Value::String(s)) => (num.to_string(), s.clone()),
                    (Value::String(s), Value::Number(num)) => (s.clone(), num.to_string()),
                    _ => unreachable!(),
                };
                Ok(compare_strings(&a, op, &b))
            }
        }

        // Boolean with non-boolean
        (Value::Boolean(_), _) | (_, Value::Boolean(_)) => Err(ExprError::TypeMismatch(
            "Cannot compare boolean with non-boolean".to_string(),
        )),
    }
}

fn compare_numbers(a: f64, op: Operator, b: f64) -> bool {
    match op {
        Operator::Equal => (a - b).abs() < f64::EPSILON,
        Operator::NotEqual => (a - b).abs() >= f64::EPSILON,
        Operator::LessThan => a < b,
        Operator::LessEqual => a <= b,
        Operator::GreaterThan => a > b,
        Operator::GreaterEqual => a >= b,
    }
}

fn compare_strings(a: &str, op: Operator, b: &str) -> bool {
    match op {
        Operator::Equal => a == b,
        Operator::NotEqual => a != b,
        Operator::LessThan => a < b,
        Operator::LessEqual => a <= b,
        Operator::GreaterThan => a > b,
        Operator::GreaterEqual => a >= b,
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
    fn numeric_comparison() {
        let attrs = make_attrs(&[("level", "3")]);

        assert!(evaluate_expression("{level} > 2", &attrs).unwrap());
        assert!(evaluate_expression("{level} >= 3", &attrs).unwrap());
        assert!(evaluate_expression("{level} == 3", &attrs).unwrap());
        assert!(evaluate_expression("{level} != 5", &attrs).unwrap());
        assert!(!evaluate_expression("{level} < 2", &attrs).unwrap());
        assert!(evaluate_expression("{level} <= 3", &attrs).unwrap());
    }

    #[test]
    fn string_comparison() {
        let attrs = make_attrs(&[("env", "production")]);

        assert!(evaluate_expression("{env} == \"production\"", &attrs).unwrap());
        assert!(evaluate_expression("{env} != \"development\"", &attrs).unwrap());
        assert!(evaluate_expression("\"abc\" < \"def\"", &attrs).unwrap());
    }

    #[test]
    fn boolean_comparison() {
        let attrs = make_attrs(&[("enabled", "true")]);

        assert!(evaluate_expression("{enabled} == true", &attrs).unwrap());
        assert!(!evaluate_expression("{enabled} == false", &attrs).unwrap());
    }

    #[test]
    fn nil_comparison() {
        let attrs = make_attrs(&[("set", "value")]);

        // Unset attribute is nil
        assert!(evaluate_expression("{unset} == nil", &attrs).unwrap());
        assert!(evaluate_expression("{set} != nil", &attrs).unwrap());
    }

    #[test]
    fn literal_numbers() {
        let attrs = HashMap::new();

        assert!(evaluate_expression("5 > 3", &attrs).unwrap());
        assert!(evaluate_expression("3.14 >= 3.14", &attrs).unwrap());
        assert!(evaluate_expression("10 == 10", &attrs).unwrap());
    }

    #[test]
    fn quoted_strings() {
        let attrs = HashMap::new();

        assert!(evaluate_expression("\"hello\" == \"hello\"", &attrs).unwrap());
        assert!(evaluate_expression("'foo' != 'bar'", &attrs).unwrap());
    }

    #[test]
    fn invalid_expression() {
        let attrs = HashMap::new();

        assert!(evaluate_expression("no operator here", &attrs).is_err());
    }

    #[test]
    fn cross_type_string_to_number() {
        let attrs = make_attrs(&[("count", "42")]);

        // String "42" coerces to number 42
        assert!(evaluate_expression("{count} == 42", &attrs).unwrap());
        assert!(evaluate_expression("42 == {count}", &attrs).unwrap());
    }

    #[test]
    fn whitespace_handling() {
        let attrs = make_attrs(&[("x", "5")]);

        assert!(evaluate_expression("  {x}  ==  5  ", &attrs).unwrap());
        assert!(evaluate_expression("{x}>3", &attrs).unwrap());
    }
}

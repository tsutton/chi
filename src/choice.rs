//! This module handles the Choice state DSL
//! It's complicated in that either
//! - It's very loosely typed if we represent it as a direct mapping of the JSON
//! - It's annoying to do serde if we represent it with saner, stricter types.
//! I'm choosing the latter: good types but complicated serde.
//! Here, "good types" means types that faithfully represent the functionality.
//! They aren't necessary all that good to work with.

use chrono::{DateTime, FixedOffset};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::cmp::Ordering;

/// ChoiceRule represents a top-level choice expression, which must also have a "Next" field.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct ChoiceRule {
    pub next: String,
    #[serde(flatten)]
    pub choice_expr: ChoiceExpr,
}
/// ChoiceExpr is an expression which can be evaluated against the input to get a boolean.
/// It consists of one basic variant, DataTestExpression, and several composite variants that
/// reference other ChoiceExprs (and/or/not).
/// Or and And are short-circuiting, meaning later elements of their Vecs will not be evaluated
/// if an earlier one can settle the truth value. Thus, later ones won't produce invalid path
/// errors if early ones settle the entire expr.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase", untagged)]
pub enum ChoiceExpr {
    #[serde(rename_all = "PascalCase")]
    And { and: Vec<ChoiceExpr> },

    #[serde(rename_all = "PascalCase")]
    Or { or: Vec<ChoiceExpr> },

    #[serde(rename_all = "PascalCase")]
    Not { not: Box<ChoiceExpr> },

    #[serde(rename_all = "PascalCase")]
    DataTestExpression {
        variable: String,
        #[serde(flatten)]
        expr: DataTestExpression,
    },
}

/// DataTestExpression consists of a pair (comparison operation, thing to compare against) represented
/// as an enum variant ComparisonOperation(TypeToCompareAgainst).
/// It's an enum to capture the different tests supported by the Choice state.
/// The body of each variant is typically the Rust type that corresponds with the JSON type
/// except Number, cause JSON numbers are hard to model correctly.
// TODO(compliance) for _Path DTEs, if the Path fails to match, is that an error or just false?
// TODO - write custom SerDe impls and change this to make the Abs vs Rel and the Op more orthogonal
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub enum DataTestExpression {
    StringEqualsPath(String),
    StringEquals(String),
    StringLessThanPath(String),
    StringLessThan(String),
    StringLessThanEqualsPath(String),
    StringLessThanEquals(String),
    StringGreaterThanPath(String),
    StringGreaterThan(String),
    StringGreaterThanEqualsPath(String),
    StringGreaterThanEquals(String),

    StringMatches(String),

    TimestampEqualsPath(String),
    TimestampEquals(String),
    TimestampLessThanPath(String),
    TimestampLessThan(String),
    TimestampLessThanEqualsPath(String),
    TimestampLessThanEquals(String),
    TimestampGreaterThanPath(String),
    TimestampGreaterThan(String),
    TimestampGreaterThanEqualsPath(String),
    TimestampGreaterThanEquals(String),

    NumericEqualsPath(String),
    NumericEquals(Value),
    NumericLessThanPath(String),
    NumericLessThan(Value),
    NumericLessThanEqualsPath(String),
    NumericLessThanEquals(Value),
    NumericGreaterThanPath(String),
    NumericGreaterThan(Value),
    NumericGreaterThanEqualsPath(String),
    NumericGreaterThanEquals(Value),

    BooleanEquals(bool),
    BooleanEqualsPath(String),

    IsNull(bool),
    IsPresent(bool),
    IsNumeric(bool),
    IsString(bool),
    IsBoolean(bool),
    IsTimestamp(bool),
}

// select_one is a convenience wrapper around jsonpath_lib::select which also errors if the path
// selects more than one value, and returns just that one value on success
fn select_one<'a>(input: &'a Value, path: &str) -> Result<&'a Value, String> {
    let value = jsonpath_lib::select(input, path).map_err(|e| e.to_string())?;
    if value.len() != 1 {
        return Err(format!(
            "Invalid path: path {} selected multiple values",
            path
        ));
    }
    Ok(value[0])
}

// select_string is like select_one, but only for strings, and returns None on both
// error selecting or mismatched type
fn select_string<'a>(input: &'a Value, path: &str) -> Option<&'a str> {
    let value = jsonpath_lib::select(input, path)
        .map_err(|e| e.to_string())
        .ok()?;
    if value.len() != 1 {
        return None;
    }
    match value[0] {
        Value::String(value) => Some(value),
        _ => None,
    }
}

// select_timestamp is like select_one, but only for strings, and returns None on both
// error selecting or mismatched type
fn select_timestamp(input: &Value, path: &str) -> Option<DateTime<FixedOffset>> {
    select_string(input, path).and_then(|s| parse_timestamp(s).ok())
}

// parse_timestamp wraps chrono's RFC3339 parsing, as a placeholder for possibly enforcing
// States' capitalization restrction in the future if I want
// TODO(compliance) enforce capitalization
fn parse_timestamp(s: &str) -> Result<DateTime<chrono::FixedOffset>, String> {
    DateTime::parse_from_rfc3339(s).map_err(|e| format!("parsing timestamp failed: {}", e))
}

// Check if aiven string matches a simplified glob-style patern.
// The glob has special characters '*' and '\', * matches any number of characters, '\*'
// is a literal * in the input, and '\\' is a literal '\' in the input.
// If pattern ends with '\' or contains a '\' followed by someting other than '*' or '\', it is an error
// We do this matching in a sorta bespoke way by splitting the pattern on occurances of *
// then, taking those parts and searching for them in sequence. For example, for the pattern
// r"abc*def\*g*hij", we form the parts: ["abc", "def*", "hij"]. To match against a given input,
// We find the leftmost occurance of "abc", and then find the leftmost occurance of "def*" in the remainder,
// and then find the leftmost occurance of "hij" in the remainder after that. If all these matches succeed,
// the pattern matches.
// An alternate implementation would convert patterns into regexps and used the (highly optimized) regexp crate,
// but that seems more complicated and error prone.
fn string_matches(input: &str, pattern: &str) -> Result<bool, ()> {
    let mut parts: Vec<String> = vec![];
    let mut buf = String::new();
    let mut chars = pattern.chars();
    while let Some(char) = chars.next() {
        match char {
            '\\' => match chars.next() {
                Some('\\') => buf.push('\\'),
                Some('*') => buf.push('*'),
                _ => return Err(()),
            },
            '*' => {
                parts.push(buf);
                buf = String::new();
            }
            char => buf.push(char),
        }
    }
    parts.push(buf);

    let mut remainder: &str = input;
    for (i, part) in parts.iter().enumerate() {
        match remainder.find(part) {
            None => return Ok(false),
            // First part must match at beginning of string
            Some(idx) if i == 0 && idx > 0 => return Ok(false),
            Some(idx) => remainder = &remainder[idx + part.len()..],
        };
    }
    // The last part must match at the end of string, unless the last part is empty.
    // If the past part is empty, we either ended with a *, in which case we don't need to check remainder
    // OR the whole pattern was empty, indicated by parts == vec![""], in which case we do need to check remainder
    // (empty pattern only matches empty string)
    if !remainder.is_empty() && (parts.last().unwrap() != "" || parts.len() == 1) {
        Ok(false)
    } else {
        Ok(true)
    }
}

// Compares two Value as numbers.
// Returns false if either one doesn't convert to f64, or if the f64s aren't comparable
// (f64 NaN shenanigans etc).
fn cmp_numeric_values<F>(lhs: &Value, rhs: &Value, predicate: F) -> bool
where
    F: Fn(Ordering) -> bool,
{
    let lhs = match lhs.as_f64() {
        None => return false,
        Some(v) => v,
    };
    let rhs = match rhs.as_f64() {
        None => return false,
        Some(v) => v,
    };
    let ord = match lhs.partial_cmp(&rhs) {
        None => return false,
        Some(o) => o,
    };
    predicate(ord)
}

impl ChoiceExpr {
    pub fn check(&self, input: &Value) -> Result<bool, String> {
        match &self {
            ChoiceExpr::And { and } => {
                for expr in and {
                    let expr_val = expr.check(input)?;
                    if !expr_val {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            ChoiceExpr::Or { or } => {
                for expr in or {
                    let expr_val = expr.check(input)?;
                    if expr_val {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            ChoiceExpr::Not { not } => {
                let inner = not.check(input)?;
                Ok(!inner)
            }
            ChoiceExpr::DataTestExpression { variable, expr } => {
                let left_hand_value = select_one(input, variable);
                expr.check(input, left_hand_value)
            }
        }
    }
}

impl DataTestExpression {
    fn check(
        &self,
        input: &Value,
        left_hand_selection: Result<&Value, String>,
    ) -> Result<bool, String> {
        if let DataTestExpression::IsPresent(present) = self {
            return Ok(*present == left_hand_selection.is_ok());
        }
        let left_hand_value = left_hand_selection?;
        match self {
            DataTestExpression::StringEqualsPath(right_hand_path) => {
                let right_hand_side = match select_string(input, &right_hand_path) {
                    None => return Ok(false),
                    Some(v) => v,
                };
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.as_str().cmp(right_hand_side);
                Ok(order.is_eq())
            }
            DataTestExpression::StringEquals(right_hand_side) => {
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_eq())
            }
            DataTestExpression::StringLessThanPath(right_hand_path) => {
                let right_hand_side = match select_string(input, &right_hand_path) {
                    None => return Ok(false),
                    Some(v) => v,
                };
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.as_str().cmp(&right_hand_side);
                Ok(order.is_lt())
            }
            DataTestExpression::StringLessThan(right_hand_side) => {
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_lt())
            }
            DataTestExpression::StringLessThanEqualsPath(right_hand_path) => {
                let right_hand_side = match select_string(input, &right_hand_path) {
                    None => return Ok(false),
                    Some(v) => v,
                };
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.as_str().cmp(&right_hand_side);
                Ok(order.is_le())
            }
            DataTestExpression::StringLessThanEquals(right_hand_side) => {
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_le())
            }
            DataTestExpression::StringGreaterThanPath(right_hand_path) => {
                let right_hand_side = match select_string(input, &right_hand_path) {
                    None => return Ok(false),
                    Some(v) => v,
                };
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.as_str().cmp(&right_hand_side);
                Ok(order.is_gt())
            }
            DataTestExpression::StringGreaterThan(right_hand_side) => {
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_gt())
            }
            DataTestExpression::StringGreaterThanEqualsPath(right_hand_path) => {
                let right_hand_side = match select_string(input, &right_hand_path) {
                    None => return Ok(false),
                    Some(v) => v,
                };
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.as_str().cmp(&right_hand_side);
                Ok(order.is_ge())
            }
            DataTestExpression::StringGreaterThanEquals(right_hand_side) => {
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_ge())
            }
            DataTestExpression::StringMatches(matcher) => {
                let left_hand_side = match left_hand_value {
                    Value::String(value) => value,
                    _ => return Ok(false),
                };
                string_matches(left_hand_side, matcher).map_err(|_| {
                    "StringMatches pattern contained improper escape sequence".to_string()
                })
            }
            DataTestExpression::TimestampEqualsPath(right_hand_path) => {
                let right_hand_side = match select_timestamp(input, &right_hand_path) {
                    None => return Ok(false),
                    Some(v) => v,
                };
                let left_hand_side = match left_hand_value
                    .as_str()
                    .and_then(|s| parse_timestamp(s).ok())
                {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_eq())
            }
            DataTestExpression::TimestampEquals(right_hand_string) => {
                let left_hand_side = match left_hand_value
                    .as_str()
                    .and_then(|s| parse_timestamp(s).ok())
                {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let right_hand_side = match parse_timestamp(right_hand_string).ok() {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_eq())
            }
            DataTestExpression::TimestampLessThanPath(right_hand_path) => {
                let right_hand_side = match select_timestamp(input, &right_hand_path) {
                    None => return Ok(false),
                    Some(v) => v,
                };
                let left_hand_side = match left_hand_value
                    .as_str()
                    .and_then(|s| parse_timestamp(s).ok())
                {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_lt())
            }
            DataTestExpression::TimestampLessThan(right_hand_string) => {
                let left_hand_side = match left_hand_value
                    .as_str()
                    .and_then(|s| parse_timestamp(s).ok())
                {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let right_hand_side = match parse_timestamp(right_hand_string).ok() {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_lt())
            }
            DataTestExpression::TimestampLessThanEqualsPath(right_hand_path) => {
                let right_hand_side = match select_timestamp(input, &right_hand_path) {
                    None => return Ok(false),
                    Some(v) => v,
                };
                let left_hand_side = match left_hand_value
                    .as_str()
                    .and_then(|s| parse_timestamp(s).ok())
                {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_le())
            }
            DataTestExpression::TimestampLessThanEquals(right_hand_string) => {
                let left_hand_side = match left_hand_value
                    .as_str()
                    .and_then(|s| parse_timestamp(s).ok())
                {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let right_hand_side = match parse_timestamp(right_hand_string).ok() {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_le())
            }
            DataTestExpression::TimestampGreaterThanPath(right_hand_path) => {
                let right_hand_side = match select_timestamp(input, &right_hand_path) {
                    None => return Ok(false),
                    Some(v) => v,
                };
                let left_hand_side = match left_hand_value
                    .as_str()
                    .and_then(|s| parse_timestamp(s).ok())
                {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_gt())
            }
            DataTestExpression::TimestampGreaterThan(right_hand_string) => {
                let left_hand_side = match left_hand_value
                    .as_str()
                    .and_then(|s| parse_timestamp(s).ok())
                {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let right_hand_side = match parse_timestamp(right_hand_string).ok() {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_gt())
            }
            DataTestExpression::TimestampGreaterThanEqualsPath(right_hand_path) => {
                let right_hand_side = match select_timestamp(input, &right_hand_path) {
                    None => return Ok(false),
                    Some(v) => v,
                };
                let left_hand_side = match left_hand_value
                    .as_str()
                    .and_then(|s| parse_timestamp(s).ok())
                {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_ge())
            }
            DataTestExpression::TimestampGreaterThanEquals(right_hand_string) => {
                let left_hand_side = match left_hand_value
                    .as_str()
                    .and_then(|s| parse_timestamp(s).ok())
                {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let right_hand_side = match parse_timestamp(right_hand_string).ok() {
                    None => return Ok(false),
                    Some(ts) => ts,
                };
                let order = left_hand_side.cmp(&right_hand_side);
                Ok(order.is_ge())
            }
            DataTestExpression::NumericEqualsPath(right_hand_path) => {
                let right_hand_side = match select_one(input, &right_hand_path) {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                Ok(cmp_numeric_values(left_hand_value, right_hand_side, |x| {
                    x.is_eq()
                }))
            }
            DataTestExpression::NumericEquals(right_hand_side) => {
                Ok(cmp_numeric_values(left_hand_value, right_hand_side, |x| {
                    x.is_eq()
                }))
            }
            DataTestExpression::NumericLessThanPath(right_hand_path) => {
                let right_hand_side = match select_one(input, &right_hand_path) {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                Ok(cmp_numeric_values(left_hand_value, right_hand_side, |x| {
                    x.is_lt()
                }))
            }
            DataTestExpression::NumericLessThan(right_hand_side) => {
                Ok(cmp_numeric_values(left_hand_value, right_hand_side, |x| {
                    x.is_lt()
                }))
            }
            DataTestExpression::NumericLessThanEqualsPath(right_hand_path) => {
                let right_hand_side = match select_one(input, &right_hand_path) {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                Ok(cmp_numeric_values(left_hand_value, right_hand_side, |x| {
                    x.is_le()
                }))
            }
            DataTestExpression::NumericLessThanEquals(right_hand_side) => {
                Ok(cmp_numeric_values(left_hand_value, right_hand_side, |x| {
                    x.is_le()
                }))
            }
            DataTestExpression::NumericGreaterThanPath(right_hand_path) => {
                let right_hand_side = match select_one(input, &right_hand_path) {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                Ok(cmp_numeric_values(left_hand_value, right_hand_side, |x| {
                    x.is_gt()
                }))
            }
            DataTestExpression::NumericGreaterThan(right_hand_side) => {
                Ok(cmp_numeric_values(left_hand_value, right_hand_side, |x| {
                    x.is_gt()
                }))
            }
            DataTestExpression::NumericGreaterThanEqualsPath(right_hand_path) => {
                let right_hand_side = match select_one(input, &right_hand_path) {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                Ok(cmp_numeric_values(left_hand_value, right_hand_side, |x| {
                    x.is_ge()
                }))
            }
            DataTestExpression::NumericGreaterThanEquals(right_hand_side) => {
                Ok(cmp_numeric_values(left_hand_value, right_hand_side, |x| {
                    x.is_ge()
                }))
            }
            DataTestExpression::BooleanEquals(right_hand_side) => match left_hand_value {
                Value::Bool(b) => Ok(b == right_hand_side),
                _ => Ok(false),
            },
            DataTestExpression::BooleanEqualsPath(right_hand_path) => {
                let right_hand_side = match select_one(input, &right_hand_path) {
                    Err(_) => return Ok(false),
                    Ok(v) => v,
                };
                match (left_hand_value, right_hand_side) {
                    (Value::Bool(lhs), Value::Bool(rhs)) => Ok(lhs == rhs),
                    _ => Ok(false),
                }
            }
            DataTestExpression::IsNumeric(is_numeric) => {
                Ok(*is_numeric == matches!(left_hand_value, Value::Number(_)))
            }
            DataTestExpression::IsNull(is_null) => {
                Ok(*is_null == matches!(left_hand_value, Value::Null))
            }
            DataTestExpression::IsBoolean(is_bool) => {
                Ok(*is_bool == matches!(left_hand_value, Value::Bool(_)))
            }
            DataTestExpression::IsString(is_string) => {
                Ok(*is_string == matches!(left_hand_value, Value::String(_)))
            }
            DataTestExpression::IsTimestamp(is_ts) => match left_hand_value {
                Value::String(s) => Ok(*is_ts == parse_timestamp(s).is_ok()),
                _ => Ok(false),
            },
            DataTestExpression::IsPresent(_) => unreachable!("IsPresent is handled above"),
        }
    }
}

#[cfg(test)]
mod test {
    use serde_json::{json, Value};

    use super::*;
    struct SerdeTest {
        json_str: &'static str,
        rule: ChoiceRule,
        description: &'static str,
    }

    fn serde_tests() -> Vec<SerdeTest> {
        return vec![
            SerdeTest {
                json_str: r#"{"Not": { "Variable": "$.type", "StringEquals": "Private" }, "Next": "Public"}"#,
                rule: ChoiceRule {
                    next: "Public".to_owned(),
                    choice_expr: ChoiceExpr::Not {
                        not: Box::new(ChoiceExpr::DataTestExpression {
                            variable: "$.type".to_owned(),
                            expr: DataTestExpression::StringEquals("Private".to_owned()),
                        }),
                    },
                },
                description: "Not",
            },
            SerdeTest {
                json_str: r#"{
       "And": [{
          "Variable": "$.value",
          "IsPresent": true
	},
	{
          "Variable": "$.value",
          "IsNumeric": true
        },
        {
          "Variable": "$.value",
          "NumericGreaterThanEquals": 20
        },
        {
          "Variable": "$.value",
          "NumericLessThan": 30
        }
      ],
      "Next": "ValueInTwenties"
    }"#,
                rule: ChoiceRule {
                    next: "ValueInTwenties".to_owned(),
                    choice_expr: ChoiceExpr::And {
                        and: vec![
                            ChoiceExpr::DataTestExpression {
                                variable: "$.value".to_owned(),
                                expr: DataTestExpression::IsPresent(true),
                            },
                            ChoiceExpr::DataTestExpression {
                                variable: "$.value".to_owned(),
                                expr: DataTestExpression::IsNumeric(true),
                            },
                            ChoiceExpr::DataTestExpression {
                                variable: "$.value".to_owned(),
                                expr: DataTestExpression::NumericGreaterThanEquals(json!(20)),
                            },
                            ChoiceExpr::DataTestExpression {
                                variable: "$.value".to_owned(),
                                expr: DataTestExpression::NumericLessThan(json!(30)),
                            },
                        ],
                    },
                },
                description: "And",
            },
            SerdeTest {
                json_str: r#"    {
      "Variable": "$.rating",
      "NumericGreaterThanPath": "$.auditThreshold",
      "Next": "StartAudit"
    }"#,
                rule: ChoiceRule {
                    next: "StartAudit".to_owned(),
                    choice_expr: ChoiceExpr::DataTestExpression {
                        variable: "$.rating".to_owned(),
                        expr: DataTestExpression::NumericGreaterThanPath(
                            "$.auditThreshold".to_owned(),
                        ),
                    },
                },
                description: "Direct comparison",
            },
        ];
    }

    #[test]
    fn deserialize() {
        for test in serde_tests() {
            let result: Result<ChoiceRule, _> = serde_json::from_str(test.json_str);
            match result {
                Err(e) => panic!("For test '{}' got {}", test.description, e),
                Ok(o) => assert_eq!(o, test.rule, "Mismatch on test {}", test.description),
            }
        }
    }

    #[test]
    fn serialize() {
        for test in serde_tests() {
            let result = serde_json::to_value(&test.rule);
            match result {
                Err(e) => panic!("For test '{}' got {}", test.description, e),
                Ok(o) => {
                    let val: Value = serde_json::from_str(test.json_str).unwrap();
                    assert_eq!(o, val, "Mismatch on test '{}'", test.description)
                }
            }
        }
    }

    struct ChoiceTest {
        description: &'static str,
        input: Value,
        expected: Option<bool>,
    }

    fn choice_test(expr: &ChoiceExpr, test: &ChoiceTest) {
        let result = expr.check(&test.input);
        match &test.expected {
            None => assert!(
                result.is_err(),
                "Expected err for {}, got {:?}",
                test.description,
                test.expected
            ),
            Some(expected) => {
                assert!(
                    result.is_ok(),
                    "Expected non-err for {}, got {:?}",
                    test.description,
                    expected
                );
                assert_eq!(result.unwrap(), *expected);
            }
        }
    }

    #[test]
    fn and_expr() {
        let expr = ChoiceExpr::And {
            and: vec![
                ChoiceExpr::DataTestExpression {
                    variable: "$.value".to_owned(),
                    expr: DataTestExpression::IsPresent(true),
                },
                ChoiceExpr::DataTestExpression {
                    variable: "$.value".to_owned(),
                    expr: DataTestExpression::IsNumeric(true),
                },
                ChoiceExpr::DataTestExpression {
                    variable: "$.value".to_owned(),
                    expr: DataTestExpression::NumericGreaterThanEquals(json!(20)),
                },
                ChoiceExpr::DataTestExpression {
                    variable: "$.value".to_owned(),
                    expr: DataTestExpression::NumericLessThan(json!(30)),
                },
            ],
        };

        let mut tests = vec![
            ChoiceTest {
                description: "First check fails",
                input: json!({}),
                expected: Some(false),
            },
            ChoiceTest {
                description: "Second check fails",
                input: json!({"value": "asdf"}),
                expected: Some(false),
            },
            ChoiceTest {
                description: "Third check fails",
                input: json!({"value": 10}),
                expected: Some(false),
            },
            ChoiceTest {
                description: "Fourth check fails",
                input: json!({"value": 30}),
                expected: Some(false),
            },
            ChoiceTest {
                description: "All succeed fails",
                input: json!({"value": 25}),
                expected: Some(true),
            },
        ];

        for test in tests.iter() {
            choice_test(&expr, test);
        }

        // confirm short circuiting but pushing an invalid to the end of the And and running all but the
        // successful one again
        let expr = match expr {
            ChoiceExpr::And { mut and } => {
                and.push(ChoiceExpr::DataTestExpression {
                    variable: "$.doesnt_exist".to_string(),
                    expr: DataTestExpression::StringEquals("value".to_string()),
                });
                ChoiceExpr::And { and }
            }
            _ => unreachable!(),
        };
        let last = tests.len() - 1;
        for test in tests[..last].iter() {
            choice_test(&expr, &test);
        }
        // run the last test again and make sure it fails this time.
        tests[last].expected = None;
        choice_test(&expr, &tests[last]);
    }

    // TODO detailed tests for each DataTestExpression

    #[test]
    fn test_string_matches() {
        struct TestCase {
            description: &'static str,
            input: &'static str,
            pattern: &'static str,
            expected_output: Result<bool, ()>,
        }
        let cases = vec![
            TestCase {
                description: "empty pattern matches empty string",
                input: "",
                pattern: "",
                expected_output: Ok(true),
            },
            TestCase {
                description: "empty pattern doesn't match non-empty string",
                input: "asdf",
                pattern: "",
                expected_output: Ok(false),
            },
            TestCase {
                description: "* matches empty string",
                input: "",
                pattern: "*",
                expected_output: Ok(true),
            },
            TestCase {
                description: "* matches non-empty string",
                input: "asdf",
                pattern: "*",
                expected_output: Ok(true),
            },
            TestCase {
                description: "** matches non-empty string",
                input: "",
                pattern: "**",
                expected_output: Ok(true),
            },
            TestCase {
                description: "** matches non-empty string",
                input: "asdf",
                pattern: "**",
                expected_output: Ok(true),
            },
            TestCase {
                description: "leading * matches 0 chars",
                input: "asdf",
                pattern: "*asdf",
                expected_output: Ok(true),
            },
            TestCase {
                description: "leading * matches some chars",
                input: "...asdf",
                pattern: "*asdf",
                expected_output: Ok(true),
            },
            TestCase {
                description: "trailing * matches 0 chars",
                input: "asdf",
                pattern: "asdf*",
                expected_output: Ok(true),
            },
            TestCase {
                description: "trailing * matches some chars",
                input: "asdf...",
                pattern: "asdf*",
                expected_output: Ok(true),
            },
            TestCase {
                description: "interior * matches no chars",
                input: "asdf",
                pattern: "as*df",
                expected_output: Ok(true),
            },
            TestCase {
                description: "interior * matches some chars",
                input: "as...df",
                pattern: "as*df",
                expected_output: Ok(true),
            },
            TestCase {
                description: "escape sequences work",
                input: r"as*\df",
                pattern: r"as\*\\df",
                expected_output: Ok(true),
            },
            TestCase {
                description: "bad escape sequence error",
                input: r"asdf",
                pattern: r"as\df",
                expected_output: Err(()),
            },
            TestCase {
                description: r"trailing \ sequence error",
                input: r"asdf",
                pattern: r"asdf\",
                expected_output: Err(()),
            },
            TestCase {
                description: r"complex true match with lead and end * works ",
                input: r"qwertyuiop",
                pattern: r"*q*w*uio*",
                expected_output: Ok(true),
            },
            TestCase {
                description: r"complex false match with lead and end * works",
                input: r"qwertyuiop",
                pattern: r"*q*w*uip*",
                expected_output: Ok(false),
            },
            TestCase {
                description: r"complex true match works ",
                input: r"qwertyuiop",
                pattern: r"q*w*uio*p",
                expected_output: Ok(true),
            },
            TestCase {
                description: r"complex false match with lead and end * works",
                input: r"qwertyuiop",
                pattern: r"we*uio",
                expected_output: Ok(false),
            },
        ];
        for case in cases {
            assert_eq!(
                string_matches(case.input, case.pattern),
                case.expected_output,
                "{}",
                case.description,
            );
        }
    }
}

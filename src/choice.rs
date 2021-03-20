//! This module handles the Choice state DSL
//! It's complicated in that either
//! - It's very loosely typed if we represent it as a direct mapping of the JSON
//! - It's annoying to do serde if we represent it with saner, stricter types.
//! I'm choosing the latter: good types but complicated serde.
//! Here, "good types" means types that faithfully represent the functionality.
//! They aren't necessary all that good to work with.

use std::cmp::Ordering;

use serde::{Deserialize, Serialize};
use serde_json::Value;

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
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub enum DataTestExpression {
    StringEquals(String),
    // StringEqualsPath(String),
    // StringLessThan(String),
    // StringLessThanPath(String),
    // StringGreaterThan(String),
    // StringGreaterThanPath(String),
    // StringLessThanEquals(String),
    // StringLessThanEqualsPath(String),
    // StringGreaterThanEquals(String),
    // StringGreaterThanEqualsPath(String),
    // StringMatches(String),
    // NumericEquals(String),
    // NumericEqualsPath(String),
    NumericLessThan(Value),
    // NumericLessThanPath(String),
    // NumericGreaterThan(String),
    NumericGreaterThanPath(String),
    //NumericLessThanEquals(Value), // maybe should be serde_json::Value::Number?
    // NumericLessThanEqualsPath(String),
    NumericGreaterThanEquals(Value),
    NumericGreaterThanEqualsPath(String),
    // BooleanEquals(bool),
    // BooleanEqualsPath(String),
    // TimestampEquals(String),
    // TimestampEqualsPath(String),
    // TimestampLessThan(String),
    // TimestampLessThanPath(String),
    // TimestampGreaterThan(String),
    // TimestampGreaterThanPath(String),
    // TimestampLessThanEquals(String),
    // TimestampLessThanEqualsPath(String),
    // TimestampGreaterThanEquals(String),
    // TimestampGreaterThanEqualsPath(String),
    // IsNull(bool),
    IsPresent(bool),
    IsNumeric(bool),
    // IsString(bool),
    // IsBoolean(bool),
    // IsTimestamp(bool),
}

mod util {

    use serde_json::Value;

    // select_one is a convenience wrapper around jsonpath_lib::select which also errors if the path
    // selects more than one value, and returns just that one value on success
    pub fn select_one<'a>(input: &'a Value, path: &str) -> Result<&'a Value, String> {
        let value = jsonpath_lib::select(input, path).map_err(|e| e.to_string())?;
        if value.len() != 1 {
            return Err(format!(
                "Invalid path: path {} selected multiple values",
                path
            ));
        }
        Ok(value[0])
    }

    use std::cmp::Ordering;

    use serde_json::Number;
    // serde_json doesn't impl Ord for Number, but I'd quite like it for choice comparison
    // TODO I'm assuming numbers are EITHER i64 or f64. Is that good enough?
    // also doesn't handle infs and NaNs with proper errors
    // TODO better error type
    pub fn number_cmp(n1: &Number, n2: &Number) -> Result<Ordering, String> {
        if let Some(n1) = n1.as_i64() {
            if let Some(n2) = n2.as_i64() {
                Ok(n1.cmp(&n2))
            } else if let Some(_n1) = n2.as_f64() {
                todo!()
            } else {
                Err(format!("{} doesn't fit into i64 or f64", n2))
            }
        } else if let Some(_n1) = n1.as_f64() {
            todo!()
        } else {
            Err(format!("{} doesn't fit into i64 or f64", n1))
        }
    }
}
use util::*;

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
            ChoiceExpr::DataTestExpression { variable, expr } => match expr {
                DataTestExpression::StringEquals(s) => match select_one(input, variable)? {
                    Value::String(value) if s == value => Ok(true),
                    _ => Ok(false),
                },
                DataTestExpression::IsPresent(present) => {
                    Ok(*present == select_one(input, variable).is_ok())
                }
                DataTestExpression::IsNumeric(numeric) => {
                    let value = select_one(input, variable)?;
                    if let Value::Number(_) = value {
                        Ok(*numeric)
                    } else {
                        Ok(!numeric)
                    }
                }
                DataTestExpression::NumericGreaterThanEquals(predicate) => match predicate {
                    Value::Number(predicate) => {
                        let value = select_one(input, variable)?;
                        if let Value::Number(value) = value {
                            let cmp = number_cmp(value, predicate)?;
                            Ok(cmp == Ordering::Greater || cmp == Ordering::Equal)
                        } else {
                            Ok(false)
                        }
                    }
                    _ => Err("Predicate for Numeric comparison is not a number".to_string()),
                },
                DataTestExpression::NumericLessThan(predicate) => match predicate {
                    Value::Number(predicate) => {
                        let value = select_one(input, variable)?;
                        if let Value::Number(value) = value {
                            Ok(number_cmp(value, predicate)? == Ordering::Less)
                        } else {
                            Ok(false)
                        }
                    }
                    _ => Err("Predicate for Numeric comparison is not a number".to_string()),
                },
                _ => {
                    todo!()
                } // DataTestExpression::NumericLessThan(_) => {}
                  // DataTestExpression::NumericGreaterThanPath(_) => {}
                  // DataTestExpression::NumericGreaterThanEqualsPath(_) => {}
            },
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
}

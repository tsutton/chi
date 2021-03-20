use serde_json::{json, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StateIOError {
    PathFailure,
}

/// InputPath has *three* cases
/// - Not provided (None) => defaults to forwarding the input as-is
/// - Value::Null: the input will be the empty object {}
/// - Value::String(s): s will be used a JSON Path.
// TODO should this be an alias for ease of use? or a newtpe for safety?
pub type InputPath = Option<Value>;

pub fn apply_input_path(input_path: &InputPath, input: &Value) -> Result<Value, StateIOError> {
    match input_path {
        None => Ok(input.clone()),
        Some(Value::Null) => Ok(json!({})),
        Some(Value::String(s)) => {
            let r = jsonpath_lib::select(input, &s);
            match r {
                Err(_) => Err(StateIOError::PathFailure),
                Ok(v) if v.len() == 1 => Ok(v[0].clone()),
                Ok(_) => Err(StateIOError::PathFailure),
            }
        }
        _ => Err(StateIOError::PathFailure),
    }
}

// Parameters has two cases:
// - Not provided (None) => defaults to forwarding the input as-is
// - Any other JSON Value: treated as a template
//   It is not possible to use a JSON Value to forward the input as-is, e.g. json!("$") will result in a final value of a string "$"
pub type Parameters = Option<Value>;

// TODO best way to share code with ResultsSelector, which is also a template?
// maybe number of dollar signs => vec of template inputs
pub fn apply_parameters(
    parameters: &Parameters,
    input: &Value,
    context: &Value,
) -> Result<Value, StateIOError> {
    match parameters {
        None => Ok(input.clone()),
        Some(parameters) => {
            // Recursive decent into parameters. using the value of fields ending in $ as templates
            // those fields either begin with $$; strip one $ and consider it a path applied to context
            // or else it begins with $ (and not $$); apply it as a path to input
            // or else it is an intrinsic function WIP
            match parameters {
                Value::Object(o) => {
                    let mut result = serde_json::Map::new();
                    for (field, value) in o {
                        if !field.ends_with(".$") {
                            // If value is an object, go deeper with recursion
                            let new_value = match value {
                                Value::Object(_) => {
                                    apply_parameters(&Some(value.clone()), input, context)?
                                }
                                _ => value.clone(),
                            };
                            // TODO handle duplicate keys
                            result.insert(field.clone(), new_value);
                            continue;
                        }
                        let field = field.strip_suffix(".$").unwrap().to_owned();
                        let value = value.as_str().ok_or(StateIOError::PathFailure)?;
                        let transformed_value = if value.starts_with("$$") {
                            let selector = value.strip_prefix('$').unwrap();
                            match jsonpath_lib::select(context, selector) {
                                Ok(selected) if selected.len() == 1 => selected[0].clone(),
                                _ => return Err(StateIOError::PathFailure),
                            }
                        } else if value.starts_with('$') {
                            match jsonpath_lib::select(input, value) {
                                Ok(selected) if selected.len() == 1 => selected[0].clone(),
                                _ => return Err(StateIOError::PathFailure),
                            }
                        } else {
                            todo!("intrinsic functions")
                        };
                        // TODO handle duplicate keys
                        result.insert(field, transformed_value);
                    }
                    Ok(Value::Object(result))
                }
                x => Ok(x.clone()),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use serde_json::Value;

    use super::*;

    #[test]
    fn input_path() {
        struct Test {
            input: Value,
            path: InputPath,
            expected: Result<Value, StateIOError>,
            description: &'static str,
        }

        let tests = vec![
            // Tests on paths that should work
            Test {
                input: json!("asdf"),
                path: None,
                expected: Ok(json!("asdf")),
                description: "None selects the input unaltered",
            },
            Test {
                input: json!("asdf"),
                path: Some(json!("$")),
                expected: Ok(json!("asdf")),
                description: "$ selects the input unaltered",
            },
            Test {
                input: json!("asdf"),
                path: Some(json!(null)),
                expected: Ok(json!({})),
                description: "null results in empty object",
            },
            Test {
                input: json!({"key": {"inner_key": "inner_value"}}),
                path: Some(json!("$.key")),
                expected: Ok(json!({"inner_key": "inner_value"})),
                description: "path returning an object",
            },
            Test {
                input: json!({"key": {"inner_key": "inner_value"}}),
                path: Some(json!("$.key.inner_key")),
                expected: Ok(json!("inner_value")),
                description: "nested path works",
            },
            // Tests on paths that should fail
            Test {
                input: json!({"key": {"inner_key": "inner_value"}}),
                path: Some(json!("$.doesnt_exist")),
                expected: Err(StateIOError::PathFailure),
                description: "key that doesn't exist fails",
            },
            Test {
                input: json!({"key": {"inner_key": "inner_value"}}),
                path: Some(json!("$.key.inner_key.doesnt_exit")),
                expected: Err(StateIOError::PathFailure),
                description: "looking up non-object fails",
            },
        ];

        for test in tests {
            let result = apply_input_path(&test.path, &test.input);
            assert_eq!(result, test.expected, "{}", test.description);
        }
    }

    #[test]
    fn parameters() {
        struct Test {
            input: Value,
            context: Value,
            path: Parameters,
            expected: Result<Value, StateIOError>,
            description: &'static str,
        }

        let tests = vec![
            // Tests on paths that should work
            Test {
                input: json!("asdf"),
                context: json!("fdsa"),
                path: None,
                expected: Ok(json!("asdf")),
                description: "None selects the input unaltered",
            },
            Test {
                input: json!("asdf"),
                context: json!("fdsa"),
                path: Some(Value::Null),
                expected: Ok(Value::Null),
                description: "Null is treated as a proper literal",
            },
            Test {
                input: json!("asdf"),
                context: json!("fdsa"),
                path: Some(json!({
                    "literal": "lit_value",
                    "from_input.$" : "$",
                    "from_context.$" : "$$",
                })),
                expected: Ok(json!({
                    "literal": "lit_value",
                    "from_input" : "asdf",
                    "from_context" : "fdsa",
                })),
                description: "simple combination of literal, input, and context",
            },
            Test {
                input: json!("asdf"),
                context: json!("fdsa"),
                #[rustfmt::skip]
                path: Some(json!({
                    "nesting": {
			"from_input.$" : "$",
                    }
		})),
                #[rustfmt::skip]
                expected: Ok(json!({
                    "nesting": {
			"from_input" : "asdf",
                    }
                })),
                description: "templates work when nested",
            },
        ];

        for test in tests {
            let result = apply_parameters(&test.path, &test.input, &test.context);
            assert_eq!(result, test.expected, "{}", test.description);
        }
    }
}

use reference_path::ReferencePath;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub struct Error {
    pub cause: String,
    pub error: String,
}

/// Path has two cases
/// - None i.e. not provided => defaults to forwarding the input as-is
/// - Some(s) => s will be used a JSON Path.
// TODO should this be an alias for ease of use? or a newtype for safety?
// Newtype would allow more control, e.g. implementing Default or moving Strings that are not valid
// paths earlier (which is desired).
pub type Path = Option<String>;

pub mod reference_path;

// TODO shifting this to apply_path instead of apply_input_path obscures error messages
pub fn apply_path(input_path: &Path, input: &Value) -> Result<Value, Error> {
    match input_path {
        None => Ok(json!({})),
        Some(s) => {
            let r = jsonpath_lib::select(input, &s);
            match r {
                Err(_) => Err(Error {
                    error: "States.Runtime".to_owned(),
                    cause: "failed to apply input path".to_owned(),
                }),
                Ok(v) if v.len() == 1 => Ok(v[0].clone()),
                Ok(v) if v.is_empty() => Err(Error {
                    error: "States.Runtime".to_owned(),
                    cause: "failed to apply input path".to_owned(),
                }),
                Ok(v) => Ok(v.into_iter().cloned().collect()),
            }
        }
    }
}

/// Parameters has two cases:
/// - Not provided (None) => defaults to forwarding the input as-is
/// - Any other JSON Value: treated as a template
///   It is not possible to use a JSON Value to forward the input as-is, e.g. json!("$") will result in a final value of a string "$"
pub type Template = Option<Value>;

// TODO best way to share code with ResultsSelector, which is also a template?
// maybe number of dollar signs => vec of template inputs
pub fn apply_parameters(
    parameters: &Template,
    input: &Value,
    context: &Value,
) -> Result<Value, Error> {
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
                        let value = value.as_str().ok_or(Error {
                            error: "States.ParameterPathFailure".to_owned(),
                            cause: "value for template field ending in .$ was not of type string"
                                .to_owned(),
                        })?;
                        let transformed_value = if value.starts_with("$$") {
                            let selector = value.strip_prefix('$').unwrap();
                            match jsonpath_lib::select(context, selector) {
                                Ok(selected) if selected.len() == 1 => selected[0].clone(),
                                Ok(selected) => {
                                    Value::Array(selected.into_iter().cloned().collect())
                                }
                                Err(_) => {
                                    return Err(Error {
                                        error: "States.ParameterPathFailure".to_owned(),
                                        cause: "Applying path failed".to_owned(),
                                    })
                                }
                            }
                        } else if value.starts_with('$') {
                            match jsonpath_lib::select(input, value) {
                                Ok(selected) if selected.len() == 1 => selected[0].clone(),
                                _ => {
                                    return Err(Error {
                                        error: "States.ParameterPathFailure".to_owned(),
                                        cause: "Applying path failed".to_owned(),
                                    });
                                }
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

pub fn apply_result_path(
    input: &Value,
    output: &Value,
    path: &Option<ReferencePath>,
) -> Result<Value, Error> {
    match path {
        None => Ok(input.clone()),
        Some(s) => {
            let mut final_output = input.clone();
            s.insert(&mut final_output, output.clone())
                .and(Ok(final_output))
                .map_err(|_| Error {
                    error: "TODO".to_owned(),
                    cause: "TODO".to_owned(),
                })
        }
    }
}

// TODO implement and combine with apply_parameters
fn apply_results_selector(output: &Value, selector: &Template) -> Result<Value, Error> {
    match selector {
        None => Ok(output.to_owned()),
        Some(_) => todo!(),
    }
}

/// For types whose default when deserializing isn't their Default::default()
/// we provide a fn that can still be used as #[serde(default="..")]
pub fn default_input_path() -> Path {
    Some("$".to_owned())
}

/// For types whose default when deserializing isn't their Default::default()
/// we provide a fn that can still be used as #[serde(default="..")]
pub fn default_output_path() -> Path {
    Some("$".to_owned())
}

/// For types whose default when deserializing isn't their Default::default()
/// we provide a fn that can still be used as #[serde(default="..")]
pub fn default_result_path() -> Option<ReferencePath> {
    Some(ReferencePath::default())
}

pub trait StateIo {
    // None corresponds to JSON null i.e. discard input and use `{}`
    fn input_path(&self) -> Path {
        default_input_path()
    }
    // Special value None means use the input as-is
    fn parameters(&self) -> Template {
        None
    }

    // Special value None means use the effective output as-is
    fn results_selector(&self) -> Template {
        None
    }

    // None means discard the output, effective output is the input
    fn result_path(&self) -> Option<ReferencePath> {
        default_result_path()
    }

    // None corresponds to JSON null i.e. discard input and output and use `{}`
    fn output_path(&self) -> Path {
        default_output_path()
    }

    fn effective_input(&self, input: &Value, context: &Value) -> Result<Value, Error> {
        apply_path(&self.input_path(), input).and_then(|after_path_input| {
            apply_parameters(&self.parameters(), &after_path_input, context)
        })
    }

    fn effective_output(&self, input: &Value, output: &Value) -> Result<Value, Error> {
        apply_results_selector(&output, &self.results_selector())
            .and_then(|effective_result| {
                apply_result_path(&input, &effective_result, &self.result_path())
            })
            .and_then(|combined_output| apply_path(&self.output_path(), &combined_output))
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
            path: Path,
            expected: Result<Value, Error>,
            description: &'static str,
        }

        let tests = vec![
            // Tests on paths that should work
            Test {
                input: json!("asdf"),
                path: Some("$".to_owned()),
                expected: Ok(json!("asdf")),
                description: "$ selects the input unaltered",
            },
            Test {
                input: json!("asdf"),
                path: None,
                expected: Ok(json!({})),
                description: "null results in empty object",
            },
            Test {
                input: json!({"key": {"inner_key": "inner_value"}}),
                path: Some("$.key".to_owned()),
                expected: Ok(json!({"inner_key": "inner_value"})),
                description: "path returning an object",
            },
            Test {
                input: json!({"key": {"inner_key": "inner_value"}}),
                path: Some("$.key.inner_key".to_owned()),
                expected: Ok(json!("inner_value")),
                description: "nested path works",
            },
            // Tests on paths that should fail
            Test {
                input: json!({"key": {"inner_key": "inner_value"}}),
                path: Some("$.doesnt_exist".to_owned()),
                expected: Err(Error {
                    error: "States.Runtime".to_owned(),
                    cause: "failed to apply input path".to_owned(),
                }),
                description: "key that doesn't exist fails",
            },
            Test {
                input: json!({"key": {"inner_key": "inner_value"}}),
                path: Some("$.key.inner_key.doesnt_exit".to_owned()),
                expected: Err(Error {
                    error: "States.Runtime".to_owned(),
                    cause: "failed to apply input path".to_owned(),
                }),
                description: "looking up non-object fails",
            },
        ];

        for test in tests {
            let result = apply_path(&test.path, &test.input);
            assert_eq!(result, test.expected, "{}", test.description);
        }
    }

    #[test]
    fn parameters() {
        struct Test {
            input: Value,
            context: Value,
            path: Template,
            expected: Result<Value, Error>,
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

    #[test]
    fn results_path() {
        struct Test {
            input: Value,
            path: Option<ReferencePath>,
            result: Value,
            expected: Result<Value, Error>,
            description: &'static str,
        }

        let tests = vec![
            // Tests on paths that should work
            Test {
                input: json!({}),
                path: None,
                result: json!("hello"),
                expected: Ok(json!({})),
                description: "none ignores the result",
            },
            Test {
                input: json!({}),
                path: ReferencePath::compile("$").ok(),
                result: json!("hello"),
                expected: Ok(json!("hello")),
                description: "$ replaces the input",
            },
            Test {
                input: json!({"master": {"detail": [1,2,3]}}),
                path: ReferencePath::compile("$.master.detail").ok(),
                result: json!(6),
                expected: Ok(json!({"master": {"detail": 6}})),
                description: "Non-trivial path on existing place in object",
            },
            Test {
                input: json!({"master": {"detail": [1,2,3]}}),
                path: ReferencePath::compile("$.master.result.sum").ok(),
                result: json!(6),
                expected: Ok(json!({"master": {"detail": [1,2,3], "result": {"sum": 6}}})),
                description: "Non-trivial path on new place",
            },
            Test {
                input: json!({"master": {"detail": [1,2,3]}}),
                path: ReferencePath::compile("$.master.detail[0]").ok(),
                result: json!(6),
                expected: Ok(json!({"master": {"detail": [6,2,3]}})),
                description: "Non-trivial path on new place in array",
            },
        ];

        for test in tests {
            let result = apply_result_path(&test.input, &test.result, &test.path);
            assert_eq!(result, test.expected, "{}", test.description);
        }
    }
}

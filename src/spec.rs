use std::collections::HashMap;

use serde_json::{json, Value};

use serde::{Deserialize, Serialize};

use crate::mock::{self, MockResource};
use crate::{Execution, StateMachine};

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
#[serde(rename_all = "PascalCase")]
pub struct Specs {
    specs: Vec<Spec>,
}

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
#[serde(rename_all = "PascalCase")]
pub struct Spec {
    resources: HashMap<String, Resource>,
    input: Value,
    expected: EndState,
}

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
#[serde(rename_all = "PascalCase")]
pub enum EndState {
    Success { output: Value },
    Failure { error: String, cause: String },
}

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
#[serde(rename_all = "PascalCase")]
pub enum Resource {
    Constant(Value),
    Match(Match),
    Jq(String),
}

impl Resource {
    pub fn into_mock_resource(self) -> Box<dyn MockResource> {
        match self {
            Resource::Constant(v) => mock::constant(v),
            Resource::Jq(filter) => mock::function(move |input| {
                jq::run(&filter, input).map_err(|e| mock::Error {
                    cause: e.to_string(),
                    error: "Runtime.jq".to_string(),
                })
            }),
            Resource::Match(m) => mock::function(move |input| {
                for case in &m.cases {
                    let out = jq::run(&case.predicate, input).map_err(|e| mock::Error {
                        cause: e.to_string(),
                        error: "Runtime.jq".to_string(),
                    })?;
                    if let Value::Bool(true) = out {
                        return case.function.clone().into_mock_resource().execute(input);
                    }
                }
                m.default.clone().into_mock_resource().execute(input)
            }),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
#[serde(rename_all = "PascalCase")]
pub struct Match {
    cases: Vec<MatchCase>,
    default: Box<Resource>,
}

#[derive(Debug, PartialEq, Eq, Deserialize, Serialize, Clone)]
#[serde(rename_all = "PascalCase")]
pub struct MatchCase {
    predicate: String,
    function: Box<Resource>,
}

impl Spec {
    pub fn run(&self, machine: &StateMachine) -> Result<(), String> {
        let resources = self
            .resources
            .iter()
            .map(|(k, v)| (k.clone(), v.clone().into_mock_resource()))
            .collect();
        let mut execution = Execution::new(machine, resources, &self.input);
        match (&execution.run(), &self.expected) {
            // passing cases:
            (Ok(got), EndState::Success { output }) if output == got => Ok(()),
            (Err(v), EndState::Failure { error, cause })
                if v["error"] == json!(error) && v["cause"] == json!(cause) =>
            {
                Ok(())
            }
            // failing cases
            (Ok(got), EndState::Success { output }) => Err(
                // TODO better JSON diffs
                format!(
                    "expected success with output {}, got success with output {}",
                    output, got,
                ),
            ),
            (Ok(got), EndState::Failure { .. }) => {
                Err(format!("expected failure, got success with output {}", got,))
            }
            (Err(v), EndState::Success { .. }) => {
                Err(format!("expected success, got failure with output {}", v))
            }
            (Err(v), EndState::Failure { error, cause }) =>            {
		Err(format!("expected failure with error='{}' and cause='{}, got failure with error='{}' and cause='{}'",
			    v["error"], v["cause"],
			    error, cause
		))
            }
        }
    }
}

mod jq {
    use serde_json::Value;
    use std::process::{Command, Stdio};

    #[derive(Debug)]
    pub struct JqError {
        error: String,
    }
    impl std::fmt::Display for JqError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "running JQ: {}", self.error)
        }
    }

    impl std::error::Error for JqError {}

    pub fn run(filter: &str, input: &Value) -> Result<Value, Box<dyn std::error::Error>> {
        let echo = Command::new("echo")
            .arg(input.to_string())
            .stdout(Stdio::piped())
            .spawn()
            .map_err(|e| JqError {
                error: format!("spawning echo into jq: {}", e),
            })?;

        let echo_out = echo.stdout.ok_or_else(|| JqError {
            error: "opening echo's stdout".to_string(),
        })?;

        let output = Command::new("jq").arg(filter).stdin(echo_out).output()?;

        if !output.status.success() {
            return Err(Box::new(JqError {
                error: "jq did not exit successfully".to_string(),
            }));
        }

        let v = serde_json::from_slice(&output.stdout)?;
        Ok(v)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn spec_with_expected(expected: EndState) -> Spec {
        Spec {
            resources: [
                ("init".to_string(), Resource::Constant(json!({"count": 0}))),
                (
                    "incr".to_string(),
                    Resource::Match(Match {
                        cases: vec![MatchCase {
                            predicate: ".count == 0".to_string(),
                            function: Box::new(Resource::Constant(json!({"count": 1}))),
                        }],
                        default: Box::new(Resource::Jq(
                            r#"setpath(["count"]; .count + 1)"#.to_owned(),
                        )),
                    }),
                ),
            ]
            .iter()
            .cloned()
            .collect(),
            input: json!({}),
            expected,
        }
    }

    #[test]
    fn maintest() {
        let machine_str = r#"
    {
      "StartAt": "choice",
      "States": {
        "choice": {
          "Type": "Choice",
          "Choices": [
            { "Variable": "$.count", "IsPresent": false, "Next": "init"},
            { "Variable": "$.count", "NumericLessThan": 3, "Next": "incr"}
          ],
          "Default": "final"
        },
        "incr": {"Type": "Task", "Resource": "incr", "Next": "choice"},
        "init": {"Type": "Task", "Resource": "init", "Next": "choice"},
        "final": {"Type": "Succeed"}
      }
    }"#;
        let machine: StateMachine = serde_json::from_str(machine_str).unwrap();

        let spec = spec_with_expected(EndState::Success {
            output: json!({"count": 3}),
        });
        let result = spec.run(&machine);
        assert!(result.is_ok());

        let spec = spec_with_expected(EndState::Success {
            output: json!({"count": 2}),
        });
        let result = spec.run(&machine);
        println!("{:?}", &result);
        assert!(result.is_err());

        let spec = spec_with_expected(EndState::Failure {
            error: "err".to_string(),
            cause: "cause".to_string(),
        });
        let result = spec.run(&machine);
        println!("{:?}", &result);
        assert!(result.is_err());
    }
}

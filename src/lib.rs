// #![warn(missing_debug_implementations, rust_2018_idioms)]
// #![warn(missing_docs)]
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;
// use thiserror::Error;

pub mod mock;
use mock::MockResource;

pub mod io;

pub mod choice;
pub use choice::*;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct StateMachine {
    pub start_at: String,
    pub states: HashMap<String, State>,
    pub comment: Option<String>,
    pub version: Option<String>,

    // The spec doesn't specify the maximum allowable value, but it's probably less than 2^32 second or ~136 years
    pub timeout_seconds: Option<u32>,
}

// #[derive(Debug, Error)]
// TODO implement debug (must be manual due to Box<dyn ..>)
pub struct Execution {
    machine: StateMachine,
    // events: Vec<ExecutionEvent>,
    resources: HashMap<String, Box<dyn MockResource>>,
    state: ExecutionState,
}

/// ExecutionState holds the information needed to advance an Execution by one step.
/// Usually, that consists of the name of the next state, and the input to pass that state.
/// Or, the machine could have Succeeded or Failed with some output.
/// Lastly, but most complicatedly, we could have several sub-executions ongoing, with Parallel and Map states.
pub enum ExecutionState {
    Succeeded {
        output: Value,
    },
    Failed {
        error: String,
        cause: String,
    },
    ExecuteState {
        state_name: String,
        input: Value,
    },
    AdvanceNestedState {
        state_name: String,
        executions: Vec<Execution>,
    },
}

impl Execution {
    pub fn new(
        machine: &StateMachine,
        resources: HashMap<String, Box<dyn MockResource>>,
        input: &Value,
    ) -> Execution {
        Execution {
            machine: machine.clone(),
            // events: Vec::new(),
            resources,
            state: ExecutionState::ExecuteState {
                state_name: machine.start_at.clone(),
                input: input.clone(),
            },
        }
    }

    /// step advances the execution by a little bit and returns true if the execution has ended.
    /// Generally this involves running one state. When a Parallel or Map state is active, instead one
    /// state within the innermost nesting is executed.
    /// No guarentees are made regarding the order of execution of the branches within a parallel or map.
    pub fn step(&mut self) -> bool {
        match &self.state {
            ExecutionState::Succeeded { .. } => true,
            ExecutionState::Failed { .. } => true,
            ExecutionState::ExecuteState { state_name, input } => {
                let state = self
                    .machine
                    .states
                    .get(state_name)
                    .unwrap_or_else(|| panic!("missing state for {}", state_name));
                match state {
                    State::Task(task) => {
                        let resource = self
                            .resources
                            .get_mut(&task.resource)
                            .unwrap_or_else(|| panic!("missing resource for {}", &task.resource));
                        let input = match io::apply_input_path(&task.input_path, input) {
                            Ok(i) => i,
                            Err(_) => {
                                self.state = ExecutionState::Failed {
                                    error: "States.Runtime".to_owned(),
                                    cause: "failed to apply input path".to_owned(),
                                };
                                return true;
                            }
                        };
                        match resource.execute(&input) {
                            Ok(output) => {
                                // self.events.push(ExecutionEvent::TaskStateSucceeded {
                                //     state_name: state_name.to_owned(),
                                //     input: input.clone(),
                                //     output: output.clone(),
                                // });
                                match &task.transition {
                                    Transition::End(true) => {
                                        // self.events.push(ExecutionEvent::ExecutionSucceeded {
                                        //     output: output.clone(),
                                        // });
                                        self.state = ExecutionState::Succeeded { output };
                                        true
                                    }
                                    Transition::End(_) => {
                                        panic!("can't provide End=false")
                                    }
                                    Transition::Next(next) => {
                                        self.state = ExecutionState::ExecuteState {
                                            state_name: next.clone(),
                                            input: output,
                                        };
                                        false
                                    }
                                }
                            }
                            Err(e) => {
                                // self.events.push(ExecutionEvent::ExecutionFailed {
                                //     error: e.clone(),
                                //     cause: "TODO".to_owned(),
                                // });
                                self.state = ExecutionState::Failed {
                                    error: e,
                                    cause: "TODO".to_owned(),
                                };
                                true
                            }
                        }
                    }
                    State::Succeed => {
                        self.state = ExecutionState::Succeeded {
                            output: input.clone(),
                        };
                        true
                    }
                    State::Fail { error, cause } => {
                        self.state = ExecutionState::Failed {
                            error: error.clone(),
                            cause: cause.clone(),
                        };
                        true
                    }
                    State::Choice(c) => {
                        for choice in &c.choices {
                            match choice.choice_expr.check(input) {
                                Ok(true) => {
                                    self.state = ExecutionState::ExecuteState {
                                        state_name: choice.next.to_string(),
                                        input: input.clone(),
                                    };
                                    return false;
                                }
                                Ok(false) => continue,
                                Err(s) => {
                                    self.state = ExecutionState::Failed {
                                        error: "Choices failed".to_string(),
                                        cause: format!("Choice state got error {}", s),
                                    };
                                    return true;
                                }
                            }
                        }
                        match &c.default {
                            Some(s) => {
                                self.state = ExecutionState::ExecuteState {
                                    input: input.clone(),
                                    state_name: s.to_owned(),
                                };
                                false
                            }
                            None => {
                                self.state = ExecutionState::Failed {
                                    error: "States.NoChoiceMatched".to_owned(),
                                    cause: "No choice matched.".to_owned(),
                                };
                                false
                            }
                        }
                        // defaul
                    }
                }
            }
            ExecutionState::AdvanceNestedState { .. } => {
                todo!("nested execution state not implemented")
            }
        }
    }

    /// Runs the state machine to completion, returning its output.
    /// `run` merely calls `step`, so the caveats regarding execution order of Map and Parallel apply.
    pub fn run(&mut self) -> Result<Value, Value> {
        loop {
            if self.step() {
                break;
            }
        }
        match &self.state {
            ExecutionState::Failed { cause, error } => Err(json!({"error": error, "cause": cause})),
            ExecutionState::Succeeded { output } => Ok(output.clone()),
            _ => unreachable!("not in a final state after running to completing"),
        }
    }
}

// #[derive(Debug)]
// enum ExecutionEvent {
//     SucceedStateExecuted {
//         state_name: String,
//         input: Value,
//         output: Value,
//     },
//     FailStateExecuted {
//         state_name: String,
//     },
//     TaskStateSucceeded {
//         state_name: String,
//         input: Value,
//         output: Value,
//     },
//     TaskStateFailed {
//         state_name: String,
//         input: Value,
//         cause: String,
//         error: String,
//     },
//     ExecutionSucceeded {
//         output: Value,
//     },
//     ExecutionFailed {
//         cause: String,
//         error: String,
//     },
// }

/// Type-safe version of "state can must have exactly one of End=true or Next=<next state>"
/// TODO I'm modeling End(bool) since it makes serde easier, but does SFN allow "End"=false?
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub enum Transition {
    Next(String),
    End(bool),
}

/// AbsRel is for a few fields that have both an absolute and a relative version.
/// For example, Task states can have either TimeoutSeconds OR TimeoutSecondsPath, same with HeartbeatSeconds
#[derive(Debug, Clone)]
pub enum AbsRel<T> {
    Absolute(T),
    Relative(String), // TODO or json path
}

// impl for resolving an AbsRel against our JSON Value

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct Task {
    pub comment: Option<String>,
    #[serde(flatten)]
    pub transition: Transition,
    pub resource: String,
    pub input_path: io::InputPath,
    // pub timeout_seconds: OptionAbsRel<u32>,
    // pub heartbeat_seconds: AbsRel<u32>,
}
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct Choice {
    pub comment: Option<String>,
    pub input_path: io::InputPath,
    pub choices: Vec<ChoiceRule>,
    pub default: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "Type")]
#[serde(rename_all = "PascalCase")]
pub enum State {
    Task(Task),
    Choice(Choice),
    Succeed,
    Fail { error: String, cause: String },
}

#[cfg(test)]
mod tests {
    use super::*;

    const HELLO_WORLD_LAMBDA: &str = "arn:aws:lambda:us-east-1:123456789012:function:HelloWorld";

    /*
    This is the hello-world example from the States spec
    {
      "Comment": "A simple minimal example of the States language",
      "StartAt": "Hello World",
      "States": {
        "Hello World": {
          "Type": "Task",
          "Resource": "arn:aws:lambda:us-east-1:123456789012:function:HelloWorld",
          "End": true
        }
      }
    }
     */
    fn hello_world_machine() -> StateMachine {
        let t = State::Task(Task {
            comment: None,
            transition: Transition::End(true),
            resource: HELLO_WORLD_LAMBDA.to_owned(),
            input_path: None,
        });
        let mut states = HashMap::new();
        states.insert("Hello World".to_owned(), t);

        StateMachine {
            comment: Some("A simple minimal example of the States language".to_owned()),
            start_at: "Hello World".to_owned(),
            states,
            version: None,
            timeout_seconds: None,
        }
    }

    #[test]
    fn hello_world_execution() {
        let resource_name = "arn:aws:lambda:us-east-1:123456789012:function:HelloWorld";

        let machine = hello_world_machine();

        let mock_resource = mock::constant(json!("Hello, World!"));
        let mut resources = HashMap::new();
        resources.insert(resource_name.to_owned(), mock_resource);

        let mut execution = Execution::new(&machine, resources, &Value::Null);
        let result = execution.run();
        assert_eq!(result, Ok(json!("Hello, World!")))
    }

    #[test]
    fn deserialize() {
        let hello = r#"
    {
      "Comment": "A simple minimal example of the States language",
      "StartAt": "Hello World",
      "States": {
        "Hello World": {
          "Type": "Task",
          "Resource": "arn:aws:lambda:us-east-1:123456789012:function:HelloWorld",
          "End": true
        }
      }
    }"#;

        let result: StateMachine = serde_json::from_str(hello).unwrap();
        assert_eq!(result, hello_world_machine());
    }

    #[test]
    fn sequential() {
        let hello = r#"
    {
      "Comment": "A simple minimal example of the States language",
      "StartAt": "first",
      "States": {
        "first": {
          "Type": "Task",
          "Resource": "first",
          "Next": "second"
        },
        "second": {
          "Type": "Task",
          "Resource": "second",
          "Next": "third"
        },
        "third": {
          "Type": "Task",
          "Resource": "third",
          "End": true
        }
      }
    }"#;

        let machine: StateMachine = serde_json::from_str(hello).unwrap();

        let mut resources = HashMap::new();
        resources.insert("first".to_owned(), mock::constant(json!("Hello world")));
        resources.insert(
            "second".to_owned(),
            mock::function(|v: &Value| match v {
                Value::String(s) => Ok(json!(format!("prefix: {}", s))),
                _ => panic!(),
            }),
        );
        resources.insert(
            "third".to_owned(),
            mock::function(|v: &Value| Ok(json!({ "nesting": v }))),
        );

        let mut execution = Execution::new(&machine, resources, &Value::Null);
        let result = execution.run();
        assert_eq!(
            result,
            Ok(json!({
                "nesting": "prefix: Hello world",
            }))
        )
    }

    #[test]
    fn input_path() {
        let machine = r#"
    {
      "Comment": "A simple minimal example of the States language",
      "StartAt": "Hello World",
      "States": {
        "Hello World": {
          "Type": "Task",
          "Resource": "ident",
          "InputPath": "$.field",
          "End": true
        }
      }
    }"#;
        let machine: StateMachine = serde_json::from_str(machine).unwrap();

        let mut resources = HashMap::new();
        resources.insert("ident".to_owned(), mock::identity());

        let mut execution = Execution::new(&machine, resources, &json!({"field": "value"}));
        assert_eq!(execution.run(), Ok(json!("value")));

        let mut resources = HashMap::new();
        resources.insert("ident".to_owned(), mock::identity());

        let mut execution = Execution::new(&machine, resources, &json!("bad input"));
        assert!(execution.run().is_err());
    }

    // TODO more tests:
    // - Error handling
    // - Succeed and Fail states
    // - Refactor tests into Table, with common resources
}

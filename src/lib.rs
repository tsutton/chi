// #![warn(missing_debug_implementations, rust_2018_idioms)]
// #![warn(missing_docs)]
use io::reference_path::ReferencePath;
use io::{apply_result_path, StateIo};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::collections::HashMap;

pub mod mock;
use mock::MockResource;

pub mod io;

pub mod choice;
pub use choice::*;

pub mod spec;

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

// #[derive(Debug)]
// TODO implement debug (must be manual due to Box<dyn ..>)
pub struct Execution {
    machine: StateMachine,
    events: Vec<ExecutionEvent>,
    resources: HashMap<String, Box<dyn MockResource>>,
    state: ExecutionState,
}

/// ExecutionState holds the information needed to advance an Execution by one step.
///
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
        retried_errors: Vec<u32>,
    },
    AdvanceNestedState {
        state_name: String,
        executions: Vec<Execution>,
        retried_errors: Vec<String>,
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
            events: Vec::new(),
            resources,
            state: ExecutionState::ExecuteState {
                state_name: machine.start_at.clone(),
                input: input.clone(),
                retried_errors: vec![],
            },
        }
    }

    fn fail(&mut self, error: &str, cause: &str) {
        self.state = ExecutionState::Failed {
            error: error.to_owned(),
            cause: cause.to_owned(),
        };
        self.events.push(ExecutionEvent::Failed {
            cause: cause.to_owned(),
            error: error.to_owned(),
        });
    }

    /// step advances the execution by a little bit and returns true if the execution has ended.
    /// Generally this involves running one state. When a Parallel or Map state is active, instead one
    /// state within the innermost nesting is executed.
    /// No guarentees are made regarding the order of execution of the branches within a parallel or map.
    pub fn step(&mut self) -> bool {
        if self.events.is_empty() {
            if let ExecutionState::ExecuteState { input, .. } = &self.state {
                self.events.push(ExecutionEvent::Started {
                    input: input.clone(),
                })
            } else {
                unreachable!("No execution events but not in a the initial ExecuteState")
            }
        }
        match &mut self.state {
            ExecutionState::Succeeded { .. } => true,
            ExecutionState::Failed { .. } => true,
            ExecutionState::ExecuteState {
                state_name,
                input,
                retried_errors,
            } => {
                let state = self
                    .machine
                    .states
                    .get(state_name)
                    .unwrap_or_else(|| panic!("missing state for {}", state_name));
                self.events.push(ExecutionEvent::StateEntered {
                    name: state_name.clone(),
                    input: input.clone(),
                });
                let effective_input = match state.effective_input(&input, &json!({})) {
                    Ok(i) => i,
                    Err(e) => {
                        self.fail(&e.error, &e.cause);
                        return true;
                    }
                };
                match state {
                    State::Pass(_) => {
                        todo!()
                    }
                    State::Task(task) => {
                        let resource = self
                            .resources
                            .get_mut(&task.resource)
                            .unwrap_or_else(|| panic!("missing resource for {}", &task.resource));
                        match resource.execute(&effective_input) {
                            Ok(output) => {
                                let effective_output = match task.effective_output(&input, &output)
                                {
                                    Ok(o) => o,
                                    Err(e) => {
                                        self.fail(&e.error, &e.cause);
                                        return true;
                                    }
                                };
                                self.events.push(ExecutionEvent::StateSucceeded {
                                    name: state_name.to_owned(),
                                    output: effective_output.clone(),
                                    // result: ...
                                });
                                match &task.transition {
                                    Transition::End(true) => {
                                        self.events.push(ExecutionEvent::Succeeded {
                                            output: effective_output.clone(),
                                        });
                                        self.state = ExecutionState::Succeeded {
                                            output: effective_output,
                                        };
                                        true
                                    }
                                    Transition::End(_) => {
                                        panic!("can't provide End=false")
                                    }
                                    Transition::Next(next) => {
                                        self.state = ExecutionState::ExecuteState {
                                            state_name: next.clone(),
                                            input: effective_output,
                                            retried_errors: vec![],
                                        };
                                        false
                                    }
                                }
                            }
                            Err(mock::Error { error, cause }) => {
                                // TODO events
                                retried_errors.resize(task.retry.len(), 0);
                                match should_retry(&error, &retried_errors, &task.retry) {
                                    Some(i) => {
                                        retried_errors[i] += 1;
                                        false
                                    }
                                    None => {
                                        match task.catch.iter().find(|catcher| {
                                            catcher.error_equals.iter().any(|err_name| {
                                                err_name == "States.ALL" || err_name == &error
                                            })
                                        }) {
                                            Some(catcher) => {
                                                let next_input = match apply_result_path(
                                                    input,
                                                    &json!({"error": error, "cause": cause}),
                                                    &catcher.result_path,
                                                ) {
                                                    Ok(i) => i,
                                                    Err(e) => {
                                                        self.fail(&e.error, &e.cause);
                                                        return true;
                                                    }
                                                };
                                                self.state = ExecutionState::ExecuteState {
                                                    state_name: catcher.next.clone(),
                                                    input: next_input,
                                                    retried_errors: vec![],
                                                };
                                                false
                                            }
                                            None => {
                                                self.fail(&error, &cause);
                                                true
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    State::Succeed(_) => {
                        self.events.push(ExecutionEvent::Succeeded {
                            output: effective_input.clone(),
                        });
                        self.state = ExecutionState::Succeeded {
                            output: effective_input,
                        };
                        true
                    }
                    State::Fail(Fail { error, cause }) => {
                        self.events.push(ExecutionEvent::Failed {
                            error: error.clone(),
                            cause: cause.to_owned(),
                        });
                        self.state = ExecutionState::Failed {
                            error: error.clone(),
                            cause: cause.clone(),
                        };
                        true
                    }
                    State::Choice(c) => {
                        // TODO support effective_output
                        for choice in &c.choices {
                            match choice.choice_expr.check(&effective_input) {
                                Ok(true) => {
                                    self.events.push(ExecutionEvent::StateSucceeded {
                                        name: state_name.clone(),
                                        output: effective_input.clone(),
                                    });
                                    self.state = ExecutionState::ExecuteState {
                                        state_name: choice.next.to_string(),
                                        input: effective_input.clone(),
                                        retried_errors: vec![],
                                    };
                                    return false;
                                }
                                Ok(false) => continue,
                                Err(s) => {
                                    let error = "States.Runtime".to_owned();
                                    let cause = format!("Choice state got error {}", s);
                                    self.events.push(ExecutionEvent::StateFailed {
                                        name: state_name.clone(),
                                        cause: cause.to_owned(),
                                        error: error.to_owned(),
                                    });
                                    self.fail(&error, &cause);
                                    return true;
                                }
                            }
                        }
                        match &c.default {
                            Some(s) => {
                                self.events.push(ExecutionEvent::StateSucceeded {
                                    name: state_name.clone(),
                                    output: effective_input.clone(),
                                });
                                self.state = ExecutionState::ExecuteState {
                                    input: effective_input,
                                    state_name: s.to_owned(),
                                    retried_errors: vec![],
                                };
                                false
                            }
                            None => {
                                let error = "States.NoChoiceMatched".to_owned();
                                let cause = "No choice matched.".to_owned();
                                self.events.push(ExecutionEvent::StateFailed {
                                    name: state_name.clone(),
                                    cause: cause.to_owned(),
                                    error: error.to_owned(),
                                });
                                self.fail(&error, &cause);
                                false
                            }
                        }
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
        while let false = self.step() {
            // Keep going!
        }
        match &self.state {
            ExecutionState::Failed { cause, error } => Err(json!({"error": error, "cause": cause})),
            ExecutionState::Succeeded { output } => Ok(output.clone()),
            _ => unreachable!("not in a final state after running to completing"),
        }
    }
}

// should_retry returns either Some of the index of the applicable retrier, or None if the error should not be retried.
// retry_counts is the number of times each retrier has been uesd.
fn should_retry(error: &str, retry_counts: &[u32], retries: &[Retry]) -> Option<usize> {
    for (i, retry) in retries.iter().enumerate() {
        if retry_counts[i] == retry.max_attempts {
            continue;
        }
        if retry
            .error_equals
            .iter()
            .any(|e| e == "States.ALL" || e == error)
        {
            return Some(i);
        }
    }
    None
}

/// ExecutionEvent describes a progression of an execution.
///
/// It is intentionally verbose and redundant to allow for easier debugging, for example,
/// It is redundant to give both the input and the parameters for StateEntered, because the
/// parameters can be deduced by applying the state's Path and Parameters to the event's input.
/// Similarly, giving the output of a state then the input of the next state is redudant, but can make debugging
/// a key part of the state easier.
///
/// Multiple events can be emitted during a single execution step() call, for example if an end state is reached,
/// events for the entering and exiting the states plus also an execution ending event will be added.
// TODO remove allow(dead_code)
#[allow(dead_code)]
#[derive(Debug)]
pub enum ExecutionEvent {
    Started {
        input: Value,
    },
    StateEntered {
        name: String,
        input: Value,
        // parameters: Value,
    },
    StateSucceeded {
        name: String,
        // result: Value,
        output: Value,
    },
    StateFailed {
        name: String,
        cause: String,
        error: String,
    },
    Failed {
        cause: String,
        error: String,
    },
    Succeeded {
        output: Value,
    },
}

/// Type-safe version of "state can must have exactly one of End=true or Next=<next state>"
/// TODO I'm modeling End(bool) since it makes serde easier, but does SFN allow "End"=false?
#[derive(Debug, Clone, PartialEq, Eq, Deserialize, Serialize)]
#[serde(rename_all = "PascalCase")]
pub enum Transition {
    Next(String),
    End(bool),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct Task {
    pub comment: Option<String>,
    pub resource: String,
    #[serde(default = "io::default_input_path")]
    pub input_path: io::Path,
    pub parameters: Option<Value>,
    #[serde(default = "io::default_result_path")]
    pub result_path: Option<ReferencePath>,

    #[serde(default)]
    pub retry: Vec<Retry>,

    #[serde(default)]
    pub catch: Vec<Catch>,

    #[serde(flatten)]
    pub transition: Transition,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct Retry {
    error_equals: Vec<String>,
    #[serde(default = "const_three")]
    max_attempts: u32,
    #[serde(default = "const_one")]
    interval_seconds: u32,
    #[serde(default = "const_2_0")]
    backoff_rate: serde_json::Number,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct Catch {
    error_equals: Vec<String>,
    next: String,
    #[serde(default = "io::default_result_path")]
    result_path: Option<ReferencePath>,
}

fn const_three() -> u32 {
    3
}

fn const_one() -> u32 {
    3
}

fn const_2_0() -> serde_json::Number {
    serde_json::Number::from_f64(2.0).unwrap()
}

impl StateIo for Task {
    fn input_path(&self) -> io::Path {
        self.input_path.clone()
    }

    fn parameters(&self) -> io::Template {
        self.parameters.clone()
    }

    // TODO
    // fn results_selector(&self) -> io::Template {
    //     None
    // }

    fn result_path(&self) -> Option<ReferencePath> {
        self.result_path.clone()
    }

    // TODO
    // fn output_path(&self) -> io::Path {
    //     Some("$".to_owned())
    // }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct Choice {
    pub comment: Option<String>,
    #[serde(default = "io::default_input_path")]
    pub input_path: io::Path,
    pub choices: Vec<ChoiceRule>,
    pub default: Option<String>,
}

impl StateIo for Choice {
    fn input_path(&self) -> io::Path {
        self.input_path.clone()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "Type")]
#[serde(rename_all = "PascalCase")]
pub enum State {
    Task(Task),
    Choice(Choice),
    Succeed(Succeed),
    Fail(Fail),
    Pass(Pass),
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct Succeed {}

impl StateIo for Succeed {}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct Fail {
    error: String,
    cause: String,
}

impl StateIo for Fail {}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct Pass {
    pub comment: Option<String>,
    pub resource: String,
    #[serde(default = "io::default_input_path")]
    pub input_path: io::Path,
    pub parameters: Option<Value>,
    #[serde(default = "io::default_result_path")]
    pub result_path: Option<ReferencePath>,

    #[serde(flatten)]
    pub transition: Transition,
}

impl StateIo for Pass {
    fn result_path(&self) -> Option<ReferencePath> {
        self.result_path.clone()
    }
    fn input_path(&self) -> io::Path {
        self.input_path.clone()
    }
    fn parameters(&self) -> io::Template {
        self.parameters.clone()
    }
}

// I found a crate with a macro for making this less annoying (enum_dispatch)
// but it seems to work with internals that don't like the trait and enum being in separate modules.
impl StateIo for State {
    fn input_path(&self) -> io::Path {
        match self {
            State::Task(x) => x.input_path(),
            State::Choice(x) => x.input_path(),
            State::Succeed(x) => x.input_path(),
            State::Fail(x) => x.input_path(),
            State::Pass(x) => x.input_path(),
        }
    }

    fn parameters(&self) -> io::Template {
        match self {
            State::Task(x) => x.parameters(),
            State::Choice(x) => x.parameters(),
            State::Succeed(x) => x.parameters(),
            State::Fail(x) => x.parameters(),
            State::Pass(x) => x.parameters(),
        }
    }

    fn results_selector(&self) -> io::Template {
        match self {
            State::Task(x) => x.results_selector(),
            State::Choice(x) => x.results_selector(),
            State::Succeed(x) => x.results_selector(),
            State::Fail(x) => x.results_selector(),
            State::Pass(x) => x.results_selector(),
        }
    }

    fn result_path(&self) -> Option<ReferencePath> {
        match self {
            State::Task(x) => x.result_path(),
            State::Choice(x) => x.result_path(),
            State::Succeed(x) => x.result_path(),
            State::Fail(x) => x.result_path(),
            State::Pass(x) => x.result_path(),
        }
    }

    fn output_path(&self) -> io::Path {
        match self {
            State::Task(x) => x.output_path(),
            State::Choice(x) => x.output_path(),
            State::Succeed(x) => x.output_path(),
            State::Fail(x) => x.output_path(),
            State::Pass(x) => x.output_path(),
        }
    }
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
    n          "End": true
            }
          }
        }
         */
    fn hello_world_machine() -> StateMachine {
        let t = State::Task(Task {
            comment: None,
            transition: Transition::End(true),
            resource: HELLO_WORLD_LAMBDA.to_owned(),
            input_path: Some("$".to_owned()),
            parameters: None,
            retry: vec![],
            catch: vec![],
            result_path: Some(ReferencePath::default()),
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
        let machine = hello_world_machine();

        let mock_resource = mock::constant(json!("Hello, World!"));
        let mut resources = HashMap::new();
        resources.insert(HELLO_WORLD_LAMBDA.to_owned(), mock_resource);

        eprintln!("{:?}", machine.states["Hello World"]);
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

    // This is an end-to-end execution of a choice-state-based counter.
    // It uses Choice to:
    // 1. Initialize $.count to 0 if not present
    // 2. Use a Task to add 1 to count until it's no longer less than 3
    // 3. use a Succeed to return.
    // It tests (some subset) of the execution control flow of Choice:
    // That it chooses the correct next state based on th Choices and Default field.
    // (Testing every single test expression is the domain of `mod choice`.)
    #[test]
    fn choice_loop() {
        let machine = r#"
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

        #[derive(Debug, Deserialize, Serialize)]
        struct IncrIo {
            count: i64,
        }

        let machine: StateMachine = serde_json::from_str(machine).unwrap();
        let mut resources = HashMap::new();
        resources.insert("init".to_owned(), mock::constant(json!({"count": 0})));
        resources.insert(
            "incr".to_owned(),
            mock::function(|v| {
                let mut inp: IncrIo =
                    serde_json::from_value(v.clone()).map_err(|e| mock::Error {
                        error: "bad.input".to_owned(),
                        cause: format!("{:?}", e),
                    })?;
                inp.count += 1;
                Ok(serde_json::to_value(inp).unwrap())
            }),
        );
        let mut execution = Execution::new(&machine, resources, &json!({}));
        assert_eq!(execution.run(), Ok(json!({"count": 3})));
        for e in execution.events.iter() {
            println!("{:?}", e);
        }
    }

    #[test]
    fn retries_simple() {
        #[rustfmt::skip]
        let machine = r#"{
            "StartAt": "main",
            "States": {
              "main": {
                "Type": "Task",
                "Resource": "fails",
		"End": true,
                "Retry": [{
		    "ErrorEquals": ["my_error"]
		}]
              }
            }
        }"#;

        let machine: StateMachine = serde_json::from_str(machine).unwrap();
        if let State::Task(task) = machine.states.get("main").unwrap() {
            eprintln!("{:?}", task.retry);
        } else {
            unreachable!();
        }

        fn fails_n_times(n: u32) -> Box<dyn MockResource> {
            let mut count = 0;
            mock::function(move |_| {
                eprintln!("called with count={}", count);
                if count < n {
                    count += 1;
                    Err(mock::Error {
                        error: "my_error".to_owned(),
                        cause: "".to_owned(),
                    })
                } else {
                    Ok(json!(count))
                }
            })
        }

        let mut resources = HashMap::new();
        resources.insert("fails".to_owned(), fails_n_times(3));

        let mut execution = Execution::new(&machine, resources, &json!({}));
        assert_eq!(execution.run(), Ok(json!(3)));

        let mut resources = HashMap::new();
        resources.insert("fails".to_owned(), fails_n_times(4));

        let mut execution = Execution::new(&machine, resources, &json!({}));
        assert_eq!(
            execution.run(),
            Err(json!({
              "error": "my_error","cause": ""
            }),)
        );
    }

    #[test]
    fn retries_complex() {
        #[rustfmt::skip]
        let machine = r#"{
            "StartAt": "main",
            "States": {
              "main": {
                "Type": "Task",
                "Resource": "fails",
		"End": true,
                "Retry": [
                  {
                    "ErrorEquals": [ "ErrorA", "ErrorB" ],
                    "IntervalSeconds": 1,
                    "BackoffRate": 2,
                    "MaxAttempts": 2
                  },
                  {
                    "ErrorEquals": [ "ErrorC" ],
                    "IntervalSeconds": 5
                  }
                ],
                "Catch": [
                  {
                    "ErrorEquals": [ "States.ALL" ],
                    "Next": "Z"
                  }
                ]
              },
              "Z": {
                "Type": "Task",
		"End": true,
                "Resource": "error_info"
              }
            }
        }"#;

        let machine: StateMachine = serde_json::from_str(machine).unwrap();

        let error_sequence = vec!["ErrorA", "ErrorB", "ErrorC", "ErrorB"];
        let mut count = 0;
        let res = mock::function(move |_| {
            eprintln!("called with count={}", count);
            let error = error_sequence[count].to_owned();
            count += 1;
            Err(mock::Error {
                error,
                cause: format!("{}", count),
            })
        });

        let mut resources = HashMap::new();
        resources.insert("fails".to_owned(), res);

        // resources.insert("error_info".to_owned(), mock::identity());
        let error_info = mock::function(move |input| {
            eprintln!("error_info called with input={}", &input);
            Ok(input.clone())
        });
        resources.insert("error_info".to_owned(), error_info);

        let mut execution = Execution::new(&machine, resources, &json!({}));
        assert_eq!(
            execution.run(),
            Ok(json!({
                "error": "ErrorB",
                "cause": "4",
            }))
        );
    }

    #[test]
    fn catch_results_path() {
        #[rustfmt::skip]
        let machine = r#"{
            "StartAt": "main",
            "States": {
              "main": {
                "Type": "Task",
                "Resource": "fails",
		"End": true,
                "Catch": [
                  {
                    "ErrorEquals": [ "States.ALL" ],
                    "ResultPath": "$.error_info",
                    "Next": "Z"
                  }
                ]
              },
              "Z": {
                "Type": "Task",
		"End": true,
                "Resource": "error_info"
              }
            }
        }"#;

        let machine: StateMachine = serde_json::from_str(machine).unwrap();

        let res = mock::function(move |_| {
            Err(mock::Error {
                error: "error".to_owned(),
                cause: "cause".to_owned(),
            })
        });

        let mut resources = HashMap::new();
        resources.insert("fails".to_owned(), res);
        resources.insert("error_info".to_owned(), mock::identity());

        let mut execution = Execution::new(&machine, resources, &json!({}));
        #[rustfmt::skip]
        assert_eq!(
            execution.run(),
            Ok(json!({
              "error_info": {
                "error": "error",
                "cause": "cause",
              }
	    }))
        );
    }

    #[test]
    fn task_results_path() {
        fn make_machine(result_path: Option<ReferencePath>) -> StateMachine {
            StateMachine {
                comment: None,
                version: None,
                timeout_seconds: None,

                start_at: "main".to_owned(),
                states: vec![(
                    "main".to_owned(),
                    State::Task(Task {
                        resource: "const".to_owned(),
                        transition: Transition::End(true),
                        result_path,

                        catch: vec![],
                        retry: vec![],
                        comment: None,
                        input_path: None,
                        parameters: None,
                    }),
                )]
                .into_iter()
                .collect(),
            }
        }

        struct Test {
            description: &'static str,
            result_path: Option<&'static str>,
            expected: Result<Value, Value>,
        }

        let tests: Vec<Test> = vec![
            Test {
                description: "no result path",
                result_path: None,
                expected: Ok(json!({})),
            },
            Test {
                description: "result path is $",
                result_path: Some("$"),
                expected: Ok(json!("result")),
            },
            Test {
                description: "result path is $.a.b",
                result_path: Some("$.a.b"),
                expected: Ok(json!({"a": {"b": "result"}})),
            },
        ];

        for test in tests {
            let res = mock::constant(json!("result"));
            let mut resources = HashMap::new();
            resources.insert("const".to_owned(), res);
            let mut execution = Execution::new(
                &make_machine(test.result_path.map(|s| ReferencePath::compile(s).unwrap())),
                resources,
                &json!({}),
            );
            assert_eq!(execution.run(), test.expected, "{}", test.description);
        }
    }

    // TODO more tests:
    // - Succeed and Fail states
    // - Refactor tests into Table, with common resources
}

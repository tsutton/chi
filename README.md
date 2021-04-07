# chi

*Chi is not ready for use yet.*

Chi is an executor for the [States language](https://states-language.net/) with a goal of enabling testing and debugging of state machines. Instead of executing Lambda functions or using any of the other AWS service integrations built into [AWS Step Functions](https://docs.aws.amazon.com/step-functions/index.html), resources are provided by a DSL for expression simple JSON manipulation; thus State Machines can be tested against a variety of scenarios without needing to setup Lambdas.

## Quick Start

* Clone this repo
* Run `cargo build --release`
* Copy the binary to somewhere on your `$PATH`, e.g. `cp ./target/release/chi $HOME/.cargo/bin`
* For most things to work, you will also need to have `jq` installed and on your `$PATH`
* Run an example! `chi -s examples/machine.json -t examples/specs.yml`

`chi` has two mandatory arguments:
* `--state-machine` (`-s` for short) is the path to a file containing a valid State Machine, as specified by the States spec.
* `--tests` (`-t` for short) is the path to a file specifying the tests to run on the state machine. The authoritative documentation for this file can be backsolved from the Rust code at [./src/spec.rs](./src/spec.rs) or you can inspect the examples in `examples/`; at some point I'll write some real documentation eventually.

Both files are parsed as YAML (and thus can be JSON, since JSON is valid YAML).

## States Language support

The following are missing entirely:
* Map and Parallel states
* `ResultsSelector`
* `Pass` and `Wait` states
* `OutputPath`
* Intrinsic functions

The following have only partially support:
* `Choice` state: only a small subset of comparision operators are implemented.
* Use of JSON Paths. This is supported, but I'm not sure if the library I use for JSONPath is fully compatible with how AWS does it.
* `Parameters`: Parameters works, but the context object is currently always the empty JSON object `{}`.

Additionally, if your state machine is invalid, I make no promises that `chi` will work on it; run it through [statelint](https://github.com/awslabs/statelint/) first.

## Future plans

In the current form, `chi` can run a test non-interactively and report back with the final result. In the future, I hope to add support for recording events from the test run so that you can verify not just the final answer, but whether the execution played out in the expected way. Additionally, I'd like to add an interactive mode, to step through an execution one state at a time. As a stretch goal, that interactive mode could be a shiny web interface.

## Programmatic Use

This repository contains a binary crate which is a thin wrapper around the library crate. As such, once I publish this to crates.io, you'll be able to use the inner workings directly from your Rust code. However, if you want to do that, maybe wait a few months and check back; the Rust API is very unstable.

## FAQ

**Why chi?**

Chi (χ) is the Greek letter commonly used in mathematics for [indicator functions](https://en.wikipedia.org/wiki/Indicator_function), which are the building blocks of [step functions](https://en.wikipedia.org/wiki/Step_function).

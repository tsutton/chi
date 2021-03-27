use std::fs::File;
use std::io::Read;

use chi::spec::Specs;
use chi::StateMachine;
use clap::{App, Arg};

fn main() {
    let matches = App::new("chi")
        .about("Runs unit tests against State Machines")
        .arg(
            Arg::with_name("state-machine")
                .short("s")
                .required(true)
                .takes_value(true)
                .help("path to state machine"),
        )
        .arg(
            Arg::with_name("tests")
                .short("t")
                .required(true)
                .takes_value(true)
                .help("path to test config"),
        )
        .get_matches();

    let machine_file_name = matches.value_of("state-machine").unwrap();
    let machine = parse_machine(machine_file_name).unwrap();

    let tests_filename = matches.value_of("tests").unwrap();
    let tests = parse_specs(tests_filename).unwrap();

    for spec in tests.specs {
        match spec.run(&machine) {
            Ok(()) => println!("PASS {}", spec.name),
            Err(e) => println!(
                r"FAIL {}:
    {}",
                spec.name, e
            ),
        };
    }
}

// TODO clean this up, there's some lifetime thing that makes "deser a T from a file" hard to write
fn parse_machine(filename: &str) -> Result<StateMachine, String> {
    let mut file =
        File::open(filename).map_err(|e| format!("Opening file {} failed: {}", filename, e))?;

    let mut buf = String::new();
    file.read_to_string(&mut buf)
        .map_err(|e| format!("Reading file {}: {}", filename, e))?;

    serde_yaml::from_str(&buf).map_err(|e| format!("Deserializing file {}: {}", filename, e))
}

fn parse_specs(filename: &str) -> Result<Specs, String> {
    let mut file =
        File::open(filename).map_err(|e| format!("Opening file {} failed: {}", filename, e))?;

    let mut buf = String::new();
    file.read_to_string(&mut buf)
        .map_err(|e| format!("Reading file {}: {}", filename, e))?;

    serde_yaml::from_str(&buf).map_err(|e| format!("Deserializing file {}: {}", filename, e))
}

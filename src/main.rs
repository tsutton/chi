use std::fs::File;
use std::io::Read;

use chi::spec::Specs;
use chi::StateMachine;
use clap::{App, Arg};
use serde::de::DeserializeOwned;

fn main() {
    let matches = App::new("chi")
        .about("Runs unit tests against State Machines")
        .arg(
            Arg::with_name("state-machine")
                .short("s")
                .long("state-machine")
                .required(true)
                .takes_value(true)
                .help("path to state machine"),
        )
        .arg(
            Arg::with_name("tests")
                .short("t")
                .long("tests")
                .required(true)
                .takes_value(true)
                .help("path to test config"),
        )
        .get_matches();

    let machine_file_name = matches.value_of("state-machine").unwrap();
    let machine: StateMachine = parse_from_file(machine_file_name).unwrap();

    let tests_filename = matches.value_of("tests").unwrap();
    let tests: Specs = parse_from_file(tests_filename).unwrap();

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

fn parse_from_file<T>(filename: &str) -> Result<T, String>
where
    T: DeserializeOwned,
{
    let mut file =
        File::open(filename).map_err(|e| format!("Opening file {} failed: {}", filename, e))?;

    let mut buf = String::new();
    file.read_to_string(&mut buf)
        .map_err(|e| format!("Reading file {}: {}", filename, e))?;

    serde_yaml::from_str(&buf).map_err(|e| format!("Deserializing file {}: {}", filename, e))
}

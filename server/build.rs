use std::convert::From;
use std::error;
use std::error::Error as StdError;
use std::fmt;
use std::fs;
use std::io;
use std::process::{Command, exit};

use ElmMakeError::{CouldNotRun, Failed};

#[derive(Debug)]
enum ElmMakeError {
    CouldNotRun(io::Error),
    Failed,
}

impl fmt::Display for ElmMakeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let desc = self.description();
        match self {
            &CouldNotRun(ref e) =>
                write!(f, "{}: {}\nAre the elm tools installed?", desc, e),
            &Failed => write!(f, "{}", desc),
        }
    }
}

impl error::Error for ElmMakeError {
    fn description(&self) -> &str {
        match self {
            &CouldNotRun(_) => "Could not run `elm make`",
            &Failed => "elm-make build failed",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match self {
            &CouldNotRun(ref e) => Some(e),
            &Failed => None,
        }
    }
}

impl From<io::Error> for ElmMakeError {
    fn from(err: io::Error) -> ElmMakeError {
        CouldNotRun(err)
    }
}

fn elm_make_once() -> Result<(), ElmMakeError> {
    // Build the command to run.
    let mut process = Command::new("elm");
    process
        .arg("make")
        .arg("Main.elm")
        .arg("--warn").arg("--yes")
        .arg("--output").arg("../static/elm.js")
        .current_dir("site");

    // Try to run the command.
    let status = try!(process.status());
    if !status.success() {
        return Err(Failed);
    }
    Ok(())
}

fn elm_make() -> Result<(), ElmMakeError> {
    if fs::metadata("site/elm-stuff").is_err() {
        println!("Will retry `elm make` if it fails.");
        for i in 0..2 {
            match elm_make_once() {
                Err(Failed) => {
                    println!("Build {} failed, perhaps because of network.", i);
                    println!("Retrying...");
                    continue
                }
                result => return result,
            }
        }
    }
    elm_make_once()
}

fn main() {
    match elm_make() {
        Ok(()) => {}
        Err(ref err) => {
            println!("{}", err);
            exit(1);
        }
    }
}


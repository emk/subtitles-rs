#[macro_use]
extern crate common_failures;
extern crate submodel;

use std::env;
use std::io;
use std::io::prelude::*;
use std::process;

use submodel::{ModelBuilder, Result};

// Ask error_chain to construct a standard `main` to handle error reporting.
quick_main!(run);

/// Our actual entry point.
fn run() -> Result<()> {
    // Parse our arguments by hand to avoid adding a bunch of dependencies
    // to the library crate as well.
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() != 1 || args[0] == "--help" {
        writeln!(io::stderr(), "Usage: submodel <iso-lang-code>

Read subtitle data from stdin, with one subtitle per line, and output a
gzipped tar archive containing a language model on stdout.
")?;
        process::exit(1);
    }

    // I'm not sure what we wanted to use this for.
    let _iso_lang_code = &args[0];

    // Set up standard input and output.  We lock and buffer them because
    // that's how we get good pipeline performance in Rust.
    let stdin = io::stdin();
    let input = io::BufReader::new(stdin.lock());
    let stdout = io::stdout();
    let output = io::BufWriter::new(stdout.lock());

    // Create a model.
    let mut model = ModelBuilder::new();
    for line in input.lines() {
        let line = line?;
        model.add_line(&line);
    }
    model.write_model(output)?;
    Ok(())
}

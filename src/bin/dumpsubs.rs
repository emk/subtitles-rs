// For error-chain.
#![recursion_limit = "1024"]

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate nom;
extern crate vobsub;

use std::env;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::process;

use vobsub::Result;
use vobsub::mpeg2::ps;
use vobsub::util::BytesFormatter;

fn run() -> Result<()> {
    // Parse our command-line argument.
    let args = env::args().skip(1).collect::<Vec<_>>();
    if args.len() != 1 {
        writeln!(io::stderr(), "Please pass a single *.sub filename to dump").unwrap();
        process::exit(1);
    }
    let path = Path::new(&args[0]);

    let mut f: fs::File = fs::File::open(path)?;

    let mut data = vec![0; 0x800];
    f.read_exact(&mut data)?;
    println!("{:?}", BytesFormatter(&data));

    println!("{:#?}", ps::pes_packet(&data).unwrap().1);

    Ok(())
}

// Use error-chain to declare a custom `main` function that calls `run` and
// prints any erors.
quick_main!(run);

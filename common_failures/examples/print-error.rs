#[macro_use]
extern crate common_failures;

use common_failures::prelude::*;
use std::fs::File;
use std::path::Path;

quick_main!(run);

fn run() -> Result<()> {
    let path = Path::new("no-such-file.txt");
    let _file = File::open(path).io_read_context(path)?;
    Ok(())
}

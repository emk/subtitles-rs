#[macro_use]
extern crate common_failures;

use std::{fs::File, path::Path};

use common_failures::prelude::*;

quick_main!(run);

fn run() -> Result<()> {
    let path = Path::new("no-such-file.txt");
    let _file = File::open(path).io_read_context(path)?;
    Ok(())
}

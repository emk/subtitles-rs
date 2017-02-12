extern crate docopt;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate image;
extern crate rustc_serialize;
extern crate vobsub;

use docopt::Docopt;
use std::env;
use std::path::Path;
use vobsub::{Index, Result};

const USAGE: &'static str = "
Usage: dump_vobsub [-i] <idx-file>

Options:
    -i, --images  Dump subtitle image data as PNGs (requires --feature=png)
";

#[derive(RustcDecodable)]
struct Args {
    arg_idx_file: String,
    flag_images: bool,
}

quick_main!(run);

fn run() -> Result<()> {
    env_logger::init().unwrap();

    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.argv(env::args().into_iter()).decode())
        .unwrap_or_else(|e| e.exit());

    if args.flag_images {
        check_png_support()?;
    }

    let path = Path::new(&args.arg_idx_file);
    let idx = Index::open(path)?;
    for (i, sub) in idx.subtitles().enumerate() {
        let sub = sub?;
        if args.flag_images {
            let image = sub.to_image(idx.palette());
            let mut out_path = path.to_owned();
            out_path.set_extension(&format!("{:04}.png", i));
            image.save(&out_path)?;
        }
    }
    Ok(())
}

fn check_png_support() -> Result<()> {
    if cfg!(feature="png") {
        Ok(())
    } else {
        Err("PNG images disabled, build with `--features png` to enable".into())
    }
}

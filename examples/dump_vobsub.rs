extern crate docopt;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate image;
extern crate rustc_serialize;
extern crate vobsub;

use docopt::Docopt;
use std::env;
use std::fs;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use vobsub::{Index, Result, Subtitle};

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

#[derive(RustcEncodable)]
struct IndexInfo {
    subtitles: Vec<SubInfo>,
}

#[derive(RustcEncodable)]
struct SubInfo {
    start: f64,
    end: f64,
    force: bool,
    position: (u16, u16),
    size: (u16, u16),
    path: PathBuf,
}

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
    let mut info = IndexInfo { subtitles: vec![] };
    for (i, sub) in idx.subtitles().enumerate() {
        let sub: Subtitle = sub?;
        if args.flag_images {
            let image = sub.to_image(idx.palette());
            let mut out_path = path.to_owned();
            out_path.set_extension(&format!("{:04}.png", i));
            image.save(&out_path)?;

            info.subtitles.push(SubInfo {
                start: sub.start_time,
                end: sub.end_time,
                force: sub.force,
                position: (sub.coordinates.left(), sub.coordinates.top()),
                size: (sub.coordinates.width(), sub.coordinates.height()),
                path: out_path,
            });
        }
    }

    let mut json_path = path.to_owned();
    json_path.set_extension("json");
    let json = rustc_serialize::json::encode(&info)
        .expect("Cannot encode JSON".into());
    let mut json_file: fs::File = fs::File::create(&json_path)?;
    json_file.write_all(json.as_bytes())?;

    Ok(())
}

fn check_png_support() -> Result<()> {
    if cfg!(feature="png") {
        Ok(())
    } else {
        Err("PNG images disabled, build with `--features png` to enable".into())
    }
}

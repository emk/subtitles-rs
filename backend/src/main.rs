#[macro_use]
extern crate common_failures;
extern crate env_logger;
#[macro_use]
extern crate failure;
extern crate log;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;
extern crate substudy;
extern crate url;

use common_failures::prelude::*;
use std::env;
use std::io;
use std::path::{Path, PathBuf};
use structopt::StructOpt;
use url::Url;

mod models;

#[derive(Debug, StructOpt)]
/// Generate JSON data for use with experimental UI.
struct Opt {
    /// Path to a video file in a browser-compatible format.
    #[structopt(parse(from_os_str))]
    video: PathBuf,

    /// Path to a foreign-langauge subtitle file in *.srt format.
    #[structopt(parse(from_os_str))]
    foreign_subs: PathBuf,

    /// Path to a native-langauge subtitle file in *.srt format.
    #[structopt(parse(from_os_str))]
    native_subs: Option<PathBuf>,
}

fn run() -> Result<()> {
    env_logger::init().unwrap();

    // Parse our command-line arguments using docopt.
    let opt = Opt::from_args();

    let mut video_path: PathBuf = env::current_dir()?;
    video_path.push(&opt.video);
    let video_url: Url = Url::from_file_path(&video_path).map_err(|_| {
        format_err!("could not build URL for {}", video_path.display())
    })?;

    let video = models::Video::new(
        &video_url,
        &opt.foreign_subs,
        opt.native_subs.as_ref().map(|s| -> &Path { &*s }),
    )?;

    serde_json::to_writer_pretty(io::stdout(), &video)?;

    Ok(())
}

quick_main!(run);

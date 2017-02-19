extern crate docopt;
extern crate env_logger;
#[macro_use]
extern crate error_chain;
extern crate image;
#[macro_use]
extern crate log;
extern crate rustc_serialize;
extern crate subtitle_ocr;
extern crate vobsub;

use docopt::Docopt;
use std::env;
use std::path::Path;
use subtitle_ocr::OcrContext;
use vobsub::Index;

mod errors {
    use subtitle_ocr;
    use vobsub;
    error_chain! {
        links {
            Vobsub(vobsub::Error, vobsub::ErrorKind);
            SubtitleOcr(subtitle_ocr::Error, subtitle_ocr::ErrorKind);
        }
    }
}

pub use self::errors::Result;

const USAGE: &'static str = "
Usage: subtitles2srt <idx-file>
";

#[derive(RustcDecodable)]
struct Args {
    arg_idx_file: String,
}

quick_main!(run);

fn run() -> Result<()> {
    env_logger::init().unwrap();

    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.argv(env::args().into_iter()).decode())
        .unwrap_or_else(|e| e.exit());
    let path = Path::new(&args.arg_idx_file);

    let mut ctx = OcrContext::new(&path)?;
    let idx = Index::open(path)?;
    for sub in idx.subtitles() {
        let sub = sub?;
        ctx.add(sub.start_time, sub.end_time, &sub.to_image(&idx.palette()))?;
    }

    Ok(())
}

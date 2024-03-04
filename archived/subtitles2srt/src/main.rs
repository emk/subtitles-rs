#[macro_use]
extern crate common_failures;
extern crate docopt;
extern crate env_logger;
extern crate failure;
extern crate image;
#[macro_use]
extern crate log;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate subtitle_ocr;
extern crate vobsub;

pub use self::common_failures::Result;
use docopt::Docopt;
use std::path::Path;
use subtitle_ocr::OcrContext;
use vobsub::Index;


const USAGE: &'static str = "
Usage: subtitles2srt <idx-file>
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_idx_file: String,
}

quick_main!(run);

fn run() -> Result<()> {
    env_logger::init().unwrap();

    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());
    debug!("args: {:?}", &args);
    let path = Path::new(&args.arg_idx_file);

    let mut ctx = OcrContext::new(&path)?;
    let idx = Index::open(path)?;
    for sub in idx.subtitles() {
        let sub = sub?;
        ctx.add(sub.start_time(), sub.end_time(), &sub.to_image(&idx.palette()))?;
    }

    Ok(())
}

#[macro_use]
extern crate common_failures;
extern crate docopt;
extern crate env_logger;
#[macro_use]
extern crate failure;
extern crate image;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate vobsub;

use docopt::Docopt;
use std::fs;
use std::path::Path;
use vobsub::{Index, Result, Subtitle};

const USAGE: &'static str = "
Usage: vobsub2png [options] <idx-file>

Options:
  -o, --out-dir=DIR   Output directory.  Defaults to something based on the name
                      of the *.idx file.
";

#[derive(Debug, Deserialize)]
struct Args {
    arg_idx_file: String,
    flag_out_dir: Option<String>,
}

quick_main!(run);

#[derive(Serialize)]
struct IndexInfo {
    subtitles: Vec<SubInfo>,
}

#[derive(Serialize)]
struct SubInfo {
    start: f64,
    end: f64,
    force: bool,
    position: (u16, u16),
    size: (u16, u16),
    path: String,
}

fn run() -> Result<()> {
    env_logger::init();

    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());

    let path = Path::new(&args.arg_idx_file);
    let out_dir = match args.flag_out_dir {
        Some(ref o) => Path::new(o).to_owned(),
        None => {
            let stem = path
                .file_stem()
                .and_then(|s| s.to_str())
                .ok_or_else(|| format_err!("no filename in {}", path.display()))?;
            Path::new(&format!("{}_subtitles", stem)).to_owned()
        }
    };
    fs::create_dir_all(&out_dir)?;

    let idx = Index::open(path)?;
    let mut info = IndexInfo { subtitles: vec![] };
    for (i, sub) in idx.subtitles().enumerate() {
        let sub: Subtitle = sub?;
        let image = sub.to_image(idx.palette());
        let image_name = format!("{:04}.png", i);
        let image_path = out_dir.join(&image_name);
        image.save(&image_path)?;

        info.subtitles.push(SubInfo {
            start: sub.start_time(),
            end: sub.end_time(),
            force: sub.force(),
            position: (sub.coordinates().left(), sub.coordinates().top()),
            size: (sub.coordinates().width(), sub.coordinates().height()),
            path: image_name,
        });
    }

    let json_path = out_dir.join("index.json");
    let mut json_file: fs::File = fs::File::create(&json_path)?;
    serde_json::to_writer(&mut json_file, &info)
        .map_err(|e| format_err!("error writing index.json: {}", e))?;

    Ok(())
}

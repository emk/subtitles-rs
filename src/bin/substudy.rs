//! Command-line iterface to substudy.

extern crate rustc_serialize;
extern crate docopt;

extern crate substudy;

use docopt::Docopt;
use std::path::Path;

use substudy::err::Result;
use substudy::srt::SubtitleFile;
use substudy::clean::clean_subtitle_file;
use substudy::align::combine_files;
#[cfg(feature = "video")]
use substudy::video;

const USAGE_HEADER: &'static str = "
Subtitle processing tools for students of foreign languages

Usage: substudy clean <subtitles>
       substudy combine <foreign-subtitles> <native-subtitles>
";

const USAGE_VIDEO: &'static str = "
       substudy tracks <media-file>
";

const USAGE_FOOTER: &'static str = "
       substudy --help

For now, all subtitles must be in *.srt format. Many common encodings
will be automatically detected, but try converting to UTF-8 if you
have problems.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    cmd_clean: bool,
    cmd_combine: bool,
    cmd_tracks: bool,
    arg_subtitles: String,
    arg_foreign_subtitles: String,
    arg_native_subtitles: String,
    arg_media_file: String,
}

// Choose and run the appropriate command.
fn run(args: &Args) -> Result<String> {
    match *args {
        Args{cmd_clean: true, arg_subtitles: ref path, ..} =>
            cmd_clean(&Path::new(path)),
        Args{cmd_combine: true, arg_foreign_subtitles: ref path1,
             arg_native_subtitles: ref path2, ..} =>
            cmd_combine(&Path::new(path1), &Path::new(path2)),
        #[cfg(feature = "video")]
        Args{cmd_tracks: true, arg_media_file: ref path, ..} =>
            cmd_tracks(&Path::new(path)),
        _ => panic!("Unexpected argument combination: {:?}", args)
    }
}

fn cmd_clean(path: &Path) -> Result<String> {
    let file1 = clean_subtitle_file(&try!(SubtitleFile::from_path(path)));
    Ok(file1.to_string())
}

fn cmd_combine(path1: &Path, path2: &Path) -> Result<String> {
    let file1 = clean_subtitle_file(&try!(SubtitleFile::from_path(path1)));
    let file2 = clean_subtitle_file(&try!(SubtitleFile::from_path(path2)));
    Ok(combine_files(&file1, &file2).to_string())
}

#[cfg(feature = "video")]
fn cmd_tracks(path: &Path) -> Result<String> {
    let v = try!(video::Video::new(path));
    for stream in v.streams() {
        let lang: &str = match stream.tags.get("language") {
            Some(ref code) => code,
            None => "unknown",
        };
        println!("#{} {:?} ({})", stream.index, stream.codec_type, lang);
    }
    Ok("".to_owned())
}

fn main() {
    let mut usage = USAGE_HEADER.trim_right().to_owned();
    if cfg!(feature = "video") {
        usage.push_str(USAGE_VIDEO.trim_right());
    }
    usage.push_str(USAGE_FOOTER.trim_right());

    // Parse our command-line arguments using docopt (very shiny).
    let args: Args = Docopt::new(usage)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    // Decide which command to run, and run it.
    match run(&args) {
        Ok(ref output) => print!("{}", output),
        Err(ref err) => println!("{}", err),
    }
}

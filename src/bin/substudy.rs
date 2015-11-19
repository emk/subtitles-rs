//! Command-line iterface to substudy.

extern crate rustc_serialize;
extern crate docopt;

extern crate substudy;

use docopt::Docopt;
use std::path::Path;

use substudy::err::SubStudyResult;
use substudy::srt::SubtitleFile;
use substudy::clean::clean_subtitle_file;
use substudy::align::combine_files;

const USAGE: &'static str = "
Subtitle processing tools for students of foreign languages

Usage: substudy clean <subtitles>
       substudy combine <foreign-subtitles> <native-subtitles>
       substudy --help

For now, all subtitles must be in *.srt format. Many common encodings
will be automatically detected, but try converting to UTF-8 if you
have problems.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    cmd_clean: bool,
    cmd_combine: bool,
    arg_subtitles: String,
    arg_foreign_subtitles: String,
    arg_native_subtitles: String,
}

// Choose and run the appropriate command.
fn run(args: &Args) -> SubStudyResult<String> {
    match *args {
        Args{cmd_clean: true, arg_subtitles: ref path, ..} =>
            cmd_clean(&Path::new(path)),
        Args{cmd_combine: true, arg_foreign_subtitles: ref path1,
             arg_native_subtitles: ref path2, ..} =>
            cmd_combine(&Path::new(path1), &Path::new(path2)),
        _ => panic!("Unexpected argument combination: {:?}", args)
    }
}

fn cmd_clean(path: &Path) -> SubStudyResult<String> {
    let file1 = clean_subtitle_file(&try!(SubtitleFile::from_path(path)));
    Ok(file1.to_string())
}

fn cmd_combine(path1: &Path, path2: &Path) -> SubStudyResult<String> {
    let file1 = clean_subtitle_file(&try!(SubtitleFile::from_path(path1)));
    let file2 = clean_subtitle_file(&try!(SubtitleFile::from_path(path2)));
    Ok(combine_files(&file1, &file2).to_string())
}

fn main() {
    // Parse our command-line arguments using docopt (very shiny).
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    // Decide which command to run, and run it.
    print!("{}", run(&args).unwrap());
}

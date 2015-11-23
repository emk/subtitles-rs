//! Command-line iterface to substudy.

extern crate rustc_serialize;
extern crate docopt;

extern crate substudy;

use docopt::Docopt;
use std::path::Path;
use std::process::exit;

use substudy::err::{err_str, Result};
use substudy::srt::SubtitleFile;
use substudy::clean::clean_subtitle_file;
use substudy::align::combine_files;
use substudy::video;

const USAGE: &'static str = "
Subtitle processing tools for students of foreign languages

Usage: substudy clean <subtitles>
       substudy combine <foreign-subtitles> <native-subtitles>
       substudy tracks <media-file>
       substudy extract <media-file> <subtitles> <index>
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
    cmd_extract: bool,
    arg_subtitles: String,
    arg_foreign_subtitles: String,
    arg_native_subtitles: String,
    arg_media_file: String,
    arg_index: usize,
}

// Choose and run the appropriate command.
fn run(args: &Args) -> Result<()> {
    match *args {
        Args{cmd_clean: true, arg_subtitles: ref path, ..} =>
            cmd_clean(&Path::new(path)),
        Args{cmd_combine: true, arg_foreign_subtitles: ref path1,
             arg_native_subtitles: ref path2, ..} =>
            cmd_combine(&Path::new(path1), &Path::new(path2)),
        Args{cmd_tracks: true, arg_media_file: ref path, ..} =>
            cmd_tracks(&Path::new(path)),
        Args{cmd_extract: true, arg_subtitles: ref sub_path,
             arg_media_file: ref media_path, arg_index: index, ..} =>
            cmd_extract(&Path::new(media_path), &Path::new(sub_path),
                        index),
        _ => panic!("Unexpected argument combination: {:?}", args)
    }
}

fn cmd_clean(path: &Path) -> Result<()> {
    let file1 = clean_subtitle_file(&try!(SubtitleFile::from_path(path)));
    println!("{}", file1.to_string());
    Ok(())
}

fn cmd_combine(path1: &Path, path2: &Path) -> Result<()> {
    let file1 = clean_subtitle_file(&try!(SubtitleFile::from_path(path1)));
    let file2 = clean_subtitle_file(&try!(SubtitleFile::from_path(path2)));
    println!("{}", combine_files(&file1, &file2).to_string());
    Ok(())
}

fn cmd_tracks(path: &Path) -> Result<()> {
    let v = try!(video::Video::new(path));
    for stream in v.streams() {
        let lang: &str = match stream.tags.get("language") {
            Some(ref code) => code,
            None => "unknown",
        };
        println!("#{} {} {:?}", stream.index, lang, stream.codec_type);
    }
    Ok(())
}

fn cmd_extract(media_path: &Path, sub_path: &Path, index: usize) ->
    Result<()>
{
    let video = try!(video::Video::new(media_path));
    let subs = clean_subtitle_file(&try!(SubtitleFile::from_path(sub_path)));
    let sub = try!(subs.find(index).ok_or_else(|| {
        err_str(format!("Can't find subtitle #{}", index))
    }));

    println!("{}", sub.to_string());
    println!("Extracting at: {}", sub.midpoint());

    let img_path_str = format!("sub{}.jpg", index);
    let img_path = Path::new(&img_path_str);
    try!(video.extract_image(sub.midpoint(), &img_path));

    let sound_path_str = format!("sub{}.mp3", index);
    let sound_path = Path::new(&sound_path_str);
    try!(video.extract_audio(sub.begin, sub.end - sub.begin, &sound_path));

    Ok(())
}

fn main() {
    // Parse our command-line arguments using docopt (very shiny).
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    // Decide which command to run, and run it.
    if let Err(ref err) = run(&args) {
        // Print any error and exit with an error status.
        println!("{}", err);
        exit(1);
    }
}

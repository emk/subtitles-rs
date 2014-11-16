//! Command-line iterface to substudy.

#![license = "Public domain (Unlicense)"]
#![deny(warnings)]

extern crate substudy;

use std::os;
use std::io::stdio::stderr;
use std::io::Writer;

use substudy::err::Result as SubStudyResult;
use substudy::srt::SubtitleFile;
use substudy::align::combine_files;

fn combine(path1: &Path, path2: &Path) -> SubStudyResult<String> {
    let file1 = try!(SubtitleFile::from_path(path1));
    let file2 = try!(SubtitleFile::from_path(path2));
    Ok(combine_files(&file1, &file2).to_string())
}

fn main() {
    // Parse our command-line arguments.
    let args = os::args();
    if args.len() != 3 {
        stderr().write_line("Usage: substudy subs1.srt subs2.srt").unwrap();
        os::set_exit_status(1);
        return;
    }
    let path1 = Path::new(args[1].as_slice());
    let path2 = Path::new(args[2].as_slice());

    // Combine the specified files.
    print!("{}", combine(&path1, &path2).unwrap());
}

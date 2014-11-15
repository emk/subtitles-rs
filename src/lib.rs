//! Tools for studying foreign languages using subtitles.

#![license = "Public domain (Unlicense)"]
#![experimental]
#![deny(missing_docs)]
#![deny(warnings)]
#![feature(phase)]

#[phase(plugin)]
extern crate peg_syntax_ext;

/// A single SRT-format subtitle, minus some of the optional fields used in
/// various versions of the file format.
pub struct Subtitle {
    /// The index of this subtitle.  We should normalize these to start
    /// with 1 on output.
    pub index: u32,

    /// The start time of this subtitle, in seconds.
    pub begin: f32,

    /// The end time of this subtitle, in seconds.
    pub end: f32,

    /// The lines of text in this subtitle.
    pub lines: Vec<String>
}

/// The contents of an SRT-format subtitle file.
pub struct SubtitleFile {
    /// The subtitles in this file.
    pub subtitles: Vec<Subtitle>
}

peg! parser(r#"
use Subtitle;
use SubtitleFile;

#[pub]
subtitle_file -> SubtitleFile
    = blank_lines? result:subtitles blank_lines? {
        SubtitleFile{subtitles: result}
    }

subtitles -> Vec<Subtitle>
    = subs:subtitle ** blank_lines { subs }

subtitle -> Subtitle
    = index:digits newline t:times newline l:lines {
        let (b, e) = t;
        Subtitle{index: index, begin: b, end: e, lines: l}
    }

times -> (f32, f32)
    = begin:time " --> " end:time { (begin, end) }

time -> f32
    = hh:digits ":" mm:digits ":" ss:comma_float {
        (hh as f32)*3600.0 + (mm as f32)*60.0 + ss
    }

lines -> Vec<String>
    = lines:line ** newline { lines }

line -> String
    = [^\r\n]+ { match_str.to_string() }

digits -> u32
    = [0-9]+ { from_str::<u32>(match_str).unwrap() }

comma_float -> f32
    = [0-9]+ "," [0-9]+ {
        let fixed: String = match_str.replace(",", ".");
        from_str::<f32>(fixed.as_slice()).unwrap()
    }

newline
    = "\r"? "\n"

blank_lines
    = newline+

"#)

/// Parse raw subtitle text into an appropriate structure.
pub fn parse_subtitles(data: &str) -> Result<SubtitleFile,String> {
    parser::subtitle_file(data)
}

#[cfg(test)]
mod test {
    use std::io::{File,IoResult};
    use parse_subtitles;

    fn read_file(path: &Path) -> IoResult<String> {
        let mut file = try!(File::open(path));
        file.read_to_string()
    }

    #[test]
    fn parse_srt_test() {
        let data = read_file(&Path::new("fixtures/sample.es.srt")).unwrap();
        let srt = parse_subtitles(data.as_slice()).unwrap();
        assert_eq!(5, srt.subtitles.len());

        let sub = &srt.subtitles[0];
        assert_eq!(16, sub.index);
        assert_eq!(62.328, sub.begin);
        assert_eq!(64.664, sub.end);
        assert_eq!(vec!("¡Si! ¡Aang ha vuelto!".to_string()), sub.lines);

        let sub2 = &srt.subtitles[2];
        assert_eq!(vec!("Tu diste la señal a la armada".to_string(),
                        "del fuego con la bengala,".to_string()),
                   sub2.lines);
    }
}


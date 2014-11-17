//! SRT-format subtitle support.

use std::io::File;
use std::num::Float;

use err::Result as MyResult;
use err::ParseError;

/// Format seconds using the standard SRT time format.
pub fn format_time(time: f32) -> String {
    let (h, rem) = ((time / 3600.0).trunc(), time % 3600.0);
    let (m, s) = ((rem / 60.0).trunc(), rem % 60.0);
    (format!("{:02f}:{:02f}:{:0>6.3f}", h, m, s)).replace(".", ",")
}

/// A single SRT-format subtitle, minus some of the optional fields used in
/// various versions of the file format.
#[deriving(Show, PartialEq, Clone)]
pub struct Subtitle {
    /// The index of this subtitle.  We should normalize these to start
    /// with 1 on output.
    pub index: uint,

    /// The start time of this subtitle, in seconds.
    pub begin: f32,

    /// The end time of this subtitle, in seconds.
    pub end: f32,

    /// The lines of text in this subtitle.
    pub lines: Vec<String>
}

impl Subtitle {
    /// Return a string representation of this subtitle.
    pub fn to_string(&self) -> String {
        format!("{}\n{} --> {}\n{}\n", self.index,
                format_time(self.begin),
                format_time(self.end),
                self.lines.connect("\n"))
    }
}

/// The contents of an SRT-format subtitle file.
#[deriving(Show, PartialEq)]
pub struct SubtitleFile {
    /// The subtitles in this file.
    pub subtitles: Vec<Subtitle>
}

impl SubtitleFile {
    /// Parse raw subtitle text into an appropriate structure.
    pub fn from_str(data: &str) -> MyResult<SubtitleFile> {
        match grammar::subtitle_file(data) {
            Ok(file) => Ok(file),
            Err(msg) => Err(ParseError(msg))
        }
    }

    /// Parse the subtitle file found at the specified path.
    pub fn from_path(path: &Path) -> MyResult<SubtitleFile> {
        let mut file = try!(File::open(path));
        let data = try!(file.read_to_string());
        SubtitleFile::from_str(data.as_slice())
    }

    /// Convert subtitles to a string.
    pub fn to_string(&self) -> String {
        let subs: Vec<String> =
            self.subtitles.iter().map(|s| s.to_string()).collect();
        // The BOM (byte-order mark) is generally discouraged on Linux, but
        // it's sometimes needed to get good results under Windows.  We
        // include it here because Wikipedia says that SRT files files
        // default to various legacy encoding, but that the BOM can be used
        // for Unicode.
        format!("\uFEFF{}", subs.connect("\n"))
    }
}

#[cfg(test)]
mod test {
    use srt::{SubtitleFile,Subtitle};

    #[test]
    fn subtitle_file_from_path() {
        let path = Path::new("fixtures/sample.es.srt");
        let srt = SubtitleFile::from_path(&path).unwrap();
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

    #[test]
    fn subtitle_to_string() {
        let sub = Subtitle{index: 4, begin: 61.5, end: 63.75,
                           lines: vec!("Line 1".to_string(),
                                       "<i>Line 2</i>".to_string())};
        let expected = r"4
00:01:01,500 --> 00:01:03,750
Line 1
<i>Line 2</i>
".to_string();
        assert_eq!(expected, sub.to_string());
    }

    #[test]
    fn subtitle_file_to_string() {
        let data = "\uFEFF16
00:01:02,328 --> 00:01:04,664
Line 1.1

17
00:01:12,839 --> 00:01:13,839
Line 2.1
";
        let srt = SubtitleFile::from_str(data).unwrap();
        assert_eq!(data, srt.to_string().as_slice());
    }
}

// Our parser expression grammar.  We'd like to move this to another file.
peg! grammar(r#"
use srt::{Subtitle,SubtitleFile};

#[pub]
subtitle_file -> SubtitleFile
    = bom? blank_lines? result:subtitles blank_lines? {
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

digits -> uint
    = [0-9]+ { from_str::<uint>(match_str).unwrap() }

comma_float -> f32
    = [0-9]+ "," [0-9]+ {
        let fixed: String = match_str.replace(",", ".");
        from_str::<f32>(fixed.as_slice()).unwrap()
    }

bom
    = "\uFEFF"

newline
    = "\r"? "\n"

blank_lines
    = newline+

"#)

//! SRT-format subtitle support.

use std::fs::File;
use std::io::Read;
use std::path::Path;
use err::Result;
use decode::smart_decode;
use grammar;

/// Format seconds using the standard SRT time format.
pub fn format_time(time: f32) -> String {
    let (h, rem) = ((time / 3600.0).trunc(), time % 3600.0);
    let (m, s) = ((rem / 60.0).trunc(), rem % 60.0);
    (format!("{:02}:{:02}:{:0>6.3}", h, m, s)).replace(".", ",")
}

/// A single SRT-format subtitle, minus some of the optional fields used in
/// various versions of the file format.
#[derive(Debug, PartialEq, Clone)]
pub struct Subtitle {
    /// The index of this subtitle.  We should normalize these to start
    /// with 1 on output.
    pub index: usize,

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
                self.lines.join("\n"))
    }
}

/// The contents of an SRT-format subtitle file.
#[derive(Debug, PartialEq)]
pub struct SubtitleFile {
    /// The subtitles in this file.
    pub subtitles: Vec<Subtitle>
}

impl SubtitleFile {
    /// Parse raw subtitle text into an appropriate structure.
    pub fn from_str(data: &str) -> Result<SubtitleFile> {
        Ok(try!(grammar::subtitle_file(data)))
    }

    /// Parse the subtitle file found at the specified path.
    pub fn from_path(path: &Path) -> Result<SubtitleFile> {
        let mut file = try!(File::open(path));
        let mut bytes = Vec::new();
        try!(file.read_to_end(&mut bytes));
        let data = try!(smart_decode(&bytes));
        SubtitleFile::from_str(&data)
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
        format!("\u{FEFF}{}", subs.join("\n"))
    }
}

#[cfg(test)]
mod test {
    use std::path::Path;
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
        let data = "\u{FEFF}16
00:01:02,328 --> 00:01:04,664
Line 1.1

17
00:01:12,839 --> 00:01:13,839
Line 2.1
";
        let srt = SubtitleFile::from_str(data).unwrap();
        assert_eq!(data, &srt.to_string());
    }
}

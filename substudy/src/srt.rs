//! SRT-format subtitle support.

use common_failures::prelude::*;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use decode::smart_decode;
use clean::{clean_subtitle_file, strip_formatting};
use grammar;
use lang::Lang;
use time::Period;

/// Format seconds using the standard SRT time format.
pub fn format_time(time: f32) -> String {
    let (h, rem) = ((time / 3600.0).trunc(), time % 3600.0);
    let (m, s) = ((rem / 60.0).trunc(), rem % 60.0);
    (format!("{:02}:{:02}:{:0>6.3}", h, m, s)).replace(".", ",")
}

/// A single SRT-format subtitle, minus some of the optional fields used in
/// various versions of the file format.
#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Subtitle {
    /// The index of this subtitle.  We should normalize these to start
    /// with 1 on output.
    pub index: usize,

    /// The time period during which this subtitle is shown.
    pub period: Period,

    /// The lines of text in this subtitle.
    pub lines: Vec<String>,
}

impl Subtitle {
    /// Return a string representation of this subtitle.
    pub fn to_string(&self) -> String {
        format!(
            "{}\n{} --> {}\n{}\n",
            self.index,
            format_time(self.period.begin()),
            format_time(self.period.end()),
            self.lines.join("\n")
        )
    }

    /// Return a plain-text version of this subtitle.
    pub fn plain_text(&self) -> String {
        strip_formatting(&self.lines.join(" ")).into_owned()
    }
}

/// The contents of an SRT-format subtitle file.
#[derive(Debug, PartialEq)]
pub struct SubtitleFile {
    /// The subtitles in this file.
    pub subtitles: Vec<Subtitle>,
}

impl SubtitleFile {
    /// Parse raw subtitle text into an appropriate structure.
    pub fn from_str(data: &str) -> Result<SubtitleFile> {
        // Use `trim_left_matches` to remove the leading BOM ("byte order mark")
        // that's present in much Windows UTF-8 data. Note that if it appears
        // multiple times, we would remove all the copies, but we've never seen
        // that in the wild.
        Ok(grammar::subtitle_file(data.trim_left_matches("\u{FEFF}"))
            .with_context(|_| format_err!("could not parse subtitles"))?)
    }

    /// Parse the subtitle file found at the specified path.
    pub fn from_path(path: &Path) -> Result<SubtitleFile> {
        let mut file = File::open(path).io_read_context(path)?;
        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes).io_read_context(path)?;
        let data = smart_decode(&bytes).io_read_context(path)?;
        Ok(SubtitleFile::from_str(&data).io_read_context(path)?)
    }

    /// Parse and normalize the subtitle file found at the specified path.
    pub fn cleaned_from_path(path: &Path) -> Result<SubtitleFile> {
        let raw = SubtitleFile::from_path(path)?;
        Ok(clean_subtitle_file(&raw)?)
    }

    /// Convert subtitles to a string.
    pub fn to_string(&self) -> String {
        let subs: Vec<String> = self.subtitles.iter().map(|s| s.to_string()).collect();
        // The BOM (byte-order mark) is generally discouraged on Linux, but
        // it's sometimes needed to get good results under Windows.  We
        // include it here because Wikipedia says that SRT files files
        // default to various legacy encoding, but that the BOM can be used
        // for Unicode.
        format!("\u{FEFF}{}", subs.join("\n"))
    }

    /// Find the subtitle with the given index.
    pub fn find(&self, index: usize) -> Option<&Subtitle> {
        self.subtitles.iter().find(|s| s.index == index)
    }

    /// Detect the language used in these subtitles.
    pub fn detect_language(&self) -> Option<Lang> {
        let subs: Vec<_> = self.subtitles.iter().map(|s| s.plain_text()).collect();
        let text = subs.join("\n");
        Lang::for_text(&text)
    }
}

#[cfg(test)]
mod test {
    use std::path::Path;
    use srt::{Subtitle, SubtitleFile};
    use lang::Lang;
    use time::Period;

    #[test]
    fn subtitle_file_from_path() {
        let path = Path::new("fixtures/sample.es.srt");
        let srt = SubtitleFile::from_path(&path).unwrap();
        assert_eq!(5, srt.subtitles.len());

        let sub = &srt.subtitles[0];
        assert_eq!(16, sub.index);
        assert_eq!(62.328, sub.period.begin());
        assert_eq!(64.664, sub.period.end());
        assert_eq!(vec!["¡Si! ¡Aang ha vuelto!".to_string()], sub.lines);

        let sub2 = &srt.subtitles[2];
        assert_eq!(
            vec![
                "Tu diste la señal a la armada".to_string(),
                "del fuego con la bengala,".to_string(),
            ],
            sub2.lines
        );
    }

    #[test]
    fn subtitle_to_string() {
        let sub = Subtitle {
            index: 4,
            period: Period::new(61.5, 63.75).unwrap(),
            lines: vec!["Line 1".to_string(), "<i>Line 2</i>".to_string()],
        };
        let expected = r"4
00:01:01,500 --> 00:01:03,750
Line 1
<i>Line 2</i>
"
            .to_string();
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

    #[test]
    fn zero_duration_subtitle() {
        let data = "\u{FEFF}16
00:00:01,000 --> 00:00:01,000
Text
";
        let srt = SubtitleFile::from_str(data).unwrap();
        assert_eq!(srt.subtitles.len(), 1);
        assert_eq!(srt.subtitles[0].period.begin(), 1.0);
        assert_eq!(srt.subtitles[0].period.end(), 1.001);
    }

    #[test]
    fn detect_language() {
        let path_es = Path::new("fixtures/sample.es.srt");
        let srt_es = SubtitleFile::from_path(&path_es).unwrap();
        assert_eq!(Some(Lang::iso639("es").unwrap()), srt_es.detect_language());

        let path_en = Path::new("fixtures/sample.en.srt");
        let srt_en = SubtitleFile::from_path(&path_en).unwrap();
        assert_eq!(Some(Lang::iso639("en").unwrap()), srt_en.detect_language());
    }
}


//! Tools for merge multiple subtitles into one.

use srt::Subtitle;

static LINE_MAX: usize = 43;

/// Merge several subtitles into a single subtitle.  Returns `None` if no
/// subtitles have been supplied, or if the resulting subtitle has no text.
pub fn merge_subtitles(subs: &[Subtitle]) -> Option<Subtitle> {
    // Break text into words.
    let mut words = vec!();
    for sub in subs.iter() {
        for line in sub.lines.iter() {
            for word in regex!(r"\s+").split(&line[]) {
                words.push(word.to_string());
            }
        }
    }

    // Assemble words back into nicely-wrapped lines.
    let mut lines = vec!();
    let mut line = "".to_string();
    for word in words.iter() {
        if line.len() == 0 {
            line.push_str(&word[]);
        } else if line.len() + 1 + word.len() <= LINE_MAX {
            line.push_str(" ");
            line.push_str(&word[]);
        } else {
            lines.push(line);
            line = word.clone();
        }

        if line.len() >= LINE_MAX {
            lines.push(line);
            line = "".to_string();
        }
    }
    if line.len() > 0 {
        lines.push(line);
    }

    if lines.is_empty() { return None; }
    Some(Subtitle{index: subs[0].index, begin: subs[0].begin,
                  end: subs[subs.len()-1].end, lines: lines})
}

#[cfg(test)]
mod test {
    use srt::SubtitleFile;
    use merge::merge_subtitles;

    fn merge_for_test(input: &str) -> String {
        let srt = SubtitleFile::from_str(input).unwrap();
        merge_subtitles(&srt.subtitles[]).unwrap().to_string()
    }

    #[test]
    fn merge_zero() {
        assert_eq!(None, merge_subtitles(&[]));
    }
    
    #[test]
    fn merge_one() {
        let example = "18
00:01:02,328 --> 00:01:03,162
Yay!
Yay!
";
        let expected = r"18
00:01:02,328 --> 00:01:03,162
Yay! Yay!
";
        assert_eq!(expected, &merge_for_test(example)[]);
    }

    #[test]
    fn merge_short() {
        let example = r"18
00:01:02,328 --> 00:01:03,162
Yay!
Yay!

19
00:01:03,163 --> 00:01:04,664
Aang's back!
";
        let expected = r"18
00:01:02,328 --> 00:01:04,664
Yay! Yay! Aang's back!
";
        assert_eq!(expected, &merge_for_test(example)[]);
    }

    #[test]
    fn merge_long() {
        let example = r"23
00:01:16,342 --> 00:01:17,176
You're leading
them straight

24
00:01:17,677 --> 00:01:18,678
to us, aren't you?
";
        let expected = r"23
00:01:16,342 --> 00:01:18,678
You're leading them straight to us, aren't
you?
";
        assert_eq!(expected, &merge_for_test(example)[]);
    }
}


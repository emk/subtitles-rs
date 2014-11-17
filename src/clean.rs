//! Tools for cleaning up subtitle files and getting them into some
//! kind of normalized format.

use regex::Regex;
use srt::{Subtitle,SubtitleFile};

// Used to remove the following common sorts of closed-caption clutter:
//
//     SPEAKER:
//     ( sound effect )
//     ♪ music ♪
static CLUTTER: Regex = regex!(r"(\( [^)]*(\)|$)|♪ [^♪]*(♪|$)|[A-Z]{2,}:)");

// Used to compress and normalize consecutive whitespace.
static WHITESPACE: Regex = regex!(r"\s+");

// Clean up a single subtitle line.
fn clean_line(line: &str) -> String {
    WHITESPACE.replace_all(CLUTTER.replace_all(line, "").as_slice(), " ")
        .trim().to_string()
}

// Clean up a subtitle, or discard it if it looks useless.
fn clean_subtitle(sub: &Subtitle) -> Option<Subtitle> {
    if sub.end <= sub.begin { return None; }
    let lines: Vec<String> = sub.lines.iter()
        .map(|l| clean_line(l.as_slice()))
        .filter(|l| l.len() > 0)
        .map(|l| l.to_string()).collect();
    if lines.len() == 0 { return None; }
    Some(Subtitle{index: sub.index, begin: sub.begin, end: sub.end,
                  lines: lines})
}

/// Clean up various issues with subtitle files, including:
///
/// * Out of order subtitles.
/// * Overlapping subtitles.
/// * Sound effects.
/// * Music symbols.
pub fn clean_subtitle_file(file: &SubtitleFile) -> SubtitleFile {
    // Clean individual subtitles and sort.
    let mut subs: Vec<Subtitle> =
        file.subtitles.iter().filter_map(clean_subtitle).collect();
    subs.sort_by(|a, b| { a.begin.partial_cmp(&b.begin).unwrap() });

    // Fix overlaps.
    if subs.len() >= 2 {
        for i in range(0, subs.len()-1) {
            if subs[i].end > subs[i+1].begin {
                subs[i].end = subs[i+1].begin - 0.001;
            }
        }
    }

    // Renumber and return.
    for (i, ref mut sub) in subs.iter_mut().enumerate() {
        sub.index = i+1;
    }
    SubtitleFile{subtitles: subs}
}

#[test]
fn test_clean_subtitle_file() {
    let dirty = SubtitleFile::from_str(r"19
00:01:03,163 --> 00:01:04,664
They've arrived.
( <i>door slams</i> )

20
00:01:07,700 --> 00:01:10,736
( <i>cheering</i> )

21
00:01:12,839 --> 00:01:13,840
♪ ♪

18
00:01:02,328 --> 00:01:03,162
JOE: Hey! ( waves arms )

51
00:02:35,636 --> 00:02:32,895
Negative length.

53
00:02:47,965 --> 00:02:50,684
Out of order.

52
00:02:42,658 --> 00:02:48,865
Overlapping.
").unwrap();

    let cleaned = "\uFEFF1
00:01:02,328 --> 00:01:03,162
Hey!

2
00:01:03,163 --> 00:01:04,664
They've arrived.

3
00:02:42,658 --> 00:02:47,964
Overlapping.

4
00:02:47,965 --> 00:02:50,684
Out of order.
";

    assert_eq!(cleaned, clean_subtitle_file(&dirty).to_string().as_slice());
}

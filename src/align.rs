//! Align two subtitle files.

use std::cmp::Ordering;
use time::MIN_SPACING;

use srt::{Subtitle, SubtitleFile};
use merge::merge_subtitles;
use clean::{clean_subtitle_file, strip_formatting};

use self::MatchQuality::{Nearby, NoMatch, Overlap};

// How well do two subtitles match each other, going solely by the time?
#[derive(PartialEq, Clone, Copy, Debug)]
enum MatchQuality {
    NoMatch,      // More than two seconds away.
    Nearby(f32),  // 0.0 <= seconds <= 2.0
    Overlap(f32), // 0.0 < seconds
}

impl PartialOrd for MatchQuality {
    fn partial_cmp(&self, other: &MatchQuality) -> Option<Ordering> {
        match (self, other) {
            (&Overlap(v1), &Overlap(v2)) => v1.partial_cmp(&v2),
            (&Nearby(v1), &Nearby(v2)) => v1.partial_cmp(&v2).map(|c| c.reverse()),
            (&NoMatch, &NoMatch) => Some(Ordering::Equal),
            (&Overlap(_), _) => Some(Ordering::Greater),
            (_, &Overlap(_)) => Some(Ordering::Less),
            (&Nearby(_), _) => Some(Ordering::Greater),
            (_, &Nearby(_)) => Some(Ordering::Less),
        }
    }
}

// Calculate the match quality of two subtitles.
fn match_quality(sub1: &Subtitle, sub2: &Subtitle) -> MatchQuality {
    match sub1.period.distance(sub2.period) {
        Some(distance) if distance > 2.0 => NoMatch,
        Some(distance) => Nearby(distance),
        None => Overlap(sub1.period.overlap(sub2.period)),
    }
}

// Find the index of the best match for `sub` in `candidates`.
fn best_match(sub: &Subtitle, candidates: &[Subtitle]) -> Option<usize> {
    let mut best: Option<(usize, MatchQuality)> = None;
    for (i, candidate) in candidates.iter().enumerate() {
        let mq = match_quality(sub, candidate);
        if mq == NoMatch {
            continue;
        }
        match best {
            None => {
                best = Some((i, mq));
            }
            Some((_, old_mq)) if mq > old_mq => {
                best = Some((i, mq));
            }
            _ => {}
        }
    }
    best.map(|(i, _)| i)
}

// Find the index of the best match each subtitle in `subs` in `candidates`.
fn best_matches(subs: &[Subtitle], candidates: &[Subtitle]) -> Vec<Option<usize>> {
    // We could be a lot more efficient about this if we wanted.
    subs.iter().map(|s| best_match(s, candidates)).collect()
}

// Make sure the subtitles are in-order and have sensible begin/end times.
//fn clean_subs(file: &SubtitleFile) -> Vec<Subtitle> {
//    // Remove subtitles with bogus begin/end times.
//    let result = file.subtitles.iter().filter(|s| s.begin < s.end).collect();
//    // Sort subtitles by begin time.
//    result.sort_by(|a, b| a.begin.cmp(b.begin));
//    result
//}

/// Alignment specification, showing how to match up the specified indices
/// in two subtitle files.
type Alignment = Vec<(Vec<usize>, Vec<usize>)>;

// Returns true if `items[i].is_some()` and the value is found in `group`.
// Returns false if `i` is out of bounds.
fn group_contains(group: &[usize], items: &[Option<usize>], i: usize) -> bool {
    if !(i < items.len()) {
        return false;
    }
    match items[i] {
        None => false,
        Some(v) => group.iter().position(|e| *e == v).is_some(),
    }
}

/// Find a good way to align two subtitle files.
fn alignment(file1: &SubtitleFile, file2: &SubtitleFile) -> Alignment {
    let (subs1, subs2) = (&file1.subtitles, &file2.subtitles);
    // assert!(subs1 && subs2 contain valid subs in ascending order)
    let matches1 = best_matches(subs1, subs2);
    let matches2 = best_matches(subs2, subs1);
    let mut alignment: Alignment = vec![];
    let mut i1 = 0;
    let mut i2 = 0;
    while i1 < subs1.len() && i2 < subs2.len() {
        debug!(
            "subs1: {} matches {:?}, subs2: {} matches {:?}",
            i1,
            matches1[i1],
            i2,
            matches2[i2]
        );
        if subs1[i1].period.begin() < subs2[i2].period.begin()
            && matches1[i1] != Some(i2)
        {
            // Subs1 has an item which doesn't match subs2.
            debug!("unmatched: [{}], []", i1);
            alignment.push((vec![i1], vec![]));
            i1 += 1;
        } else if subs2[i2].period.begin() < subs1[i1].period.begin()
            && matches2[i2] != Some(i1)
        {
            // Subs2 has an item which doesn't match subs1.
            debug!("unmatched: [], [{}]", i2);
            alignment.push((vec![], vec![i2]));
            i2 += 1;
        } else {
            // We have some matches, so let's gather them all together.
            assert!(matches1[i1] == Some(i2) || matches2[i2] == Some(i1));
            let mut matched1 = vec![i1];
            i1 += 1;
            let mut matched2 = vec![i2];
            i2 += 1;
            while group_contains(&matched1, &matches2, i2)
                || group_contains(&matched2, &matches1, i1)
            {
                if group_contains(&matched1, &matches2, i2) {
                    // i2 matches something in matched1, so add to matched2.
                    matched2.push(i2);
                    i2 += 1;
                } else if group_contains(&matched2, &matches1, i1) {
                    // i1 matches something in matched2, so add to matched1.
                    matched1.push(i1);
                    i1 += 1;
                }
                debug!("grouping: {:?}, {:?}", matched1, matched2);
            }
            alignment.push((matched1, matched2));
        }
    }
    alignment
}

#[test]
fn test_alignment() {
    use std::path::Path;

    // Load sample subtitles.
    let path_es = Path::new("fixtures/sample.es.srt");
    let srt_es = SubtitleFile::from_path(&path_es).unwrap();
    let path_en = Path::new("fixtures/sample.en.srt");
    let srt_en = SubtitleFile::from_path(&path_en).unwrap();

    let expected = vec![
        (vec![0], vec![0, 1]),
        (vec![], vec![2]),
        (vec![1], vec![3]),
        (vec![2], vec![4]),
        (vec![3], vec![5, 6]),
        (vec![4], vec![7]),
    ];
    assert_eq!(expected, alignment(&srt_es, &srt_en));
}

/// Align two subtitle files, merging subtitles as necessary.
pub fn align_files(
    file1: &SubtitleFile,
    file2: &SubtitleFile,
) -> Vec<(Option<Subtitle>, Option<Subtitle>)> {
    fn merge(file: &SubtitleFile, indices: &[usize]) -> Option<Subtitle> {
        let mut subs = vec![];
        for &i in indices.iter() {
            subs.push(file.subtitles[i].clone())
        }
        merge_subtitles(&subs)
    }

    alignment(file1, file2)
        .iter()
        .map(|&(ref indices1, ref indices2)| {
            (merge(file1, &indices1), merge(file2, &indices2))
        })
        .collect()
}

/// If we have two files, align them.  If one is missing, just return its
/// subtitles by themselves.
pub fn align_available_files(
    file1: &SubtitleFile,
    file2_opt: Option<&SubtitleFile>,
) -> Vec<(Option<Subtitle>, Option<Subtitle>)> {
    match file2_opt {
        Some(ref file2) => align_files(file1, file2),
        None => file1
            .subtitles
            .iter()
            .cloned()
            .map(|s| (Some(s), None))
            .collect(),
    }
}

// Clone a subtitle and wrap its lines with formatting.
fn clone_as(sub: &Subtitle, before: &str, after: &str) -> Subtitle {
    let lines = sub.lines
        .iter()
        .map(|l| {
            // For now, strip out existing formatting.  We'll change this once
            // color works.
            let cleaned = strip_formatting(&l);
            format!("{}{}{}", before, &cleaned, after)
        })
        .collect();
    Subtitle {
        index: sub.index,
        period: sub.period,
        lines: lines,
    }
}

static STYLE1B: &'static str = "<font color=\"yellow\">";
static STYLE1E: &'static str = "</font>";
static STYLE2B: &'static str = "<i>";
static STYLE2E: &'static str = "</i>";

/// Combine two subtitle files into an aligned file.
pub fn combine_files(file1: &SubtitleFile, file2: &SubtitleFile) -> SubtitleFile {
    let mut subs: Vec<Subtitle> = align_files(file1, file2)
        .iter()
        .map(|pair| match pair {
            &(None, None) => panic!("Shouldn't have empty alignment pair!"),
            &(Some(ref sub), None) => clone_as(sub, STYLE1B, STYLE1E),
            &(None, Some(ref sub)) => clone_as(sub, STYLE2B, STYLE2E),
            &(Some(ref sub1), Some(ref sub2)) => {
                let mut new = clone_as(sub1, STYLE1B, STYLE1E);
                let to_merge = clone_as(sub2, STYLE2B, STYLE2E);
                let mut lines = to_merge.lines.clone();
                lines.extend(new.lines);
                new.lines = lines;
                new
            }
        })
        .collect();

    // Restore our invariants, which may have been thrown off by weird matches.
    // TODO: Print out weird matches and analyze.
    subs = clean_subtitle_file(&SubtitleFile { subtitles: subs })
        .unwrap()
        .subtitles;

    // Extend the time of each sub to account for increased text.  We rely
    // on clean_subtitle_file to clean up any remaining overlaps.
    for i in 0..subs.len() {
        debug!("growing: {:?} ({})", subs[i].period, subs[i].plain_text());
        let mut wanted = subs[i].period.grow(2.0, 2.0);
        if i != 0 {
            debug!("  previous: {:?}", subs[i - 1].period);
            wanted.begin_after(subs[i - 1].period.end()).unwrap();
        }
        if i + 1 < subs.len() {
            // Extend forward into any unused space, but don't intefere
            // with the buffer we want to add before the next subtitle (or
            // allow that space to cut back into space which was originally
            // ours).
            debug!("  next: {:?}", subs[i + 1].period);
            let limit = (subs[i + 1].period.begin() - 2.0)
                .max(subs[i].period.end() + MIN_SPACING);
            wanted.end_before(limit).unwrap();
        }
        subs[i].period = wanted;
    }
    clean_subtitle_file(&SubtitleFile { subtitles: subs }).unwrap()
}

#[test]
fn test_combine_files() {
    use std::path::Path;

    // Load sample subtitles.
    let path_es = Path::new("fixtures/sample.es.srt");
    let srt_es = SubtitleFile::from_path(&path_es).unwrap();
    let path_en = Path::new("fixtures/sample.en.srt");
    let srt_en = SubtitleFile::from_path(&path_en).unwrap();
    let path_combined = Path::new("fixtures/combined.srt");
    let expected = SubtitleFile::from_path(&path_combined).unwrap();
    assert_diff!(
        &expected.to_string(),
        &combine_files(&srt_es, &srt_en).to_string(),
        "\n",
        0
    );
}

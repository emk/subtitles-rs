use std::{fmt, fs::File, iter::repeat, path::Path};

use anyhow::Context as _;
use lazy_static::lazy_static;
use lending_iterator::{lending_iterator::constructors::windows_mut, LendingIterator};
use log::{debug, log_enabled, trace, warn, Level};
use ordered_float::NotNan;
use regex::Regex;
use serde::{Deserialize, Serialize};

use crate::{
    clean::clean_subtitle_file,
    segment::segment_subtitle_text,
    srt::{AppendWithOffset, Subtitle, SubtitleFile},
    time::Period,
    Result,
};

/// Default characters per second for speech. Speech in any language supposedly
/// runs around [39 bits per second][bps]. Word entropy in English is [about
/// 11.82 bits per word][bpw], with 4.5 characters plus ~1 space per word.
///
/// So for English, we have:
///
/// - 11.82 bits/word / 5.5 chars/word = 2.15 bits/char
/// - 39 bits/second / 2.15 bits/char = 18.14 chars/second
///
/// Wikipedia gives us [Speech tempo][]:
///
/// > An alternative measure that has been proposed is that of sounds per
/// > second. One study found rates varying from an average of 9.4 sounds per
/// > second for poetry reading to 13.83 per second for sports commentary.
///
/// But English has an average of more than 1 character per sound. So this
/// checks. We'll use a value in this general range as a default.
///
/// [bps]:
///     https://www.science.org/content/article/human-speech-may-have-universal-transmission-rate-39-bits-second
/// [bpw]:
///     https://cs.stanford.edu/people/eroberts/courses/soco/projects/1999-00/information-theory/entropy_of_english_9.html
/// [Speech tempo]: https://en.wikipedia.org/wiki/Speech_tempo
const DEFAULT_CHARS_PER_SECOND: f32 = 15.0;

/// Import a Whisper JSON file and convert it to an SRT file.
pub fn import_whisper_json(whisper_json: &WhisperJson) -> Result<SubtitleFile> {
    let mut whisper = whisper_json.clone().clean();
    whisper.resegment();
    let words = whisper.words_for_each_segment(&whisper.words);
    let mut analyzed = AnalyzedSegments::new(&whisper.segments, &words);
    analyzed.fix_times();
    analyzed.check_for_problems();
    let mut srt = analyzed.to_subtitle_file()?;
    if false {
        srt = clean_subtitle_file(&srt)?;
    }
    Ok(srt)
}

/// Different degrees of subtitle overlap.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Overlap {
    /// No overlap.
    None,

    /// Slight overlap. Probably fixable with some slight adjustment.
    Slight,

    /// Severe overlap. Probably garbage data.
    Severe,
}

impl Overlap {
    /// Compute overlap type from start and end times.
    fn from_times(_start1: f32, end1: f32, start2: f32, end2: f32) -> Overlap {
        if end1 > start2 + 1.0 || end1 > end2 {
            Overlap::Severe
        } else if end1 + 0.01 > start2 {
            Overlap::Slight
        } else {
            Overlap::None
        }
    }
}

/// Clean up common cruft from Whisper text. The first processing step for
/// either words or segments.
fn scrub_text(text: &str) -> String {
    lazy_static! {
        static ref CRUFT: regex::Regex = Regex::new(r"♪|♫|🎵|🎶").unwrap();
    }
    CRUFT.replace_all(text, "").to_string()
}

/// Normalize a word for diffing. We assume the word has already been cleaned.
fn normalize_word_text_for_diff(text: &str) -> Option<String> {
    lazy_static! {
        static ref NON_WORD: regex::Regex = Regex::new(r"[^\w]").unwrap();
    }

    let normalized = NON_WORD.replace_all(&text, "").to_string();
    if normalized.is_empty() {
        None
    } else {
        Some(normalized)
    }
}

/// Compute the number of characters per second for a string. Returns `None` if
/// the caculated result would be obvious nonsense.
fn chars_per_second(text: &str, start: f32, end: f32) -> Option<f32> {
    let duration = end - start;
    let char_count = text.chars().count();
    if duration < 0.1 || char_count == 0 {
        None
    } else {
        Some(char_count as f32 / duration)
    }
}

/// Contents of the Whisper JSON file.
#[derive(Clone, Debug, Deserialize, Serialize)]
#[allow(dead_code)]
pub struct WhisperJson {
    language: String,
    duration: f32,
    text: String,
    words: Vec<Word>,
    segments: Vec<Segment>,

    /// Other keys we don't recognize but want to keep.
    #[serde(flatten)]
    extra: serde_json::Map<String, serde_json::Value>,
}

impl WhisperJson {
    /// Read from a file.
    pub fn from_path(path: &Path) -> Result<WhisperJson> {
        let rdr = File::open(path).with_context(|| {
            format!("Failed to open Whisper JSON file: {}", path.display())
        })?;
        serde_json::from_reader(rdr).with_context(|| {
            format!("Failed to parse Whisper JSON file: {}", path.display())
        })
    }

    /// Read from a string.
    pub fn from_str(s: &str) -> Result<WhisperJson> {
        serde_json::from_str(s)
            .with_context(|| format!("Failed to parse Whisper JSON string: {:?}", s))
    }

    /// Clean up and normalize the Whisper JSON file.
    fn clean(mut self) -> WhisperJson {
        // We don't bother to clean the "text" field, because we don't use it to
        // generate output.
        self.words = self.words.into_iter().filter_map(Word::clean).collect();
        self.segments = self
            .segments
            .into_iter()
            .filter_map(Segment::clean)
            .collect();
        self
    }

    /// Re-segment into reasonable length subtitles.
    fn resegment(&mut self) {
        let mut segments = vec![];
        for segment in &self.segments {
            let resegmented = segment_subtitle_text(&segment.text);
            let new_segment_duration =
                (segment.end - segment.start) / resegmented.len() as f32;
            let mut start = segment.start;
            for text in resegmented {
                let end = start + new_segment_duration;
                segments.push(Segment {
                    text,
                    start,
                    end,
                    no_speech_prob: segment.no_speech_prob,
                    extra: segment.extra.clone(),
                });
                start = end;
            }
        }
        self.segments = segments;
    }

    /// Get words associated with each segment.
    fn words_for_each_segment<'words>(
        &self,
        words: &'words [Word],
    ) -> Vec<Vec<&'words Word>> {
        let mut segment_words_for_diff: Vec<String> = vec![];
        let mut segment_indices = vec![];
        for (segment_idx, segment) in self.segments.iter().enumerate() {
            let diff_words = segment.diff_words();
            segment_indices.extend(repeat(segment_idx).take(diff_words.len()));
            segment_words_for_diff.extend(diff_words);
        }
        assert_eq!(segment_words_for_diff.len(), segment_indices.len());

        let mut individual_words_for_diff: Vec<String> = vec![];
        let mut individual_indices = vec![];
        for word in words {
            let idx = individual_words_for_diff.len();
            if let Some(diff_word) = word.diff_word() {
                individual_words_for_diff.push(diff_word);
                individual_indices.push(idx);
            }
        }
        assert_eq!(individual_words_for_diff.len(), individual_indices.len());

        let diffed = diff::slice(&segment_words_for_diff, &individual_words_for_diff);

        let mut segment_words: Vec<Vec<&Word>> = vec![vec![]; self.segments.len()];
        let mut segment_word_idx = 0;
        let mut individual_word_idx = 0;
        for diff in diffed {
            assert!(segment_word_idx <= segment_words_for_diff.len());
            assert!(individual_word_idx <= individual_words_for_diff.len());
            match diff {
                diff::Result::Left(w) => {
                    debug!("Segment word {:?} missing from individual words", w);
                    assert!(segment_word_idx < segment_words_for_diff.len());
                    segment_word_idx += 1;
                }
                diff::Result::Right(w) => {
                    debug!("Individual word {:?} missing from segment words", w);
                    assert!(individual_word_idx < individual_words_for_diff.len());
                    individual_word_idx += 1;
                }
                diff::Result::Both(w1, _) => {
                    trace!("Matched {:?}", w1);
                    assert!(segment_word_idx < segment_words_for_diff.len());
                    assert!(individual_word_idx < individual_words_for_diff.len());

                    let word = &words[individual_indices[individual_word_idx]];
                    segment_words[segment_indices[segment_word_idx]].push(word);

                    segment_word_idx += 1;
                    individual_word_idx += 1;
                }
            }
        }
        segment_words
    }
}

impl AppendWithOffset for WhisperJson {
    /// Append another Whisper JSON file, shifting it by the specified time offset.
    /// We use this to reassamble transcription segments.
    fn append_with_offset(&mut self, mut other: WhisperJson, time_offset: f32) {
        if self.language != other.language {
            warn!(
                "Whisper transcriptions have different languages: {:?} vs {:?}",
                self.language, other.language
            );
        }
        self.duration = time_offset + other.duration;
        self.text.push('\n');
        self.text.push_str(&other.text);
        for word in &mut other.words {
            word.offset(time_offset);
        }
        self.words.extend(other.words);
        for segment in &mut other.segments {
            segment.offset(time_offset);
        }
        self.segments.extend(other.segments);
        if log_enabled!(Level::Debug) && !other.extra.is_empty() {
            debug!(
                "Ignoring extra keys in appended Whisper JSON: {:?}",
                other.extra
            );
        }
    }
}

// Format of the "words" field in the Whisper JSON file.
#[derive(Clone, Debug, Deserialize, Serialize)]
struct Word {
    word: String,
    start: f32,
    end: f32,

    /// Other keys we don't recognize but want to keep.
    #[serde(flatten)]
    extra: serde_json::Map<String, serde_json::Value>,
}

impl Word {
    /// Offset by the specified time.
    fn offset(&mut self, time_offset: f32) {
        self.start += time_offset;
        self.end += time_offset;
    }

    /// Clean up a word, discarding it if it looks useless.
    fn clean(mut self) -> Option<Word> {
        let scrubbed = scrub_text(&self.word);
        if scrubbed.is_empty() {
            debug!("Discarding word with no useful text: {:?}", self);
            None
        } else {
            self.word = scrubbed;
            Some(self)
        }
    }

    /// Word to use for diffing.
    fn diff_word(&self) -> Option<String> {
        normalize_word_text_for_diff(&self.word)
    }

    /// Compute the number of characters per second for this word.
    fn chars_per_second(&self) -> Option<f32> {
        chars_per_second(&self.word, self.start, self.end)
    }
}

// Format of the "segments" field in the Whisper JSON file.
#[derive(Clone, Deserialize, Serialize)]
struct Segment {
    text: String,
    start: f32,
    end: f32,
    no_speech_prob: f32,

    /// Other keys we don't recognize but want to keep.
    #[serde(flatten)]
    extra: serde_json::Map<String, serde_json::Value>,
}

impl Segment {
    /// Offset by the specified time.
    fn offset(&mut self, time_offset: f32) {
        self.start += time_offset;
        self.end += time_offset;
    }

    /// Clean up a segment, discarding it if it looks useless.
    fn clean(mut self) -> Option<Segment> {
        lazy_static! {
            static ref WHITESPACE: regex::Regex = Regex::new(r"\s+").unwrap();
        }

        if self.no_speech_prob > 0.5 {
            debug!("Discarding segment with no_speech_prob: {:?}", self);
            return None;
        }

        let scrubbed = scrub_text(&self.text);
        let normalized = WHITESPACE.replace_all(&scrubbed, " ").trim().to_string();
        if normalized.is_empty() {
            debug!("Discarding segment with no useful text: {:?}", self);
            None
        } else {
            self.text = normalized;
            Some(self)
        }
    }

    /// Words to use for diffing.
    fn diff_words(&self) -> Vec<String> {
        let segment: &str = &self.text;
        lazy_static! {
            static ref WHITESPACE: regex::Regex = Regex::new(r"\s+").unwrap();
        }

        WHITESPACE
            .split(&segment)
            .filter_map(|word| normalize_word_text_for_diff(word))
            .collect()
    }

    /// Compute characters per second for this segment.
    fn chars_per_second(&self) -> Option<f32> {
        chars_per_second(&self.text, self.start, self.end)
    }
}

impl fmt::Debug for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Segment {{ {:?} {:.3}-{:.3} nsp={:.2} }}",
            &self.text, self.start, self.end, self.no_speech_prob
        )
    }
}

/// Analyzed subtitles.
#[derive(Debug)]
struct AnalyzedSegments {
    /// Analyzed segments.
    segments: Vec<AnalyzedSegment>,

    /// Median characters per second for all segments.
    median_chars_per_sec: f32,
}

impl AnalyzedSegments {
    /// Create analyzed segments from segments.
    fn new(segments: &[Segment], words: &[Vec<&Word>]) -> AnalyzedSegments {
        assert_eq!(segments.len(), words.len());

        // Compute median characters per second. Most of our subtitles have
        // excellent timing, but the outliers are awful.
        let mut chars_per_sec = segments
            .iter()
            .filter_map(Segment::chars_per_second)
            .filter_map(|cps| NotNan::new(cps).ok())
            .collect::<Vec<_>>();
        chars_per_sec.sort_unstable();
        let median_chars_per_sec = if chars_per_sec.is_empty() {
            warn!("No valid segment times found");
            DEFAULT_CHARS_PER_SECOND
        } else {
            chars_per_sec[chars_per_sec.len() / 2].into_inner()
        };
        debug!("Median characters per second: {}", median_chars_per_sec);

        let segments = segments
            .iter()
            .enumerate()
            .map(|(i, s)| AnalyzedSegment::new(s, median_chars_per_sec, &words[i]))
            .collect();
        AnalyzedSegments {
            segments,
            median_chars_per_sec,
        }
    }

    /// Fix segment start and end times.
    fn fix_times(&mut self) {
        // Fix the individual segments.
        self.segments = self
            .segments
            .iter()
            .cloned()
            .filter_map(|s| s.fix_times(self.median_chars_per_sec))
            .collect();

        // Fix slight overlaps.
        let mut windows = self.segments.windows_mut::<2>();
        while let Some([s1, s2]) = windows.next() {
            let overlap = Overlap::from_times(s1.start, s1.end, s2.start, s2.end);
            if overlap == Overlap::Slight {
                let compromise = (s1.end + s2.start) / 2.0;
                s1.end = compromise - 0.025;
                s2.start = compromise + 0.025;
                assert!(s1.end < s2.start);
            }
        }
    }

    /// Check for problems with our segments.
    fn check_for_problems(&self) {
        // Look for overlaps.
        for win in self.segments.windows(2) {
            let overlap = Overlap::from_times(
                win[0].start,
                win[0].end,
                win[1].start,
                win[1].end,
            );
            match overlap {
                Overlap::None => {}
                Overlap::Slight => {
                    debug!(
                        "Slightly overlapping segments: {:?} and {:?}",
                        win[0].segment, win[1].segment
                    );
                }
                Overlap::Severe => {
                    warn!(
                        "OVERLAP: Severely overlapping segments: {:?} and {:?}",
                        win[0].segment, win[1].segment
                    );
                }
            }
        }

        // Check individual segments for problems.
        for segment in &self.segments {
            segment.check_for_problems();
        }
    }

    /// Convert to a subtitle file.
    fn to_subtitle_file(&self) -> Result<SubtitleFile> {
        let mut subtitles = vec![];
        for (idx, segment) in self.segments.iter().enumerate() {
            let text = segment.segment.text.clone();

            // Check for invalid periods.
            if segment.start <= 0.0 || segment.end <= segment.start {
                debug!("Invalid time period: {:?}", segment);
            }
            let begin = f32::max(0.0, segment.start);
            let end = f32::max(begin + 0.05, segment.end);
            let period =
                Period::new(begin, end).expect("didn't catch invalid time period");

            subtitles.push(Subtitle {
                index: idx,
                period,
                lines: vec![text],
            });
        }
        Ok(SubtitleFile { subtitles })
    }
}

/// Extra information we generate about a segment.
#[derive(Clone, Debug)]
struct AnalyzedSegment {
    /// Our original segment.
    segment: Segment,

    /// Start time for this segment (will be adjusted).
    start: f32,

    /// End time for this segment (will be adjusted).
    end: f32,

    /// Characters per second for this segment.
    chars_per_sec: Option<f32>,

    /// Relative characters per second for this segment compared to the median.
    relative_chars_per_sec: Option<f32>,

    /// Words associated with this segment.
    words: Vec<AnalyzedWord>,
}

impl AnalyzedSegment {
    /// Create an analyzed segment from a segment.
    fn new(
        segment: &Segment,
        median_chars_per_sec: f32,
        words: &[&Word],
    ) -> AnalyzedSegment {
        let chars_per_sec = segment.chars_per_second();
        let relative_chars_per_sec =
            chars_per_sec.map(|cps| cps / median_chars_per_sec);
        let words = words
            .into_iter()
            .cloned()
            .map(|w| AnalyzedWord::new(w, median_chars_per_sec))
            .collect();
        AnalyzedSegment {
            segment: segment.clone(),
            start: segment.start,
            end: segment.end,
            chars_per_sec,
            relative_chars_per_sec,
            words,
        }
    }

    /// Fix segment start and end times based on the [`Word`]s associated with
    /// this segment. This is messy because:
    ///
    /// 1. Segment times routinely have errors of multiple seconds. They're just
    ///    bad.
    /// 2. Word times are also weird, but they're usually better than the
    ///    segment times.
    fn fix_times(mut self, median_chars_per_sec: f32) -> Option<Self> {
        if self.words.is_empty() {
            warn!(
                "NO WORDS: Discarding {:?} because it matches no timed words",
                self.segment
            );
            return None;
        }

        // Get plausible time periods for each word.
        let mut plausible_periods = self
            .words
            .iter()
            .map(|w| w.plausible_time_period())
            .collect::<Vec<_>>();

        // Estimate any missing time periods at the start of the segment.
        if let Some(first_plausible) =
            plausible_periods.iter().position(Option::is_some)
        {
            if first_plausible > 0 {
                debug!(
                    "Re-estimating start times for {:?} using {:?}",
                    self.segment, self.words[first_plausible].word
                );
                let words_iter = self.words.iter().take(first_plausible).rev();
                let periods_iter =
                    plausible_periods.iter_mut().take(first_plausible).rev();
                let mut next_begin = self.words[first_plausible].word.start;
                for (word, word_period) in words_iter.zip(periods_iter) {
                    let new_period = word.estimated_time_period_before(
                        next_begin,
                        median_chars_per_sec,
                    );
                    trace!("Estimated time period for word: {:?}", new_period);
                    *word_period = Some(new_period);
                    next_begin = new_period.begin();
                }
            }
        }

        // Estimate any missing time periods at the end of the segment.
        if let Some(last_plausible) =
            plausible_periods.iter().rposition(Option::is_some)
        {
            if last_plausible < self.words.len() - 1 {
                debug!(
                    "Re-estimating end times for {:?} using {:?}",
                    self.segment, self.words[last_plausible].word
                );

                let words_iter = self.words.iter().skip(last_plausible + 1);
                let periods_iter =
                    plausible_periods.iter_mut().skip(last_plausible + 1);
                let mut prev_end = self.words[last_plausible].word.end;
                for (word, word_period) in words_iter.zip(periods_iter) {
                    let new_period = word
                        .estimated_time_period_after(prev_end, median_chars_per_sec);
                    trace!("Estimated time period for word: {:?}", new_period);
                    *word_period = Some(new_period);
                    prev_end = new_period.end();
                }
            }
        }

        if let (Some(Some(first)), Some(Some(last))) =
            (plausible_periods.first(), plausible_periods.last())
        {
            trace!("Fixing times for segment {:?}", self.segment);
            self.start = first.begin();
            self.end = last.end();
            self.chars_per_sec =
                chars_per_second(&self.segment.text, self.start, self.end);
            self.relative_chars_per_sec =
                self.chars_per_sec.map(|cps| cps / median_chars_per_sec);
            Some(self)
        } else {
            warn!(
                "BAD TIMES: No plausible time periods found for {:?}",
                self.segment
            );
            None
        }
    }

    /// Check for problems in this segment.
    fn check_for_problems(&self) {
        // Check for segments that are too long or too short relative to their
        // text.
        if let (Some(chars_per_sec), Some(relative_chars_per_sec)) =
            (self.chars_per_sec, self.relative_chars_per_sec)
        {
            if relative_chars_per_sec > 3.0 {
                warn!(
                    "HIGH C/S: {:?} has too many characters per second: {} is {}x median",
                    self.segment, chars_per_sec, relative_chars_per_sec
                );
            } else if relative_chars_per_sec < (1.0 / 3.0) {
                warn!(
                    "LOW C/S: {:?} has too few characters per second: {} is {}x median",
                    self.segment, chars_per_sec, relative_chars_per_sec
                );
            }
        } else {
            warn!(
                "NO C/S: {:?} has no valid characters per second",
                self.segment
            );
        }

        // Check for segments which might not be real.
        if self.segment.no_speech_prob > 0.1 {
            warn!("NSP: {:?} has high no_speech_prob", self.segment);
        }
    }
}

/// Extra information we generate about a word.
#[derive(Clone, Debug)]
struct AnalyzedWord {
    /// Our original word.
    word: Word,

    /// Relative characters per second for this word compared to the median.
    relative_chars_per_sec: Option<f32>,
}

impl AnalyzedWord {
    /// Create an analyzed word from a word.
    fn new(word: &Word, median_chars_per_sec: f32) -> AnalyzedWord {
        let chars_per_sec = word.chars_per_second();
        let relative_chars_per_sec =
            chars_per_sec.map(|cps| cps / median_chars_per_sec);
        AnalyzedWord {
            word: word.to_owned(),
            relative_chars_per_sec,
        }
    }

    /// Return when we think this word occurs, if we believe the data we have.
    fn plausible_time_period(&self) -> Option<Period> {
        let start = self.word.start;
        let end = self.word.end;
        if start >= 0.0 || end > start {
            if let Some(rel_cps) = self.relative_chars_per_sec {
                if rel_cps >= 0.2 {
                    return Period::new(start, end).ok();
                }
            }
        }
        None
    }

    /// Assuming word ends at `next_begin`, and we don't trust `self.word.begin`
    /// or `self.word.end`, what time period would we expect it to have?
    fn estimated_time_period_before(
        &self,
        next_begin: f32,
        median_chars_per_sec: f32,
    ) -> Period {
        let duration =
            (self.word.word.chars().count() + 1) as f32 / median_chars_per_sec;
        let start = f32::max(next_begin - duration, 0.0);
        let end = next_begin;
        Period::new(start, end).expect("didn't catch invalid time period")
    }

    /// Assuming word begins at `prev_end`, and we don't trust `self.word.begin`
    /// or `self.word.end`, what time period would we expect it to have?
    fn estimated_time_period_after(
        &self,
        prev_end: f32,
        median_chars_per_sec: f32,
    ) -> Period {
        let duration =
            (1 + self.word.word.chars().count()) as f32 / median_chars_per_sec;
        let start = prev_end;
        let end = prev_end + duration;
        Period::new(start, end).expect("didn't catch invalid time period")
    }
}

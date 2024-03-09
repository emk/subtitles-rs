use std::{fs::File, iter::repeat, path::Path};

use anyhow::Context as _;
use lazy_static::lazy_static;
use log::{debug, trace};
use regex::Regex;
use serde::Deserialize;

use crate::{
    clean::clean_subtitle_file,
    segment::segment_subtitle_text,
    srt::{Subtitle, SubtitleFile},
    time::Period,
    Result,
};

/// Import a Whisper JSON file and convert it to an SRT file.
pub fn import_whisper_json(whisper_json: &Path) -> Result<SubtitleFile> {
    let rdr = File::open(whisper_json).with_context(|| {
        format!(
            "Failed to open Whisper JSON file: {}",
            whisper_json.display()
        )
    })?;
    let mut whisper = serde_json::from_reader::<_, WhisperJson>(rdr)
        .with_context(|| {
            format!(
                "Failed to parse Whisper JSON file: {}",
                whisper_json.display()
            )
        })?
        .clean();
    whisper.resegment();
    whisper.fix_segment_times();
    whisper.to_subtitle_file()
    // clean_subtitle_file(&whisper.to_subtitle_file()?)
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

// Format of the Whisper JSON file.
#[derive(Debug, Deserialize)]
#[allow(dead_code)]
struct WhisperJson {
    language: String,
    duration: f32,
    text: String,
    words: Vec<Word>,
    segments: Vec<Segment>,
}

impl WhisperJson {
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

    /// Get words associated with each segment.
    fn segment_words<'words>(&self, words: &'words [Word]) -> Vec<Vec<&'words Word>> {
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
                });
                start = end;
            }
        }
        self.segments = segments;
    }

    /// Fix segment start and end times.
    fn fix_segment_times(&mut self) {
        let segment_words = self.segment_words(&self.words);
        for (segment, words) in self.segments.iter_mut().zip(segment_words) {
            segment.fix_times(&words);
        }
    }

    /// Convert to a subtitle file.
    fn to_subtitle_file(&self) -> Result<SubtitleFile> {
        let mut subtitles = vec![];
        for (idx, segment) in self.segments.iter().enumerate() {
            let text = segment.text.clone();

            // Check for invalid periods.
            if segment.start <= 0.0 || segment.end <= segment.start {
                debug!("Invalid time period: {:?}", segment);
            }
            let begin = f32::max(0.0, segment.start);
            let end = f32::max(begin + 1.0, segment.end);
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

// Format of the "words" field in the Whisper JSON file.
#[derive(Debug, Deserialize)]
struct Word {
    word: String,
    start: f32,
    end: f32,
}

impl Word {
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
}

// Format of the "segments" field in the Whisper JSON file.
#[derive(Debug, Deserialize)]
struct Segment {
    text: String,
    start: f32,
    end: f32,
    no_speech_prob: f32,
}

impl Segment {
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

    /// Re-segment into reasonable length subtitles.

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

    /// Fix segment start and end times based on the [`Word`]s associated with
    /// this segment. This is messy because:
    ///
    /// 1. Segment times routinely have errors of multiple seconds. They're just
    ///    bad.
    /// 2. Word times are also weird, but they're usually better than the
    ///    segment times.
    fn fix_times(&mut self, words: &[&Word]) {
        if words.is_empty() {
            debug!("No words found for segment, keeping times: {:?}", self);
            return;
        }

        // This is the most basic thing that produces semi-reasonable results.
        // We could probably do better with a more sophisticated algorithm.
        let first = words[0];
        let last = words[words.len() - 1];
        trace!(
            "Fixing times for segment {:?} using words {:?} and {:?}",
            self,
            first,
            last
        );
        self.start = first.start;
        self.end = last.end;
    }
}

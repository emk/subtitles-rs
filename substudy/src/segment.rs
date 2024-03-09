//! Break text into lines and subtitles.

use lazy_static::lazy_static;
use log::{debug, trace};
use paragraph_breaker::{Item, INFINITE_PENALTY};
use regex::Regex;
use unicode_width::UnicodeWidthChar;

/// The ideal maximum number of characters in a line. I've seen at least one
/// webpage that recommends 41. But that's actually pretty huge in `mpv`, for
/// example, and it's getting too long to make a good audio flashcard. 30 seems
/// to work nicely.
const MAX_LINE_LENGTH: usize = 30;

/// The ideal maximum number of lines in a subtitle.
const MAX_LINES: usize = 2;

/// Does this word end a phrase or sentence?
#[derive(Debug)]
enum Ends {
    Phrase,
    Sentence,
}

impl Ends {
    /// Knuth-Plass penalty for ending a line with this kind of word.
    fn penalty(&self) -> i32 {
        match self {
            Ends::Phrase => -1000,
            Ends::Sentence => -5000,
        }
    }
}

#[derive(Debug)]
struct WordInfo<'a> {
    word: &'a str,
    width: usize,
    ends: Option<Ends>,
}

impl WordInfo<'_> {
    fn new(word: &str) -> WordInfo {
        lazy_static! {
            // This may need further adjustment for for other languages. See
            // https://unicode.org/L2/L2003/03145-sentence-term.htm. We want to
            // keep an eye out for things like "!," in Spanish.
            static ref SENTENCE_END: Regex = Regex::new(r"[\p{Sentence_Terminal}][\p{Terminal_Punctuation}]?$").unwrap();
            static ref PHRASE_END: Regex = Regex::new(r"[\p{Terminal_Punctuation}]$").unwrap();
        }

        let is_sentence_end = SENTENCE_END.is_match(word);
        let is_phrase_end = PHRASE_END.is_match(word);
        let ends = if is_sentence_end {
            Some(Ends::Sentence)
        } else if is_phrase_end {
            Some(Ends::Phrase)
        } else {
            None
        };

        WordInfo {
            word,
            width: word.chars().map(|c| c.width().unwrap_or(1)).sum(),
            ends,
        }
    }
}

/// Break a subtitle into multiple lines and subtitles as needed.
///
/// TODO: Fix `as` casts to use `try_into`.
pub fn segment_subtitle_text(text: &str) -> Vec<String> {
    lazy_static! {
        static ref WHITESPACE: Regex = Regex::new(r"\s+").unwrap();
    }

    // Break the text into words.
    let words = WHITESPACE
        .split(text)
        .map(WordInfo::new)
        .collect::<Vec<_>>();

    let total_width = words.iter().map(|w| w.width).sum::<usize>() + words.len() - 1;

    // If the text is short enough, we're done.
    if total_width <= MAX_LINE_LENGTH {
        return vec![text.to_string()];
    }

    // Convert to Knuth-Plass input.
    let mut items = vec![];
    for word in &words {
        items.push(Item::Box {
            width: word.width as i32,
            data: word.word,
        });
        if let Some(ends) = &word.ends {
            items.push(Item::Penalty {
                width: 0,
                penalty: ends.penalty(),
                flagged: false,
            });
            // I _think_ this does what we want.
            items.push(Item::Glue {
                width: 0,
                stretch: 3,
                shrink: 0,
            });
        } else {
            items.push(Item::Glue {
                width: 1,
                stretch: 2,
                shrink: 0,
            })
        }
    }

    // Paragraph end (by convention). We omit this because we want to balance
    // the final line, too.
    //
    // items.push(Item::Glue {
    //     width: 0,
    //     stretch: 1000,
    //     shrink: 0,
    // });
    items.push(Item::Penalty {
        width: 0,
        penalty: -INFINITE_PENALTY,
        flagged: false,
    });

    // Break the text into lines.
    let ideal_line_length = MAX_LINE_LENGTH as i32;
    let breakpoints =
        paragraph_breaker::total_fit(&items, &[ideal_line_length], 20.0, 0);

    // Convert the breakpoints into lines.
    let mut lines = vec![];
    let mut start_idx = 0;
    trace!("Words: {:?}", words);
    trace!("Breakpoints: {:?}", breakpoints);
    for &bp in &breakpoints {
        let end_idx = bp.index as usize;
        let line = items[start_idx..end_idx]
            .iter()
            .filter_map(|item| match item {
                Item::Box { data, .. } => Some(*data),
                _ => None,
            })
            .collect::<Vec<_>>()
            .join(" ");
        lines.push(line);
        start_idx = end_idx;
    }
    let last_line = items[start_idx..]
        .iter()
        .filter_map(|item| match item {
            Item::Box { data, .. } => Some(*data),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join(" ");
    if !last_line.is_empty() {
        lines.push(last_line);
    }

    // Break lines into subtitles.
    let subtitles = lines
        .chunks(MAX_LINES)
        .map(|chunk| chunk.join("\n"))
        .collect::<Vec<_>>();
    if subtitles.len() > 1 {
        debug!("Broke subtitles into: {:?}", subtitles);
    }
    subtitles
}

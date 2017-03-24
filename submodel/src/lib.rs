extern crate csv;
#[macro_use]
extern crate error_chain;
extern crate flate2;
extern crate tar;
extern crate unicode_casefold;
extern crate unicode_segmentation;

use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::io::prelude::*;
use unicode_casefold::{Locale, UnicodeCaseFold, Variant};
use unicode_segmentation::UnicodeSegmentation;

mod errors;

use errors::*;

const MODEL_SIZE_LIMIT: usize = 10000;

/// Used to build a "language model" describing a human language, where the
/// input data is assumed to come from subtitle files.
struct ModelBuilder {
    grapheme_counts: HashMap<String, u64>,
    word_counts: HashMap<String, u64>,
}

impl ModelBuilder {
    /// Create a new `ModelBuilder`.
    fn new() -> ModelBuilder {
        ModelBuilder {
            grapheme_counts: HashMap::new(),
            word_counts: HashMap::new(),
        }
    }

    /// Add a subtitle line to the `ModelBuilder`.
    fn add_line(&mut self, line: &str) {
        for grapheme in line.graphemes(true) {
            if grapheme != " " {
                incr_map(&mut self.grapheme_counts, grapheme.to_owned());
            }
        }
        for word in line.unicode_words() {
            // TODO: Handle Turkish "i".
            let word = word.case_fold_with(Variant::Full, Locale::NonTurkic)
                .collect();
            incr_map(&mut self.word_counts, word);
        }
    }

    /// Write our current grapheme frequencies to `out`.
    fn grapheme_frequencies<W: Write>(&self, out: W) -> Result<()> {
        self.frequencies(&self.grapheme_counts, out)
    }

    /// Write our current word frequencies to `out`.
    fn word_frequencies<W: Write>(&self, out: W) -> Result<()> {
        self.frequencies(&self.word_counts, out)
    }

    /// Write the frequencies in `counts` to `out`, labelling them with
    /// `label`.
    fn frequencies<W: Write>(&self,
                             counts: &HashMap<String, u64>,
                             out: W) -> Result<()> {
        // Count the total number of graphemes we've seen.
        let mut total: f64 = 0.0;
        for &count in counts.values() {
            total += count as f64;
        }

        // Sort our results into a stable order and replace counts with
        // probabilities.
        let mut rows = counts.iter().collect::<Vec<_>>();
        rows.sort_by(|&(k1, c1), &(k2, c2)| {
            match c1.cmp(c2).reverse() {
                Ordering::Equal => k1.cmp(k2),
                other => other,
            }
        });

        // Write output to a CSV.
        let mut wtr = csv::Writer::from_writer(out);
        for (key, &count) in rows.into_iter().take(MODEL_SIZE_LIMIT) {
            wtr.encode((key, count as f64 / total))?;
        }
        Ok(())
    }
}

/// Increment a key in a map.
fn incr_map(map: &mut HashMap<String, u64>, key: String) {
    match map.entry(key.to_owned()) {
        Entry::Occupied(mut occupied) => {
            *occupied.get_mut() += 1;
        }
        Entry::Vacant(vacant) => {
            vacant.insert(1);
        }
    }
}

#[test]
fn grapheme_frequency() {
    use std::str;

    let mut builder = ModelBuilder::new();
    builder.add_line("Hello world");
    let mut csv = vec![];
    builder.grapheme_frequencies(&mut csv).unwrap();
    assert_eq!(str::from_utf8(&csv).unwrap(),
               "\
l,0.3
o,0.2
H,0.1
d,0.1
e,0.1
r,0.1
w,0.1
");
}

#[test]
fn word_frequency() {
    use std::str;

    let mut builder = ModelBuilder::new();
    builder.add_line("One potato, two potato!");
    let mut csv = vec![];
    builder.word_frequencies(&mut csv).unwrap();
    assert_eq!(str::from_utf8(&csv).unwrap(),
               "\
potato,0.5
one,0.25
two,0.25
");
}

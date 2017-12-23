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
use std::io;
use std::io::prelude::*;
use unicode_casefold::{Locale, UnicodeCaseFold, Variant};
use unicode_segmentation::UnicodeSegmentation;

mod errors;

pub use self::errors::{Result, Error, ErrorKind};

const MODEL_SIZE_LIMIT: usize = 10000;

/// Used to build a "language model" describing a human language, where the
/// input data is assumed to come from subtitle files.
pub struct ModelBuilder {
    grapheme_counts: HashMap<String, u64>,
    pair_counts: HashMap<String, u64>,
    word_counts: HashMap<String, u64>,
}

impl ModelBuilder {
    /// Create a new `ModelBuilder`.
    pub fn new() -> ModelBuilder {
        ModelBuilder {
            grapheme_counts: HashMap::new(),
            pair_counts: HashMap::new(),
            word_counts: HashMap::new(),
        }
    }

    /// Add a subtitle line to the `ModelBuilder`.
    pub fn add_line(&mut self, line: &str) {
        let grapheme_buffer = line.graphemes(true).collect::<Vec<_>>();
        for &grapheme in &grapheme_buffer {
            if grapheme != " " {
                incr_map(&mut self.grapheme_counts, grapheme.to_owned());
            }
        }
        if !grapheme_buffer.is_empty() {
            incr_map(&mut self.pair_counts, format!("\n{}", grapheme_buffer[0]));
            incr_map(&mut self.pair_counts,
                     format!("{}\n", grapheme_buffer[grapheme_buffer.len() - 1]));
        }
        for pair in grapheme_buffer.windows(2) {
            incr_map(&mut self.pair_counts, format!("{}{}", pair[0], pair[1]));
        }
        for word in line.unicode_words() {
            // TODO: Handle Turkic "i".
            let word = word.case_fold_with(Variant::Full, Locale::NonTurkic).collect();
            incr_map(&mut self.word_counts, word);
        }
    }

    /// Write our current grapheme frequencies to `out`.
    pub fn grapheme_frequencies<W: Write>(&self, out: W) -> Result<()> {
        self.frequencies(&self.grapheme_counts, out)
    }

    /// Write our current pair frequencies to `out`.
    pub fn pair_frequencies<W: Write>(&self, out: W) -> Result<()> {
        self.frequencies(&self.pair_counts, out)
    }

    /// Write our current word frequencies to `out`.
    pub fn word_frequencies<W: Write>(&self, out: W) -> Result<()> {
        self.frequencies(&self.word_counts, out)
    }

    /// Write the frequencies in `counts` to `out`, labelling them with
    /// `label`.
    fn frequencies<W: Write>(&self,
                             counts: &HashMap<String, u64>,
                             out: W)
                             -> Result<()> {
        // Count the total number of graphemes we've seen.
        let mut total: f64 = 0.0;
        for &count in counts.values() {
            total += count as f64;
        }

        // Sort our results into a stable order and replace counts with
        // probabilities.
        let mut rows = counts.iter().collect::<Vec<_>>();
        rows.sort_by(|&(k1, c1), &(k2, c2)| match c1.cmp(c2).reverse() {
                         Ordering::Equal => k1.cmp(k2),
                         other => other,
                     });

        // Write output to a CSV.
        let mut wtr = csv::Writer::from_writer(out);
        for (key, &count) in rows.into_iter().take(MODEL_SIZE_LIMIT) {
            wtr.encode((key, count as f64 / total))?;
        }
        Ok(())
    }

    /// Write out our language model to `out`.  This is actually a gzipped
    /// tar file containing multiple CSV files:
    ///
    /// - `graphemes.csv`: Frequencies of single grapheme clusters.
    /// - `pairs.csv`: Frequencies of grapheme pairs.
    /// - `words.csv`: Frequencies of case-folded words.
    ///
    /// All models will be truncted if they exceed a certain limit.
    pub fn write_model<W: Write>(&self, out: W) -> Result<()> {
        let gzip = flate2::write::GzEncoder::new(out, flate2::Compression::best());
        let mut tar = tar::Builder::new(gzip);
        self.append_model_part(&mut tar, "graphemes.csv", &self.grapheme_counts)?;
        self.append_model_part(&mut tar, "pairs.csv", &self.pair_counts)?;
        self.append_model_part(&mut tar, "words.csv", &self.word_counts)?;
        tar.into_inner()?.finish()?;
        Ok(())
    }

    /// Append a file to our model.
    fn append_model_part<W: Write>(&self,
                                   builder: &mut tar::Builder<W>,
                                   path: &str,
                                   counts: &HashMap<String, u64>)
                                   -> Result<()> {
        let mut csv = vec![];
        self.frequencies(counts, &mut csv)?;

        let mut header = tar::Header::new_old();
        header.set_path(path)?;
        // TODO: Can this fail with a cast error?
        header.set_size(csv.len() as u64);
        header.set_mode(0o600);
        header.set_cksum();
        builder.append(&header, io::Cursor::new(&csv))?;
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
fn pair_frequency() {
    use std::str;

    let mut builder = ModelBuilder::new();
    builder.add_line("Help");
    let mut csv = vec![];
    builder.pair_frequencies(&mut csv).unwrap();
    assert_eq!(str::from_utf8(&csv).unwrap(),
               "\
\"\nH\",0.2
He,0.2
el,0.2
lp,0.2
\"p\n\",0.2
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

#[test]
fn write_model() {
    let mut builder = ModelBuilder::new();
    builder.add_line("One potato, two potato!");
    let mut model = vec![];
    builder.write_model(&mut model).unwrap();
    assert!(model.len() > 0);
}

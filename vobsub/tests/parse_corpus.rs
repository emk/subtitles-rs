extern crate env_logger;
extern crate glob;
#[macro_use]
extern crate log;
extern crate vobsub;

use std::{fs, io::prelude::*, path::Path};

// To run this test, use `cargo test -- --ignored`.  This tests against a
// larger selection of *.sub files in our private corpus, which is
// unfortunately not open source.
#[test]
#[ignore]
fn private_corpus() {
    let _ = env_logger::init();

    let options = glob::MatchOptions {
        case_sensitive: true,
        require_literal_separator: true,
        require_literal_leading_dot: true,
    };
    for entry in glob::glob_with("../private/**/*.sub", options).unwrap() {
        let entry = entry.unwrap();
        process_file(&entry, false);
    }
}

// This corpus was generated using `cargo fuzz`, and it represents all the
// crashes that we've found so far.
#[test]
fn error_corpus() {
    let _ = env_logger::init();

    let options = glob::MatchOptions {
        case_sensitive: true,
        require_literal_separator: true,
        require_literal_leading_dot: true,
    };
    for entry in glob::glob_with("../fixtures/invalid/*", options).unwrap() {
        let entry = entry.unwrap();
        process_file(&entry, true);
    }
}

fn process_file(path: &Path, expect_err: bool) {
    debug!("Processing {}", path.display());
    let mut f = fs::File::open(path).unwrap();
    let mut buffer = vec![];
    f.read_to_end(&mut buffer).unwrap();
    let count = vobsub::subtitles(&buffer)
        .map(|s| {
            assert_eq!(s.is_err(), expect_err);
        })
        .count();
    debug!("Found {} subtitles", count);
}

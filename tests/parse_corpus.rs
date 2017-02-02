extern crate env_logger;
extern crate glob;
#[macro_use]
extern crate log;
extern crate vobsub;

use std::fs;
use std::io::prelude::*;
use std::path::Path;
use vobsub::Result;

// To run this test, use `cargo test -- --ignored`.  This tests against a
// larger selection of *.sub files in our private corpus, which is
// unfortunately not open source.
#[test]
#[ignore]
fn private_corpus() {
    env_logger::init().unwrap();

    let options = glob::MatchOptions {
        case_sensitive: true,
        require_literal_separator: true,
        require_literal_leading_dot: true,
    };
    for entry in glob::glob_with("private/**/*.sub", &options).unwrap() {
        let entry = entry.unwrap();
        process_file(&entry);
    }
}

fn process_file(path: &Path) {
    debug!("Processing {}", path.display());
    let mut f = fs::File::open(path).unwrap();
    let mut buffer = vec![];
    f.read_to_end(&mut buffer).unwrap();

    for packet in vobsub::mpeg2::pes::ps_packets(&buffer) {
        let packet = packet.unwrap();
        trace!("{:#?}", &packet);
    }
}

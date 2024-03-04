extern crate env_logger;
extern crate glob;
#[macro_use]
extern crate log;
extern crate subtitle_ocr;
extern crate vobsub;

use std::path::Path;

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
    for entry in glob::glob_with("../private/**/*.idx", &options).unwrap() {
        let entry = entry.unwrap();
        process_file(&entry);
    }
}

fn process_file(path: &Path) {
    debug!("Processing {}", path.display());
    let idx = vobsub::Index::open(path).unwrap();
    let mut ctx = subtitle_ocr::OcrContext::new(path).unwrap();
    for sub in idx.subtitles() {
        let sub = sub.unwrap();
        ctx.add(sub.start_time(), sub.end_time(), &sub.to_image(idx.palette()))
            .unwrap();
    }
}

extern crate cli_test_dir;

use cli_test_dir::*;

#[test]
fn parse_file() {
    let testdir = TestDir::new("opusraw2txt", "parse_file");
    let output = testdir.cmd()
        .arg(testdir.src_path("../fixtures/opus.raw.tar.gz"))
        .expect_success();
    assert_eq!(output.stdout_str(), "\
Sentence 1.
Sentence 2.
Sentence 1.
Sentence 2.
");
    assert!(output.stderr_str().contains("4 sentences"));
}

#[test]
#[ignore]
fn parse_large_file() {
    let testdir = TestDir::new("opusraw2txt", "parse_large_file");
    testdir.cmd()
        .arg(testdir.src_path("../private/opus_open_subtitles/ca.raw.tar.gz"))
        .expect_success();
}

extern crate cli_test_dir;

use cli_test_dir::TestDir;
use std::io;
use std::io::prelude::*;
use std::str::from_utf8;

#[test]
fn parse_file() {
    let testdir = TestDir::new("opusraw2txt", "parse_file");
    let output = testdir.cmd()
        .arg(testdir.src_path("../fixtures/opus.raw.tar.gz"))
        .output()
        .expect("could not run opusraw2txt");
    if !output.status.success() {
        io::stderr().write(&output.stderr).expect("write stderr");
    }
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().find("Sentence 1").is_some());
    assert!(from_utf8(&output.stderr).unwrap().find("4 sentences").is_some());
}

#[test]
#[ignore]
fn parse_large_file() {
    let testdir = TestDir::new("opusraw2txt", "parse_large_file");
    let output = testdir.cmd()
        .arg(testdir.src_path("../private/opus_open_subtitles/ca.raw.tar.gz"))
        .output()
        .expect("could not run opusraw2txt");
    if !output.status.success() {
        io::stderr().write(&output.stderr).expect("write stderr");
    }
    assert!(output.status.success());
}

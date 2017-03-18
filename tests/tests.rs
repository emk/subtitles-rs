extern crate cli_test_dir;

use cli_test_dir::TestDir;
use std::str::from_utf8;

#[test]
fn show_help() {
    let testdir = TestDir::new("substudy", "show_help");
    let output = testdir.cmd()
        .arg("--help")
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().find("substudy --help").is_some());
}

#[test]
fn show_version() {
    let testdir = TestDir::new("substudy", "show_version");
    let output = testdir.cmd()
        .arg("--version")
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().find("substudy ").is_some());
}

#[test]
fn cmd_clean() {
    let testdir = TestDir::new("substudy", "cmd_clean");
    let output = testdir.cmd()
        .arg("clean")
        .arg(testdir.src_path("fixtures/sample.en.srt"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().find("Yay!").is_some());
}

#[test]
fn cmd_combine() {
    let testdir = TestDir::new("substudy", "cmd_combine");
    let output = testdir.cmd()
        .arg("combine")
        .arg(testdir.src_path("fixtures/sample.es.srt"))
        .arg(testdir.src_path("fixtures/sample.en.srt"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().find("Yay!").is_some());
    assert!(from_utf8(&output.stdout).unwrap().find("¡Si!").is_some());
}

#[test]
fn cmd_export_csv() {
    let testdir = TestDir::new("substudy", "cmd_export_csv");
    let output = testdir.cmd()
        .args(&["export", "csv"])
        .arg(testdir.src_path("fixtures/empty.mp4"))
        .arg(testdir.src_path("fixtures/sample.es.srt"))
        .arg(testdir.src_path("fixtures/sample.en.srt"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    testdir.expect_path("empty_csv/cards.csv");
    testdir.expect_path("empty_csv/empty_00063_496.jpg");
    testdir.expect_path("empty_csv/empty_00060_828-00066_164.es.mp3");
}

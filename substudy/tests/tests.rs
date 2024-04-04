//! Integration tests for our command-line interface.  We actually run the
//! binary and make sure it produces the expected output.

use std::str::from_utf8;

use cli_test_dir::{TeeOutputExt, TestDir};

#[test]
fn show_help() {
    let testdir = TestDir::new("substudy", "show_help");
    let output = testdir
        .cmd()
        .arg("--help")
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().find("Usage").is_some());
}

#[test]
fn show_version() {
    let testdir = TestDir::new("substudy", "show_version");
    let output = testdir
        .cmd()
        .arg("--version")
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout)
        .unwrap()
        .find("substudy ")
        .is_some());
}

#[test]
fn cmd_clean() {
    let testdir = TestDir::new("substudy", "cmd_clean");
    let output = testdir
        .cmd()
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
    let output = testdir
        .cmd()
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
    let output = testdir
        .cmd()
        .args(&["export", "csv"])
        .arg(testdir.src_path("fixtures/empty.mp4"))
        .arg(testdir.src_path("fixtures/sample.es.srt"))
        .arg(testdir.src_path("fixtures/sample.en.srt"))
        .output()
        .tee_output()
        .expect("could not run substudy");
    assert!(output.status.success());
    testdir.expect_path("empty_csv/cards.csv");
    testdir.expect_path("empty_csv/empty_00063_496.jpg");
    testdir.expect_path("empty_csv/empty_00060_828-00066_164.es.mp3");
}

#[test]
fn cmd_export_review() {
    let testdir = TestDir::new("substudy", "cmd_export_review");
    let output = testdir
        .cmd()
        .args(&["export", "review"])
        .arg(testdir.src_path("fixtures/empty.mp4"))
        .arg(testdir.src_path("fixtures/sample.es.srt"))
        .arg(testdir.src_path("fixtures/sample.en.srt"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    testdir.expect_path("empty_review/index.html");
    testdir.expect_path("empty_review/empty_00063_496.jpg");
    testdir.expect_path("empty_review/empty_00061_828-00065_164.es.mp3");
}

#[test]
fn cmd_export_tracks() {
    let testdir = TestDir::new("substudy", "cmd_export_tracks");
    let output = testdir
        .cmd()
        .args(&["export", "tracks"])
        .arg(testdir.src_path("fixtures/empty.mp4"))
        .arg(testdir.src_path("fixtures/sample.es.srt"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    testdir.expect_path("empty_tracks/playlist.m3u8");
    testdir.expect_path("empty_tracks/empty_00059_828-00067_164.es.mp3");
}

#[test]
fn cmd_list_tracks() {
    let testdir = TestDir::new("substudy", "cmd_export_tracks");
    let output = testdir
        .cmd()
        .args(&["list", "tracks"])
        .arg(testdir.src_path("fixtures/empty.mp4"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout)
        .unwrap()
        .find("#1 es Audio")
        .is_some());
}

#[ignore]
#[test]
fn cmd_transcribe_example_text() {
    let testdir = TestDir::new("substudy", "cmd_transcribe_example_text");
    let output = testdir
        .cmd()
        .arg("transcribe")
        .arg(testdir.src_path("fixtures/poem.es.mp3"))
        .arg("--example-text")
        .arg(testdir.src_path("fixtures/poem.es.txt"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    // This is does not reliably produce text because no_speech_prop tends to be
    // too high.
    //
    //assert!(from_utf8(&output.stdout).unwrap().find("viento").is_some());
}

#[ignore]
#[test]
fn cmd_transcribe_expected_text() {
    let testdir = TestDir::new("substudy", "cmd_transcribe_expected_text");
    let output = testdir
        .cmd()
        .arg("transcribe")
        .arg(testdir.src_path("fixtures/poem.es.mp3"))
        .arg("--expected-text")
        .arg(testdir.src_path("fixtures/poem.es.txt"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().find("viento").is_some());
}

#[ignore]
#[test]
fn cmd_transcribe_no_text() {
    let testdir = TestDir::new("substudy", "cmd_transcribe_no_text");
    let output = testdir
        .cmd()
        .arg("transcribe")
        .arg(testdir.src_path("fixtures/poem.es.mp3"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    // I don't even know whether I expect this to work.
}

#[ignore]
#[test]
fn cmd_transcribe_format_whisper_srt() {
    let testdir = TestDir::new("substudy", "cmd_transcribe_format_whisper_srt");
    let output = testdir
        .cmd()
        .arg("transcribe")
        .arg(testdir.src_path("fixtures/poem.es.mp3"))
        .arg("--format=whisper-srt")
        .arg("--example-text")
        .arg(testdir.src_path("fixtures/poem.es.txt"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().find("viento").is_some());
}

#[ignore]
#[test]
fn cmd_transcribe_format_whisper_json() {
    let testdir = TestDir::new("substudy", "cmd_transcribe_format_whisper_json");
    let output = testdir
        .cmd()
        .arg("transcribe")
        .arg(testdir.src_path("fixtures/poem.es.mp3"))
        .arg("--format=whisper-json")
        .arg("--example-text")
        .arg(testdir.src_path("fixtures/poem.es.txt"))
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().find("viento").is_some());
}

#[ignore]
#[test]
fn cmd_translate() {
    let testdir = TestDir::new("substudy", "cmd_translate");
    let output = testdir
        .cmd()
        .arg("translate")
        .arg(testdir.src_path("fixtures/poem.es.srt"))
        .arg("--native-lang=en")
        .output()
        .expect("could not run substudy");
    assert!(output.status.success());
    assert!(from_utf8(&output.stdout).unwrap().find("trees").is_some());
}

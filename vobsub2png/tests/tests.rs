//! # Integration tests.
//!
//! These tests are run on our executable to make sure that all the
//! command-line options work correctly.

extern crate cli_test_dir;

use cli_test_dir::*;

#[test]
fn generates_png_and_json_files() {
    let workdir =
        TestDir::new("vobsub2png", "generates_png_and_json_files");
    workdir.cmd()
        .arg("-o")
        .arg("out")
        .arg(workdir.src_path("../fixtures/example.idx"))
        .expect_success();
    workdir.expect_path("out/index.json");
    workdir.expect_path("out/0000.png");
    workdir.expect_path("out/0001.png");
}

#[test]
fn defaults_out_dir_based_on_input_name() {
    let workdir =
        TestDir::new("vobsub2png", "defaults_out_dir_based_on_input_name");
    workdir.cmd()
        .arg(workdir.src_path("../fixtures/example.idx"))
        .expect_success();
    workdir.expect_path("example_subtitles/index.json");
    workdir.expect_path("example_subtitles/0000.png");
    workdir.expect_path("example_subtitles/0001.png");
}

//! # Integration tests.
//!
//! These tests are run on our executable to make sure that all the
//! command-line options work correctly.

extern crate cli_test_dir;

use cli_test_dir::TestDir;

#[test]
fn does_not_fail() {
    let workdir = TestDir::new("subtitles2srt", "does_not_fail");
    let status = workdir.cmd()
        .arg(workdir.src_path("../fixtures/example.idx"))
        .status()
        .expect("could not run command");
    assert!(status.success());
}

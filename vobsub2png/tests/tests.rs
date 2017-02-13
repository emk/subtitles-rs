//! # Integration tests.
//!
//! These tests are run on our executable to make sure that all the
//! command-line options work correctly.
//!
//! This code is inspired by the `WorkDir` pattern that BurntSushi uses to
//! test CLI tools like `ripgrep` and `xsv`.

use std::env;
use std::fs;
use std::process;
use std::sync::atomic::{ATOMIC_USIZE_INIT, AtomicUsize, Ordering};

static TEST_ID: AtomicUsize = ATOMIC_USIZE_INIT;

#[test]
fn generates_png_and_json_files() {
    let mut bin_dir = env::current_exe().unwrap().parent().unwrap().to_path_buf();
    if bin_dir.ends_with("deps") {
        bin_dir.pop();
    }
    let cmd = bin_dir.join("vobsub2png");
    let id = TEST_ID.fetch_add(1, Ordering::SeqCst);
    let scratch_dir = bin_dir.join("vobsub2png-tests").join(format!("{}", id));
    if scratch_dir.exists() {
        fs::remove_dir_all(&scratch_dir).unwrap();
    }
    fs::create_dir_all(&scratch_dir).unwrap();

    let status = process::Command::new(&cmd)
        .arg("-o")
        .arg(&scratch_dir)
        .arg("../fixtures/example.idx")
        .status()
        .expect("could not run command");
    assert!(status.success());
    assert!(scratch_dir.join("index.json").exists());
    assert!(scratch_dir.join("0000.png").exists());
    assert!(scratch_dir.join("0001.png").exists());
}

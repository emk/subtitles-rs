//! # Integration tests.
//!
//! These tests are run on our executable to make sure that all the
//! command-line options work correctly.

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::sync::atomic::{ATOMIC_USIZE_INIT, AtomicUsize, Ordering};

static TEST_ID: AtomicUsize = ATOMIC_USIZE_INIT;

/// This code is inspired by the `WorkDir` pattern that BurntSushi uses to
/// test CLI tools like `ripgrep` and `xsv`.
struct WorkDir {
    bin: PathBuf,
    dir: PathBuf,
}

impl WorkDir {
    fn new(test_name: &str) -> WorkDir {
        let mut bin_dir = env::current_exe()
            .expect("Could not find executable")
            .parent()
            .expect("Could not find parent directory for executable")
            .to_path_buf();
        if bin_dir.ends_with("deps") {
            bin_dir.pop();
        }
        let id = TEST_ID.fetch_add(1, Ordering::SeqCst);
        let dir = bin_dir.join("vobsub2png-tests")
            .join(test_name)
            .join(format!("{}", id));
        if dir.exists() {
            fs::remove_dir_all(&dir)
                .expect("Could not remove test output directory");
        }
        fs::create_dir_all(&dir)
            .expect("Could not create test output directory");
        WorkDir {
            bin: bin_dir.join("vobsub2png"),
            dir: dir,
        }
    }

    fn cmd(&self) -> process::Command {
        let mut cmd = process::Command::new(&self.bin);
        cmd.current_dir(&self.dir);
        cmd
    }

    fn fixture<P: AsRef<Path>>(&self, path: P) -> PathBuf {
        let cwd = env::current_dir().expect("Could not get current dir");
        fs::canonicalize(cwd.join(path))
            .expect("Could not canonicalize path")
    }

    fn expect_path<P: AsRef<Path>>(&self, path: P) {
        let path = self.dir.join(path);
        assert!(path.exists(), format!("{} exists", path.display()));
    }
}

#[test]
fn generates_png_and_json_files() {
    let workdir = WorkDir::new("generates_png_and_json_files");
    let status = workdir.cmd()
        .arg("-o")
        .arg("out")
        .arg(workdir.fixture("../fixtures/example.idx"))
        .status()
        .expect("could not run command");
    assert!(status.success());
    workdir.expect_path("out/index.json");
    workdir.expect_path("out/0000.png");
    workdir.expect_path("out/0001.png");
}

#[test]
fn defaults_out_dir_based_on_input_name() {
    let workdir = WorkDir::new("defaults_out_dir_based_on_input_name");
    let status = workdir.cmd()
        .arg(workdir.fixture("../fixtures/example.idx"))
        .status()
        .expect("could not run command");
    assert!(status.success());
    workdir.expect_path("example_subtitles/index.json");
    workdir.expect_path("example_subtitles/0000.png");
    workdir.expect_path("example_subtitles/0001.png");
}

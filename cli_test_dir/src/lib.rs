//! This crate makes it easier to write integration tests for CLI
//! applications.  It's based on the "workdir" pattern used by BurntSushi's
//! [xsv][] and [ripgrep][] crates, but packaged in an easy-to-reuse form.
//!
//! To use this crate, add the following lines to your `Cargo.toml` file:
//!
//! ```toml
//! [dev-dependencies]
//! # You can replace "*" with the current version of this crate.
//! cli_test_dir = "*"
//! ```
//!
//! Then add the following at the top of `tests/tests.rs`:
//!
//! ```no_run
//! extern crate cli_test_dir;
//! ```
//!
//! Once this is done, you can set up a simple test.
//!
//! ```no_run
//! use cli_test_dir::TestDir;
//!
//! #[test]
//! fn write_output_file() {
//!     let testdir = TestDir::new("mybin", "write_output_file");
//!     let status = testdir.cmd()
//!         .arg("-o")
//!         .arg("out.txt")
//!         .status()
//!         .expect("could not run mybin");
//!     assert!(status.success());
//!     testdir.expect_path("out.txt");
//! }
//! ```
//!
//! ## Contributing
//!
//! Your feedback and contributions are welcome!  Please see
//! [GitHub](https://github.com/emk/subtitles-rs) for details.
//!
//! [ripgrep]: https://github.com/BurntSushi/ripgrep
//! [xsv]: https://github.com/BurntSushi/xsv

use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;
use std::sync::atomic::{ATOMIC_USIZE_INIT, AtomicUsize, Ordering};
use std::thread;
use std::time;

static TEST_ID: AtomicUsize = ATOMIC_USIZE_INIT;

/// This code is inspired by the `WorkDir` pattern that BurntSushi uses to
/// test CLI tools like `ripgrep` and `xsv`.
pub struct TestDir {
    bin: PathBuf,
    dir: PathBuf,
}

impl TestDir {
    /// Create a new `TestDir` for the current test.  You must specify
    /// `bin_name` (the name of a binary built by the current crate) and
    /// `test_name` (a unique name for the current test).
    ///
    /// If our output directory exists from a previous test run, it will be
    /// deleted.
    pub fn new(bin_name: &str, test_name: &str) -> TestDir {
        let mut bin_dir = env::current_exe()
            .expect("Could not find executable")
            .parent()
            .expect("Could not find parent directory for executable")
            .to_path_buf();
        if bin_dir.ends_with("deps") {
            bin_dir.pop();
        }
        let id = TEST_ID.fetch_add(1, Ordering::SeqCst);
        let dir = bin_dir.join("integration-tests")
            .join(test_name)
            .join(format!("{}", id));
        if dir.exists() {
            fs::remove_dir_all(&dir)
                .expect("Could not remove test output directory");
        }

        // Work around https://github.com/rust-lang/rust/issues/33707.
        let mut err = None;
        for _ in 0..10 {
            match fs::create_dir_all(&dir) {
                Ok(_) => {
                    err = None;
                    break;
                }
                Err(e) => {
                    err = Some(e);
                }
            }
            thread::sleep(time::Duration::from_millis(500));
        }
        if let Some(e) = err {
            panic!("Could not create test output directory: {}", e);
        }

        TestDir {
            bin: bin_dir.join(&bin_name),
            dir: dir,
        }
    }

    /// Return a `std::process::Command` object that can be used to execute
    /// the binary.
    pub fn cmd(&self) -> process::Command {
        let mut cmd = process::Command::new(&self.bin);
        cmd.current_dir(&self.dir);
        cmd
    }

    /// Return a path relative to the source directory of the current
    /// crate.  Useful for finding fixtures.
    pub fn src_path<P: AsRef<Path>>(&self, path: P) -> PathBuf {
        let cwd = env::current_dir().expect("Could not get current dir");
        fs::canonicalize(cwd.join(path))
            .expect("Could not canonicalize path")
    }

    /// If `path` does not point to valid path, fail the current test.
    pub fn expect_path<P: AsRef<Path>>(&self, path: P) {
        let path = self.dir.join(path);
        assert!(path.exists(), format!("{} exists", path.display()));
    }
}

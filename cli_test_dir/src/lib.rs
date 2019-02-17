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
//! Then add the following to the top of `tests/tests.rs`:
//!
//! ```rust,no_run
//! extern crate cli_test_dir;
//! ```
//!
//! You should now be able to write tests as follows:
//!
//! ```
//! use cli_test_dir::*;
//!
//! #[test]
//! fn write_output_file() {
//!     let testdir = TestDir::new("myapp", "write_output_file");
//!     testdir.cmd()
//!         .arg("out.txt")
//!         .expect_success();
//!     testdir.expect_path("out.txt");
//! }
//! ```
//!
//! You can use any options from [`std::process::Command`][Command] to invoke
//! your program.
//!
//! [Command]: https://doc.rust-lang.org/std/process/struct.Command.html
//!
//! ## Testing that the program ran successfully
//!
//! To check that a command succeeds, we can write:
//!
//! ```
//! # use cli_test_dir::*;
//! # #[cfg(unix)]
//! let testdir = TestDir::new("true", "true_succeeds");
//! # #[cfg(windows)]
//! # let testdir = TestDir::new("cmd", "true_succeeds");
//! let mut cmd = testdir.cmd();
//! # #[cfg(windows)]
//! # cmd.args(&["/C", "exit 0"]);
//! cmd.expect_success();
//! ```
//!
//! But this test would fail:
//!
//! ```rust,should_panic
//! # use cli_test_dir::*;
//! // Fails.
//! # #[cfg(unix)]
//! let testdir = TestDir::new("false", "false_succeeds");
//! # #[cfg(windows)]
//! let testdir = TestDir::new("cmd", "false_succeeds");
//! let mut cmd = testdir.cmd();
//! # #[cfg(windows)]
//! # cmd.args(&["/C", "exit 1"]);
//! cmd.expect_success();
//! ```
//!
//! ## Testing that the program exited with an error.
//!
//! Sometimes you want to test that a program fails to run successfully.
//!
//! ```
//! # use cli_test_dir::*;
//! # #[cfg(unix)]
//! let testdir = TestDir::new("false", "false_fails");
//! # #[cfg(windows)]
//! # let testdir = TestDir::new("cmd", "false_fails");
//! let mut cmd = testdir.cmd();
//! # #[cfg(windows)]
//! # cmd.args(&["/C", "exit 1"]);
//! cmd.expect_failure();
//! ```
//!
//! And as you would expect, this test would fail:
//!
//! ```rust,should_panic
//! # use cli_test_dir::*;
//! // Fails.
//! # #[cfg(unix)]
//! let testdir = TestDir::new("true", "true_fails");
//! # #[cfg(windows)]
//! # let testdir = TestDir::new("cmd", "true_fails");
//! let mut cmd = testdir.cmd();
//! # #[cfg(windows)]
//! # cmd.args(&["/C", "exit 0"]);
//! cmd.expect_failure();
//! ```
//!
//! ## File input and output
//!
//! The `src_path` function can be used to build paths relative to the
//! top-level of our crate, and `expect_path` can be used to make sure an
//! output file exists:
//!
//! ```rust
//! # use cli_test_dir::*;
//! # #[cfg(unix)]
//! let testdir = TestDir::new("cp", "cp_copies_files");
//! # #[cfg(windows)]
//! # let testdir = TestDir::new("cmd", "cp_copies_files");
//! let mut cmd = testdir.cmd();
//! # #[cfg(windows)]
//! # cmd.args(&["/C", "copy"]);
//! cmd
//!   .arg(testdir.src_path("fixtures/input.txt"))
//!   .arg("output.txt")
//!   .expect_success();
//! testdir.expect_path("output.txt");
//! ```
//!
//! We can also create the input file manually or look for specific
//! contents in the output file if we wish:
//!
//! ```
//! # use cli_test_dir::*;
//! # #[cfg(unix)]
//! let testdir = TestDir::new("cp", "cp_copies_files_2");
//! # #[cfg(windows)]
//! # let testdir = TestDir::new("cmd", "cp_copies_files_2");
//! let mut cmd = testdir.cmd();
//! # #[cfg(windows)]
//! # cmd.args(&["/C", "copy"]);
//! testdir.create_file("input.txt", "Hello, world!\n");
//! cmd
//!   .arg("input.txt")
//!   .arg("output.txt")
//!   .expect_success();
//! testdir.expect_contains("output.txt", "Hello");
//! testdir.expect_file_contents("output.txt", "Hello, world!\n");
//! ```
//!
//! There are also negative versions of these functions where useful:
//!
//! ```
//! # use cli_test_dir::*;
//! # #[cfg(unix)]
//! let testdir = TestDir::new("cp", "negative_tests");
//! # #[cfg(windows)]
//! # let testdir = TestDir::new("cmd", "negative_tests");
//! let mut cmd = testdir.cmd();
//! # #[cfg(windows)]
//! # cmd.args(&["/C", "copy"]);
//! testdir.create_file("input.txt", "Hello, world!\n");
//! cmd
//!   .arg("input.txt")
//!   .arg("output.txt")
//!   .expect_success();
//! testdir.expect_does_not_contain("output.txt", "Goodbye");
//! testdir.expect_no_such_path("does_not_exist.txt");
//! ```
//!
//! ## Standard input and output
//!
//! We can also test standard input and output:
//!
//! ```
//! # use cli_test_dir::*;
//! # #[cfg(unix)]
//! let testdir = TestDir::new("cat", "cat_passes_data_through");
//! # #[cfg(windows)]
//! # let testdir = TestDir::new("cmd", "type_passes_data_through");
//! let mut cmd = testdir.cmd();
//! # #[cfg(windows)]
//! # cmd.args(&["/C", "findstr x*"]); // https://superuser.com/a/853718
//! let output = cmd
//!   .output_with_stdin("Hello\n")
//!   .expect_success();
//! assert_eq!(output.stdout_str(), "Hello\n");
//! ```
//!
//! If you wish, you can display a command's output using `tee_output`:
//!
//! ```
//! # use cli_test_dir::*;
//! # #[cfg(unix)]
//! let testdir = TestDir::new("cat", "tee_output_shows_output");
//! # #[cfg(windows)]
//! # let testdir = TestDir::new("cmd", "tee_output_shows_output");
//! let mut cmd = testdir.cmd();
//! # #[cfg(windows)]
//! # cmd.args(&["/C", "findstr x*"]); // https://superuser.com/a/853718
//! let output = cmd
//!   .output_with_stdin("Hello\n")
//!   // Show `stdout` and `stderr`.
//!   .tee_output()
//!   .expect_success();
//! assert_eq!(output.stdout_str(), "Hello\n");
//! ```
//!
//! Note that this will currently print out all of `stdout` first, _then_ all of
//! `stderr`, instead of interleaving them normally.
//!
//! To see the output of `tee_output`, you will also need to invoke `cargo` as
//! follows:
//!
//! ```sh
//! cargo test -- --nocapture
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
use std::fmt;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process;
use std::str;
use std::sync::atomic::{ATOMIC_USIZE_INIT, AtomicUsize, Ordering};
use std::thread;
use std::time;
use std::borrow::Cow;

static TEST_ID: AtomicUsize = ATOMIC_USIZE_INIT;

/// This code is inspired by the `WorkDir` pattern that BurntSushi uses to
/// test CLI tools like `ripgrep` and `xsv`.
pub struct TestDir {
    bin: PathBuf,
    dir: PathBuf,
}

#[cfg(unix)]
fn exe_name(name: &str) -> Cow<str> {
    Cow::Borrowed(name)
}

#[cfg(windows)]
fn exe_name(name: &str) -> Cow<str> {
    // Maybe something like...
    if name.ends_with(".exe") {
        Cow::Borrowed(name)
    } else {
        Cow::Owned(format!("{}.exe", name))
    }
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

        let mut bin = bin_dir.join(&*exe_name(bin_name));
        if !bin.exists() {
            writeln!(io::stderr(),
                     "WARNING: could not find {}, will search PATH",
                     bin.display())
                .expect("could not write to stderr");
            bin = Path::new(&bin_name).to_owned();
        }

        TestDir {
            bin: bin,
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

    /// Construct a path relative to our test directory.
    ///
    /// ```
    /// # use cli_test_dir::*;
    /// # #[cfg(unix)]
    /// let testdir = TestDir::new("touch", "path_builds_paths");
    /// # #[cfg(windows)]
    /// # let testdir = TestDir::new("cmd", "path_builds_paths");
    /// let mut cmd = testdir.cmd();
    /// # #[cfg(windows)]
    /// # cmd.args(&["/C", "type", "nul", ">"]);
    /// cmd
    ///   .arg("example.txt")
    ///   .expect_success();
    /// assert!(testdir.path("example.txt").exists());
    /// ```
    pub fn path<P: AsRef<Path>>(&self, path: P) -> PathBuf {
        self.dir.join(path)
    }

    /// Return a path relative to the source directory of the current
    /// crate.  Useful for finding fixtures.
    pub fn src_path<P: AsRef<Path>>(&self, path: P) -> PathBuf {
        let cwd = env::current_dir().expect("Could not get current dir");
        fs::canonicalize(cwd.join(path))
            .expect("Could not canonicalize path")
    }

    /// Create a file in our test directory with the specified contents.
    pub fn create_file<P, S>(&self, path: P, contents: S)
        where P: AsRef<Path>, S: AsRef<[u8]>
    {
        let path = self.dir.join(path);
        fs::create_dir_all(path.parent().expect("expected parent"))
            .expect("could not create directory");
        let mut f = fs::File::create(&path).expect("can't create file");
        f.write_all(contents.as_ref()).expect("can't write to file");
    }

    /// If `path` does not point to valid path, fail the current test.
    pub fn expect_path<P: AsRef<Path>>(&self, path: P) {
        let path = self.dir.join(path);
        assert!(path.exists(), format!("{} should exist", path.display()));
    }

    /// If `path` does not point to valid path, fail the current test.
    pub fn expect_no_such_path<P: AsRef<Path>>(&self, path: P) {
        let path = self.dir.join(path);
        assert!(!path.exists(), format!("{} should not exist", path.display()));
    }

    /// Verify that the file contains the specified data.
    pub fn expect_file_contents<P, S>(&self, path: P, expected: S)
        where P: AsRef<Path>, S: AsRef<[u8]>
    {
        let path = self.dir.join(path);
        let expected = expected.as_ref();
        self.expect_path(&path);
        let mut f = fs::File::open(&path).expect("could not open file");
        let mut found = vec![];
        f.read_to_end(&mut found).expect("could not read file");
        expect_data_eq(path.display(), &found, expected);
    }

    /// (Internal.) Read a `Path` and return a `String`.
    fn read_file(&self, path: &Path) -> String
    {
        self.expect_path(&path);
        let mut f = fs::File::open(&path).expect("could not open file");
        let mut found = vec![];
        f.read_to_end(&mut found).expect("could not read file");
        str::from_utf8(&found).expect("expected UTF-8 file").to_owned()
    }

    /// Verify that the contents of the file match the specified pattern.
    /// Someday this should support `std::str::pattern::Pattern` so that we
    /// can support both strings and regular expressions, but that hasn't
    /// been stabilized yet.
    pub fn expect_contains<P>(&self, path: P, pattern: &str)
        where P: AsRef<Path>
    {
        let path = self.dir.join(path);
        let contents = self.read_file(&path);
        assert!(contents.contains(pattern),
                format!("expected {} to match {:?}, but it contained {:?}",
                        path.display(), pattern, contents));
    }

    /// Verify that the contents of the file do not match the specified pattern.
    /// Someday this should support `std::str::pattern::Pattern` so that we can
    /// support both strings and regular expressions, but that hasn't been
    /// stabilized yet.
    pub fn expect_does_not_contain<P>(&self, path: P, pattern: &str)
        where P: AsRef<Path>
    {
        let path = self.dir.join(path);
        let contents = self.read_file(&path);
        assert!(!contents.contains(pattern),
                format!("expected {} to not match {:?}, but it contained {:?}",
                        path.display(), pattern, contents));
    }
}

/// Internal helper function which compares to blobs of potentially binary data.
fn expect_data_eq<D>(source: D, found: &[u8], expected: &[u8])
    where D: fmt::Display
{
    if found != expected {
        // TODO: If the data appears to be actual binary, do a better job
        // of printing it.
        panic!("expected {} to equal {:?}, found {:?}",
               source,
               String::from_utf8_lossy(expected).as_ref(),
               String::from_utf8_lossy(found).as_ref());
    }
}

/// Extension methods for `std::process::Command`.
pub trait CommandExt {
    /// Spawn this command, passing it the specified data on standard
    /// input.
    fn output_with_stdin<S: AsRef<[u8]>>(&mut self, input: S)
                                         -> io::Result<process::Output>;
}

impl CommandExt for process::Command {
    fn output_with_stdin<S>(&mut self, input: S)
                            -> io::Result<process::Output>
        where S: AsRef<[u8]>
    {
        let input = input.as_ref().to_owned();
        let mut child: process::Child = self.stdin(process::Stdio::piped())
            .stdout(process::Stdio::piped())
            .stderr(process::Stdio::piped())
            .spawn()
            .expect("error running command");
        let mut stdin = child.stdin.take()
            .expect("std in is unexpectedly missing");
        let worker = thread::spawn(move || {
            stdin.write_all(&input)
                .expect("could not write to stdin");
            stdin.flush()
                .expect("could not flush data to child's stdin");
        });
        let result = child.wait_with_output();
        worker.join().expect("stdin writer failed");
        result
    }
}

/// Display command output and return it for examination.
pub trait TeeOutputExt {
    /// Display the output of a test command on `stdout` and `stderr`, then return
    /// the `Output` object for further processing.
    fn tee_output(self) -> io::Result<process::Output>;
}

impl TeeOutputExt for &mut process::Command {
    fn tee_output(self) -> io::Result<process::Output> {
        self.output().tee_output()
    }
}

impl TeeOutputExt for io::Result<process::Output> {
    fn tee_output(self) -> io::Result<process::Output> {
        let output = self?;
        io::stdout().write_all(&output.stdout)?;
        io::stderr().write_all(&output.stderr)?;
        Ok(output)
    }
}

/// Extension methods for `std::process::Output`.
pub trait OutputExt {
    /// Get standard output as a `str`.
    fn stdout_str(&self) -> &str;

    /// Get standard error as a `str`.
    fn stderr_str(&self) -> &str;
}

impl OutputExt for process::Output {
    fn stdout_str(&self) -> &str {
        str::from_utf8(&self.stdout)
            .expect("stdout was not UTF-8 text")
    }

    fn stderr_str(&self) -> &str {
        str::from_utf8(&self.stderr)
            .expect("stderr was not UTF-8 text")
    }
}

/// We define `expect_status` on quite a few related types to support
/// different calling patterns.
pub trait ExpectStatus {
    /// Expect the child process to succeed, and return a
    /// `std::process::Output` object with its output.
    fn expect_success(self) -> process::Output;

    /// Expect the child process to fail, and return `std::process::Output`
    /// object with its output.
    fn expect_failure(self) -> process::Output;
}

impl ExpectStatus for process::Output {
    fn expect_success(self) -> process::Output {
        if !self.status.success() {
            io::stdout().write_all(&self.stdout)
                .expect("could not write to stdout");
            io::stderr().write_all(&self.stderr)
                .expect("could not write to stderr");
            panic!("expected command to succeed, got {}", self.status)
        }
        self
    }

    fn expect_failure(self) -> process::Output {
        if self.status.success() {
            io::stdout().write_all(&self.stdout)
                .expect("could not write to stdout");
            io::stderr().write_all(&self.stderr)
                .expect("could not write to stderr");
            panic!("expected command to fail, got {}", self.status)
        }
        self
    }

}

impl<ES: ExpectStatus, E: fmt::Debug> ExpectStatus for Result<ES, E> {
    fn expect_success(self) -> process::Output {
        // Unwrap the result, fail on error, and pass `expect_success` to
        // our wrapped type.
        match self {
            Ok(es) => es.expect_success(),
            Err(err) => panic!("error running command: {:?}", err),
        }
    }

    fn expect_failure(self) -> process::Output {
        // Unwrap the result, fail on error, and pass `expect_failure` to
        // our wrapped type.
        match self {
            Ok(es) => es.expect_failure(),
            // Note that this means we couldn't _run_ the command (perhaps
            // because it doesn't exist or wasn't in our path), not that it
            // ran but failed.
            Err(err) => panic!("error running command: {:?}", err),
        }
    }
}

impl<'a> ExpectStatus for &'a mut process::Command {
    fn expect_success(self) -> process::Output {
        self.output().expect_success()
    }

    fn expect_failure(self) -> process::Output {
        self.output().expect_failure()
    }
}

impl ExpectStatus for process::Child {
    fn expect_success(self) -> process::Output {
        self.wait_with_output().expect_success()
    }

    fn expect_failure(self) -> process::Output {
        self.wait_with_output().expect_failure()
    }
}

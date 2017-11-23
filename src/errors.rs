//! Error-handling for this library.

use failure;
use std::fmt;
use std::path::{Path, PathBuf};
use std::result;

/// Our standard error type. If you need more information, you may be able to
/// downcast it to one of the `Fail` implementations defined in this module.
pub type Error = failure::Error;

/// The result type returned by the functions in this library.
pub type Result<T> = result::Result<T, Error>;

/// Declare a custom failure type that wraps a `PathBuf`. This is used to help
/// tie error messages to the associated file system location.
macro_rules! failure_with_pathbuf {
    ($type:ident, $msg:expr) => (

        /// An error occurred performing an operation on a path.
        #[derive(Debug, Fail)]
        pub struct $type {
            path: PathBuf
        }

        impl $type {
            /// Create a new error for the specified path.
            pub fn new<P: Into<PathBuf>>(path: P) -> $type {
                $type { path: path.into() }
            }

            /// The path associated with this error.
            pub fn path(&self) -> &Path {
                &self.path
            }
        }

        impl fmt::Display for $type {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, $msg, self.path.display())
            }
        }
    )
}

/// An error occurred while trying to create a directory.
failure_with_pathbuf!(CreateDir, "error creating directory {:?}");
/// An error occured while trying to read a file.
failure_with_pathbuf!(ReadFile, "error reading {:?}");
/// An error occured while trying to write a file.
failure_with_pathbuf!(WriteFile, "error writing {:?}");

/// An error occurred running an external command.
#[derive(Debug, Fail)]
#[fail(display = "error running external command {:?}", command)]
pub struct RunCommand {
    command: String,
}

impl RunCommand {
    /// Create a new error for the specified command. This is private because
    /// we probably want to add the command arguments at some point.
    pub(crate) fn new<S: Into<String>>(command: S) -> RunCommand {
        RunCommand {
            command: command.into(),
        }
    }

    /// The name of the command that failed.
    pub fn command(&self) -> &str {
        &self.command
    }
}

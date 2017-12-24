//! Implementations of `Fail` that are particularly useful with
//! `std::io::Error`, which does not include the pathname that caused an error.
//! However, these wrappers may also be useful with `serde` and other routines
//! that process the data that's being read or written to files.

use std::fmt;
use std::path::{Path, PathBuf};

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

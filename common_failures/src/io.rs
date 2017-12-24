//! Implementations of `Fail` that are particularly useful with
//! `std::io::Error`, which does not include the pathname that caused an error.
//! However, these wrappers may also be useful with `serde` and other routines
//! that process the data that's being read or written to files.

use failure::{Context, Error, Fail};
use std::fmt;
use std::path::{Path, PathBuf};
use std::result;

/// General categories of I/O operations that might fail.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Operation {
    /// Creating a file or a directory.
    Create,
    /// Reading a file.
    Read,
    /// Writing a file.
    Write,
    /// Deleting a file or a directory.
    Delete,
    /// Another operation.
    Other,
}

// TODO - Replace with something based on `fluent` if
// https://github.com/projectfluent/fluent-rs/issues/42 is addressed.
impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let text = match *self {
            Operation::Create => "creating",
            Operation::Read => "reading",
            Operation::Write => "writing",
            Operation::Delete => "deleting",
            Operation::Other => "operating on",
        };
        write!(f, "{}", text)
    }
}

/// The target of an I/O operation that failed.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Target {
    /// The directory at the specified path.
    Directory(PathBuf),
    /// The file at the specified path.
    File(PathBuf),
    /// Standard input.
    Stdin,
    /// Standard output.
    Stdout,
    /// Standard error.
    Stderr,
    /// Another target.
    Other(String)
}

// TODO - Replace with something based on `fluent` if
// https://github.com/projectfluent/fluent-rs/issues/42 is addressed.
impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Target::Directory(ref path) => {
                write!(f, "the directory {}", path.display())
            }
            Target::File(ref path) => {
                write!(f, "the file {}", path.display())
            }
            Target::Stdin => write!(f, "standard input"),
            Target::Stdout => write!(f, "standard output"),
            Target::Stderr => write!(f, "standard error"),
            Target::Other(ref name) => write!(f, "{}", name),
        }
    }
}

impl From<PathBuf> for Target {
    /// Convert a path to a `Target::File`.
    fn from(path: PathBuf) -> Self {
        Target::File(path)
    }
}

impl<'a> From<&'a PathBuf> for Target {
    /// Convert a path to a `Target::File`.
    fn from(path: &'a PathBuf) -> Self {
        Target::File(path.to_owned())
    }
}

impl<'a> From<&'a Path> for Target {
    /// Convert a path to a `Target::File`.
    fn from(path: &'a Path) -> Self {
        Target::File(path.to_path_buf())
    }
}

/// An I/O error with enough context to display a user-friendly message.
#[derive(Debug, Fail)]
pub struct IoError {
    /// The general category of the operation which failed.
    pub operation: Operation,
    /// The target we were performing the operation on.
    pub target: Target,
}

impl fmt::Display for IoError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error {} {}", self.operation, self.target)
    }
}

/// Trait which extends `Result` with methods for generating better error
/// messages.
pub trait IoContextExt<T, E>: Sized {
    /// Specify the "context" within which an I/O operation occurred. Will be
    /// used to generate a human-readable error message if needed.
    ///
    /// ```no_run
    /// # extern crate common_failures;
    /// # fn main() {}
    /// use common_failures::prelude::*;
    /// use common_failures::io::{Operation, Target};
    /// use std::fs::create_dir_all;
    /// use std::path::Path;
    ///
    /// fn create_directory(path: &Path) -> Result<()> {
    ///     create_dir_all(path).io_context(
    ///         Operation::Create,
    ///         Target::Directory(path.to_owned()),
    ///     )?;
    ///     Ok(())
    /// }
    /// ```
    fn io_context<Tgt>(
        self,
        operation: Operation,
        target: Tgt,
    ) -> result::Result<T, Context<IoError>>
    where
        Tgt: Into<Target>;

    /// Specify the "context" within which an I/O read operation occurred. Will
    /// be used to generate a human-readable error message if needed.
    ///
    /// ```no_run
    /// # extern crate common_failures;
    /// # fn main() {}
    /// use common_failures::prelude::*;
    /// use std::fs::File;
    /// use std::path::Path;
    ///
    /// fn open_example(path: &Path) -> Result<File> {
    ///     Ok(File::open(path).io_read_context(path)?)
    /// }
    /// ```
    fn io_read_context<Tgt>(
        self,
        target: Tgt
    ) -> result::Result<T, Context<IoError>>
    where
        Tgt: Into<Target>
    {
        self.io_context(Operation::Read, target)
    }

    /// Specify the "context" within which an I/O write operation occurred. Will
    /// be used to generate a human-readable error message if needed.
    fn io_write_context<Tgt>(
        self,
        target: Tgt
    ) -> result::Result<T, Context<IoError>>
    where
        Tgt: Into<Target>
    {
        self.io_context(Operation::Write, target)
    }
}

impl<T, E> IoContextExt<T, E> for result::Result<T, E>
where
    E: Fail,
{
    fn io_context<Tgt>(
        self,
        operation: Operation,
        target: Tgt,
    ) -> result::Result<T, Context<IoError>>
    where
        Tgt: Into<Target>
    {
        self.map_err(|failure| {
            let ioerr = IoError {
                operation,
                target: target.into(),
            };
            failure.context(ioerr)
        })
    }
}

/// This is an unfortunate duplicate of `IoContextExt` that we require because
/// Rust doesn't allow allow us to implement `IoContextExt` for both `E: Fail`
/// and `failure::Error`, at least not from outside the `failure` crate.
pub trait IoContextErrorExt<T, E>: Sized {
    /// Specify the "context" within which an I/O operation occurred. Will be
    /// used to generate a human-readable error message if needed.
    ///
    /// ```no_run
    /// # extern crate common_failures;
    /// # fn main() {}
    /// use common_failures::prelude::*;
    /// use common_failures::io::{Operation, Target};
    /// use std::fs::create_dir_all;
    /// use std::path::Path;
    ///
    /// fn create_directory(path: &Path) -> Result<()> {
    ///     create_dir_all(path).io_context(
    ///         Operation::Create,
    ///         Target::Directory(path.to_owned()),
    ///     )?;
    ///     Ok(())
    /// }
    /// ```
    fn io_context<Tgt>(
        self,
        operation: Operation,
        target: Tgt,
    ) -> result::Result<T, Context<IoError>>
    where
        Tgt: Into<Target>;

    /// Specify the "context" within which an I/O read operation occurred. Will
    /// be used to generate a human-readable error message if needed.
    ///
    /// ```no_run
    /// # extern crate common_failures;
    /// # fn main() {}
    /// use common_failures::prelude::*;
    /// use std::fs::File;
    /// use std::path::Path;
    ///
    /// fn open_example(path: &Path) -> Result<File> {
    ///     Ok(File::open(path).io_read_context(path)?)
    /// }
    /// ```
    fn io_read_context<Tgt>(
        self,
        target: Tgt
    ) -> result::Result<T, Context<IoError>>
    where
        Tgt: Into<Target>
    {
        self.io_context(Operation::Read, target)
    }

    /// Specify the "context" within which an I/O write operation occurred. Will
    /// be used to generate a human-readable error message if needed.
    fn io_write_context<Tgt>(
        self,
        target: Tgt
    ) -> result::Result<T, Context<IoError>>
    where
        Tgt: Into<Target>
    {
        self.io_context(Operation::Write, target)
    }
}

impl<T> IoContextErrorExt<T, Error> for result::Result<T, Error> {
    fn io_context<Tgt>(
        self,
        operation: Operation,
        target: Tgt,
    ) -> result::Result<T, Context<IoError>>
    where
        Tgt: Into<Target>
    {
        self.map_err(|err| {
            let ioerr = IoError {
                operation,
                target: target.into(),
            };
            err.context(ioerr)
        })
    }
}


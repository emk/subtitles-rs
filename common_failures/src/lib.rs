//! Helpers for the `failure` crate, including:
//!
//! - Implementations of `Fail` which provide better error messages than the
//!   standard library. For example, we have wrappers for common `io::Error`
//!   uses which include the path to the underlying file.
//! - Various helper functions and macros for common use cases. In particular,
//!   we try to include helpers for people who've migrated from `error-chain`.

#[macro_use]
extern crate failure;

use std::fmt;
use std::result;

pub mod io;

/// Re-export `failure::Error` for convenience.
pub type Error = failure::Error;

/// A short alias for `Result<T, failure::Error>`.
pub type Result<T> = result::Result<T, Error>;

/// A wrapper which prints a human-readable list of the causes of an error, plus
/// a backtrace if present.
pub struct DisplayCausesAndBacktrace<'a> {
    err: &'a Error,
}

impl<'a> fmt::Display for DisplayCausesAndBacktrace<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for cause in self.err.causes() {
            if first {
                first = false;
                writeln!(f, "Error: {}", cause)?;
            } else {
                writeln!(f, "  caused by: {}", cause)?;
            }
        }
        write!(f, "{}",  self.err.backtrace())?;
        Ok(())
    }
}

/// Extensions to standard `failure::Error` trait.
pub trait FailureErrorExt {
    /// Wrap the error in `DisplayCausesAndBacktrace`, causing it to be
    /// formatted with a human-readable list of all causes, plus an optional
    /// backtrace.
    fn display_causes_and_backtrace(&self) -> DisplayCausesAndBacktrace;
}

impl FailureErrorExt for failure::Error {
    fn display_causes_and_backtrace(&self) -> DisplayCausesAndBacktrace {
        DisplayCausesAndBacktrace { err: self }
    }
}

/// Generate a `main` function which calls the specified function. If the
/// function returns `Result::Err(_)`, then `main` will print the error and exit
/// with a non-zero status code.
#[macro_export]
macro_rules! quick_main {
    ($wrapped:ident) => (
        fn main() {
            if let Err(err) = $wrapped() {
                use $crate::FailureErrorExt;
                use ::std::io::Write;
                let stderr = ::std::io::stderr();
                write!(
                    &mut stderr.lock(),
                    "{}",
                    err.display_causes_and_backtrace(),
                ).expect("Error occurred while trying to display error");
                ::std::process::exit(1);
            }
        }
    )
}

//! Utilities for formatting and displaying errors.

use failure;
use std::fmt;

use Error;

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
pub trait DisplayCausesAndBacktraceExt {
    /// Wrap the error in `DisplayCausesAndBacktrace`, causing it to be
    /// formatted with a human-readable list of all causes, plus an optional
    /// backtrace.
    fn display_causes_and_backtrace(&self) -> DisplayCausesAndBacktrace;
}

impl DisplayCausesAndBacktraceExt for failure::Error {
    fn display_causes_and_backtrace(&self) -> DisplayCausesAndBacktrace {
        DisplayCausesAndBacktrace { err: self }
    }
}

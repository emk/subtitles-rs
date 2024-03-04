//! Error-handling for this library.

use std::{error, fmt};

/// An error occurred running an external command.
#[derive(Debug)]
pub struct RunCommandError {
    command: String,
}

impl RunCommandError {
    /// Create a new error for the specified command. This is private because
    /// we probably want to add the command arguments at some point.
    pub(crate) fn new<S: Into<String>>(command: S) -> RunCommandError {
        RunCommandError {
            command: command.into(),
        }
    }

    /// The name of the command that failed.
    pub fn command(&self) -> &str {
        &self.command
    }
}

impl fmt::Display for RunCommandError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error running external command {:?}", self.command)
    }
}

impl error::Error for RunCommandError {}

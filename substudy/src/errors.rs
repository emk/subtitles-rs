//! Error-handling for this library.

pub use common_failures::{Error, Result};
pub use common_failures::io::*;

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

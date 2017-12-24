//! Error-handling for this library.

/// An error occurred running an external command.
#[derive(Debug, Fail)]
#[fail(display = "error running external command {:?}", command)]
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

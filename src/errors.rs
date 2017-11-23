//! Error-handling for this library.

// `error-chain` makes it hard to document all the definitions it
// generates.
#![allow(missing_docs, unused_doc_comment)]

use handlebars;
use std::path::PathBuf;
use std::str;

use uchardet;

error_chain! {
    foreign_links {
        Render(handlebars::RenderError);
        Template(handlebars::TemplateError);
        Uchardet(uchardet::Error);
    }

    errors {
        CreateDir(path: PathBuf) {
            description("error creating directory")
            display("error creating directory {:?}", path.display())
        }
        ReadFile(path: PathBuf) {
            description("error reading file")
            display("error reading {:?}", path.display())
        }
        RunCommand(command: String) {
            description("error running external command")
            display("error running {:?}", command)
        }
        WriteFile(path: PathBuf) {
            description("error writing file")
            display("error writing {:?}", path.display())
        }
    }
}

impl ErrorKind {
    pub fn create_dir<P: Into<PathBuf>>(path: P) -> ErrorKind {
        ErrorKind::CreateDir(path.into())
    }

    pub fn read_file<P: Into<PathBuf>>(path: P) -> ErrorKind {
        ErrorKind::ReadFile(path.into())
    }

    pub fn run_command<S: Into<String>>(command: S) -> ErrorKind {
        ErrorKind::RunCommand(command.into())
    }

    pub fn write_file<P: Into<PathBuf>>(path: P) -> ErrorKind {
        ErrorKind::WriteFile(path.into())
    }
}

/// Create a new error from something that can be turned into a string.
pub fn err_str<T: Into<String>>(message: T) -> Error {
    From::from(message.into())
}

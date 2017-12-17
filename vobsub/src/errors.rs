//! Custom `Error` and `Result` types, declared using `error-chain`.

// There's no way for us to document `ErrorrKind::Io`, unfortunately.
#![allow(missing_docs)]

use nom::IResult;
use std::default::Default;
use std::fmt;
use std::io;
use std::path::PathBuf;

error_chain! {
    foreign_links {
        Io(io::Error);
    }

    errors {
        /// Our input data ended sooner than we expected.
        IncompleteInput {
            description("Input ended unexpectedly")
            display("Input ended unexpectedly")
        }

        /// We were unable to find a required key in an `*.idx` file.
        MissingKey(key: &'static str) {
            description("Could not find required key")
            display("Could not find required key '{}'", key)
        }

        /// We could not parse a value.
        Parse(message: String) {
            description("Parse error")
            display("Could not parse: {}", &message)
        }

        /// We could not parse a file for some reason.
        ReadFile(path: PathBuf) {
            description("Could not read file")
            display("Could not read {}", path.display())
        }

        /// We have leftover input that we didn't expect.
        UnexpectedInput {
            description("Unexpected extra input")
            display("Unexpected extra input")
        }
    }
}

pub trait IResultExt<I, O, E> {
    fn ignore_trailing_data(self) -> IResult<I, O, E>;
    fn to_vobsub_result(self) -> Result<O>;
}

impl<I: Default+Eq, O, E: fmt::Debug> IResultExt<I, O, E> for IResult<I, O, E> {
    fn ignore_trailing_data(self) -> IResult<I, O, E> {
        match self {
            IResult::Done(_, val) => IResult::Done(I::default(), val),
            other => other,
        }
    }

    fn to_vobsub_result(self) -> Result<O> {
        match self {
            IResult::Done(rest, val) => {
                if rest == I::default() {
                    Ok(val)
                } else {
                    Err(ErrorKind::UnexpectedInput.into())
                }
            }
            IResult::Incomplete(_) => Err(ErrorKind::IncompleteInput.into()),
            IResult::Error(err) => {
                Err(ErrorKind::Parse(format!("{:?}", err)).into())
            }
        }
    }
}

//! Error-handling for this library.

use std::error::{Error,FromError};
use std::io::IoError;

/// Our library's error class.  This may grow new enumeration values.
#[deriving(Show)]
pub enum SubStudyError {
    /// An error occurred while reading or writing subtitle data.
    Io(IoError),

    /// An error occurred while parsing subtitle data.
    Parse(String)
}

/// Our library's result type.
pub type SubStudyResult<T> = Result<T, SubStudyError>;

impl Error for SubStudyError {
    fn description(&self) -> &str {
        match *self {
            SubStudyError::Io(ref err) => err.description(),
            SubStudyError::Parse(_) => "parse error"
        }
    }

    fn detail(&self) -> Option<String> {
        match *self {
            SubStudyError::Io(ref err) => err.detail(),
            SubStudyError::Parse(ref str) => Some(str.clone())
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl FromError<IoError> for SubStudyError {
    fn from_error(err: IoError) -> SubStudyError {
        SubStudyError::Io(err)
    }
}

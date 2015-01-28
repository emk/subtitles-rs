//! Error-handling for this library.

use std::error::{Error,FromError};
use std::fmt;
use std::io::IoError;
use uchardet::EncodingDetectorError;

/// Our library's error class.  This may grow new enumeration values.
#[derive(Show)]
pub enum SubStudyError {
    /// Error reading or writing subtitle data.
    Io(IoError),

    /// Error detecting the subtitle encoding.
    EncodingDetector(EncodingDetectorError),

    /// Error decoding the subtiles to UTF-8.
    Decode(String),

    /// An error which occurred while parsing subtitle data.
    Parse(String)
}

/// Our library's result type.
pub type SubStudyResult<T> = Result<T, SubStudyError>;

impl Error for SubStudyError {
    fn description(&self) -> &str {
        match *self {
            SubStudyError::Io(ref err) => err.description(),
            SubStudyError::EncodingDetector(ref err) => err.description(),
            SubStudyError::Decode(_) => "decode error",
            SubStudyError::Parse(_) => "parse error"
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

impl fmt::Display for SubStudyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SubStudyError::Io(ref err) => err.fmt(f),
            SubStudyError::EncodingDetector(ref err) => err.fmt(f),
            SubStudyError::Decode(ref s) => write!(f, "{}", &s),
            SubStudyError::Parse(ref s) => write!(f, "{}", &s)
        }
    }
}

impl FromError<IoError> for SubStudyError {
    fn from_error(err: IoError) -> SubStudyError {
        SubStudyError::Io(err)
    }
}

impl FromError<EncodingDetectorError> for SubStudyError {
    fn from_error(err: EncodingDetectorError) -> SubStudyError {
        SubStudyError::EncodingDetector(err)
    }
}

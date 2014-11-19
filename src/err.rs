//! Error-handling for this library.

use std::error::{Error,FromError};
use std::io::IoError;
use uchardet::EncodingDetectorError;

/// Our library's error class.  This may grow new enumeration values.
#[deriving(Show)]
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

    fn detail(&self) -> Option<String> {
        match *self {
            SubStudyError::Io(ref err) => err.detail(),
            SubStudyError::EncodingDetector(ref err) => err.detail(),
            SubStudyError::Decode(ref str) => Some(str.clone()),
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

impl FromError<EncodingDetectorError> for SubStudyError {
    fn from_error(err: EncodingDetectorError) -> SubStudyError {
        SubStudyError::EncodingDetector(err)
    }
}

//! Error-handling for this library.

use std::error;
use std::io;
use std::result;
pub use self::Error::{IoError, ParseError};

/// Our library's error class.  This may grow new enumeration values.
#[deriving(Show)]
pub enum Error {
    /// An error occurred while reading or writing subtitle data.
    IoError(io::IoError),

    /// An error occurred while parsing subtitle data.
    ParseError(String)
}

/// Our library's result type.
pub type Result<T> = result::Result<T, Error>;

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            IoError(ref err) => err.description(),
            ParseError(_) => "parse error"
        }
    }

    fn detail(&self) -> Option<String> {
        match *self {
            IoError(ref err) => err.detail(),
            ParseError(ref str) => Some(str.clone())
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        None
    }
}

impl error::FromError<io::IoError> for Error {
    fn from_error(err: io::IoError) -> Error {
        IoError(err)
    }
}

    

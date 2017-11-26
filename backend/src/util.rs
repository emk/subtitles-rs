//! Utility types and functions.

use iron::{IronError, IronResult};
use iron::status;
use std::error;
use std::fmt;
use std::io;

/// Convert an `io::Result` into an `IronResult`.
pub fn iron_from_io<T>(result: io::Result<T>) -> IronResult<T> {
    match result {
        Ok(val) => Ok(val),
        Err(err) => {
            let status = match err.kind() {
                io::ErrorKind::NotFound => status::NotFound,
                io::ErrorKind::PermissionDenied => status::Forbidden,
                _ => status::InternalServerError
            };
            Err(IronError::new(err, status))
        }
    }
}

/// Create a simple `IronError` with the specified message and HTTP status
/// code.
pub fn iron_err<S: Into<String>>(code: status::Status, msg: S) -> IronError {
    IronError::new(StringError::new(msg), code)
}

#[macro_export]
macro_rules! iron_err {
    ( $code:expr, $fmt:expr ) => {
        return Err(iron_err($code, format!($fmt)));
    };
    ( $code:expr, $fmt:expr, $($arg:tt)* ) => {
        return Err(iron_err($code, format!($fmt, $($arg)*)));
    };
}

/// A simple error containing nothing but a string.
#[derive(Debug)]
pub struct StringError {
    message: String
}

impl StringError {
    /// Create a new error.
    pub fn new<S: Into<String>>(message: S) -> StringError {
        StringError { message: message.into() }
    }
}

impl fmt::Display for StringError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", &self.message)
    }
}

impl error::Error for StringError {
    fn description(&self) -> &str {
        &self.message
    }
}


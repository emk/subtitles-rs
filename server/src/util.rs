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

#[derive(Debug)]
pub struct StringError {
    message: String
}

impl StringError {
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


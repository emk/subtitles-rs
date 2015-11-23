//! Error-handling for this library.

use std::error;
use std::result;

/// Our library's error class.  We just use a generic error type for
/// everything; it works fine for our needs.
pub type Error = Box<error::Error + Send + Sync>;

/// Our library's result type.
pub type Result<T> = result::Result<T, Error>;

/// Create a new error from something that can be turned into a string.
pub fn err_str<T: Into<String>>(message: T) -> Error {
    From::from(message.into())
}

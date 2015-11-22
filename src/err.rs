//! Error-handling for this library.

use std::error;
use std::result;

/// Our library's error class.  We just use a generic error type for
/// everything; it works fine for our needs.
pub type Error = Box<error::Error + Send + Sync>;

/// Our library's result type.
pub type Result<T> = result::Result<T, Error>;

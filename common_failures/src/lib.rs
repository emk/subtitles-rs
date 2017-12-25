//! We provide support for:
//!
//! - User-friendly `io::Error` wrappers with pathnames,
//! - Formatting errors for display to the user (with the entire cause chain!),
//!   and
//! - Handy helper utilities like `quick_main!`.
//!
//! Basically, the goal is to make `failure` as ergonomic as possible, so that
//! everybody can stop re-inventing common bits of supporting code.
//!
//! ## User-friendly `io::Error` wrappers
//!
//! By default, Rust's I/O errors do not include any information about the
//! operation that failed. This means that you'll often see errors like:
//!
//! ```txt
//! No such file or directory (os error 2)
//! ```
//!
//! But it's much nicer for users if we print something like:
//!
//! ```txt
//! Error: error reading the file no-such-file.txt
//!   caused by: No such file or directory (os error 2)
//! ```
//!
//! To do this, we can use `io_read_context` and related functions:
//!
//! ```
//! # extern crate common_failures;
//! # fn main() {}
//! use common_failures::prelude::*;
//! use std::fs::File;
//! use std::path::Path;
//!
//! fn open_example(path: &Path) -> Result<File> {
//!     Ok(File::open(path).io_read_context(path)?)
//! }
//! ```
//!
//! ## Formatting errors for display to the user
//!
//! We also provide a support for formatting errors as strings, including the
//! entire chain of "causes" of the error:
//!
//! ```no_run
//! # extern crate common_failures;
//! # extern crate failure;
//! # fn main() {
//! use common_failures::prelude::*;
//!
//! # let err: Error = failure::err_msg("Example error");
//! format!("{}", err.display_causes_and_backtrace());
//! # }
//! ```
//!
//! ## The `quick_main!` macro
//!
//! This is a replacement for `quick_main!` from the `error-chain` crate. It
//! generates a `main` function that calls a second function returning
//! `Result<()>`, and prints out any errors.
//!
//! ```
//! # fn main() {} // Dummy `main` to disable doctest wrapper.
//! #[macro_use]
//! extern crate common_failures;
//! #[macro_use]
//! extern crate failure;
//!
//! // This imports `Result`, `Error`, `failure::ResultExt`, and possibly
//! // other useful extension traits, to get you a minimal useful API.
//! use common_failures::prelude::*;
//!
//! // Uncomment this to define a `main` function that calls `run`, and prints
//! // any errors that it returns to standard error.
//! //quick_main!(run);
//!
//! fn run() -> Result<()> {
//!     if true {
//!         Ok(())
//!     } else {
//!         Err(format_err!("an error occurred"))
//!     }
//! }
//! ```

#![warn(missing_docs)]

// We would love to re-export the macros from this crate, but that's not
// feasible on standard Rust. If we _could_ export the macros, it would also
// be desirable to `pub extern crate` it, so that it's easier to keep in sync
// with this crate.
#[macro_use]
extern crate failure;

use std::result;

/// Import this module to get a useful error-handling API.
pub mod prelude {
    pub use {Error, Result};
    pub use io::{IoContextExt, IoContextErrorExt};
    pub use display::DisplayCausesAndBacktraceExt;
    pub use failure::ResultExt;
}

pub mod display;
pub mod io;

/// Re-export `failure::Error` for convenience.
pub type Error = failure::Error;

/// A short alias for `Result<T, failure::Error>`.
pub type Result<T> = result::Result<T, Error>;

/// Generate a `main` function which calls the specified function. If the
/// function returns `Result::Err(_)`, then `main` will print the error and exit
/// with a non-zero status code.
#[macro_export]
macro_rules! quick_main {
    ($wrapped:ident) => (
        fn main() {
            if let Err(err) = $wrapped() {
                use $crate::display::DisplayCausesAndBacktraceExt;
                use ::std::io::Write;
                let stderr = ::std::io::stderr();
                write!(
                    &mut stderr.lock(),
                    "{}",
                    err.display_causes_and_backtrace(),
                ).expect("Error occurred while trying to display error");
                ::std::process::exit(1);
            }
        }
    )
}

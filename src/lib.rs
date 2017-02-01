// For error-chain.
#![recursion_limit = "1024"]

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate nom;

mod errors;
pub mod mpeg2;

pub use self::errors::{Error, Result};

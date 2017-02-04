// For error-chain.
#![recursion_limit = "1024"]

extern crate cast;
#[macro_use]
extern crate error_chain;
#[cfg(test)]
extern crate env_logger;
extern crate image;
#[macro_use]
extern crate log;
#[macro_use]
extern crate nom;
extern crate safemem;

mod errors;
mod img;
mod mpeg2;
mod sub;
mod util;

pub use self::errors::{Error, Result};
pub use self::sub::{Coordinates, Subtitle, Subtitles, subtitles};

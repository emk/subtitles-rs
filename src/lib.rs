//! Tools for studying foreign languages using subtitles.  All APIs are
//! currently experimental or unstable, but if you'd like me to stabilize
//! things, please get in touch.
//!
//! For further details about how to use substudy, see [the main GitHub
//! page](http://www.rust-ci.org/emk/substudy/doc/substudy/).

//#![deny(missing_docs)]

#[macro_use] extern crate log;
extern crate regex;
extern crate uchardet;
extern crate encoding;
extern crate ffmpeg;

pub mod err;
pub mod decode;
mod grammar;
pub mod srt;
pub mod clean;
pub mod merge;
pub mod align;

//! Tools for studying foreign languages using subtitles.  All APIs are
//! currently experimental or unstable, but if you'd like me to stabilize
//! things, please get in touch.
//!
//! For further details about how to use substudy, see [the main GitHub
//! page](http://github.com/emk/substudy).

#![warn(missing_docs)]

extern crate cast;
extern crate chardet;
extern crate csv;
#[cfg(test)]
#[macro_use]
extern crate difference;
extern crate encoding;
#[macro_use]
extern crate failure;
#[macro_use]
extern crate failure_derive;
extern crate handlebars;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
extern crate num;
extern crate pbr;
extern crate regex;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate whatlang;

pub mod errors;
pub mod contexts;
pub mod decode;
pub mod lang;
pub mod srt;
pub mod clean;
pub mod merge;
pub mod time;
pub mod align;
pub mod video;
pub mod export;

mod grammar {
    // Include generated source code for grammar.
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

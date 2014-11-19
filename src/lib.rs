//! Tools for studying foreign languages using subtitles.  All APIs are
//! currently experimental or unstable, but if you'd like me to stabilize
//! things, please get in touch.
//!
//! For further details about how to use substudy, see [the main GitHub
//! page](http://www.rust-ci.org/emk/substudy/doc/substudy/).

#![license = "Public domain (Unlicense)"]
#![experimental]
#![deny(missing_docs)]
#![deny(warnings)]
#![feature(phase)]

extern crate collections;
#[phase(plugin, link)] extern crate log;
#[phase(plugin)] extern crate peg_syntax_ext;
extern crate regex;
#[phase(plugin)] extern crate regex_macros;
extern crate uchardet;
extern crate encoding;

pub mod err;
pub mod decode;
pub mod srt;
pub mod clean;
pub mod merge;
pub mod align;

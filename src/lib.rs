//! Tools for studying foreign languages using subtitles.  All APIs are
//! currently experimental or unstable, but if you'd like me to stabilize
//! things, please get in touch.
//!
//! For further details about how to use substudy, see [the main GitHub
//! page](http://www.rust-ci.org/emk/substudy/doc/substudy/).

#![experimental]
#![feature(plugin)]
#![deny(missing_docs)]
#![deny(warnings)]
#![allow(unstable)]

extern crate collections;
#[macro_use] extern crate log;
#[plugin] #[no_link] extern crate peg_syntax_ext;
extern crate regex;
#[plugin] #[no_link] extern crate regex_macros;
extern crate uchardet;
extern crate encoding;

pub mod err;
pub mod decode;
pub mod srt;
pub mod clean;
pub mod merge;
pub mod align;

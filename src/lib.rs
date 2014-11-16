//! Tools for studying foreign languages using subtitles.

#![license = "Public domain (Unlicense)"]
#![experimental]
#![deny(missing_docs)]
#![deny(warnings)]
#![feature(phase)]

#[phase(plugin, link)] extern crate log;
#[phase(plugin)] extern crate peg_syntax_ext;
extern crate regex;
#[phase(plugin)] extern crate regex_macros;

pub mod err;
pub mod srt;
pub mod merge;
pub mod align;

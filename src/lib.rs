//! Tools for studying foreign languages using subtitles.

#![license = "Public domain (Unlicense)"]
#![experimental]
#![deny(missing_docs)]
#![deny(warnings)]
#![feature(phase)]

#[phase(plugin)]
extern crate peg_syntax_ext;

pub mod err;
pub mod srt;


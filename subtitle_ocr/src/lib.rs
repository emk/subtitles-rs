//! This crate performs specialized OCR on subtitles.  Two major approaches
//! have been used to solve this problem in the past:
//!
//! 1. Reuse standard OCR algorithms. Standard OCR algorithms are designed
//!    to work on scanned images, where each occurance of a letter is
//!    slightly different from all the others.  Standard OCR alogrithms
//!    also require fairly extensive training to adapt to new languages and
//!    character sets, or even to unusual fonts.
//! 2. Ask the user to identify each new character image once, and then
//!    reuse that information every time that letter appears.  This works
//!    fairly well, but requires more manual effort from the user.  It also
//!    tends to confuse uppercase "I" and lowercase "l" in many fonts,
//!    requiring a spelling-correction pass to clean up the worst problems.
//!    However, it tends to adapt well to new character sets.
//!
//! The goal of this library is to experiment with a third approach,
//! exploiting the unusual features of subtitles, but vastly reducing the
//! amount of work demanded of the user.
//!
//! ## Design notes
//!
//! Here are some possible steps we might take:
//!
//! 1. Binarization.  Reduce image to two colors.
//! 2. Segmentation.  Find all the connected segments in the image.
//! 3. (Line detection.)
//! 4. Letter detection.  Collect segments into letters.  This may also
//!    involve detecting ligatures of several letters.
//! 5. Word detection.  Collect letters into words.
//!
//! At this point, we have a number of choices:
//!
//! - We could maintain a large database of known letter forms in common
//!   subtitle fonts.
//! - We could perform traditional OCR on individual letters.
//! - We could treat the letter images as a simple substituion cipher and
//!   perfom basic cryptanalysis using bigram and trigram frequencies and
//!   a frequency dictionary.
//! - We could ask the user for help.
//! - We could use some combination of the above techniques.

#![warn(missing_docs)]

// For error-chain.
#![recursion_limit = "1024"]

extern crate cast;
extern crate common_failures;
#[cfg(test)]
extern crate env_logger;
#[macro_use]
extern crate failure;
#[cfg(test)]
extern crate glob;
extern crate image;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
extern crate palette;
#[cfg(test)]
#[macro_use]
extern crate quickcheck;
#[cfg(test)]
extern crate vobsub;

#[macro_use]
mod logimg;

mod binarization;
mod ctx;
mod ext;
mod geom;
mod pixmap;
mod segmentation;
#[cfg(test)]
mod test_util;

pub use self::ctx::OcrContext;
pub use common_failures::{Error, Result};

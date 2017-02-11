//! This crate reads DVD subtitles in VobSub format.  These are typically
//! stored as two files: an `*.idx` file summarizing the subtitles, and an
//! MPEG-2 Program Stream containing the actual subtitle packets.
//!
//! ## Example code
//!
//! ```
//! extern crate image;
//! extern crate vobsub;
//!
//! let idx = vobsub::Index::open("examples/example.idx").unwrap();
//! for sub in idx.subtitles() {
//!     let sub = sub.unwrap();
//!     println!("Time: {:0.3}-{:0.3}", sub.start_time, sub.end_time);
//!     println!("Always show: {:?}", sub.force);
//!     println!("At: {}, {}", sub.coordinates.left(), sub.coordinates.top());
//!     println!("Size: {}x{}", sub.coordinates.width(), sub.coordinates.height());
//!     let img: image::RgbaImage = sub.to_image(idx.palette());
//!
//!     // You can save or manipulate `img` using the APIs provided by the Rust
//!     // `image` crate.
//! }
//! ```
//!
//! ## Limitations
//!
//! The initial version of this library is focused on extracting just the
//! information shown above, and it does not have full support for all the
//! options found in `*.idx` files.  It also lacks support for rapidly
//! finding the subtitle associated with a particular time during playback.

#![warn(missing_docs)]

// For error-chain.
#![recursion_limit = "1024"]

extern crate cast;
#[macro_use]
extern crate error_chain;
#[cfg(test)]
extern crate env_logger;
extern crate image;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate nom;
extern crate regex;
extern crate safemem;

mod errors;
mod idx;
mod img;
mod mpeg2;
mod sub;
mod util;

pub use self::errors::{Error, Result};
pub use self::idx::{Index, Palette};
pub use self::sub::{Coordinates, Subtitle, Subtitles, subtitles};

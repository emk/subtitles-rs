//! # MPEG-2 Program Streams (PS)
//!
//! This is the container format used at the top-level of a `*.sub` file.

use std::fmt;

use super::clock::{Clock, clock_and_ext};

/// A parsed [MPEG-2 Program Stream header][MPEG-PS] (MPEG-PS).
///
/// [MPEG-PS]: https://en.wikipedia.org/wiki/MPEG_program_stream
#[derive(Debug, PartialEq, Eq)]
pub struct Header {
    /// The System Clock Reference (SCR) and SCR extension field.
    pub scr: Clock,
    /// The bit rate, in units of 50 bytes per second.
    pub bit_rate: u32,
}

impl fmt::Display for Header {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[PS packet @ {}, {} kbps]", self.scr, (self.bit_rate*50*8)/1024)
    }
}

/// Parse a Program Stream header.
named!(pub header<Header>,
    do_parse!(
        // Sync bytes.
        tag!(&[0x00, 0x00, 0x01, 0xba]) >>
        // 10-byte header.
        header: bits!(
            do_parse!(
                // MPEG-2 version tag.
                tag_bits!(u8, 2, 0b01) >>
                // System Clock Reference.
                scr: call!(clock_and_ext) >>
                // Bit rate.
                bit_rate: take_bits!(u32, 22) >>
                // Marker bits.
                tag_bits!(u8, 2, 0b11) >>
                // Reserved.
                take_bits!(u8, 5) >>
                // Number of bytes of stuffing.
                stuffing_length: take_bits!(usize, 3) >>
                // Stuffing bytes.
                take_bits!(u32, stuffing_length * 8) >>
                (Header {
                    scr: scr,
                    bit_rate: bit_rate,
                })
            )
        ) >>
        (header)
    )
);

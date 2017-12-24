//! # MPEG-2 Program Streams (PS)
//!
//! This is the container format used at the top-level of a `*.sub` file.

use common_failures::prelude::*;
use nom::IResult;
use std::fmt;

use super::clock::{Clock, clock_and_ext};
use super::pes;

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
                // Stuffing bytes.  We just want to ignore these, but use a
                // large enough type to prevent overflow panics when
                // fuzzing.
                take_bits!(u64, stuffing_length * 8) >>
                (Header {
                    scr: scr,
                    bit_rate: bit_rate,
                })
            )
        ) >>
        (header)
    )
);

/// A [Packetized Elementary Stream][pes] packet with a Program Stream
/// header.
///
/// [pes]: http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
#[derive(Debug, PartialEq, Eq)]
pub struct PesPacket<'a> {
    pub ps_header: Header,
    pub pes_packet: pes::Packet<'a>,
}

/// Parse a Program Stream packet and the following PES packet.
named!(pub pes_packet<PesPacket>,
    do_parse!(
        ps_header: call!(header) >>
        pes_packet: call!(pes::packet) >>
        (PesPacket {
            ps_header: ps_header,
            pes_packet: pes_packet,
        })
    )
);

/// An iterator over all the PES packets in an MPEG-2 Program Stream.
pub struct PesPackets<'a> {
    /// The remaining input to parse.
    remaining: &'a [u8],
}

impl<'a> Iterator for PesPackets<'a> {
    type Item = Result<PesPacket<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Search for the start of a ProgramStream packet.
            let needle = &[0x00, 0x00, 0x01, 0xba];
            let start = self.remaining.windows(needle.len())
                .position(|window| needle == window);

            if let Some(start) = start {
                // We found the start, so try to parse it.
                self.remaining = &self.remaining[start..];
                match pes_packet(self.remaining) {
                    // We found a packet!
                    IResult::Done(remaining, packet) => {
                        self.remaining = remaining;
                        trace!("Decoded packet {:?}", &packet);
                        return Some(Ok(packet));
                    }
                    // We have only a partial packet, and we hit the end of our
                    // data.
                    IResult::Incomplete(needed) => {
                        self.remaining = &[];
                        warn!("Incomplete packet, need: {:?}", needed);
                        return Some(Err(format_err!("Incomplete PES packet")));
                    }
                    // We got something that looked like a packet but
                    // wasn't parseable.  Log it and keep trying.
                    IResult::Error(err) => {
                        self.remaining = &self.remaining[needle.len()..];
                        debug!("Skipping packet {:?}", &err);
                    }
                }
            } else {
                // We didn't find the start of a packet.
                self.remaining = &[];
                trace!("Reached end of data");
                return None;
            }
        }
    }
}

/// Iterate over all the PES packets in an MPEG-2 Program Stream (or at
/// least those which contain subtitles).
pub fn pes_packets(input: &[u8]) -> PesPackets {
    PesPackets { remaining: input }
}

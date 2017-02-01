//! # MPEG-2 Packetized Elementary Streams (PES)
//!
//! These packets are nested inside the MPEG-2 Program Stream packets found
//! in a `*.sub` file.

use nom::IResult;

use super::clock::{Clock, clock};

/// Possible combinations of PTS and DTS data which might appear inside a
/// PES header.
///
/// See the [PES header documentation][PES] for details.
///
/// [PES]: http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PtsDtsFlags {
    /// No time stamps.
    None,
    /// Presentation Time Stamp only.
    Pts,
    /// Presentation and Decode Time Stamps.
    PtsDts,
}

impl PtsDtsFlags {
    /// How many bytes long should our header extension be?
    fn bytes_expected(self) -> usize {
        match self {
            PtsDtsFlags::None => 0,
            PtsDtsFlags::Pts => 5,
            PtsDtsFlags::PtsDts => 10,
        }
    }
}

/// Parse PTS & DTS flags in a PES packet header.  Consumes two bits.
named!(pts_dts_flags<(&[u8], usize), PtsDtsFlags>,
   alt!(value!(PtsDtsFlags::None,   tag_bits!(u8, 2, 0b00)) |
        value!(PtsDtsFlags::Pts,    tag_bits!(u8, 2, 0b10)) |
        value!(PtsDtsFlags::PtsDts, tag_bits!(u8, 2, 0b11)))
);

#[test]
fn parse_pts_dts_flags() {
    assert_eq!(pts_dts_flags((&[0b00][..], 6)),
               IResult::Done((&[][..], 0), PtsDtsFlags::None));
    assert_eq!(pts_dts_flags((&[0b10][..], 6)),
               IResult::Done((&[][..], 0), PtsDtsFlags::Pts));
    assert_eq!(pts_dts_flags((&[0b11][..], 6)),
               IResult::Done((&[][..], 0), PtsDtsFlags::PtsDts));
}

/// Presentation and Decode Time Stamps, if available.
#[derive(Debug, PartialEq, Eq)]
pub struct PtsDts {
    /// Presentation Time Stamp.
    pub pts: Clock,
    /// Decode Time Stamp.
    pub dts: Option<Clock>,
}

named!(pts_only<PtsDts>,
    bits!(
        do_parse!(
            tag_bits!(u8, 4, 0b0010) >>
            pts: call!(clock) >>
            (PtsDts { pts: pts, dts: None })
        )
    )
);

named!(pts_and_dts<PtsDts>,
    bits!(
        do_parse!(
            tag_bits!(u8, 4, 0b0011) >>
            pts: call!(clock) >>
            tag_bits!(u8, 4, 0b0001) >>
            dts: call!(clock) >>
            (PtsDts { pts: pts, dts: Some(dts) })
        )
    )
);

/// Parse a `PtsDts` value in the format specified by `flags`.
fn pts_dts(i: &[u8], flags: PtsDtsFlags) -> IResult<&[u8], Option<PtsDts>> {
    match flags {
        PtsDtsFlags::None => IResult::Done(i, None),
        PtsDtsFlags::Pts => pts_only(i).map(Some),
        PtsDtsFlags::PtsDts => pts_and_dts(i).map(Some),
    }
}

#[test]
fn parse_pts_dts() {
    assert_eq!(pts_dts(&[][..], PtsDtsFlags::None),
               IResult::Done(&[][..], None));
    assert_eq!(pts_dts(&[0x21, 0x00, 0xab, 0xe9, 0xc1][..], PtsDtsFlags::Pts),
               IResult::Done(&[][..],
                             Some(PtsDts {
                                 pts: Clock::base(2815200),
                                 dts: None,
                             })));
}

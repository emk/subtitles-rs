//! # Subtitle data parsing.
//!
//! For background, see [this documentation on the DVD subtitle format][subs].
//!
//! [subs]: http://sam.zoy.org/writings/dvd/subtitles/

use cast;
use nom::{be_u16, IResult};
use std::fmt;

use errors::*;
use idx;
use image::{ImageBuffer, Rgba, RgbaImage};
use img::{decompress, Size};
use mpeg2::ps;
use util::BytesFormatter;

/// Parse four 4-bit palette entries.
named!(palette_entries<[u8; 4]>, bits!(count_fixed!(u8, take_bits!(u8, 4), 4)));

#[test]
fn parse_palette_entries() {
    assert_eq!(palette_entries(&[0x03, 0x10][..]),
               IResult::Done(&[][..], [0x00, 0x03, 0x01, 0x00]));
}

/// Location at which to display the subtitle.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Coordinates {
    x1: u16,
    y1: u16,
    x2: u16,
    y2: u16,
}

impl Coordinates {
    /// The leftmost edge of the subtitle.
    pub fn left(&self) -> u16 {
        self.x1
    }

    /// The rightmost edge of the subtitle.
    pub fn top(&self) -> u16 {
        self.y1
    }

    /// The width of the subtitle.
    pub fn width(&self) -> u16 {
        self.x2 + 1 - self.x1
    }

    /// The height of the subtitle.
    pub fn height(&self) -> u16 {
        self.y2 + 1 - self.y1
    }

    /// The size of the subtitle.
    fn size(&self) -> Size {
        Size {
            w: cast::usize(self.width()),
            h: cast::usize(self.height()),
        }
    }
}

/// Parse a 12-bit coordinate value.
named!(coordinate<(&[u8], usize), u16>, take_bits!(u16, 12));

/// Parse four 12-bit coordinate values as a rectangle (with right and
/// bottom coordinates inclusive).
named!(coordinates<Coordinates>,
    bits!(
        do_parse!(
            x1: call!(coordinate) >>
            x2: call!(coordinate) >>
            y1: call!(coordinate) >>
            y2: call!(coordinate) >>
            (Coordinates {
                x1: x1,
                y1: y1,
                x2: x2,
                y2: y2,
            })
        )
    )
);

/// Parse a pair of 16-bit RLE offsets.
named!(rle_offsets<[u16; 2]>, bits!(count_fixed!(u16, take_bits!(u16, 16), 2)));

/// Individual commands which may appear in a control sequence.
#[derive(Clone, Debug, PartialEq, Eq)]
enum ControlCommand<'a> {
    /// Should this subtitle be displayed even if subtitles are turned off?
    Force,
    /// We should start displaying the subtitle at the `date` for this
    /// `ControlSequence`.
    StartDate,
    /// We should stop displaying the subtitle at the `date` for this
    /// `ControlSequence`.
    StopDate,
    /// Map each of the 4 colors in this subtitle to a 4-bit palette.
    Palette([u8; 4]),
    /// Map each of the 4 colors in this subtitle to 4 bits of alpha
    /// channel data.
    Alpha([u8; 4]),
    /// Coordinates at which to display the subtitle.
    Coordinates(Coordinates),
    /// Offsets of first and second scan line in our data buffer.  Note
    /// that the data buffer stores alternating scan lines separately, so
    /// these are the first line in each of the two chunks.
    RleOffsets([u16; 2]),
    /// Unsupported trailing data that we don't know how to parse.
    Unsupported(&'a [u8]),
}

/// Parse a single command in a control sequence.
named!(control_command<ControlCommand>,
    alt!(
        value!(ControlCommand::Force, tag!(&[0x00])) |
        value!(ControlCommand::StartDate, tag!(&[0x01])) |
        value!(ControlCommand::StopDate, tag!(&[0x02])) |
        map!(preceded!(tag!(&[0x03]), call!(palette_entries)),
             ControlCommand::Palette) |
        map!(preceded!(tag!(&[0x04]), call!(palette_entries)),
             ControlCommand::Alpha) |
        map!(preceded!(tag!(&[0x05]), call!(coordinates)),
             ControlCommand::Coordinates) |
        map!(preceded!(tag!(&[0x06]), call!(rle_offsets)),
             ControlCommand::RleOffsets) |
        // We only capture this so we have something to log.  Note that we
        // may not find the _true_ `ControlCommand::End` in this case, but
        // that doesn't matter, because we'll use the `next` field of
        // `ControlSequence` to find the next `ControlSequence`.
        map!(take_until!(&[0xff][..]), ControlCommand::Unsupported)
    )
);

/// The end of a control sequence.
named!(control_command_end, tag!(&[0xff]));

/// The control packet for a subtitle.
#[derive(Debug, Clone, PartialEq, Eq)]
struct ControlSequence<'a> {
    /// The time associated with this control sequence, specified in
    /// 1/100th of a second after the Presentation Time Stamp for this
    /// subtitle's packet.
    date: u16,
    /// The offset of the next control sequence, relative to ???.  If this
    /// equals the offset of the current control sequence, this is the last
    /// control packet.
    next: u16,
    /// Individual commands in this sequence.
    commands: Vec<ControlCommand<'a>>,
}

/// Parse a single control sequence.
named!(control_sequence<ControlSequence>,
    do_parse!(
        date: call!(be_u16) >>
        next: call!(be_u16) >>
        commands: many_till!(call!(control_command),
                             call!(control_command_end)) >>
        (ControlSequence {
            date: date,
            next: next,
            commands: commands.0,
        })
    )
);

#[test]
fn parse_control_sequence() {
    let input_1 = &[
        0x00, 0x00, 0x0f, 0x41,
        0x01,
        0x03, 0x03, 0x10,
        0x04, 0xff, 0xf0,
        0x05, 0x29, 0xb4, 0xe6, 0x3c, 0x54, 0x00,
        0x06, 0x00, 0x04, 0x07, 0x7b,
        0xff
    ][..];
    let expected_1 = ControlSequence {
        date: 0x0000,
        next: 0x0f41,
        commands: vec![
            ControlCommand::StartDate,
            ControlCommand::Palette([0x0, 0x3, 0x1, 0x0]),
            ControlCommand::Alpha([0xf, 0xf, 0xf, 0x0]),
            ControlCommand::Coordinates(Coordinates {
                x1: 0x29b,
                x2: 0x4e6,
                y1: 0x3c5,
                y2: 0x400,
            }),
            ControlCommand::RleOffsets([0x0004, 0x077b]),
        ]
    };
    assert_eq!(control_sequence(input_1),
               IResult::Done(&[][..], expected_1));

    let input_2 = &[
        0x00, 0x77, 0x0f, 0x41,
        0x02,
        0xff
    ][..];
    let expected_2 = ControlSequence {
        date: 0x0077,
        next: 0x0f41,
        commands: vec![ControlCommand::StopDate],
    };
    assert_eq!(control_sequence(input_2),
               IResult::Done(&[][..], expected_2));

    // An out of order example.
    let input_3 = &[
        0x00, 0x00, 0x0b, 0x30,
        0x01,
        0x00,
        // ...other commands would appear here...
        0xff,
    ][..];
    let expected_3 = ControlSequence {
        date: 0x0000,
        next: 0x0b30,
        commands: vec![
            ControlCommand::StartDate,
            ControlCommand::Force,
        ],
    };
    assert_eq!(control_sequence(input_3),
               IResult::Done(&[][..], expected_3));
}

/// A single subtitle.
#[derive(Clone)]
pub struct Subtitle {
    /// Start time of subtitle, in seconds.
    pub start_time: f64,
    /// End time of subtitle, in seconds.
    pub end_time: f64,
    /// Should this subtitle be shown even when subtitles are off?
    pub force: bool,
    /// Coordinates at which to display the subtitle.
    pub coordinates: Coordinates,
    /// Map each of the 4 colors in this subtitle to a 4-bit palette.
    pub palette: [u8; 4],
    /// Map each of the 4 colors in this subtitle to 4 bits of alpha
    /// channel data.
    pub alpha: [u8; 4],
    /// Our decompressed image, stored with 2 bits per byte in row-major
    /// order, that can be used as indices into `palette` and `alpha`.
    pub raw_image: Vec<u8>,
    /// A private placeholder for future extensibility.
    _placeholder: ()
}

impl Subtitle {
    /// Decompress to subtitle to an RBGA image.
    pub fn to_image(&self, palette: &idx::Palette) -> RgbaImage {
        let width = cast::u32(self.coordinates.width());
        let height = cast::u32(self.coordinates.height());
        ImageBuffer::from_fn(width, height, |x, y| {
            let offset = cast::usize(y*width + x);
            // We need to subtract the raw index from 3 to get the same
            // results as everybody else.  I found this by inspecting the
            // Handbrake subtitle decoding routines.
            let px = cast::usize(3-self.raw_image[offset]);
            let rgb = palette[cast::usize(self.palette[px])].data;
            let a = self.alpha[px];
            let aa = a << 4 | a;
            Rgba { data: [rgb[0], rgb[1], rgb[2], aa] }
        })
    }
}

impl<'a> fmt::Debug for Subtitle {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("Subtitle")
            .field("start_time", &self.start_time)
            .field("end_time", &self.end_time)
            .field("force", &self.force)
            .field("coordinates", &self.coordinates)
            .field("palette", &self.palette)
            .field("alpha", &self.alpha)
            .finish()
    }
}

/// Parse a single `u16` value from a buffer.  We don't use `nom` for this
/// because it has an inconvenient error type.
fn parse_be_u16_as_usize(buff: &[u8]) -> Result<(&[u8], usize)> {
    if buff.len() < 2 {
        Err("unexpected end of buffer while parsing 16-bit size".into())
    } else {
        Ok((&buff[2..], usize::from(buff[0]) << 8 | usize::from(buff[1])))
    }
}

/// Parse a subtitle.
fn subtitle(raw_data: &[u8], base_time: f64) -> Result<Subtitle> {
    // This parser is somewhat non-standard, because we need to work with
    // explicit offsets into `packet` in several places.

    // Figure out where our control data starts.
    let (_, initial_control_offset) = parse_be_u16_as_usize(&raw_data[2..])?;
    if initial_control_offset >= raw_data.len() {
        return Err(format!("control offset is 0x{:x}, but packet is only 0x{:x} \
                            bytes",
                           initial_control_offset,
                           raw_data.len()).into());
    }

    // Declare data we want to collect from our control packets.
    let mut start_time = None;
    let mut end_time = None;
    let mut force = false;
    let mut coordinates = None;
    let mut palette = None;
    let mut alpha = None;
    let mut rle_offsets = None;

    // Loop over the individual control sequences.
    let mut control_offset = initial_control_offset;
    loop {
        trace!("looking for control sequence at: 0x{:x}", control_offset);
        let control_data = &raw_data[control_offset..];
        match control_sequence(control_data) {
            IResult::Done(_, control) => {
                trace!("parsed control sequence: {:?}", &control);

                // Extract as much data as we can from this control sequence.
                let time = base_time + f64::from(control.date) / 100.0;
                for command in &control.commands {
                    match *command {
                        ControlCommand::Force => {
                            force = true;
                        }
                        ControlCommand::StartDate => {
                            start_time = start_time.or(Some(time));
                        }
                        ControlCommand::StopDate => {
                            end_time = end_time.or(Some(time));
                        }
                        ControlCommand::Palette(p) => {
                            palette = palette.or(Some(p));
                        }
                        ControlCommand::Alpha(a) => {
                            alpha = alpha.or(Some(a));
                        }
                        ControlCommand::Coordinates(ref c) => {
                            coordinates = coordinates.or(Some(c.clone()));
                        }
                        ControlCommand::RleOffsets(r) => {
                            rle_offsets = Some(r);
                        }
                        ControlCommand::Unsupported(b) => {
                            warn!("unsupported control sequence: {:?}",
                                  BytesFormatter(b));
                        }
                    }
                }

                // Figure out where to look for the next control sequence,
                // if any.
                let next_control_offset = cast::usize(control.next);
                if next_control_offset == control_offset {
                    // This points back at us, so we're the last packet.
                    break;
                } else if next_control_offset < control_offset {
                    return Err("control offset went backwards".into());
                } else {
                    control_offset = next_control_offset;
                }
            }
            IResult::Incomplete(_) => {
                return Err("incomplete control packet".into());
            }
            IResult::Error(err) => {
                return Err(format!("error parsing subtitle: {:?}", err).into());
            }
        }
    }

    // Make sure we found all the control commands that we expect.
    let start_time = start_time.ok_or_else(|| -> Error {
        "no start time for subtitle".into()
    })?;
    let end_time = end_time.ok_or_else(|| -> Error {
        "no end time for subtitle".into()
    })?;
    let coordinates = coordinates.ok_or_else(|| -> Error {
        "no coordinates for subtitle".into()
    })?;
    let palette = palette.ok_or_else(|| -> Error {
        "no palette for subtitle".into()
    })?;
    let alpha = alpha.ok_or_else(|| -> Error {
        "no alpha for subtitle".into()
    })?;
    let rle_offsets = rle_offsets.ok_or_else(|| -> Error {
        "no RLE offsets for subtitle".into()
    })?;

    // Decompress our image.
    //
    // We use `initial_control_offset+2`, because the second set of scan
    // lines is often overlapped with the first `[0x00, 0x00]` bytes of the
    // control block.
    let image = decompress(coordinates.size(),
                           [&raw_data[cast::usize(rle_offsets[0])..
                                      cast::usize(rle_offsets[1])],
                            &raw_data[cast::usize(rle_offsets[1])..
                                      cast::usize(initial_control_offset+2)]])?;

    // Return our parsed subtitle.
    let result = Subtitle {
        start_time: start_time,
        end_time: end_time,
        force: force,
        coordinates: coordinates,
        palette: palette,
        alpha: alpha,
        raw_image: image,
        _placeholder: (),
    };
    trace!("Parsed subtitle: {:?}", &result);
    Ok(result)
}

/// An iterator over subtitles.
pub struct Subtitles<'a> {
    pes_packets: ps::PesPackets<'a>,
}

/// Like `?` and `try!`, but assume that we're working with
/// `Option<Result<T, E>>` instead of `Result<T, E>`, and pass through
/// `None`.
macro_rules! try_iter {
    ($e:expr) => {
        match $e {
            None => return None,
            Some(Err(e)) => return Some(Err(From::from(e))),
            Some(Ok(value)) => value,
        }
    }
}

impl<'a> Iterator for Subtitles<'a> {
    type Item = Result<Subtitle>;

    fn next(&mut self) -> Option<Self::Item> {
        // Get the PES packet containing the first chunk of our subtitle.
        let first: ps::PesPacket = try_iter!(self.pes_packets.next());

        // Fetch useful information from our first packet.
        let pts_dts = match first.pes_packet.header_data.pts_dts {
            Some(v) => v,
            None => return Some(Err("found subtitle without timing into".into())),
        };
        let base_time = pts_dts.pts.to_seconds();
        let substream_id = first.pes_packet.substream_id;

        // Figure out how many total bytes we'll need to collect from one
        // or more PES packets, and collect the first chunk into a buffer.
        if first.pes_packet.data.len() < 2 {
            return Some(Err("packet is too short".into()));
        }
        let wanted = usize::from(first.pes_packet.data[0]) << 8
            | usize::from(first.pes_packet.data[1]);
        let mut sub_packet = first.pes_packet.data.to_owned();

        // Keep fetching more packets until we have enough.
        while sub_packet.len() < wanted {
            // Get the next PES packet in the Program Stream.
            let next: ps::PesPacket = try_iter!(self.pes_packets.next());

            // Make sure this is part of the same subtitle stream.  This is
            // mostly just paranoia; I don't expect this to happen.
            if next.pes_packet.substream_id != substream_id {
                warn!("Found subtitle for stream 0x{:x} while looking for 0x{:x}",
                      next.pes_packet.substream_id, substream_id);
                continue;
            }

            // Add the extra bytes to our buffer.
            sub_packet.extend_from_slice(next.pes_packet.data);
        }

        // Check to make sure we didn't get too _many_ bytes.  Again, this
        // is paranoia.
        if sub_packet.len() > wanted {
            warn!("Found 0x{:x} bytes of data in subtitle packet, wanted 0x{:x}",
                  sub_packet.len(), wanted);
            sub_packet.truncate(wanted);
        }

        // Parse our subtitle buffer.
        Some(subtitle(&sub_packet, base_time))
    }
}

/// Return an iterator over the subtitles in this data stream.
pub fn subtitles(input: &[u8]) -> Subtitles {
    Subtitles { pes_packets: ps::pes_packets(input) }
}

#[test]
fn parse_subtitles() {
    //use env_logger;
    use std::fs;
    use std::io::prelude::*;

    //let _ = env_logger::init();

    let mut f = fs::File::open("../fixtures/example.sub").unwrap();
    let mut buffer = vec![];
    f.read_to_end(&mut buffer).unwrap();
    let mut subs = subtitles(&buffer);
    let sub1 = subs.next().expect("missing sub 1").unwrap();
    assert!(sub1.start_time - 49.4 < 0.1);
    assert!(sub1.end_time - 50.9 < 0.1);
    assert_eq!(sub1.force, false);
    assert_eq!(sub1.coordinates,
               Coordinates { x1: 750, y1: 916, x2: 1172, y2: 966 });
    assert_eq!(sub1.palette, [0,3,1,0]);
    assert_eq!(sub1.alpha, [15,15,15,0]);
    subs.next().expect("missing sub 2").unwrap();
    assert!(subs.next().is_none());
}

//! Parse a file in `*.idx` format.

use cast;
use common_failures::prelude::*;
use image::Rgb;
use regex::Regex;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::Path;

use errors::{IResultExt, VobsubError};
use sub;

/// Parse a single hexadecimal digit.
named!(hex_digit<u8>,
    map!(one_of!(b"0123456789abcdefABCDEF"), |c: char| -> u8 {
        cast::u8(c.to_digit(16).unwrap()).unwrap()
    })
);

/// Parse a single byte hexadecimal byte.
named!(hex_u8<u8>,
    do_parse!(
        h1: call!(hex_digit) >>
        h2: call!(hex_digit) >>
        (h1 << 4 | h2)
    )
);

/// Parse a 3-byte hexadecimal RGB color.
named!(rgb<Rgb<u8>>,
    map!(count_fixed!(u8, call!(hex_u8), 3), |rgb| { Rgb { data: rgb } })
);

#[test]
fn parse_rgb() {
    use nom::IResult;
    assert_eq!(rgb(&b"1234ab"[..]),
               IResult::Done(&b""[..], Rgb::<u8> { data: [0x12, 0x34, 0xab] }));
}

/// The 16-color pallette used by the subtitles.
pub type Palette = [Rgb<u8>; 16];

named!(palette<Palette>,
    map_res!(separated_list!(tag!(b", "), call!(rgb)), |vec: Vec<Rgb<u8>>| {
        if vec.len() != 16 {
            return Err(format_err!("Palettes must have 16 entries"));
        }
        // Coerce vector to known-size slice.  Based on
        // http://stackoverflow.com/q/25428920/12089.
        let mut result = [Rgb { data: [0, 0, 0] }; 16];
        <[Rgb<u8>; 16] as AsMut<_>>::as_mut(&mut result)
            .clone_from_slice(&vec[0..16]);
        Ok(result)
    })
);

#[test]
fn parse_palette() {
    use nom::IResult;
    let input = b"\
000000, f0f0f0, cccccc, 999999, 3333fa, 1111bb, fa3333, bb1111, \
33fa33, 11bb11, fafa33, bbbb11, fa33fa, bb11bb, 33fafa, 11bbbb";
    assert_eq!(palette(input),
               IResult::Done(&[][..],
                             [Rgb { data: [0x00, 0x00, 0x00] },
                              Rgb { data: [0xf0, 0xf0, 0xf0] },
                              Rgb { data: [0xcc, 0xcc, 0xcc] },
                              Rgb { data: [0x99, 0x99, 0x99] },
                              Rgb { data: [0x33, 0x33, 0xfa] },
                              Rgb { data: [0x11, 0x11, 0xbb] },
                              Rgb { data: [0xfa, 0x33, 0x33] },
                              Rgb { data: [0xbb, 0x11, 0x11] },
                              Rgb { data: [0x33, 0xfa, 0x33] },
                              Rgb { data: [0x11, 0xbb, 0x11] },
                              Rgb { data: [0xfa, 0xfa, 0x33] },
                              Rgb { data: [0xbb, 0xbb, 0x11] },
                              Rgb { data: [0xfa, 0x33, 0xfa] },
                              Rgb { data: [0xbb, 0x11, 0xbb] },
                              Rgb { data: [0x33, 0xfa, 0xfa] },
                              Rgb { data: [0x11, 0xbb, 0xbb] }]));
}



/// A `*.idx` file describing the subtitles in a `*.sub` file.
#[derive(Debug)]
pub struct Index {
    // Frame size.
    //size: Size,
    /// The colors used for the subtitles.
    palette: Palette,
    /// Our compressed subtitle data.
    sub_data: Vec<u8>,
}

impl Index {
    /// Open an `*.idx` file and the associated `*.sub` file.
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Index> {
        lazy_static! {
            static ref KEY_VALUE: Regex =
                Regex::new("^([A-Za-z/ ]+): (.*)").unwrap();
        }

        let path = path.as_ref();
        let mut sub_path = path.to_owned();
        sub_path.set_extension("sub");

        let mkerr = || {
            format_err!("Could not parse {}", path.display())
        };

        let mut palette_val: Option<Palette> = None;

        let f = fs::File::open(path).with_context(|_| mkerr())?;
        let input = io::BufReader::new(f);

        for line in input.lines() {
            let line = line.with_context(|_| mkerr())?;
            if let Some(cap) = KEY_VALUE.captures(&line) {
                let key = cap.get(1).unwrap().as_str();
                let val = cap.get(2).unwrap().as_str();
                match key {
                    "palette" => {
                        palette_val =
                            Some(palette(val.as_bytes()).to_vobsub_result()?);
                    }
                    _ => trace!("Unimplemented idx key: {}", key),
                }
            }
        }

        let mut sub = fs::File::open(sub_path)?;
        let mut sub_data = vec![];
        sub.read_to_end(&mut sub_data)?;

        Ok(Index {
            palette: palette_val
                .ok_or_else(|| Error::from(VobsubError::MissingKey { key: "palette" }))
                .with_context(|_| mkerr())?,
            sub_data: sub_data,
        })
    }

    /// Get the palette associated with this `*.idx` file.
    pub fn palette(&self) -> &Palette {
        &self.palette
    }

    /// Iterate over the subtitles associated with this `*.idx` file.
    pub fn subtitles(&self) -> sub::Subtitles {
        sub::subtitles(&self.sub_data)
    }
}

#[test]
fn parse_index() {
    use env_logger;
    let _ = env_logger::init();

    let idx = Index::open("../fixtures/example.idx").unwrap();

    //assert_eq!(idx.size(), Size { w: 1920, h: 1080 });
    assert_eq!(idx.palette()[0], Rgb { data: [0x00, 0x00, 0x00] });
    assert_eq!(idx.palette()[15], Rgb { data: [0x11, 0xbb, 0xbb] });
}

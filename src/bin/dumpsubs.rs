// For error-chain.
#![recursion_limit = "1024"]

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate nom;
extern crate vobsub;

use nom::IResult;
use std::env;
use std::fmt;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::Path;
use std::process;

use vobsub::{Error, Result};
use vobsub::mpeg2::{Clock, ps};

/// Wrapper to force a `&[u8]` to display as nicely-formatted hexadecimal
/// bytes.
struct HexBytes<'a>(&'a [u8]);

impl<'a> fmt::Display for HexBytes<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let HexBytes(bytes) = *self;
        let mut first = true;
        for byte in bytes {
            if first {
                first = false;
                write!(f, "{:02x}", byte)?;
            } else {
                write!(f, " {:02x}", byte)?;
            }
        }
        Ok(())
    }
}


/// This is one of the packets that may be wrapped inside a Program Stream packet.
///
/// [pes]: http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
#[derive(Debug)]
struct PacketizedElementaryStreamHeader {
    scrambling_control: u8,
    priority: bool,
    data_alignment_indicator: bool,
    copyright: bool,
    original: bool,
    pts: Option<Clock>,
    dts: Option<Clock>,
    //escr: Option<Clock>,
    //es_rate: Option<u32>,
    //additional_copy_info: Option<u8>,
    //previous_crc: Option<u16>,
    // etc.
}


/*
fn pes_extension(input: (&[u8], usize), pts_dts_flags: u8, data_length: u8)
                 -> IResult<(&[u8], usize), (Option<Clock>, Option<Clock>)> {
    let (input_bytes, input_offset) = input;
    if input_offset != 0 {
        return IResult::Error(nom::ErrorKind::Custom(0x100));
    }


    match bytes!(input, data_length) {

        match pts_dts_flags {

        0b00 => IResult::Done((input, 0), (None, None)),
        0b10 & => {}
        0b11 => {}
        _ =>
    }
}
*/
/*
named!(packetized_elementary_stream_header<PacketizedElementaryStreamHeader>,
    do_parse!(
        // Sync bytes.
        tag!(&[0x00, 0x00, 0x01, 0xbd]) >>
        header: bits!(
            do_parse!(
                // PES packet length.
                packet_length: take_bits!(u16, 16) >>
                // PES extension start.
                tag_bits!(u8, 2, 0b10) >>
                scrambling_control: take_bits!(u8, 2) >>
                priority: take_bits!(u8, 1) >>
                data_alignment_indicator: take_bits!(u8, 1) >>
                copyright: take_bits!(u8, 1) >>
                original: take_bits!(u8, 1) >>
                pts_dts_flags: take_bits!(u8, 2) >>
                // Ignore these fields for now.
                //escr_flag: take_bits!(u8, 1) >>
                //es_rate_flag: take_bits!(u8, 1) >>
                //dsm_trick_mode_flag: take_bits!(u8, 1) >>
                //additional_copy_info_flag: take_bits!(u8, 1) >>
                //crc_flag: take_bits!(u8, 1) >>
                //extension_flag: take_bits!(u8, 1) >>
                take_bits!(u8, 6) >>
                header_data_length: take_bits!(u8, 8) >>

            )
     */

fn run() -> Result<()> {
    // Parse our command-line argument.
    let args = env::args().skip(1).collect::<Vec<_>>();
    if args.len() != 1 {
        writeln!(io::stderr(), "Please pass a single *.sub filename to dump").unwrap();
        process::exit(1);
    }
    let path = Path::new(&args[0]);

    let mut f: fs::File = fs::File::open(path)?;

    let mut ps_header = vec![0; 14];
    f.read_exact(&mut ps_header)?;
    println!("{}", HexBytes(&ps_header));

    println!("{}", ps::header(&ps_header).unwrap().1);

    Ok(())
}

// Use error-chain to declare a custom `main` function that calls `run` and
// prints any erors.
quick_main!(run);

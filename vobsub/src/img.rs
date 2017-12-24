//! Run-length encoded image format for subtitles.

use cast;
use common_failures::prelude::*;
use nom::IResult;
use safemem::write_bytes;

use util::BytesFormatter;

/// The dimensions of an image.
#[derive(Debug)]
pub struct Size {
    /// Width in pixels.
    pub w: usize,
    /// Height in pixels.
    pub h: usize,
}

/// A run-length encoded value.
#[derive(Debug)]
struct Rle {
    /// The number of times to repeat this value.  A value of 0 indicates that
    /// we should fill to the end of the line.
    cnt: u16,
    /// The value to repeat.  This is 2 bits wide.
    val: u8,
}

/// Parse the count for a `Rle`.
named!(count<(&[u8], usize), u16>,
    alt!(
        // Fill to end of line.
        value!(0, tag_bits!(u16, 14, 0)) |
        // Count for 4-nibble RLE.
        preceded!(tag_bits!(u8, 6, 0), take_bits!(u16, 8)) |
        // Count for 3-nibble RLE.
        preceded!(tag_bits!(u8, 4, 0), take_bits!(u16, 6)) |
        // Count for 2-nibble RLE.
        preceded!(tag_bits!(u8, 2, 0), take_bits!(u16, 4)) |
        // Count for 1-nibble RLE.
        take_bits!(u16, 2)
    )
);

/// Parse an `Rle`.
named!(rle<(&[u8], usize), Rle>,
    do_parse!(
        cnt: call!(count) >>
        val: take_bits!(u8, 2) >>
        (Rle { cnt: cnt, val: val })
    )
);

/// Decompress the scan-line `input` into `output`, returning the number of
/// input bytes consumed.
fn scan_line(input: &[u8], output: &mut [u8]) -> Result<usize> {
    trace!("scan line starting with {:?}", BytesFormatter(input));
    let width = output.len();
    let mut x = 0;
    let mut pos = (input, 0);
    while x < width {
        match rle(pos) {
            IResult::Done(new_pos, run) => {
                //trace!("RLE: {:?}", &run);
                pos = new_pos;
                let count = if run.cnt == 0 {
                    width - x
                } else {
                    cast::usize(run.cnt)
                };
                if x+count > output.len() {
                    return Err(format_err!("scan line is too long"));
                }
                write_bytes(&mut output[x..x+count], run.val);
                x += count;
            }
            IResult::Error(err) => {
                return Err(format_err!("error parsing subtitle scan line: {:?}",
                                       err));
            }
            IResult::Incomplete(needed) => {
                return Err(format_err!("not enough bytes parsing subtitle scan \
                                       line: {:?}",
                                       needed));
            }
        }
    }
    if x > width {
        return Err(format_err!("decoded scan line is too long"));
    }
    // Round up to the next full byte.
    if pos.1 > 0 {
        pos = (&pos.0[1..], 0);
    }
    Ok(input.len() - pos.0.len())
}

/// Decompress a run-length encoded image, and return a vector in row-major
/// order, starting at the upper-left and scanning right and down, with one
/// byte for each 2-bit value.
pub fn decompress(size: Size, data: [&[u8]; 2])
                  -> Result<Vec<u8>> {
    trace!("decompressing image {:?}, max: [0x{:x}, 0x{:x}]",
           &size, data[0].len(), data[1].len());
    let mut img = vec![0; size.w * size.h];
    let mut offsets = [0; 2];
    for y in 0..size.h {
        let odd = y % 2;
        trace!("line {:?}, offset 0x{:x}", y, offsets[odd]);
        let consumed = scan_line(&data[odd][offsets[odd]..],
                                 &mut img[y*size.w..(y+1)*size.w])?;
        offsets[odd] += consumed;
    }
    // TODO: Warn if we didn't consume everything.
    Ok(img)
}

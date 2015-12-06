//! We use `RangeReader` to read specific ranges of files and to write them
//! as HTTP responses.

use iron::{IronError, IronResult};
use iron::headers::{ByteRangeSpec, ContentRange, ContentRangeSpec, Range};
use iron::response::{ResponseBody, WriteBody};
use iron::status;
use std::cmp::min;
use std::io::{self, Read, Seek};
use std::fs;

use util::{iron_from_io, StringError};

// Convert a `ByteRangeSpec` and a file lenth to an actual byte range.
fn parse_range(range: &ByteRangeSpec, len: u64) -> IronResult<(u64, u64)> {
    // TODO: len == 0.

    // Note that these ranges are inclusive.
    let (from, to) = match range {
        &ByteRangeSpec::FromTo(from, to) => (from, to),
        &ByteRangeSpec::AllFrom(from) => (from, len-1),
        // TODO: Wrap error.
        &ByteRangeSpec::Last(from_end) => (len-from_end, len-1),
    };

    if from > to {
        let err = format!("Invalid range {}-{}", from, to);
        return Err(IronError::new(StringError::new(err), status::BadRequest));
    }
    if to >= len {
        // TODO: This is not actually an error.
        let err = format!("Range {}-{} extends beyond end of file at {}",
                          from, to, len);
        return Err(IronError::new(StringError::new(err),
                                  status::RangeNotSatisfiable));
    }
    Ok((from, to))
}

pub struct RangeReader {
    /// The file from which we're reading.
    file: fs::File,
    /// The range we want to read.
    range: ContentRange,
    // If this is None, it means we should read until the end.
    bytes_left: u64,
}

impl RangeReader {
    pub fn new(mut file: fs::File, range: Range) -> IronResult<RangeReader> {
        match range {
            Range::Bytes(ref ranges) if ranges.len() == 1 => {
                let len = try!(iron_from_io(file.metadata())).len();
                let (from, to) = try!(parse_range(&ranges[0], len));
                let content_range = ContentRange(ContentRangeSpec::Bytes {
                    range: Some((from, to)),
                    instance_length: Some(len),
                });

                try!(iron_from_io(file.seek(io::SeekFrom::Start(from))));

                Ok(RangeReader {
                    file: file,
                    range: content_range,
                    bytes_left: to + 1 - from,
                })
            }
            _ => {
                let err = "Only simple byte ranges are supported";
                Err(IronError::new(StringError::new(err), status::InternalServerError))
            }
        }
    }

    pub fn content_range(&self) -> ContentRange {
        self.range.clone()
    }
}

impl io::Read for RangeReader {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        // TODO: Handle 32-bit usize overflow.
        let to_read = min(buf.len(), self.bytes_left as usize);
        let read = try!(self.file.read(&mut buf[..to_read]));
        self.bytes_left -= read as u64;
        Ok(read)
    }
}

impl WriteBody for RangeReader {
    fn write_body(&mut self, res: &mut ResponseBody) -> io::Result<()> {
        io::copy(self, res).map(|_| ())
    }
}

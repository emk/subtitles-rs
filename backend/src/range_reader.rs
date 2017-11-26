//! We use `RangeReader` to read specific ranges of files and to write them
//! as HTTP responses.

use iron::IronResult;
use iron::headers::{ByteRangeSpec, ContentRange, ContentRangeSpec, Range};
use iron::response::{ResponseBody, WriteBody};
use iron::status;
use std::cmp::min;
use std::io::{self, Read, Seek};
use std::fs;

use util::{iron_err, iron_from_io};

// Convert a `ByteRangeSpec` and a file lenth to an actual byte range.
fn parse_range(range: &ByteRangeSpec, len: u64) -> IronResult<(u64, u64)> {
    // Eliminate this case early so we can write `len-1` below.
    if len == 0 {
        iron_err!(status::RangeNotSatisfiable,
                  "Tried to get a range of bytes from the empty file");
    }

    // Note that these ranges are inclusive.
    let (from, to) = match range {
        &ByteRangeSpec::FromTo(from, to) => (from, min(to, len-1)),
        &ByteRangeSpec::AllFrom(from) => (from, len-1),
        &ByteRangeSpec::Last(from_end) if len >= from_end =>
            (len-from_end, len-1),
        &ByteRangeSpec::Last(_) =>
            (0, len-1),
    };

    if from > len-1 {
        iron_err!(status::RangeNotSatisfiable,
                  "Range {}-{} starts beyond end of file at {}",
                  from, to, len);
    } else if from > to {
        iron_err!(status::BadRequest, "Invalid range {}-{}", from, to);
    }
    Ok((from, to))
}

#[test]
fn test_parse_range() {
    use iron::headers::ByteRangeSpec::*;

    assert_eq!((5, 9), parse_range(&FromTo(5, 9), 10).unwrap());
    assert_eq!((5, 9), parse_range(&AllFrom(5), 10).unwrap());
    assert_eq!((6, 9), parse_range(&Last(4), 10).unwrap());

    assert!(parse_range(&FromTo(2, 1), 10).is_err());
    assert!(parse_range(&FromTo(0, 0), 0).is_err());
    assert!(parse_range(&FromTo(10, 10), 10).is_err());

    assert_eq!((5, 9), parse_range(&FromTo(5, 15), 10).unwrap());
    assert_eq!((0, 9), parse_range(&Last(20), 10).unwrap());
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
                iron_err!(status::InternalServerError,
                          "Only simple byte ranges are supported");
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

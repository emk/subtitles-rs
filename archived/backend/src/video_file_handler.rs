use iron::IronResult;
use iron::headers::{AcceptRanges, ByteRangeSpec, Range, RangeUnit};
use iron::middleware::Handler;
use iron::request::Request;
use iron::response::{Response, WriteBody};
use iron::status;
use rustless::Nesting;
use std::fs;
use std::path::{Path, PathBuf};

use range_reader::RangeReader;
use util::iron_from_io;

pub struct VideoFileHandler {
    path: PathBuf,
}

impl VideoFileHandler {
    pub fn new(path: &Path) -> VideoFileHandler {
        VideoFileHandler { path: path.to_owned() }
    }
}

impl Handler for VideoFileHandler {
    fn handle(&self, req: &mut Request) -> IronResult<Response> {
        let range: Range = req.headers.get().cloned().unwrap_or_else(|| {
            Range::Bytes(vec!(ByteRangeSpec::AllFrom(0)))
        });

        let file = try!(iron_from_io(fs::File::open(&self.path)));
        let reader = try!(RangeReader::new(file, range));
        let content_range = reader.content_range();
        let wb: Box<WriteBody+Send> = Box::new(reader);
        let mut response = Response::with((status::PartialContent, wb));
        response.headers.set(AcceptRanges(vec![RangeUnit::Bytes]));
        response.headers.set(content_range);
        Ok(response)
    }
}

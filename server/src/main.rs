extern crate iron;
extern crate mount;
extern crate rustc_serialize;
extern crate rustless;
extern crate substudy;
extern crate staticfile;

use iron::IronResult;
use iron::headers::{AcceptRanges, ByteRangeSpec, Range, RangeUnit};
use iron::middleware::Handler;
use iron::request::Request;
use iron::response::{Response, WriteBody};
use iron::status;
use mount::Mount;
use rustc_serialize::json;
use rustless::Nesting;
use staticfile::Static;
use std::fs;
use std::path::{Path, PathBuf};

use range_reader::RangeReader;
use util::iron_from_io;

#[macro_use]
mod util;
mod range_reader;

#[derive(RustcEncodable)]
struct Video {
    url: String,
}

struct VideoFile {
    path: PathBuf,
}

impl VideoFile {
    fn new(path: &Path) -> VideoFile {
        VideoFile { path: path.to_owned() }
    }
}

impl Handler for VideoFile {
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

fn main() {
    let app = rustless::Application::new(rustless::Api::build(|api| {
        api.version("v1", rustless::Versioning::Path);

        api.get("video.json", |endpoint| {
            endpoint.handle(|mut client, _params| {
                let resp = Video {
                    url: "/video.mp4".to_owned(),
                };
                client.set_json_content_type();
                client.text(json::encode(&resp).unwrap())
            })
        });
    }));

    let mut mount = Mount::new();
    mount.mount("/", Static::new(Path::new("assets/")));
    mount.mount("/video.mp4", VideoFile::new(Path::new("assets/video.mp4")));
    mount.mount("/api", app);

    println!("Running on http://localhost:4000/");
    iron::Iron::new(mount).http("localhost:4000").unwrap();
}

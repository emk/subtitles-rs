extern crate docopt;
extern crate env_logger;
extern crate iron;
extern crate mount;
extern crate rustc_serialize;
extern crate rustless;
extern crate substudy;
extern crate staticfile;

use docopt::Docopt;
use iron::headers::ContentType;
use iron::middleware::Handler;
use mount::Mount;
use rustc_serialize::json;
use rustless::Nesting;
use staticfile::Static;
use std::path::Path;
use std::process::exit;

use video_file_handler::VideoFileHandler;

#[macro_use]
mod util;
mod range_reader;
mod models;
mod video_file_handler;

const USAGE: &'static str = "
Web interface for working with subtitled video

Usage: substudy-server <video> <foreign-subs> [<native-subs>]
       substudy-server --help

For now, all subtitles must be in *.srt format, and video must be in
a browser-compatible format.
";

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_video: String,
    arg_foreign_subs: String,
    arg_native_subs: Option<String>,
}

fn run(args: &Args) -> substudy::err::Result<()> {
    let foreign_path = Path::new(&args.arg_foreign_subs);
    let native_path = args.arg_native_subs.as_ref().map(|s| Path::new(s));
    let video = try!(models::Video::new("/video.mp4", &foreign_path,
                                        native_path));

    let app = rustless::Application::new(rustless::Api::build(|api| {
        api.version("v1", rustless::Versioning::Path);

        api.get("video.json", |endpoint| {
            endpoint.handle(move |mut client, _params| {
                client.set_header(ContentType::json());
                client.text(json::encode(&video).unwrap())
            })
        });
    }));

    let mut mount = Mount::new();
    mount.mount("/", Static::new(Path::new("assets/")));
    mount.mount("/video.mp4",
                VideoFileHandler::new(Path::new(&args.arg_video)));
    mount.mount("/api", app);

    println!("Running on http://localhost:4000/");
    try!(iron::Iron::new(mount).http("localhost:4000"));

    // Shouldn't actually be reached.
    Ok(())
}

fn main() {
    env_logger::init().unwrap();

    // Parse our command-line arguments using docopt.
    let args: Args = Docopt::new(USAGE)
        .and_then(|d| d.decode())
        .unwrap_or_else(|e| e.exit());

    if let Err(ref err) = run(&args) {
        println!("error: {}", err);
        exit(1);
    }
}

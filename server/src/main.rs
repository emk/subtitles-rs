extern crate iron;
extern crate mount;
extern crate rustc_serialize;
extern crate rustless;
extern crate substudy;
extern crate staticfile;

use iron::middleware::Handler;
use mount::Mount;
use rustc_serialize::json;
use rustless::Nesting;
use staticfile::Static;
use std::path::Path;

use video_file_handler::VideoFileHandler;

#[macro_use]
mod util;
mod range_reader;
mod models;
mod video_file_handler;

fn main() {
    let app = rustless::Application::new(rustless::Api::build(|api| {
        api.version("v1", rustless::Versioning::Path);

        api.get("video.json", |endpoint| {
            endpoint.handle(|mut client, _params| {
                let resp = models::Video {
                    url: "/video.mp4".to_owned(),
                    subtitles: vec!(),
                };
                client.set_json_content_type();
                client.text(json::encode(&resp).unwrap())
            })
        });
    }));

    let mut mount = Mount::new();
    mount.mount("/", Static::new(Path::new("assets/")));
    mount.mount("/video.mp4",
                VideoFileHandler::new(Path::new("assets/video.mp4")));
    mount.mount("/api", app);

    println!("Running on http://localhost:4000/");
    iron::Iron::new(mount).http("localhost:4000").unwrap();
}

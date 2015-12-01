extern crate iron;
extern crate mount;
extern crate rustc_serialize;
extern crate rustless;
extern crate substudy;
extern crate staticfile;

use mount::Mount;
use rustc_serialize::json;
use rustless::Nesting;
use staticfile::Static;
use std::path::Path;

#[derive(RustcEncodable)]
struct SampleResponse {
    message: String,
}

fn main() {
    let app = rustless::Application::new(rustless::Api::build(|api| {
        api.version("v1", rustless::Versioning::Path);

        api.get("hello", |endpoint| {
            endpoint.handle(|mut client, _params| {
                let resp = SampleResponse { message: "Hello!".to_owned() };
                client.set_json_content_type();
                client.text(json::encode(&resp).unwrap())
            })
        });
    }));

    let mut mount = Mount::new();
    mount.mount("/", Static::new(Path::new("assets/")));
    mount.mount("/api", app);

    println!("Running on http://localhost:4000/");
    iron::Iron::new(mount).http("localhost:4000").unwrap();
}

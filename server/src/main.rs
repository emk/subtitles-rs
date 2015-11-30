extern crate rustc_serialize;
#[macro_use]
extern crate rustful;

use rustc_serialize::json;
use rustful::{Context, Handler, Response, StatusCode, Server, TreeRouter};
use rustful::header::ContentType;

enum Api {
    NotFound,
    Hello,
}

#[derive(RustcEncodable)]
struct SampleResponse {
    message: String,
}

impl Handler for Api {
    fn handle_request(&self, _: Context, mut response: Response) {
        match self {
            &Api::NotFound => {
                response.set_status(StatusCode::NotFound);
                response.send("Endpoint not found.")
            }
            &Api::Hello => {
                let data = SampleResponse { message: "Hello!".to_owned() };
                match json::encode(&data) {
                    Ok(json) => {
                        response.headers_mut().set(ContentType::json());
                        response.send(json)
                    }
                    Err(_) => {
                        response.set_status(StatusCode::InternalServerError);
                        response.send("internal error")
                    }
                }
            }
        }
    }
}

fn main() {
    let router = insert_routes! {
        TreeRouter::new() => {
            "api" => {
                "v1" => {
                    "hello" => {
                        Get: Api::Hello,
                    },
                },
            },
        }
    };


    println!("Running on http://localhost:8080/");
    Server {
        handlers: router,
        fallback_handler: Some(Api::NotFound),
        host: 8080.into(),
        ..Server::default()
    }.run().unwrap();
}

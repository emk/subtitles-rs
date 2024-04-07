#![allow(non_snake_case, unused)]
use std::ops::Deref;

use dioxus::fullstack::prelude::*;
use dioxus::prelude::*;
use serde::{Deserialize, Serialize};

mod deck;
mod html;

#[cfg(feature = "server")]
mod server_setup {
    use std::{
        env,
        error::Error,
        fs::File,
        io::{BufRead as _, BufReader},
        path::PathBuf,
        sync::RwLock,
    };

    use super::*;

    /// Aligned subtitles to serve.
    pub static ALIGNED_SUBS: RwLock<Vec<Aligned>> = RwLock::new(Vec::new());

    /// Parse our arguments and load the aligned subtitles.
    pub fn parse_arguments() -> anyhow::Result<()> {
        dotenv::dotenv().ok();

        let aligned_jsonl_path = env::var("ALIGNED_JSONL_PATH")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("aligned.jsonl"));

        let file = File::open(&aligned_jsonl_path)?;
        let reader = BufReader::new(file);

        let mut aligned_subs = ALIGNED_SUBS.write().expect("lock poisoned");
        for line in reader.lines() {
            let line = line?;
            let aligned: Aligned = serde_json::from_str(&line)?;
            aligned_subs.push(aligned);
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Aligned {
    #[serde(rename = "f")]
    pub foreign: String,
    #[serde(rename = "n")]
    pub native: String,
}

#[cfg(feature = "server")]
fn main() {
    tracing_subscriber::fmt::init();
    server_setup::parse_arguments().unwrap();
    launch(app);
}

#[cfg(feature = "web")]
fn main() {
    tracing_wasm::set_as_global_default();
    launch(app);
}

#[server]
pub async fn aligned_subtitles() -> Result<Vec<Aligned>, ServerFnError> {
    let aligned_subs = server_setup::ALIGNED_SUBS.read().expect("lock poisoned");
    Ok(aligned_subs.iter().take(200).cloned().collect())
}

fn app() -> Element {
    let aligned = use_resource(aligned_subtitles);
    match aligned() {
        None => rsx! { p { "Loading..." } },
        Some(Err(err)) => rsx! { p { { format!("Error: {}", err.to_string()) } } },
        Some(Ok(aligned)) => rsx! { BookViewer { aligned } },
    }
}

#[component]
fn BookViewer(aligned: Vec<Aligned>) -> Element {
    rsx! {
        div {
            class: "max-w-5xl h-full v-full mx-auto flex flex-row space-x-2",
            AlignedSentences { aligned }
            Explanations { }
        }
    }
}

#[component]
fn AlignedSentences(aligned: Vec<Aligned>) -> Element {
    rsx! {
        div {
            class: "flex-auto w-3/4 overflow-y-scroll p-4 bg-slate-50 scroll-smooth",
            for aligned in aligned {
                div {
                    class: "flex gap-4 hover:bg-slate-100",
                    div {
                        class: "w-1/2",
                        { aligned.foreign }
                    }
                    div {
                        class: "w-1/2 text-gray-400",
                        { aligned.native }
                    }
                }
            }
        }
    }
}

#[component]
fn AlignedSentence(aligned: Aligned) -> Element {
    rsx! {
        div {
            class: "flex",
            div {
                class: "w-1/2",
                { aligned.foreign }
            }
            div {
                class: "w-1/2",
                { aligned.native }
            }
        }
    }
}

#[component]
fn Explanations() -> Element {
    rsx! {
        div {
            class: "flex-auto w-1/4 v-full p-4 bg-slate-100",
            p { "Explanations" }
        }
    }
}

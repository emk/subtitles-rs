#![allow(non_snake_case, unused)]
use std::ops::Deref;

use dioxus::fullstack::prelude::*;
use dioxus::prelude::*;

mod deck;
mod html;

fn main() {
    let config = LaunchBuilder::new();

    #[cfg(feature = "ssr")]
    let config = config.incremental(
        IncrementalRendererConfig::default()
            .invalidate_after(std::time::Duration::from_secs(120)),
    );

    config.launch(app)
}

fn app() -> Element {
    rsx! {
        div {
            class: "p-1 max-w-lg h-full mx-auto space-y-1 flex flex-col",
            Card {}
            Buttons {}
        }
    }
}

/// A card to review.
fn Card() -> Element {
    rsx! {
        div {
            class: "flex-auto w-full p-4 bg-white shadow-lg rounded-lg prose prose-xl prose-hr:my-4",

            p { "Text on the front of the card" }
            hr {}
            p { "Text on the back of the card" }
        }
    }
}

fn Buttons() -> Element {
    rsx! {
        div {
            class: "flex-initial w-full flex space-x-1",
            AnswerButton { label: "Ugh" }
            AnswerButton { label: "More", days: 4 }
            AnswerButton { label: "Got it!", days: 6 }
            AnswerButton { label: "Less", days: 10 }
        }
    }
}

#[component]
fn AnswerButton(label: String, days: Option<u16>) -> Element {
    rsx! {
        button {
            class: "flex-auto p-4 bg-white shadow-lg rounded-lg",
            "{label}"
            if let Some(days) = days {
                br {}
                "{days} days"
            }
        }
    }
}

#[server]
async fn add_server(x: i32, y: i32) -> Result<i32, ServerFnError> {
    Ok(x + y)
}

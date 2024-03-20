//! Anki templates.

use serde::Serialize;

use super::connect::{CardTemplate, CreateModelRequest};

/// An Anki audio note's data.
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "PascalCase")]
pub(crate) struct AudioNote {
    pub(crate) sound: String,
    pub(crate) time: String,
    pub(crate) source: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) image: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) foreign_curr: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) native_curr: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) foreign_prev: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) native_prev: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) foreign_next: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) native_next: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) hint: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub(crate) notes: Option<String>,
}

/// Parameters for our Anki audio card model.
pub(crate) static AUDIO_MODEL: CreateModelRequest = CreateModelRequest {
    model_name: "Substudy Audio",
    in_order_fields: &[
        "Sound",
        "Time",
        "Source",
        "Image",
        "ForeignCurr",
        "NativeCurr",
        "ForeignPrev",
        "NativePrev",
        "ForeignNext",
        "NativeNext",
        "Hint",
        "Notes",
    ],
    css: include_str!("audio/style.css"),
    is_cloze: false,
    card_templates: &[CardTemplate {
        name: "Audio",
        front: include_str!("audio/front.html"),
        back: include_str!("audio/back.html"),
    }],
};

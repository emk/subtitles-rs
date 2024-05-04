//! Interface to Anki-Connect.
//!
//! See also [`AnkiBridge`](https://gitlab.com/kerkmann/anki_bridge), if we ever
//! want to replace this with a library.

use std::fmt;

use anyhow::{anyhow, Context as _};
use log::trace;
use reqwest::Client;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_json::Value;

use crate::Result;

/// The version of Anki-Connect we expect to talk to.
const ANKI_CONNECT_VERSION: u16 = 6;

/// A request to Anki.
pub trait AnkiRequestParams: fmt::Debug + Serialize {
    /// The name of the action to perform.
    const ACTION: &'static str;

    /// Our response type.
    type Response: fmt::Debug + DeserializeOwned;

    /// Does this request actually have no parameters?
    fn omit_params(&self) -> bool {
        false
    }
}

impl<T> AnkiRequestParams for &T
where
    T: AnkiRequestParams,
{
    const ACTION: &'static str = T::ACTION;
    type Response = T::Response;

    fn omit_params(&self) -> bool {
        (*self).omit_params()
    }
}

/// A request to Anki.
#[derive(Serialize)]
struct AnkiRequest<R: AnkiRequestParams>
where
    R: AnkiRequestParams,
{
    action: &'static str,
    version: u16,
    #[serde(skip_serializing_if = "R::omit_params")]
    params: R,
}

impl<R> AnkiRequest<R>
where
    R: AnkiRequestParams,
{
    /// Create a new Anki request.
    fn new(params: R) -> Self {
        Self {
            action: R::ACTION,
            version: ANKI_CONNECT_VERSION,
            params,
        }
    }
}

/// A response from Anki.
#[derive(Debug, Deserialize)]
struct AnkiResponse<T> {
    result: Option<T>,
    error: Option<String>,
}

pub(crate) struct AnkiConnect {
    client: Client,
}

impl AnkiConnect {
    pub(crate) fn new() -> Self {
        Self {
            client: Client::new(),
        }
    }

    pub(crate) async fn request<P>(&self, params: P) -> Result<P::Response>
    where
        P: AnkiRequestParams,
    {
        trace!("Anki-Connect request: {:?}", params);
        let request = AnkiRequest::new(params);
        let response = self
            .client
            .post("http://localhost:8765")
            .json(&request)
            .send()
            .await
            .context("Could not connect to Anki-Connect")?;
        let response = response
            .json::<AnkiResponse<P::Response>>()
            .await
            .context("Could not parse Anki-Connect response")?;
        trace!("Anki-Connect response: {:?}", response);
        match response {
            AnkiResponse {
                result: None,
                error: Some(error),
            } => Err(anyhow!("Anki-Connect error: {}", error)),
            AnkiResponse {
                result: Some(result),
                error: None,
            } => Ok(result),
            _ => Err(anyhow!("Anki-Connect response is missing result")),
        }
    }
}

/// An Anki version request.
#[derive(Debug, Serialize)]
pub(crate) struct VersionRequest;

impl AnkiRequestParams for VersionRequest {
    const ACTION: &'static str = "version";
    type Response = u16;

    fn omit_params(&self) -> bool {
        true
    }
}

/// Get all deck names.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct DeckNamesRequest;

impl AnkiRequestParams for DeckNamesRequest {
    const ACTION: &'static str = "deckNames";
    type Response = Vec<String>;

    fn omit_params(&self) -> bool {
        true
    }
}

/// Get all model names.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ModelNamesRequest;

impl AnkiRequestParams for ModelNamesRequest {
    const ACTION: &'static str = "modelNames";
    type Response = Vec<String>;

    fn omit_params(&self) -> bool {
        true
    }
}

/// An Anki model, describing the data that can go in an Anki note,
/// and how to turn it into cards.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateModelRequest {
    pub model_name: &'static str,
    pub in_order_fields: &'static [&'static str],
    pub css: &'static str,
    pub is_cloze: bool,
    pub card_templates: &'static [CardTemplate],
}

impl AnkiRequestParams for CreateModelRequest {
    const ACTION: &'static str = "createModel";
    /// Honestly we don't care about the response here for now.
    type Response = Value;
}

/// An Anki card template, describing the front and back of a card.
#[derive(Debug, Serialize)]
#[serde(rename_all = "PascalCase")]
pub struct CardTemplate {
    pub name: &'static str,
    pub front: &'static str,
    pub back: &'static str,
}

/// Can we add the following notes to Anki?
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CanAddNotesWithErrorDetailRequest<Fields> {
    pub(crate) notes: Vec<Note<Fields>>,
}

impl<Fields> AnkiRequestParams for CanAddNotesWithErrorDetailRequest<Fields>
where
    Fields: Serialize + fmt::Debug,
{
    const ACTION: &'static str = "canAddNotesWithErrorDetail";
    type Response = Vec<AddNoteErrorDetail>;
}

/// An error detail for a note that can't be added to Anki.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct AddNoteErrorDetail {
    pub(crate) can_add: bool,
    pub(crate) error: Option<String>,
}

/// Adding notes to Anki.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct AddNotesRequest<Fields> {
    pub(crate) notes: Vec<Note<Fields>>,
}

impl<Fields> AnkiRequestParams for AddNotesRequest<Fields>
where
    Fields: Serialize + fmt::Debug,
{
    const ACTION: &'static str = "addNotes";
    type Response = Vec<Option<u64>>;
}

/// An Anki note's data, which can be used to create multiple cards.
/// Note that we handle media files separately, because that's closer
/// to how we thought about them when we jhu
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Note<Fields> {
    pub(crate) deck_name: String,
    pub(crate) model_name: String,
    pub(crate) fields: Fields,
    pub(crate) options: NoteOptions,
    pub(crate) tags: Vec<String>,
}

/// Options for an Anki note.
#[derive(Clone, Debug, Default, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct NoteOptions {
    pub(crate) allow_duplicate: bool,
}

/// Store a media file in Anki.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct StoreMediaFileRequest {
    pub(crate) filename: String,
    /// Note that this would normally be a `PathBuf`, but we need to send it as
    /// UTF-8, so we can't accept any characters that won't work in a `String`.
    pub(crate) path: String,
}

impl AnkiRequestParams for StoreMediaFileRequest {
    const ACTION: &'static str = "storeMediaFile";
    type Response = String;
}

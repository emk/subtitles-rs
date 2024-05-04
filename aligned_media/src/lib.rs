//! # Aligned media and text format for language-learning software
//!
//! This is a Rust implementation of the [aligned media specification][spec]
//! for language-learning software.
//!
//! [spec]: https://github.com/language-learners/aligned-media-spec

#![warn(missing_docs)]

use relative_path::{RelativePath, RelativePathBuf};
use serde::de::Error as DeError;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::hash::Hash;
use std::path::Path;
use std::result;
use thiserror::Error;
use uuid::Uuid;

pub mod html;

/// Our standard result type.
pub type Result<T, E = Error> = result::Result<T, E>;

/// Errors which can be returned by this crate.
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum Error {
    /// We could not parse the specified HTML.
    #[error("could not parse HTML {html:?}")]
    #[non_exhaustive]
    CouldNotParseHtml {
        /// The HTML that we could not parse.
        html: String,

        /// The underlying error.
        source: Box<dyn std::error::Error + Send + Sync + 'static>,
    },

    /// We could not parse the input data.
    #[error("could not parse metadata")]
    #[non_exhaustive]
    CouldNotParseMetadata {
        /// The underlying error.
        source: Box<dyn std::error::Error + Send + Sync + 'static>,
    },

    /// We could not serialize the metadata.
    #[error("could not serialize metadata")]
    #[non_exhaustive]
    CouldNotSerializeMetadata {
        /// The underlying error.
        source: Box<dyn std::error::Error + Send + Sync + 'static>,
    },

    /// We encountered an unsupported HTML attribute.
    #[error("the HTML attribute {name:?} is not allowed")]
    #[non_exhaustive]
    HtmlAttributeForbidden {
        /// The name of the forbidden attribute.
        name: String,
    },

    /// We encountered an unsupported HTML element.
    #[error("the HTML element {name:?} is not allowed")]
    #[non_exhaustive]
    HtmlElementForbidden {
        /// The name of the forbidden element.
        name: String,
    },

    /// We encountered an unsupported HTML entity.
    #[error("the HTML entity {name:?} is not allowed")]
    #[non_exhaustive]
    HtmlEntityForbidden {
        /// The name of the forbidden entity.
        name: String,
    },

    /// We encountered an invalid path.
    #[error("path {path:?} is not allowed")]
    #[non_exhaustive]
    InvalidPath {
        /// The invalid path.
        path: String,
    },

    /// We encountered an invalid span.
    #[error("beginning of time span {begin},{end} is greater than end")]
    #[non_exhaustive]
    InvalidSpan {
        /// The beginning of the invalid span.
        begin: f32,
        /// The end of the invalid span.
        end: f32,
    },

    /// We encountered an unknown track type that didn't begin with "x-".
    #[error(
        "unsupported track type {value:?} (did you want to prefix it with \"x-\"?)"
    )]
    #[non_exhaustive]
    UnsupportedTrackType {
        /// The unknown track type.
        value: String,
    },
}

/// A unique identifier for a track, within the context of a file.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(transparent)]
pub struct TrackId(pub String);

/// A single media file, typically an episode of a TV series, a film, an chapter
/// of an audiobook. It might also be something more exotic, like a PDF of a
/// graphic novel.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[cfg_attr(feature = "no_forwards_compatibility", serde(deny_unknown_fields))]
#[non_exhaustive]
pub struct Metadata {
    /// Authors, etc., of this work. This may be used to group works by the same
    /// creator together.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub creators: Vec<String>,

    /// Information about how this work fits into a series. This may be used to
    /// group related works together, such as episodes of a TV series, books in a
    /// series, or songs in an album.
    pub series: Option<SeriesMetadata>,

    /// The title of a book, a TV episode, or a song, etc.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,

    /// The year in which this work was published.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub year: Option<i32>,

    /// Tracks associated with this file.
    ///
    pub tracks: HashMap<TrackId, Track>,

    /// The primary media track for this `MediaFile`.
    ///
    /// This is used as the "time base" for all `Alignment`s.
    pub base_track_id: TrackId,

    /// Tags. These may be used by the user or application to categorize the
    /// media file.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub tags: Vec<String>,

    /// Application-specific extension data.
    #[serde(default, skip_serializing_if = "ExtensionData::is_empty")]
    pub ext: ExtensionData,

    /// A list of synchronized sentences, subtitles, or other linguistic
    /// content.
    pub alignments: Vec<AlignmentOrGroup>,

    /// Unknown fields.
    #[serde(flatten)]
    pub unknown: HashMap<String, serde_json::Value>,
}

impl Metadata {
    /// Parse `metadata.json` represented as raw bytes. This will be interpreted
    /// as UTF-8, because the format is strict.
    pub fn from_bytes(data: &[u8]) -> Result<Metadata> {
        Ok(serde_json::from_slice(data).map_err(|err| {
            Error::CouldNotParseMetadata {
                source: Box::new(err),
            }
        })?)
    }

    /// Parse `metadata.json` represented as a UTF-8 Rust string.
    pub fn from_str(data: &str) -> Result<Metadata> {
        Self::from_bytes(data.as_bytes())
    }

    /// Convert this metadata to a JSON string.
    pub fn to_string(&self) -> Result<String> {
        serde_json::to_string_pretty(self).map_err(|err| {
            Error::CouldNotSerializeMetadata {
                source: Box::new(err),
            }
        })
    }
}

#[test]
fn parse_metadata() {
    let examples = &[
        include_str!("../fixtures/examples/book_example.aligned/metadata.json"),
        include_str!("../fixtures/examples/subtitle_example.aligned/metadata.json"),
        include_str!(
            "../fixtures/examples/subtitle_extracted_example.aligned/metadata.json"
        ),
    ];
    for example in examples {
        Metadata::from_str(example).expect("failed to parse example metadata");
    }
}

/// Information about how a media file fits into a series.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[cfg_attr(feature = "no_forwards_compatibility", serde(deny_unknown_fields))]
#[non_exhaustive]
pub struct SeriesMetadata {
    /// The title of this series. This might be "The Lord of the Rings" for a series
    /// of books, "Avatar: La Leyenda de Aang" for a TV series, or "Abbey Road" for
    /// an album.
    series_title: String,
    /// The position of this work in the series. This might be a book number, an
    /// episode number, or a track number.
    index_in_series: Option<u32>,

    /// Unknown fields.
    #[serde(flatten)]
    pub unknown: HashMap<String, serde_json::Value>,
}

/// An individual "track" of context. This might be a single subtitle in a
/// single language, or a still image taken from a video
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[cfg_attr(feature = "no_forwards_compatibility", serde(deny_unknown_fields))]
#[non_exhaustive]
pub struct Track {
    /// The origin of this track.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub origin: Option<TrackOrigin>,

    /// If this track was derived from another track, this should contain the
    /// `id` of the original track.
    pub derived_from_track_id: Option<String>,

    /// The kind of data stored in this track.
    #[serde(rename = "type")]
    pub track_type: TrackType,

    /// The language stored in this track, represented as a two-letter ISO 639-1
    /// code when possible, and a three-letter 639-3 code for languages not
    /// included in ISO 639-1. If this is omitted, then programs may assume that
    /// this track might be something like a still image from a video or an
    /// illustration, that provides context but contains no linguistic data.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub lang: Option<isolang::Language>,

    /// A file containing the entire contents of this track. If this is not
    /// present, we will assume that the actual data is stored in
    pub file: Option<FilePath>,

    /// How was this generated? For AI-generated data, this should contain the
    /// model name, such as `"whisper-1"` or `"gpt-3.5-turbo"`.
    pub generated_by: Option<String>,

    /// Application-specific extension data.
    #[serde(default, skip_serializing_if = "ExtensionData::is_empty")]
    pub ext: ExtensionData,

    /// Unknown fields.
    #[serde(flatten)]
    pub unknown: HashMap<String, serde_json::Value>,
}

impl Track {
    /// Create a new track with the specified type. This is pretty much useless
    /// unless you also set some additional fields manually.
    pub fn new(track_type: TrackType) -> Track {
        Track {
            track_type: track_type,
            origin: None,
            derived_from_track_id: None,
            lang: None,
            file: None,
            generated_by: None,
            ext: ExtensionData::default(),
            unknown: HashMap::default(),
        }
    }
}

/// Different origins for a track.
#[derive(
    Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord, Deserialize, Serialize,
)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum TrackOrigin {
    /// This track is the original source material, as created by the author.
    Original,
    /// This track is a translation of the original source material **by a
    /// human.**
    HumanTranslated,
    /// This track was extracted from another track.
    Extracted,
    /// This track was converted from another format. For example, if the original
    /// was a high-res MKV file, this might be a lower-res MP4.
    Converted,
    /// This track was generated by an AI model. This includes AI translations,
    /// as well as AI-generated images, audio, etc. We mark these separately
    /// because they may have a higher rate of errors, and because using
    /// AI-generated data to train another AI model can lead to strange errors.
    AiGenerated,
}

/// Different possible track types.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum TrackType {
    /// This track contains HTML data.
    Html,
    /// This track contains audio or video data.
    Media,
    /// This track contains an image.
    Image,
    /// This track contains notes in HTML format. These are not part of the
    /// media themselves, but contain other useful information.
    Notes,
    /// This track contains a non-standard form of data. When serialized, it
    /// will be named starting with `"x-"`, followed by the `String` value.
    Ext(String),
}

impl<'de> Deserialize<'de> for TrackType {
    fn deserialize<D: Deserializer<'de>>(d: D) -> result::Result<Self, D::Error> {
        let value: &str = Deserialize::deserialize(d)?;
        match value {
            "html" => Ok(TrackType::Html),
            "media" => Ok(TrackType::Media),
            "image" => Ok(TrackType::Image),
            "notes" => Ok(TrackType::Notes),
            other if other.starts_with("x-") => {
                Ok(TrackType::Ext(other[2..].to_owned()))
            }
            other => Err(D::Error::custom(Error::UnsupportedTrackType {
                value: other.to_owned(),
            })),
        }
    }
}

impl Serialize for TrackType {
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match *self {
            TrackType::Html => "html".serialize(serializer),
            TrackType::Media => "media".serialize(serializer),
            TrackType::Image => "image".serialize(serializer),
            TrackType::Notes => "notes".serialize(serializer),
            TrackType::Ext(ref name) => format!("x-{}", name).serialize(serializer),
        }
    }
}

/// Either an alignment, or a group of alignments. This allows us to work with
/// both single alignments (for example, individual subtitles or sentences) and
/// groups of alignments (for example, a paragraph of text).
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(untagged)]
#[non_exhaustive]
pub enum AlignmentOrGroup {
    /// A group of alignments.
    Group(AlignmentGroup),
    /// A single alignment.
    Alignment(Alignment),
}

/// A group of alignments. This can be used to represent a paragraph of text, or
/// perhaps a verse of a song.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[cfg_attr(feature = "no_forwards_compatibility", serde(deny_unknown_fields))]
#[non_exhaustive]
pub struct AlignmentGroup {
    /// One or more alignments in this group.
    pub alignments: Vec<Alignment>,
    // This will require custom deserialization for `AlignmentGroup` to work.
    //
    // /// Unknown fields.
    // #[serde(flatten)]
    // pub unknown: HashMap<String, serde_json::Value>,
}

/// The smallest unit of alignment or synchronization. This might be a subtitle,
/// a sentence, or perhaps multiple sentences if that's the best the aligning
/// application can do.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[cfg_attr(feature = "no_forwards_compatibility", serde(deny_unknown_fields))]
#[non_exhaustive]
pub struct Alignment {
    /// A globally unique ID for this alignment. This may be used for things
    /// like bookmarking, or for keeping track of the link between alignments
    /// and generated Anki cards.
    pub id: Uuid,

    /// The heading level of this alignment. This works like `h1`, `h2`, etc.,
    /// in HTML. A book title might be `h1`, a chapter title `h2`, and a section
    /// title `h3`. This is used by applications that want to display a table of
    /// contents to the user.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub heading: Option<u8>,

    /// The time span associated with this alignment, relative to the first
    /// entry in `MediaFile.trakcs`. If that track does not contained timed
    /// media, this must be omitted.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    time_span: Option<TimeSpan>,

    /// One or more representations of the `Alignment`. For example, subtitle
    /// text in one or more languages, or an image, or a short audio clip.
    ///
    /// Normally this does **not** include any version of the
    /// `MediaFile.baseTrack` track, because we can already use
    /// `MediaFile.baseTrack` and `Alignment.span` to figure out what portion
    /// of the base track corresponds to this alignment.
    pub tracks: HashMap<TrackId, TrackSegment>,

    /// Tags. These may be used by the user or application to categorize the
    /// media file. The following values have special meanings by convention:
    ///
    /// - `"star"`: This alignment is of particular interest to the user. This might
    ///   also be used to mark alignment for export (for example, to flash cards).
    /// - `"fix"`: This track has been marked for manual correction.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub tags: Vec<String>,

    /// Application-specific extension data.
    #[serde(default, skip_serializing_if = "ExtensionData::is_empty")]
    pub ext: ExtensionData,

    /// Unknown fields.
    #[serde(flatten)]
    pub unknown: HashMap<String, serde_json::Value>,
}

/// A segment of a track included in an alignment. This might be a single
/// subtitle, a sentence or a line of a song. Or it might be a single image or
/// audio clip.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[cfg_attr(feature = "no_forwards_compatibility", serde(deny_unknown_fields))]
#[non_exhaustive]
pub struct TrackSegment {
    /// Text content, represented as HTML. Either this or `text` must be
    /// present, but not both.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub html: Option<html::Fragment>,

    /// External content, stored in a file. Either this or `html` must be
    /// present, but not both.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub file: Option<FilePath>,

    /// Unknown fields.
    #[serde(flatten)]
    pub unknown: HashMap<String, serde_json::Value>,
}

impl TrackSegment {
    /// Create a new HTML track with specified language and content.
    pub fn html<F>(html: F) -> TrackSegment
    where
        F: Into<html::Fragment>,
    {
        TrackSegment {
            html: Some(html.into()),
            file: None,
            unknown: HashMap::default(),
        }
    }

    /// Create a new HTML track from plain text.
    pub fn text<S>(text: S) -> TrackSegment
    where
        S: Into<String>,
    {
        TrackSegment {
            html: Some(html::Fragment::from_text(text)),
            file: None,
            unknown: HashMap::default(),
        }
    }

    /// Create a new file track.
    pub fn file(file: FilePath) -> TrackSegment {
        TrackSegment {
            html: None,
            file: Some(file),
            unknown: HashMap::default(),
        }
    }
}

/// A span of time, measured in floating-point seconds.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct TimeSpan {
    /// The starting time, in seconds (inclusive).
    begin: f32,
    /// The ending time, in seconds (exclusive).
    end: f32,
}

impl TimeSpan {
    /// Construct a new `TimeSpan`.
    pub fn new(begin: f32, end: f32) -> Result<TimeSpan> {
        if begin < end {
            Ok(TimeSpan { begin, end })
        } else {
            Err(Error::InvalidSpan { begin, end })
        }
    }

    /// The beginning of this time span.
    pub fn begin(&self) -> f32 {
        self.begin
    }

    /// The end of the this time span.
    pub fn end(&self) -> f32 {
        self.end
    }
}

impl<'de> Deserialize<'de> for TimeSpan {
    fn deserialize<D: Deserializer<'de>>(d: D) -> result::Result<Self, D::Error> {
        let (begin, end) = Deserialize::deserialize(d)?;
        TimeSpan::new(begin, end).map_err(D::Error::custom)
    }
}

impl Serialize for TimeSpan {
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        (self.begin, self.end).serialize(serializer)
    }
}

/// A portable, relative path in Unix notation. These are highly restricted to
/// help prevent directory traversal attacks. Specifically, they:
///
/// - Must not begin with "/".
/// - Must not contain path segments with the values "", "." or "..".
/// - Must contain valid UTF-8 data.
///
/// These paths will be interpreted as relative to the directory containing the
/// `metadata.json` file, and should point into a `"file/"` sudirectory located
/// in the same directory.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct FilePath {
    path: RelativePathBuf,
}

impl FilePath {
    /// Construct a relative path from an OS-specific [`Path`], and validate
    /// that it points into our `./file/` directory.
    pub fn from_path(path: &Path) -> Result<FilePath> {
        let path = RelativePath::from_path(path).map_err(|_| {
            (|| Error::InvalidPath {
                path: path.display().to_string(),
            })()
        })?;
        Self::from_rel_path(&path)
    }

    /// Construct a relative path from a portable [`RelativePath`], and validate
    /// that it points into our `./file/` directory.
    fn from_rel_path(path: &RelativePath) -> Result<FilePath> {
        let components = path.components().collect::<Vec<_>>();
        eprintln!("{:?}", components);
        if components.len() < 2 || components[0].as_str() != "files" {
            return Err((|| Error::InvalidPath {
                path: path.to_string(),
            })());
        }
        for component in components {
            let component = component.as_str();
            if component == ""
                || component == "."
                || component == ".."
                || component.contains("\\")
            {
                return Err((|| Error::InvalidPath {
                    path: path.to_string(),
                })());
            }
        }
        Ok(FilePath {
            path: path.to_relative_path_buf(),
        })
    }
}

#[test]
fn file_path_checks_validity() {
    assert!(FilePath::from_rel_path(RelativePath::new("files/good.txt")).is_ok());
    assert!(FilePath::from_rel_path(RelativePath::new("files/dir/good.txt")).is_ok());

    assert!(FilePath::from_rel_path(RelativePath::new("")).is_err());
    assert!(FilePath::from_rel_path(RelativePath::new("files")).is_err());
    assert!(FilePath::from_rel_path(RelativePath::new("./files")).is_err());
    assert!(FilePath::from_rel_path(RelativePath::new("files/..")).is_err());
    assert!(FilePath::from_rel_path(RelativePath::new("../files")).is_err());
    assert!(FilePath::from_rel_path(RelativePath::new("/absolute")).is_err());
    // These two are normalized by `RelativePath`. We'd disallow them otherwise,
    // but this is an extremely popular crate for handling portable paths, so
    // we'll accept these oddities.
    //
    // assert!(FilePath::from_rel_path(RelativePath::new("files/trailing/")).is_err());
    // assert!(FilePath::from_rel_path(RelativePath::new("files/dir//file")).is_err());
    assert!(FilePath::from_rel_path(RelativePath::new("files/back\\slash")).is_err());
}

impl<'de> Deserialize<'de> for FilePath {
    fn deserialize<D: Deserializer<'de>>(d: D) -> result::Result<Self, D::Error> {
        let path: RelativePathBuf = Deserialize::deserialize(d)?;
        FilePath::from_rel_path(&path).map_err(D::Error::custom)
    }
}

impl Serialize for FilePath {
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.path.serialize(serializer)
    }
}

/// Custom extension data which hasn't been standardized. All custom fields
/// **must** go in a block of this type, and should generally have names like
/// `myapp-attrname`, where `myapp` is the application that uses them. This is a
/// map with string keys and arbitrary JSON values.
pub type ExtensionData = HashMap<String, serde_json::Value>;

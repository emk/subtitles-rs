//! # Aligned media and text format for language-learning software
//!
//! This is a Rust implementation of the [aligned media specification][spec]
//! for language-learning software.
//!
//! [spec]: https://github.com/language-learners/aligned-media-spec

#[warn(missing_docs)]

#[macro_use]
extern crate failure;
extern crate isolang;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;

use failure::ResultExt;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde::de::Error as DeError;
use std::collections::HashMap;
use std::result;

pub mod html;

/// Errors which can be returned by this crate.
#[derive(Debug, Fail)]
pub enum Error {
    /// We could not parse the specified HTML.
    #[fail(display = "could not parse HTML {:?}", html)]
    CouldNotParseHtml {
        html: String,
    },

    /// We could not parse the input data.
    #[fail(display = "could not parse metadata")]
    CouldNotParseMetadata,

    /// We encountered an unsupported HTML attribute.
    #[fail(display = "the HTML attribute {:?} is not allowed", name)]
    HtmlAttributeForbidden {
        name: String,
    },

    /// We encountered an unsupported HTML element.
    #[fail(display = "the HTML element {:?} is not allowed", name)]
    HtmlElementForbidden {
        name: String,
    },

    /// We encountered an unsupported HTML entity.
    #[fail(display = "the HTML entity {:?} is not allowed", name)]
    HtmlEntityForbidden {
        name: String,
    },

    /// We encountered an invalid path.
    #[fail(display = "path {:?} is not allowed", path)]
    InvalidPath {
        path: String,
    },

    /// We encountered an invalid span.
    #[fail(display = "beginning of time span {},{} is greater than end", begin, end)]
    InvalidSpan {
        /// The beginning of the invalid span.
        begin: f32,
        /// The end of the invalid span.
        end: f32,
    },

    /// We encountered an unknown track type that didn't begin with "x-".
    #[fail(display = "unsupported track type {:?} (did you want to prefix it with \"x-\"?)", value)]
    UnsupportedTrackType {
        /// The unknown track type.
        value: String,
    },
}

/// Type alias for results returned by our crate.
pub type Result<T> = result::Result<T, Error>;

/// A single media file, typically an episode of a TV series, a film, an chapter
/// of an audiobook. It might also be something more exotic, like a PDF of a
/// graphic novel.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
#[cfg_attr(feature="no_forwards_compatibility", serde(deny_unknown_fields))]
pub struct Metadata {
    /// The title of a book, TV series, album, etc. This may be the same for
    /// multiple files if `section_number` and/or `section_title` are used.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,

    /// The "section" number. This might be an episode number, a track number,
    /// or a chapter number.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub section_number: Option<u32>,

    /// The title of this particular section. Typically a song name or chapter
    /// title, if somebody wants to record that.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub section_title: Option<String>,

    /// Authors, etc., of this work.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub creators: Vec<String>,

    /// The year in which this work was published.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub year: Option<i32>,

    /// The primary media track for this `MediaFile`. This is used as the "time
    /// base" for all `Alignment`s. This may be omitted if no timed media is
    /// available, as would be in the case of two texts aligned against each
    /// other.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub base_track: Option<Track>,

    /// Optional other tracks associated with this file.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub tracks: Vec<Track>,

    /// A list of synchronized sentences, subtitles, or other linguistic
    /// content.
    pub alignments: Vec<Alignment>,

    /// Application-specific extension data.
    #[serde(default, skip_serializing_if = "ExtensionData::is_empty")]
    pub ext: ExtensionData,

    /// Placeholder to allow for future extensibility without breaking the API.
    #[serde(default, skip_serializing)]
    _placeholder: (),
}

impl Metadata {
    /// Parse `metadata.json` represented as raw bytes. This will be interpreted
    /// as UTF-8, because the format is strict.
    pub fn from_bytes(data: &[u8]) -> result::Result<Metadata, failure::Error> {
        Ok(serde_json::from_slice(data).context(Error::CouldNotParseMetadata)?)
    }

    /// Parse `metadata.json` represented as a UTF-8 Rust string.
    pub fn from_str(data: &str) -> result::Result<Metadata, failure::Error> {
        Self::from_bytes(data.as_bytes())
    }
}

#[test]
fn parse_metadata() {
    let examples = &[
        include_str!("../fixtures/examples/book_example.aligned/metadata.json"),
        include_str!("../fixtures/examples/subtitle_example.aligned/metadata.json"),
        include_str!("../fixtures/examples/subtitle_extracted_example.aligned/metadata.json"),
    ];
    for example in examples {
        Metadata::from_str(example)
            .expect("failed to parse example metadata");
    }
}

/// An individual "track" of context. This might be a single subtitle in a
/// single language, or a still image taken from a video
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
#[cfg_attr(feature="no_forwards_compatibility", serde(deny_unknown_fields))]
pub struct Track {
    // The kind of data stored in this track.
    #[serde(rename = "type")]
    pub track_type: TrackType,

    /// The language stored in this track, represented as a two-letter ISO 639-1
    /// code when possible, and a three-letter 639-3 code for languages not
    /// included in ISO 639-1. If this is omitted, then programs may assume that
    /// this track might be something like a still image from a video or an
    /// illustration, that provides context but contains no linguistic data.
    #[serde(default, skip_serializing_if = "Option::is_none", with = "iso_short_code_serialization::opt")]
    pub lang: Option<isolang::Language>,

    // The actual underlying file on disk, if any. Either this or `html` should
    // be present, but not both.
    file: Option<FilePath>,

    // TODO: Do we want a `fileSpan: Span` element, to select only a portion of
    // a media file?

    /// Textual context, which should be valid HTML 5, optionally with embedded
    /// tags like `<b>`, `<i>` and `<br>`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub html: Option<html::Fragment>,

    /// Application-specific extension data.
    #[serde(default, skip_serializing_if = "ExtensionData::is_empty")]
    pub ext: ExtensionData,

    /// Placeholder to allow for future extensibility without breaking the API.
    #[serde(default, skip_serializing)]
    _placeholder: (),
}

impl Track {
    /// Create a new track with the specified type. This is pretty much useless
    /// unless you also set some additional fields manually.
    pub fn with_type(track_type: TrackType) -> Track {
        Track {
            track_type: track_type,
            lang: None,
            file: None,
            html: None,
            ext: ExtensionData::default(),
            _placeholder: (),
        }
    }

    /// Create a new HTML track with specified language and content.
    pub fn html<F>(lang: isolang::Language, html: F) -> Track
    where
        F: Into<html::Fragment>,
    {
        Track {
            track_type: TrackType::Html,
            lang: Some(lang),
            file: None,
            html: Some(html.into()),
            ext: ExtensionData::default(),
            _placeholder: (),
        }
    }

    /// Create a new HTML track from plain text.
    pub fn text<S>(lang: isolang::Language, text: S) -> Track
    where
        S: Into<String>,
    {
        Track {
            track_type: TrackType::Html,
            lang: Some(lang),
            file: None,
            html: Some(html::Fragment::from_text(text)),
            ext: ExtensionData::default(),
            _placeholder: (),
        }
    }
}

/// Different possible track types.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum TrackType {
    /// This track contains HTML data.
    Html,
    /// This track contains audio or video data.
    Media,
    /// This track contains an image.
    Image,
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
            other if other.starts_with("x-") => {
                Ok(TrackType::Ext(other[2..].to_owned()))
            }
            other => {
                Err(D::Error::custom(Error::UnsupportedTrackType {
                    value: other.to_owned(),
                }))
            }
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
            TrackType::Ext(ref name) => {
                format!("x-{}", name).serialize(serializer)
            }
        }
    }
}

/// The smallest unit of alignment or synchronization. This might be a subtitle,
/// a sentence, or perhaps multiple sentences if that's the best the aligning
/// application can do.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
#[cfg_attr(feature="no_forwards_compatibility", serde(deny_unknown_fields))]
pub struct Alignment {
    /// The time span associated with this alignment, relative to
    /// `MediaFile.baseTrack`. If `MediaFile.baseTrack` was not specified, this
    /// element must be omitted.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    time_span: Option<TimeSpan>,

    /// One or more representations of the `Alignment`. For example, subtitle
    /// text in one or more languages, or an image, or a short audio clip.
    ///
    /// Normally this does **not** include any version of the
    /// `MediaFile.baseTrack` track, because we can already use
    /// `MediaFile.baseTrack` and `Alignment.span` to figure out what portion
    /// of the base track corresponds to this alignment.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub tracks: Vec<Track>,

    /// Application-specific extension data.
    #[serde(default, skip_serializing_if = "ExtensionData::is_empty")]
    pub ext: ExtensionData,

    /// Placeholder to allow for future extensibility without breaking the API.
    #[serde(default, skip_serializing)]
    _placeholder: (),
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
/// These paths will be interpreted as relative to the `"file/"` sudirectory
/// located in the same directory as the `metadata.json` file.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct FilePath {
    path: String,
}

impl FilePath {
    /// Construct a relative path from a string, being sure to validate it.
    pub fn new<S: Into<String>>(path: S) -> Result<FilePath> {
        let path = path.into();
        for component in path.split("/") {
            if component == "" || component == "." || component == ".."
                || component.contains("\\")
            {
                return Err(Error::InvalidPath { path: path.clone() });
            }
        }
        Ok(FilePath { path })
    }
}

#[test]
fn file_path_checks_validity() {
    assert!(FilePath::new("good.txt").is_ok());
    assert!(FilePath::new("dir/good.txt").is_ok());

    assert!(FilePath::new("").is_err());
    assert!(FilePath::new(".").is_err());
    assert!(FilePath::new("..").is_err());
    assert!(FilePath::new("dir/..").is_err());
    assert!(FilePath::new("/absolute").is_err());
    assert!(FilePath::new("trailing/").is_err());
    assert!(FilePath::new("dir//file").is_err());
    assert!(FilePath::new("back\\slash").is_err());
}

impl<'de> Deserialize<'de> for FilePath {
    fn deserialize<D: Deserializer<'de>>(d: D) -> result::Result<Self, D::Error> {
        let path: String = Deserialize::deserialize(d)?;
        FilePath::new(path).map_err(D::Error::custom)
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

/// Routines for serialzing and deserialzing optional ISO 639-1 and 639-3 codes,
/// represented as `isolang::Language`. These are intended for use with
/// `serde_derive`'s `with =` argument.
pub mod iso_short_code_serialization {
    /// Serialize and deserialize `Option<isolang::Language>`.
    pub mod opt {
        use isolang::Language;
        use serde::{Deserialize, Deserializer, Serialize, Serializer};
        use serde::de::Error;
        use std::result::Result;

        // Deserialize an ISO 639-1 or 639-3 code.
        pub fn deserialize<'de, D>(d: D) -> Result<Option<Language>, D::Error>
        where
            D: Deserializer<'de>,
        {
            // We need to do this manually because of
            // https://github.com/humenda/isolang-rs/issues/7
            let code: Option<&str> = Deserialize::deserialize(d)?;
            match code {
                None => Ok(None),
                Some(c) => {
                    let lang = Language::from_639_3(c)
                        .or_else(|| Language::from_639_1(c))
                        .ok_or_else(|| {
                            D::Error::unknown_variant(
                                c,
                                &["an ISO 639-1 or 639-3 language code"],
                            )
                        })?;
                    Ok(Some(lang))
                }
            }
        }

        /// Serialize an `isolang::Language` as a 2-letter code if possible, or
        /// a 3-letter code otherwise.
        pub fn serialize<S>(
            lang: &Option<Language>,
            serializer: S,
        ) -> Result<S::Ok, S::Error>
        where
            S: Serializer,
        {
            let code = lang.map(|l| l.to_639_1().unwrap_or_else(|| l.to_639_3()));
            code.serialize(serializer)
        }
    }
}



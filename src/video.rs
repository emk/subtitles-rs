//! Tools for working with video files.

use num::rational::Ratio;
use regex::Regex;
use rustc_serialize::{Decodable, Decoder, json};
use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::result;
use std::str::{FromStr, from_utf8};

use err::{err_str, Error, Result};
use time::Period;

/// Individual streams inside a video are labelled with a codec type.
#[derive(Debug, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum CodecType {
    Audio,
    Video,
    Subtitle,
    Other(String),
}

impl Decodable for CodecType {
    fn decode<D: Decoder>(d: &mut D) -> result::Result<Self, D::Error> {
        match &try!(d.read_str())[..] {
            "audio" => Ok(CodecType::Audio),
            "video" => Ok(CodecType::Video),
            "subtitle" => Ok(CodecType::Subtitle),
            s => Ok(CodecType::Other(s.to_owned())),
        }
    }
}

/// A wrapper around `Ratio` with custom serialization support.
#[derive(Debug)]
pub struct Fraction(Ratio<u32>);

impl Fraction {
    fn decode_parts<D>(d: &mut D) -> result::Result<(u32, u32), D::Error>
        where D: Decoder
    {
        let s = try!(d.read_str());
        let re = Regex::new(r"^(\d+)/(\d+)$").unwrap();
        let cap = try!(re.captures(&s).ok_or_else(|| {
            d.error(&format!("Expected fraction: {}", &s))
        }));
        Ok((FromStr::from_str(cap.at(1).unwrap()).unwrap(),
            FromStr::from_str(cap.at(2).unwrap()).unwrap()))
    }
}

impl Decodable for Fraction {
    fn decode<D: Decoder>(d: &mut D) -> result::Result<Self, D::Error> {
        let (num, denom) = try!(Fraction::decode_parts(d));
        if denom == 0 {
            Err(d.error("Found fraction with a denominator of 0"))
        } else {
            Ok(Fraction(Ratio::new(num, denom)))
        }
    }
}


/// An individual content stream within a video.
#[derive(Debug, RustcDecodable)]
#[allow(missing_docs, dead_code)]
pub struct Stream {
    pub index: usize,
    pub codec_name: String,
    pub codec_long_name: Option<String>,
    pub codec_type: CodecType,
    pub codec_time_base: Fraction,
    pub codec_tag_string: String,
    pub codec_tag: String,
    pub profile: Option<String>,
    pub width: Option<usize>,
    pub height: Option<usize>,
    //has_b_frames
    //sample_aspect_ratio
    //display_aspect_ratio
    pub pix_fmt: Option<String>,
    pub level: Option<u32>,
    pub sample_rate: Option<f64>,
    pub channels: Option<usize>,
    pub bits_per_sample: Option<u32>,
    //avg_frame_rate
    pub time_base: Fraction,
    pub start_time: f64,
    //duration
    pub tags: BTreeMap<String, String>,
}

#[test]
fn test_stream_decode() {
    let json = "
{
  \"index\" : 2,
  \"codec_name\" : \"aac\",
  \"codec_long_name\" : \"AAC (Advanced Audio Coding)\",
  \"codec_type\" : \"audio\",
  \"codec_time_base\" : \"1/48000\",
  \"codec_tag_string\" : \"[0][0][0][0]\",
  \"codec_tag\" : \"0x0000\",
  \"sample_rate\" : \"48000.000000\",
  \"channels\" : 2,
  \"bits_per_sample\" : 0,
  \"avg_frame_rate\" : \"0/0\",
  \"time_base\" : \"1/1000\",
  \"start_time\" : \"0.000000\",
  \"duration\" : \"N/A\",
  \"tags\" : {
    \"language\" : \"eng\"
  }
}
";
    let stream: Stream = json::decode(json).unwrap();
    assert_eq!(CodecType::Audio, stream.codec_type);
}

/// What kind of data do we want to extract, and from what position in the
/// video clip?
pub enum ExtractionSpec {
    /// Extract an image at the specified time.
    Image(f32),
    /// Extract an audio clip covering the specified period.
    Audio(Period),
}

impl ExtractionSpec {
    /// Figure out what ffmpeg/avconv args we would need to extract the
    /// requested data.
    fn add_args(&self, cmd: &mut Command) {
        match self {
            &ExtractionSpec::Image(time) => {
                let scale_filter =
                    format!("scale=iw*min(1\\,min({}/iw\\,{}/ih)):-1",
                            240, 160);
                cmd.arg("-ss").arg(format!("{}", time))
                    .arg("-vframes").arg("1")
                    /* .arg("-filter_complex").arg(&scale_filter) */
                    .arg("-f").arg("image2");
            }
            &ExtractionSpec::Audio(period) => {
                cmd.arg("-ss").arg(format!("{}", period.begin()))
                    .arg("-t").arg(format!("{}", period.duration()));
            }
        }
    }
}

/// Information about what kind of data we want to extract.
pub struct Extraction {
    /// The path to extract to.
    pub path: PathBuf,
    /// What kind of data to extract.
    pub spec: ExtractionSpec,
}

impl Extraction {
    /// Add the necessary args to `cmd` to perform this extraction.
    fn add_args(&self, cmd: &mut Command) {
        self.spec.add_args(cmd);
        cmd.arg(self.path.clone());
    }
}

/// Metadata associated with a video.
#[derive(Debug, RustcDecodable)]
struct Metadata {
    streams: Vec<Stream>,
}

/// Represents a video file on disk.
#[derive(Debug)]
pub struct Video {
    path: PathBuf,
    metadata: Metadata,
}

impl Video {
    /// Create a new video file, given a path.
    pub fn new(path: &Path) -> Result<Video> {
        // Ensure we have an actual file name before doing anything else.
        try!(path.file_name().ok_or_else(|| {
            err_str(format!("Video path does not have a filename: {}",
                            path.to_string_lossy()))
        }));

        // Run our probe command.
        let cmd = Command::new("avprobe")
            .arg("-v").arg("quiet")
            .arg("-show_streams")
            .arg("-of").arg("json")
            .arg(path)
            .output();
        let output = try!(cmd);
        let stdout = try!(from_utf8(&output.stdout));
        let metadata = try!(json::decode(stdout));

        Ok(Video { path: path.to_owned(), metadata: metadata })
    }

    /// Get just the file name of this video file.
    pub fn file_name(&self) -> &OsStr {
        self.path.file_name().unwrap()
    }

    /// Get just the file stem of this video file, stripped of any
    /// extensions.
    pub fn file_stem(&self) -> &OsStr {
        self.path.file_stem().unwrap()
    }

    /// List all the tracks in a video file.
    pub fn streams(&self) -> &[Stream] {
        &self.metadata.streams
    }

    /// Perform a list of extractions as efficiently as possible.  We use a
    /// batch interface to avoid making many passes through the file.
    pub fn extract(&self, extractions: &[Extraction]) -> Result<()> {
        let total = (extractions.len() + 1) as f32;
        let chunk_size = 10;
        for (i, chunk) in extractions.chunks(chunk_size).enumerate() {
            let done = (chunk_size*i + 1) as f32;
            println!("Extracting: {}%", 100.0*done/total);
            let mut cmd = Command::new("avconv");
            cmd.arg("-i").arg(&self.path);
            for e in chunk {
                e.add_args(&mut cmd);
            }
            try!(cmd.output());
        }
        Ok(())
    }
}

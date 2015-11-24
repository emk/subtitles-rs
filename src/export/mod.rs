//! Interfaces to various spaced repetition systems.

use handlebars::Handlebars;
use rustc_serialize::json::{ToJson, Json};
use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::str::from_utf8;

use err::Result;
use srt::{Subtitle, SubtitleFile};
use video::Video;

/// Information about media file and associated subtitles that the user
/// wants to export.
pub struct ExportRequest {
    /// The video file from which to extract images and audio clips.
    pub video: Video,

    /// The foreign-language subtitles.
    pub foreign_subtitles: SubtitleFile,

    /// The optional native-language subtitles, if available.  Highly
    /// recommended for beginners, and surprisingly harmless even for
    /// advanced learners who are otherwise trying to maintain immersion.
    pub native_subtitles: Option<SubtitleFile>,
}

/// Information about a subtitle for use by our Handlebars HTML template.
/// This needs to implement `ToJson`, because that's what Handlebars wants
/// as input.
#[derive(RustcEncodable)]
struct SubtitleInfo {
    index: usize,
    image_path: String,
    audio_path: String,
    foreign_text: String,
    native_text: Option<String>,
}

impl ToJson for SubtitleInfo {
    fn to_json(&self) -> Json {
        let mut m: BTreeMap<String, Json> = BTreeMap::new();
        m.insert("index".to_string(), self.index.to_json());
        m.insert("image_path".to_string(), self.image_path.to_json());
        m.insert("audio_path".to_string(), self.audio_path.to_json());
        m.insert("foreign_text".to_string(), self.foreign_text.to_json());
        if let &Some(ref s) = &self.native_text {
            m.insert("native_text".to_string(), s.to_json());
        }
        m.to_json()
    }
}

/// Export-related information for use by our Handlebars HTML template.
#[derive(RustcEncodable)]
struct ExportInfo {
    filename: String,
    subtitles: Vec<SubtitleInfo>,
}

impl ToJson for ExportInfo {
    fn to_json(&self) -> Json {
        let mut m: BTreeMap<String, Json> = BTreeMap::new();
        m.insert("filename".to_string(), self.filename.to_json());
        m.insert("subtitles".to_string(), self.subtitles.to_json());
        m.to_json()
    }
}

// Write a chunk of data to a file.
fn write_all(path: &Path, data: &[u8]) -> Result<()> {
    let mut f = try!(fs::File::create(path));
    try!(f.write_all(data));
    Ok(())
}

/// Export flashcards and associated media files.
pub fn export(request: &ExportRequest) -> Result<()> {
    // Convert an OsStr to a String, losing any non-Unicode data, because we're
    // not going to create _new_ files with non-Unicode names.
    let path_str = |os_str: &OsStr| { os_str.to_string_lossy().into_owned() };
    let stem = path_str(request.video.file_stem());

    // Construct a path `dir` which we'll use to store our output files.
    // This is much uglier than it ought to be because paths are not
    // necessarily valid Unicode strings on all OSes, so we need to jump
    // through extra hoops.
    let dir = Path::new("./").join(format!("{}_review", &stem));
    try!(fs::create_dir_all(&dir));
    let media_path = |index: usize, ext: &str| -> PathBuf {
        dir.join(format!("{}_{:03}.{}", stem, index, ext))
    };

    // Start preparing information we'll pass to our HTML template.
    let mut bindings = ExportInfo {
        filename: path_str(request.video.file_name()),
        subtitles: vec!(),
    };

    // TODO: Offer some way to specify which subs.
    let subs: Vec<&Subtitle> = request.foreign_subtitles.subtitles.iter()
        .take(6).collect();
    for sub in subs {
        println!("Subtitle #{}: Extracting audio and video", sub.index);

        let image_path = media_path(sub.index, "jpg");
        try!(request.video.extract_image(sub.period.midpoint(), &image_path));

        let audio_path = media_path(sub.index, "mp3");
        try!(request.video.extract_audio(sub.period, &audio_path));

        bindings.subtitles.push(SubtitleInfo {
            index: sub.index,
            image_path: path_str(image_path.file_name().unwrap()),
            audio_path: path_str(audio_path.file_name().unwrap()),
            foreign_text: sub.lines.join(" "),
            native_text: None,
        });
    }

    // Write out our support files.
    try!(write_all(&dir.join("style.css"), &include_bytes!("style.css")[..]));
    try!(write_all(&dir.join("play.svg"), &include_bytes!("play.svg")[..]));


    // Render and write out our HTML.
    let template = try!(from_utf8(include_bytes!("review.html.hbs")));
    let mut handlebars = Handlebars::new();
    try!(handlebars.register_template_string("review", template.to_owned()));
    let html = try!(handlebars.render("review", &bindings));
    try!(write_all(&dir.join("index.html"), html.as_bytes()));

    Ok(())
}

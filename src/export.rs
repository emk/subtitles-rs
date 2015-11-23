//! Interfaces to various spaced repetition systems.

use std::path::Path;

use err::Result;
use srt::SubtitleFile;
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

/// Export flashcards and associated media files.
pub fn export(request: &ExportRequest) -> Result<()> {
    for sub in &request.foreign_subtitles.subtitles {
        println!("Extracting subtitle #{}", sub.index);

        let img_path_str = format!("sub{}.jpg", sub.index);
        let img_path = Path::new(&img_path_str);
        try!(request.video.extract_image(sub.midpoint(), &img_path));
        
        let sound_path_str = format!("sub{}.mp3", sub.index);
        let sound_path = Path::new(&sound_path_str);
        try!(request.video.extract_audio(sub.begin, sub.end - sub.begin,
                                         &sound_path));
    }
    Ok(())
}

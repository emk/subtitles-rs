//! Code shared between multiple exporters.

use std::{
    convert::AsRef,
    default::Default,
    ffi::OsStr,
    fmt::Write as fmt_Write,
    fs,
    io::Write,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Context as _};
use log::debug;

use crate::{
    align::align_available_files,
    lang::Lang,
    srt::{Subtitle, SubtitleFile},
    time::{Period, ToTimestamp},
    ui::Ui,
    video::{Extraction, ExtractionSpec, Id3Metadata, ImageSourceType, Video},
    Result,
};

/// Take a platform-specific pathname fragment and turn it into a regular
/// Unicode string.
pub fn os_str_to_string(os_str: &OsStr) -> String {
    os_str.to_string_lossy().into_owned()
}

/// Information about a specific language.
#[derive(Clone)]
pub struct LanguageResources {
    /// The subtitles associated with this language.
    pub subtitles: SubtitleFile,

    /// The language used in our subtitles, if we can figure it out.
    pub language: Option<Lang>,
}

impl LanguageResources {
    /// Create a list of per-language resources.
    fn new(subtitles: SubtitleFile) -> LanguageResources {
        let language = subtitles.detect_language();
        LanguageResources {
            subtitles: subtitles,
            language: language,
        }
    }
}

/// Information about a still image attached to a media file, typically the
/// album cover for an audio file.
#[derive(Default, Eq, PartialEq)]
enum AttachedPicState {
    /// We haven't checked for an attached picture yet.
    #[default]
    NeedToCheck,
    /// We found an attached picture and exported it at this
    /// relative path.
    Exported { rel_path: String },
    /// We didn't find an attached picture.
    NotFound,
}

impl AttachedPicState {
    /// Convert our state to an option, if possible.
    fn as_option(&self) -> Option<&str> {
        match self {
            AttachedPicState::Exported { rel_path } => Some(rel_path),
            _ => None,
        }
    }
}

/// Builder for an [`Exporter`].
pub struct ExporterBuilder {
    video: Video,
    foreign_subtitles: SubtitleFile,
    native_subtitles: Option<SubtitleFile>,
    out_dir: Option<PathBuf>,
}

impl ExporterBuilder {
    /// Create a new builder for the specified video and subtitles.
    pub fn new(
        video: Video,
        foreign_subtitles: SubtitleFile,
        native_subtitles: Option<SubtitleFile>,
    ) -> Self {
        Self {
            video,
            foreign_subtitles,
            native_subtitles,
            out_dir: None,
        }
    }

    /// Set the output directory based on a label.
    pub fn out_dir_label(mut self, label: &str) -> Result<Self> {
        let file_stem = os_str_to_string(self.video.file_stem());
        let out_dir = Path::new("./").join(format!("{}_{}", &file_stem, label));
        if fs::metadata(&out_dir).is_ok() {
            return Err(anyhow!(
                "Directory already exists: {}",
                &out_dir.to_string_lossy()
            ));
        }
        fs::create_dir_all(&out_dir)
            .with_context(|| format!("could not create {}", out_dir.display()))?;

        self.out_dir = Some(out_dir);
        Ok(self)
    }

    /// Set the output directory for this exporter.
    pub fn out_dir(mut self, out_dir: PathBuf) -> Self {
        self.out_dir = Some(out_dir);
        self
    }

    /// Create an exporter using the parameters we've set.
    pub fn build(self) -> Result<Exporter> {
        let out_dir = self
            .out_dir
            .ok_or_else(|| anyhow!("output directory not set"))?;
        Exporter::new(
            self.video,
            self.foreign_subtitles,
            self.native_subtitles,
            &out_dir,
        )
    }
}

/// Information about media file and associated subtitles that the user
/// wants to export.
pub struct Exporter {
    /// The video file from which to extract images and audio clips.
    video: Video,

    /// Resources related to the foreign language.
    foreign: LanguageResources,

    /// Resources related to the native language, if any.
    native: Option<LanguageResources>,

    /// If we need images, where should we look for them?
    image_source_type: Option<ImageSourceType>,

    /// If we only have an attached picture (usually an album cover), we want to
    /// use it in place of the video images we couldn't export.
    attached_picture: AttachedPicState,

    /// The base name to use when constructing other filenames.
    file_stem: String,

    /// The directory into which we want to output files.
    dir: PathBuf,

    /// A list of media files we want to extract from our video as
    /// efficiently as possible.
    extractions: Vec<Extraction>,
}

impl Exporter {
    /// Create a new exporter for the specified video and subtitles, using the
    /// specified output directory.
    pub fn new(
        video: Video,
        foreign_subtitles: SubtitleFile,
        native_subtitles: Option<SubtitleFile>,
        out_dir: &Path,
    ) -> Result<Exporter> {
        let image_source_type = video.image_source_type();
        let foreign = LanguageResources::new(foreign_subtitles);
        let native = native_subtitles.map(|subs| LanguageResources::new(subs));
        let file_stem = os_str_to_string(video.file_stem());
        Ok(Exporter {
            video: video,
            image_source_type,
            attached_picture: AttachedPicState::NeedToCheck,
            foreign: foreign,
            native: native,
            file_stem: file_stem,
            dir: out_dir.to_owned(),
            extractions: vec![],
        })
    }

    /// The base name of this file, with the directory and file extension
    /// removed.
    pub fn file_stem(&self) -> &str {
        &self.file_stem
    }

    /// The directory into which we're exporting files.
    pub fn dir(&self) -> &Path {
        &self.dir
    }

    /// Return a title for this video.
    pub fn title(&self) -> &str {
        &self.file_stem
    }

    /// Get the video we're exporting.
    pub fn video(&self) -> &Video {
        &self.video
    }

    /// Get data related to the foreign language.
    pub fn foreign(&self) -> &LanguageResources {
        &self.foreign
    }

    /// Get data related to the native language.
    pub fn native(&self) -> Option<&LanguageResources> {
        self.native.as_ref()
    }

    /// Align our two sets of subtitles.
    pub fn align(&self) -> Vec<(Option<Subtitle>, Option<Subtitle>)> {
        align_available_files(
            &self.foreign.subtitles,
            self.native.as_ref().map(|n| &n.subtitles),
        )
    }

    /// Construct a path to an extracted media file, including timestamps
    /// and language information as appropriate.
    fn media_path<T: ToTimestamp>(
        &self,
        timestamp: T,
        lang: Option<Lang>,
        extension: &str,
    ) -> PathBuf {
        let mut file_name =
            format!("{}_{}", &self.file_stem, timestamp.to_file_timestamp());
        if let Some(l) = lang {
            write!(&mut file_name, ".{}", l.as_str()).unwrap();
        }
        write!(&mut file_name, ".{}", extension).unwrap();
        self.dir.join(file_name)
    }

    /// Export an attached picture and return the file name we used as a string.
    fn export_attached_picture(&self) -> Result<String> {
        let pic = self.video.attached_pic()?;
        let ext = match &pic.mime_type[..] {
            "image/jpeg" => "jpg",
            "image/png" => "png",
            "image/gif" => "gif",
            _ => {
                return Err(anyhow!(
                    "don't recognize MIME type for attached picture: {}",
                    pic.mime_type
                ));
            }
        };
        let path = format!("{}_pic.{}", self.file_stem, ext);
        self.export_data_file(&path, &pic.data)?;
        Ok(path)
    }

    /// Try to export an attached picture, if we haven't already.
    fn export_attached_picture_if_needed(&mut self) -> Option<&str> {
        if self.attached_picture == AttachedPicState::NeedToCheck {
            self.attached_picture = match self.export_attached_picture() {
                Ok(rel_path) => AttachedPicState::Exported { rel_path },
                Err(e) => {
                    debug!("could not export attached picture: {}", e);
                    AttachedPicState::NotFound
                }
            };
        }
        self.attached_picture.as_option()
    }

    /// Schedule an export of the image at the specified time code.
    /// Returns the path to which the image will be written, if any.
    pub fn schedule_image_export(&mut self, time: f32) -> Option<String> {
        match self.image_source_type {
            Some(ImageSourceType::Video) => {
                let path = self.media_path(time, None, "jpg");
                self.extractions.push(Extraction {
                    path: path.clone(),
                    spec: ExtractionSpec::Image { time },
                });
                Some(os_str_to_string(path.file_name().unwrap()))
            }
            Some(ImageSourceType::AttachedPic) => self
                .export_attached_picture_if_needed()
                .map(|s| s.to_owned()),
            None => None,
        }
    }

    /// Schedule an export of the audio at the specified time period.
    /// Returns the path to which the audio will be written.
    pub fn schedule_audio_export(
        &mut self,
        lang: Option<Lang>,
        period: Period,
    ) -> String {
        self.schedule_audio_export_ext(lang, period, Default::default())
    }

    /// Schedule an export of the audio at the specified time period, using
    /// the specified metadata.  Returns the path to which the audio will
    /// be written.
    pub fn schedule_audio_export_ext(
        &mut self,
        lang: Option<Lang>,
        period: Period,
        metadata: Id3Metadata,
    ) -> String {
        let path = self.media_path(period, lang, "mp3");
        let stream = lang.and_then(|l| self.video.audio_track_for(l));
        self.extractions.push(Extraction {
            path: path.clone(),
            spec: ExtractionSpec::Audio {
                stream,
                period,
                metadata,
            },
        });
        os_str_to_string(path.file_name().unwrap())
    }

    /// Write a raw chunk of bytes to a file in our export directory.
    pub fn export_data_file<P>(&self, rel_path: P, data: &[u8]) -> Result<()>
    where
        P: AsRef<Path>,
    {
        let path = self.dir.join(rel_path.as_ref());
        let mut f = fs::File::create(&path)
            .with_context(|| format!("could not open {}", path.display()))?;
        f.write_all(data)
            .with_context(|| format!("could not write to {}", path.display()))?;
        Ok(())
    }

    /// Finish all scheduled exports.
    pub async fn finish_exports(&mut self, ui: &Ui) -> Result<()> {
        self.video.extract(ui, &self.extractions).await?;
        Ok(())
    }
}

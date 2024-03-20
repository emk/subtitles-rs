//! Tools for studying foreign languages using subtitles.

#![warn(missing_docs)]

use std::path::{Path, PathBuf};

pub use anyhow::{Error, Result};
use clap::{Parser, Subcommand};
use dotenv::dotenv;
use export::ExporterBuilder;
use tempfile::tempdir;
use video::Video;

use crate::{
    align::combine_files,
    import::{import_whisper_json, WhisperJson},
    lang::Lang,
    services::oai::{
        transcribe_subtitles_to_srt_file, transcribe_subtitles_to_whisper_json,
        translate_subtitle_file, TranscriptionFormat,
    },
    srt::SubtitleFile,
    ui::Ui,
};

pub mod align;
pub mod clean;
pub mod contexts;
pub mod decode;
pub mod errors;
pub mod export;
pub mod import;
pub mod lang;
pub mod merge;
pub mod segment;
pub mod services;
pub mod srt;
pub mod time;
pub mod ui;
mod vad;
pub mod video;

#[derive(Debug, Parser)]
/// Subtitle processing tools for students of foreign languages. (For now, all
/// subtitles must be in *.srt format. Many common encodings will be
/// automatically detected, but try converting to UTF-8 if you have problems.)
#[command(name = "substudy", version)]
enum Args {
    /// Clean a subtitle file, removing things that don't look like dialog.
    #[command(name = "clean")]
    Clean {
        /// Path to the subtitle file to clean.
        subs: PathBuf,
    },

    /// Combine two subtitle files into a single bilingual subtitle file.
    #[command(name = "combine")]
    Combine {
        /// Path to the foreign language subtitle file to be combined.
        foreign_subs: PathBuf,

        /// Path to the native language subtitle file to be combined.
        native_subs: PathBuf,
    },

    /// Export subtitles in one of several formats (Anki cards, music tracks,
    /// etc).
    #[command(name = "export")]
    Export {
        #[command(subcommand)]
        format: ExportFormat,
    },

    /// Import subtitles from one of several formats (Whisper JSON, etc).
    #[command(name = "import")]
    Import {
        #[command(subcommand)]
        format: ImportFormat,
    },

    /// List information about a file.
    #[command(name = "list")]
    List {
        #[command(subcommand)]
        to_list: ToList,
    },

    /// Transcribe subtitles from audio.
    #[command(name = "transcribe")]
    Transcribe {
        /// Path to the video.
        video: PathBuf,

        /// Example text related to or similar to audio.
        #[arg(long)]
        example_text: PathBuf,

        /// Output format for the transcription.
        #[arg(long, default_value = "srt")]
        format: TranscriptionFormat,
    },

    /// Translate subtitles.
    #[command(name = "translate")]
    Translate {
        /// Path to the subtitle file to translate.
        foreign_subs: PathBuf,

        /// Target language code (e.g. "en" for English).
        #[arg(long)]
        native_lang: String,
    },
}

#[derive(Debug, Subcommand)]
enum ExportFormat {
    /// Export to Anki via Anki-Connect plugin:
    /// https://ankiweb.net/shared/info/2055492159.
    #[command(name = "anki")]
    Anki {
        /// Path to the video.
        video: PathBuf,

        /// Path to the file containing foreign language subtitles.
        foreign_subs: PathBuf,

        /// Path to the file containing native language subtitles.
        native_subs: Option<PathBuf>,

        /// Deck name to use in Anki.
        #[arg(long)]
        deck: String,

        /// Tags to add in Anki.
        #[arg(long = "tag")]
        tags: Vec<String>,

        /// Skip duplicate subtitles (useful for music).
        #[arg(long)]
        skip_duplicates: bool,
    },

    /// Export as CSV file and media for use with Anki.
    #[command(name = "csv")]
    Csv {
        /// Path to the video.
        video: PathBuf,

        /// Path to the file containing foreign language subtitles.
        foreign_subs: PathBuf,

        /// Path to the file containing native language subtitles.
        native_subs: Option<PathBuf>,
    },

    /// Export as an HTML page allowing you to review the subtitles.
    #[command(name = "review")]
    Review {
        /// Path to the video.
        video: PathBuf,

        /// Path to the file containing foreign language subtitles.
        foreign_subs: PathBuf,

        /// Path to the file containing native language subtitles.
        native_subs: Option<PathBuf>,
    },

    /// Export as MP3 tracks for listening on the go.
    #[command(name = "tracks")]
    Tracks {
        /// Path to the video.
        video: PathBuf,

        /// Path to the file containing foreign language subtitles.
        foreign_subs: PathBuf,
    },
}

impl ExportFormat {
    /// Get the path to the video.
    fn video(&self) -> &Path {
        match self {
            ExportFormat::Anki { video, .. } => &video,
            ExportFormat::Csv { video, .. } => &video,
            ExportFormat::Review { video, .. } => &video,
            ExportFormat::Tracks { video, .. } => &video,
        }
    }

    /// Get the path to the foreign-language subtitles, if present.
    fn foreign_subs(&self) -> &Path {
        match self {
            ExportFormat::Anki { foreign_subs, .. } => foreign_subs,
            ExportFormat::Csv { foreign_subs, .. } => foreign_subs,
            ExportFormat::Review { foreign_subs, .. } => foreign_subs,
            ExportFormat::Tracks { foreign_subs, .. } => foreign_subs,
        }
    }

    /// Get the path to the native-language subtitles, if present.
    fn native_subs(&self) -> Option<&Path> {
        match self {
            ExportFormat::Anki { native_subs, .. } => {
                native_subs.as_ref().map(|p| p.as_path())
            }
            ExportFormat::Csv { native_subs, .. } => {
                native_subs.as_ref().map(|p| p.as_path())
            }
            ExportFormat::Review { native_subs, .. } => {
                native_subs.as_ref().map(|p| p.as_path())
            }
            ExportFormat::Tracks { .. } => None,
        }
    }
}

/// Formats we can import.
#[derive(Debug, Subcommand)]
enum ImportFormat {
    /// Import from a Whisper JSON file.
    #[command(name = "whisper-json")]
    WhisperJson {
        /// Path to the Whisper JSON file.
        whisper_json: PathBuf,
    },
}

#[derive(Debug, Subcommand)]
enum ToList {
    /// List the various audio and video tracks in a video file.
    #[command(name = "tracks")]
    Tracks {
        /// Path to the video.
        video: PathBuf,
    },
}

// Choose and run the appropriate command.
#[tokio::main]
async fn main() -> Result<()> {
    dotenv().ok();
    let ui = Ui::init();

    // Parse our command-line arguments using docopt (very shiny).
    let args: Args = Args::parse();
    match args {
        Args::Clean { subs } => cmd_clean(&subs),
        Args::Combine {
            foreign_subs,
            native_subs,
        } => cmd_combine(&foreign_subs, &native_subs),
        Args::Export { format } => cmd_export(&ui, format).await,
        Args::Import { format } => cmd_import(format),
        Args::List {
            to_list: ToList::Tracks { video },
        } => cmd_tracks(&video).await,
        Args::Transcribe {
            video,
            example_text,
            format,
        } => cmd_transcribe(&ui, &video, &example_text, format).await,
        Args::Translate {
            foreign_subs,
            native_lang,
        } => cmd_translate(&ui, &foreign_subs, &native_lang).await,
    }
}

fn cmd_clean(path: &Path) -> Result<()> {
    let file1 = SubtitleFile::cleaned_from_path(path)?;
    print!("{}", file1.to_string());
    Ok(())
}

fn cmd_combine(path1: &Path, path2: &Path) -> Result<()> {
    let file1 = SubtitleFile::cleaned_from_path(path1)?;
    let file2 = SubtitleFile::cleaned_from_path(path2)?;
    print!("{}", combine_files(&file1, &file2).to_string());
    Ok(())
}

async fn cmd_tracks(path: &Path) -> Result<()> {
    let v = Video::new(path).await?;
    for stream in v.streams() {
        let lang = stream.language();
        let lang_str = lang
            .map(|l| l.as_str().to_owned())
            .unwrap_or("??".to_owned());
        println!("#{} {} {:?}", stream.index, &lang_str, stream.codec_type);
    }
    Ok(())
}

async fn cmd_export(ui: &Ui, format: ExportFormat) -> Result<()> {
    // Load our input files.
    let video = Video::new(format.video()).await?;
    let foreign_subs = SubtitleFile::cleaned_from_path(format.foreign_subs())?;
    let native_subs = match format.native_subs() {
        None => None,
        Some(p) => Some(SubtitleFile::cleaned_from_path(p)?),
    };

    let builder = ExporterBuilder::new(video, foreign_subs, native_subs);
    match format {
        ExportFormat::Anki {
            deck,
            tags,
            skip_duplicates,
            ..
        } => {
            let tmp = tempdir()?;
            let mut exporter = builder.out_dir(tmp.path().to_owned()).build()?;
            let options = export::AnkiExportOptions {
                deck,
                tags,
                skip_duplicates,
            };
            export::export_anki(ui, &mut exporter, options).await?;
        }
        ExportFormat::Csv { .. } => {
            let mut exporter = builder.out_dir_label("csv")?.build()?;
            export::export_csv(ui, &mut exporter).await?;
        }
        ExportFormat::Review { .. } => {
            let mut exporter = builder.out_dir_label("review")?.build()?;
            export::export_review(ui, &mut exporter).await?;
        }
        ExportFormat::Tracks { .. } => {
            let mut exporter = builder.out_dir_label("tracks")?.build()?;
            export::export_tracks(ui, &mut exporter).await?;
        }
    }

    Ok(())
}

fn cmd_import(format: ImportFormat) -> Result<()> {
    match format {
        ImportFormat::WhisperJson { whisper_json } => {
            let whisper_json = WhisperJson::from_path(&whisper_json)?;
            let srt = import_whisper_json(&whisper_json)?;
            print!("{}", srt.to_string());
            Ok(())
        }
    }
}

async fn cmd_transcribe(
    ui: &Ui,
    video: &Path,
    example_text: &Path,
    format: TranscriptionFormat,
) -> Result<()> {
    let v = Video::new(video).await?;
    let text = std::fs::read_to_string(example_text)?;
    match format {
        TranscriptionFormat::WhisperJson => {
            let json = transcribe_subtitles_to_whisper_json(ui, &v, &text).await?;
            let json_str = serde_json::to_string_pretty(&json)?;
            print!("{}", json_str);
        }
        TranscriptionFormat::Srt => {
            let srt = transcribe_subtitles_to_srt_file(ui, &v, &text).await?;
            print!("{}", srt.to_string());
        }
    }
    Ok(())
}

async fn cmd_translate(ui: &Ui, foreign_subs: &Path, native_lang: &str) -> Result<()> {
    let file = SubtitleFile::cleaned_from_path(foreign_subs)?;
    let native_lang = Lang::iso639(native_lang)?;
    let translated = translate_subtitle_file(ui, &file, native_lang).await?;
    print!("{}", translated.to_string());
    Ok(())
}

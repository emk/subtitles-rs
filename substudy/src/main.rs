//! Tools for studying foreign languages using subtitles.

#![warn(missing_docs)]

use std::{
    fs::read_to_string,
    io::{stdout, BufWriter},
    path::{Path, PathBuf},
};

use anyhow::{bail, Context};
pub use anyhow::{Error, Result};
use clap::{Parser, Subcommand};
use dotenv::dotenv;
use export::ExporterBuilder;
use services::oai::TranscriptionPrompt;
use tempfile::tempdir;
use video::Video;

use crate::{
    align::combine_files,
    import::{import_whisper_json, WhisperJson},
    lang::Lang,
    services::oai::{translate_subtitle_file, TranscriptionFormat},
    srt::SubtitleFile,
    ui::Ui,
};

pub(crate) mod ai;
pub mod align;
pub(crate) mod cache;
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

        /// Path to sample text which resembles the content of the video.
        #[arg(long, conflicts_with = "expected_text")]
        example_text: Option<PathBuf>,

        /// Path to expected text, for when you already know more or less what
        /// the subtitles should say, but you want to sync them up with the
        /// video. Line breaks will be treated as subtitle breaks. Most useful
        /// for music.
        #[arg(long, conflicts_with = "example_text")]
        expected_text: Option<PathBuf>,

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
            expected_text,
            format,
        } => {
            let prompt = prompt_from(example_text, expected_text)?;
            cmd_transcribe(&ui, &video, prompt.as_ref(), format).await
        }
        Args::Translate {
            foreign_subs,
            native_lang,
        } => cmd_translate(&ui, &foreign_subs, &native_lang).await,
    }
}

/// Build our transcription prompt from the command-line arguments.
fn prompt_from(
    example_text: Option<PathBuf>,
    expected_text: Option<PathBuf>,
) -> Result<Option<TranscriptionPrompt>> {
    let read = |p: &Path| {
        read_to_string(p)
            .with_context(|| format!("Could not read file: {}", p.display()))
    };
    match (example_text, expected_text) {
        (Some(_), Some(_)) => {
            // Clap should prevent this from happening.
            bail!("Cannot specify both --example-text and --expected-text")
        }
        (Some(example_text), None) => {
            let example_text = read(&example_text)?;
            Ok(Some(TranscriptionPrompt::Example(example_text)))
        }
        (None, Some(expected_text)) => {
            let expected_text = read(&expected_text)?;
            Ok(Some(TranscriptionPrompt::Expected(expected_text)))
        }
        (None, None) => Ok(None),
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
    video_path: &Path,
    prompt: Option<&TranscriptionPrompt>,
    format: TranscriptionFormat,
) -> Result<()> {
    let video = Video::new(video_path).await?;
    let out = stdout();
    let writer = out.lock();
    format
        .write_transcription(ui, &video, prompt, &mut BufWriter::new(writer))
        .await?;
    Ok(())
}

async fn cmd_translate(ui: &Ui, foreign_subs: &Path, native_lang: &str) -> Result<()> {
    let file = SubtitleFile::cleaned_from_path(foreign_subs)?;
    let native_lang = Lang::iso639(native_lang)?;
    let translated = translate_subtitle_file(ui, &file, native_lang).await?;
    print!("{}", translated.to_string());
    Ok(())
}

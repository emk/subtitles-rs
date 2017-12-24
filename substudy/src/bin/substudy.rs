//! Command-line iterface to substudy.

#[macro_use]
extern crate common_failures;
extern crate env_logger;
extern crate structopt;
#[macro_use]
extern crate structopt_derive;
extern crate substudy;

use common_failures::prelude::*;
use std::path::{Path, PathBuf};
use structopt::StructOpt;
use substudy::srt::SubtitleFile;
use substudy::align::combine_files;
use substudy::video;
use substudy::export;

#[derive(Debug, StructOpt)]
/// Subtitle processing tools for students of foreign languages. (For now, all
/// subtitles must be in *.srt format. Many common encodings will be
/// automatically detected, but try converting to UTF-8 if you have problems.)
#[structopt(name = "substudy")]
enum Args {
    /// Clean a subtitle file, removing things that don't look like dialog.
    #[structopt(name = "clean")]
    Clean {
        /// Path to the subtitle file to clean.
        #[structopt(parse(from_os_str))]
        subs: PathBuf,
    },

    /// Combine two subtitle files into a single bilingual subtitle file.
    #[structopt(name = "combine")]
    Combine {
        /// Path to the foreign language subtitle file to be combined.
        #[structopt(parse(from_os_str))]
        foreign_subs: PathBuf,

        /// Path to the native language subtitle file to be combined.
        #[structopt(parse(from_os_str))]
        native_subs: PathBuf,
    },

    /// Export subtitles in one of several formats (Anki cards, music tracks,
    /// etc).
    #[structopt(name = "export")]
    Export {
        #[structopt(subcommand)]
        format: ExportFormat,
    },

    /// List information about a file.
    #[structopt(name = "list")]
    List {
        #[structopt(subcommand)]
        to_list: ToList,
    },
}

#[derive(Debug, StructOpt)]
enum ExportFormat {
    /// Export as CSV file and media for use with Anki.
    #[structopt(name = "csv")]
    Csv {
        /// Path to the video.
        #[structopt(parse(from_os_str))]
        video: PathBuf,

        /// Path to the file containing foreign language subtitles.
        #[structopt(parse(from_os_str))]
        foreign_subs: PathBuf,

        /// Path to the file containing native language subtitles.
        #[structopt(parse(from_os_str))]
        native_subs: Option<PathBuf>,
    },

    /// Export as an HTML page allowing you to review the subtitles.
    #[structopt(name = "review")]
    Review {
        /// Path to the video.
        #[structopt(parse(from_os_str))]
        video: PathBuf,

        /// Path to the file containing foreign language subtitles.
        #[structopt(parse(from_os_str))]
        foreign_subs: PathBuf,

        /// Path to the file containing native language subtitles.
        #[structopt(parse(from_os_str))]
        native_subs: Option<PathBuf>,
    },

    /// Export as MP3 tracks for listening on the go.
    #[structopt(name = "tracks")]
    Tracks {
        /// Path to the video.
        #[structopt(parse(from_os_str))]
        video: PathBuf,

        /// Path to the file containing foreign language subtitles.
        #[structopt(parse(from_os_str))]
        foreign_subs: PathBuf,
    },
}

impl ExportFormat {
    /// Get the name of the export format to use.
    fn name(&self) -> &str {
        match *self {
            ExportFormat::Csv { .. } => "csv",
            ExportFormat::Review { .. } => "review",
            ExportFormat::Tracks { .. } => "tracks",
        }
    }

    /// Get the path to the video.
    fn video(&self) -> &Path {
        match *self {
            ExportFormat::Csv { ref video, .. } => &video,
            ExportFormat::Review { ref video, .. } => &video,
            ExportFormat::Tracks { ref video, .. } => &video,
        }
    }

    /// Get the path to the foreign-language subtitles, if present.
    fn foreign_subs(&self) -> &Path {
        match *self {
            ExportFormat::Csv { ref foreign_subs, .. } => &foreign_subs,
            ExportFormat::Review { ref foreign_subs, .. } => &foreign_subs,
            ExportFormat::Tracks { ref foreign_subs, .. } => &foreign_subs,
        }
    }

    /// Get the path to the native-language subtitles, if present.
    fn native_subs(&self) -> Option<&Path> {
        match *self {
            ExportFormat::Csv { ref native_subs, .. } => native_subs.as_ref().map(|p| p.as_path()),
            ExportFormat::Review { ref native_subs, .. } => native_subs.as_ref().map(|p| p.as_path()),
            ExportFormat::Tracks { .. } => None,
        }
    }
}

#[derive(Debug, StructOpt)]
enum ToList {
    /// List the various audio and video tracks in a video file.
    #[structopt(name = "tracks")]
    Tracks {
        /// Path to the video.
        #[structopt(parse(from_os_str))]
        video: PathBuf,
    },
}

// Choose and run the appropriate command.
fn run() -> Result<()> {
    env_logger::init().expect("could not initialize logging");

    // Parse our command-line arguments using docopt (very shiny).
    let args: Args = Args::from_args();

    match args {
        Args::Clean { ref subs } => {
            cmd_clean(subs)
        }
        Args::Combine { ref foreign_subs, ref native_subs } => {
            cmd_combine(foreign_subs, native_subs)
        }
        Args::Export { ref format } => {
            cmd_export(
                format.name(),
                format.video(),
                format.foreign_subs(),
                format.native_subs()
            )
        }
        Args::List { to_list: ToList::Tracks { ref video } } => {
            cmd_tracks(video)
        }
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

fn cmd_tracks(path: &Path) -> Result<()> {
    let v = video::Video::new(path)?;
    for stream in v.streams() {
        let lang = stream.language();
        let lang_str = lang.map(|l| l.as_str().to_owned())
            .unwrap_or("??".to_owned());
        println!("#{} {} {:?}", stream.index, &lang_str, stream.codec_type);
    }
    Ok(())
}

fn cmd_export(
    kind: &str,
    video_path: &Path,
    foreign_sub_path: &Path,
    native_sub_path: Option<&Path>,
) -> Result<()> {
    // Load our input files.
    let video = video::Video::new(video_path)?;
    let foreign_subs = SubtitleFile::cleaned_from_path(foreign_sub_path)?;
    let native_subs = match native_sub_path {
        None => None,
        Some(p) => Some(SubtitleFile::cleaned_from_path(p)?),
    };

    let mut exporter = export::Exporter::new(video, foreign_subs, native_subs, kind)?;
    match kind {
        "csv" => export::export_csv(&mut exporter)?,
        "review" => export::export_review(&mut exporter)?,
        "tracks" => export::export_tracks(&mut exporter)?,
        _ => panic!("Uknown export type: {}", kind),
    }

    Ok(())
}

quick_main!(run);

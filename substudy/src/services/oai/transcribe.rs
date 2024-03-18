//! Transcribe subtitles using OpenAI's Whisper model.

use std::{fmt, path::Path, str::FromStr, time::Duration};

use anyhow::{anyhow, Context as _};
use async_openai::{
    types::{
        AudioInput, AudioResponseFormat, CreateTranscriptionRequest,
        TimestampGranularity,
    },
    Client,
};
use log::{debug, trace};
use tempfile::tempdir;
use tokio::task::spawn_blocking;

use crate::{
    import::{import_whisper_json, WhisperJson},
    lang::Lang,
    services::oai::retry_openai_request,
    srt::SubtitleFile,
    ui::Ui,
    vad::segment_on_dialog_breaks,
    video::{Extraction, ExtractionSpec, Id3Metadata, Video},
    Result,
};

/// Output format for subtitle transcriptions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranscriptionFormat {
    /// Whisper JSON format.
    WhisperJson,

    /// SubRip format.
    Srt,
}

impl FromStr for TranscriptionFormat {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "whisper-json" => Ok(TranscriptionFormat::WhisperJson),
            "srt" => Ok(TranscriptionFormat::Srt),
            _ => Err(anyhow::anyhow!("invalid transcription format: {}", s)),
        }
    }
}

impl fmt::Display for TranscriptionFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TranscriptionFormat::WhisperJson => write!(f, "whisper-json"),
            TranscriptionFormat::Srt => write!(f, "srt"),
        }
    }
}

/// Transcribe a video file to SRT format.
pub async fn transcribe_subtitles_to_srt_file(
    ui: &Ui,
    video: &Video,
    prompt: &str,
) -> Result<SubtitleFile> {
    let whisper_json =
        transcribe_subtitles_to_whisper_json(&ui, video, prompt).await?;
    import_whisper_json(&whisper_json)
}

/// Transcribe a video file to Whisper JSON format.
pub async fn transcribe_subtitles_to_whisper_json(
    ui: &Ui,
    video: &Video,
    prompt: &str,
) -> Result<WhisperJson> {
    // Find our language.
    let lang = Lang::for_text(prompt)
        .ok_or_else(|| anyhow!("could not infer language from example text"))?;

    // Figure out where to split the video to fit under the 25 MB limit.
    let stream_id = video
        .audio_track_for(lang)
        .ok_or_else(|| anyhow!("no audio track found for language: {}", lang))?;
    let periods = segment_on_dialog_breaks(ui, video, stream_id, 10.0 * 60.0).await?;
    debug!("split into periods: {:?}", periods);

    // Extract audio tracks for transcription.
    let temp_dir = tempdir().context("failed to create temporary directory")?;
    let extractions = periods
        .into_iter()
        .enumerate()
        .map(|(i, period)| {
            let mut file_name = video.file_stem().to_owned();
            file_name.push(format!("_{}.mp3", i));
            let path = temp_dir.path().join(&file_name);
            Extraction {
                path,
                spec: ExtractionSpec::Audio {
                    stream: Some(stream_id),
                    period,
                    metadata: Id3Metadata::default(),
                },
            }
        })
        .collect::<Vec<_>>();
    {
        let ui = ui.to_owned();
        let video = video.clone();
        let extractions = extractions.to_owned();
        spawn_blocking(move || video.extract(&ui, &extractions)).await??;
    }

    // Transcribe the audio tracks.
    let pb = ui.new_progress_bar(extractions.len() as u64);
    pb.set_prefix("🎧");
    pb.set_message("Transcribing dialog");
    pb.enable_steady_tick(Duration::from_secs(1));
    let mut transcription = None;
    for extraction in extractions {
        let path = &extraction.path;
        let whisper_json =
            retry_openai_request(|| transcribe_file(&path, lang, prompt)).await?;
        pb.inc(1);
        match &mut transcription {
            None => transcription = Some(whisper_json),
            Some(t) => {
                t.append_with_offset(whisper_json, extraction.spec.earliest_time())?
            }
        }
    }
    pb.finish_with_message("Transcribed dialog!");

    Ok(transcription.expect("should always have at least one period"))
}

/// Transcribe a single media file using OpenAI's Whisper model.
async fn transcribe_file(
    path: &Path,
    lang: Lang,
    prompt: &str,
) -> Result<WhisperJson> {
    let client = Client::new();
    let req = CreateTranscriptionRequest {
        file: AudioInput {
            source: async_openai::types::InputSource::Path {
                path: path.to_owned(),
            },
        },
        model: "whisper-1".to_owned(),
        prompt: Some(prompt.to_owned()),
        response_format: Some(AudioResponseFormat::VerboseJson),
        language: Some(lang.to_string()),
        timestamp_granularities: Some(vec![
            TimestampGranularity::Word,
            TimestampGranularity::Segment,
        ]),
        ..Default::default()
    };
    trace!("transcribe request: {:#?}", req);
    let resp = client.audio().transcribe(req).await?;
    trace!("transcribe response: {:#?}", resp);
    let reserialized = serde_json::to_string(&resp)
        .map_err(|e| anyhow!("failed to serialize response: {}", e))?;
    WhisperJson::from_str(&reserialized)
}

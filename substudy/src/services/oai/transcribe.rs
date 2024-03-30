//! Transcribe subtitles using OpenAI's Whisper model.

use std::{
    fmt,
    io::{BufWriter, Write},
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{anyhow, Context as _};
use async_openai::{
    types::{
        AudioInput, AudioResponseFormat, CreateTranscriptionRequest,
        TimestampGranularity,
    },
    Client,
};
use async_trait::async_trait;
use log::{debug, trace};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use tempfile::tempdir;

use crate::{
    ai::{AiRequest, AiRequestStatic},
    import::{import_whisper_json, WhisperJson},
    lang::Lang,
    srt::{AppendWithOffset, SubtitleFile},
    ui::{ProgressConfig, Ui},
    vad::segment_on_dialog_breaks,
    video::{Extraction, ExtractionSpec, Id3Metadata, Video},
    Result,
};

/// Output format for subtitle transcriptions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranscriptionFormat {
    /// SubRip format, post-processed by `substudy`.
    Srt,

    /// SRT format generated entirely by Whisper. This may work better for some
    /// languages, especially those which don't use spaces between words.
    WhisperSrt,

    /// Whisper JSON format. Very low level.
    WhisperJson,
}

impl TranscriptionFormat {
    /// Transcribe a video file to the specified format, and write the result.
    pub async fn write_transcription<W>(
        &self,
        ui: &Ui,
        video: &Video,
        prompt: &str,
        writer: &mut BufWriter<W>,
    ) -> Result<()>
    where
        // We do sync writing here, because async file writing is basically
        // fake, and because the files are small text files that we can normally
        // write with a single syscall.
        W: Write,
    {
        match self {
            TranscriptionFormat::Srt => {
                let srt = transcribe_subtitles_to_substudy_srt_file(ui, video, prompt)
                    .await?;
                writer
                    .write_all(srt.to_string().as_bytes())
                    .context("failed to write SRT transcription")?;
            }
            TranscriptionFormat::WhisperSrt => {
                let srt =
                    transcribe_subtitles::<SubtitleFile>(ui, video, prompt).await?;
                writer
                    .write_all(srt.to_string().as_bytes())
                    .context("failed to write SRT transcription")?;
            }
            TranscriptionFormat::WhisperJson => {
                let json =
                    transcribe_subtitles::<WhisperJson>(ui, video, prompt).await?;
                serde_json::to_writer(writer, &json)
                    .context("failed to write Whisper JSON transcription")?;
            }
        }
        Ok(())
    }
}

impl FromStr for TranscriptionFormat {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "srt" => Ok(TranscriptionFormat::Srt),
            "whisper-srt" => Ok(TranscriptionFormat::WhisperSrt),
            "whisper-json" => Ok(TranscriptionFormat::WhisperJson),
            _ => Err(anyhow::anyhow!("invalid transcription format: {}", s)),
        }
    }
}

impl fmt::Display for TranscriptionFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TranscriptionFormat::Srt => write!(f, "srt"),
            TranscriptionFormat::WhisperSrt => write!(f, "whisper-srt"),
            TranscriptionFormat::WhisperJson => write!(f, "whisper-json"),
        }
    }
}

/// Transcribe a video file to SRT format.
async fn transcribe_subtitles_to_substudy_srt_file(
    ui: &Ui,
    video: &Video,
    prompt: &str,
) -> Result<SubtitleFile> {
    let whisper_json = transcribe_subtitles(&ui, video, prompt).await?;
    import_whisper_json(&whisper_json)
}

/// Transcribe a video file to Whisper JSON format.
async fn transcribe_subtitles<Subs>(
    ui: &Ui,
    video: &Video,
    prompt: &str,
) -> Result<Subs>
where
    Subs: TranscribeFile + DeserializeOwned + Serialize + Send + Sync,
{
    // Find our language.
    let lang = Lang::for_text(prompt)
        .ok_or_else(|| anyhow!("could not infer language from example text"))?;

    // Figure out where to split the video to fit under the 25 MB limit.
    let stream = video.audio_track_for(lang);
    let periods = segment_on_dialog_breaks(ui, video, stream, 10.0 * 60.0).await?;
    debug!("split into periods: {:?}", periods);

    // Extract audio tracks for transcription.
    let temp_dir = tempdir().context("failed to create temporary directory")?;
    let extractions = periods
        .iter()
        .cloned()
        .enumerate()
        .map(|(i, period)| {
            let mut file_name = video.file_stem().to_owned();
            file_name.push(format!("_{}.mp3", i));
            let path = temp_dir.path().join(&file_name);
            Extraction {
                path,
                spec: ExtractionSpec::Audio {
                    stream,
                    period,
                    metadata: Id3Metadata::default(),
                },
            }
        })
        .collect::<Vec<_>>();
    video.extract(&ui, &extractions).await?;

    // Transcribe the audio tracks.
    let reqs = extractions
        .iter()
        .map(|extraction| TranscriptionRequest {
            path: extraction.path.clone(),
            lang,
            prompt: prompt.to_owned(),
            _phantom: std::marker::PhantomData::<Subs>,
        })
        .collect::<Vec<_>>();
    let resps = TranscriptionRequest::perform_requests(ui, reqs).await?;

    let transcription = resps
        .into_iter()
        .zip(periods.into_iter().map(|p| p.begin()))
        .reduce(|(mut a_subs, a_start), (b_subs, b_start)| {
            a_subs.append_with_offset(b_subs, b_start);
            (a_subs, a_start)
        });
    Ok(transcription
        .expect("should always have at least one period")
        .0)
}

/// A request to transcribe a file.
///
/// We don't actually cache these, because they require large files on disk.
#[derive(Debug, Deserialize, Serialize)]
struct TranscriptionRequest<Subs> {
    path: PathBuf,
    lang: Lang,
    prompt: String,
    _phantom: std::marker::PhantomData<Subs>,
}

#[async_trait]
impl<Subs> AiRequest for TranscriptionRequest<Subs>
where
    Subs: TranscribeFile + DeserializeOwned + Serialize + Send + Sync,
{
    type Response = Subs;

    async fn perform(
        &self,
        _error_history: &[anyhow::Error],
    ) -> Result<Self::Response> {
        Subs::transcribe_file(&self.path, self.lang, &self.prompt).await
    }
}

impl<Subs> AiRequestStatic for TranscriptionRequest<Subs>
where
    Subs: TranscribeFile + DeserializeOwned + Serialize + Send + Sync,
{
    fn progress_config() -> &'static ProgressConfig<'static> {
        &ProgressConfig {
            emoji: "🎤",
            msg: "Transcribing audio",
            done_msg: "Transcribed audio",
        }
    }

    fn cache_name() -> &'static str {
        // Since our `cache_size` is 0, we don't actually need a real cache
        // name.
        "unused"
    }

    fn cache_size() -> u64 {
        0
    }

    fn concurrency_limit() -> usize {
        4
    }
}

/// The Whisper model to use.
const WHISPER_MODEL: &str = "whisper-1";

/// Build an [`AudioInput`] source for a path.
fn audio_input_for_path(path: &Path) -> AudioInput {
    AudioInput {
        source: async_openai::types::InputSource::Path {
            path: path.to_owned(),
        },
    }
}

/// Create a new transcription in the specified format.
#[async_trait]
trait TranscribeFile: AppendWithOffset + Sized + 'static {
    async fn transcribe_file(path: &Path, lang: Lang, prompt: &str) -> Result<Self>;
}

#[async_trait]
impl TranscribeFile for WhisperJson {
    /// Transcribe a single media file in JSON format using OpenAI's Whisper model.
    async fn transcribe_file(
        path: &Path,
        lang: Lang,
        prompt: &str,
    ) -> Result<WhisperJson> {
        let client = Client::new();
        let req = CreateTranscriptionRequest {
            file: audio_input_for_path(path),
            model: WHISPER_MODEL.to_owned(),
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
        let resp = client.audio().transcribe_verbose_json(req).await?;
        trace!("transcribe response: {:#?}", resp);
        let reserialized = serde_json::to_string(&resp)
            .map_err(|e| anyhow!("failed to serialize response: {}", e))?;
        WhisperJson::from_str(&reserialized)
    }
}

#[async_trait]
impl TranscribeFile for SubtitleFile {
    /// Transcribe a single media file as in SRT format using OpenAI's Whisper
    /// model.
    async fn transcribe_file(
        path: &Path,
        lang: Lang,
        prompt: &str,
    ) -> Result<SubtitleFile> {
        let client = Client::new();
        let req = CreateTranscriptionRequest {
            file: audio_input_for_path(path),
            model: WHISPER_MODEL.to_owned(),
            prompt: Some(prompt.to_owned()),
            response_format: Some(AudioResponseFormat::Srt),
            language: Some(lang.to_string()),
            ..Default::default()
        };
        trace!("transcribe request: {:#?}", req);
        let srt_bytes = client.audio().transcribe_raw(req).await?;
        let srt_str = String::from_utf8(srt_bytes.into())
            .map_err(|e| anyhow!("failed to convert SRT to UTF-8: {}", e))?;
        trace!("transcribe response: {:#?}", srt_str);
        SubtitleFile::from_str(&srt_str)
    }
}

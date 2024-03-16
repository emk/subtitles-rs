//! OpenAI translation.

use anyhow::anyhow;
use async_openai::{config::OpenAIConfig, types::CreateChatCompletionRequest, Client};
use lazy_static::lazy_static;
use log::{debug, trace};
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::json;

use super::{
    function_tool, function_tool_choice, retry_openai_request, system_message,
    tool_call_response, user_message,
};
use crate::{
    lang::Lang,
    srt::{Subtitle, SubtitleFile},
    ui::Ui,
    Result,
};

/// Always send this many lines in a prompt, and the try to end
/// on a sentence boundary.
const MIN_CHUNK_SIZE: usize = 8;

/// If we can't find a sentence boundary, end no later than this.
const MAX_CHUNK_SIZE: usize = 12;

lazy_static! {
    /// A JSON Schema for the report_translation"function" we tell OpenAI to
    /// call. This is really just the output we want from the LLM.
    static ref REPORT_TRANSLATION_PARAMETERS_SCHEMA: serde_json::Value = json!({
        "type": "object",
        "properties": {
            "lines": {
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "original": {
                            "type": "string"
                        },
                        "translation": {
                            "type": "string"
                        }
                    },
                    "required": [
                        "original",
                        "translation"
                    ]
                }
            }
        },
        "required": [
            "lines"
        ]
    });

    /// Unicode-aware regex for identifying the likely end of a sentence. This
    /// includes ".", "!", "?", plus other punctuation used in other languages.
    static ref SENTENCE_END: Regex =
        Regex::new(r"[\p{Sentence_Terminal}]\s*$").unwrap();
}

/// Translate subtitle lines using OpenAI's GPT API.
pub async fn translate_subtitle_file(
    ui: &Ui,
    file: &SubtitleFile,
    to_lang: Lang,
) -> Result<SubtitleFile> {
    // Infer the language of the subtitle file.
    let from_lang = file.detect_language().ok_or_else(|| {
        anyhow!("Could not detect the language of the input subtitle file")
    })?;

    // Split into chunks of at least `MIN_CHUNK_SIZE`, but then try to end on a
    // sentence boundary. Even if we can't find a sentence boundary, end
    // no later than `MAX_CHUNK_SIZE`.
    let mut sub_chunks = vec![];
    let mut current_chunk = vec![];
    for sub in &file.subtitles {
        current_chunk.push(sub.clone());
        let last_line = sub.lines.last().cloned().unwrap_or_else(|| "".to_owned());
        if current_chunk.len() >= MIN_CHUNK_SIZE
            && (current_chunk.len() >= MAX_CHUNK_SIZE
                || SENTENCE_END.is_match(&last_line))
        {
            sub_chunks.push(current_chunk.clone());
            current_chunk.clear();
        }
    }
    if current_chunk.len() > 0 {
        sub_chunks.push(current_chunk);
    }

    let progress = ui.new_progress_bar(file.subtitles.len() as u64);
    progress.set_prefix("📖");
    progress.set_message("Translating");
    progress.tick();

    let client = Client::new();
    let mut translated_subs = vec![];
    for chunk in &sub_chunks {
        let translated_lines = retry_openai_request(|| {
            translate_chunk(&client, chunk, from_lang, to_lang)
        })
        .await?;
        for (sub, translated) in chunk.iter().zip(translated_lines) {
            let mut translated_sub = sub.clone();
            translated_sub.lines =
                vec![translated.translation.clone().ok_or_else(|| {
                    anyhow!(
                        "OpenAI did not return a translation for a line: {:?}",
                        translated.original
                    )
                })?];
            translated_subs.push(translated_sub);
        }
        progress.inc(chunk.len() as u64);
    }
    progress.finish_with_message("Translated subtitles");

    // Reassemble the translated chunks.
    Ok(SubtitleFile {
        subtitles: translated_subs,
    })
}

async fn translate_chunk(
    client: &Client<OpenAIConfig>,
    chunk: &[Subtitle],
    from_lang: Lang,
    to_lang: Lang,
) -> Result<Vec<LineTranslation>> {
    let prompt = prompt_from_chunk(chunk, from_lang, to_lang)?;
    debug!("OpenAI request (prompt): {}", prompt);
    let req = CreateChatCompletionRequest {
        model: "gpt-3.5-turbo".to_owned(),
        messages: vec![
            system_message("You are a subtitle translator helping language learners."),
            user_message(prompt),
        ],
        tools: Some(vec![function_tool(
            "report_translations",
            "Report the translations of the lines of dialog.",
            &REPORT_TRANSLATION_PARAMETERS_SCHEMA,
        )]),
        tool_choice: Some(function_tool_choice("report_translations")),
        ..Default::default()
    };
    trace!("OpenAI request (full): {:?}", req);
    let resp = client.chat().create(req).await?;
    trace!("OpenAI response (full): {:?}", resp);
    let args = tool_call_response::<ReportTranslationParameters>(
        &resp,
        "report_translations",
    )?;
    let translated_lines = args.lines;
    if translated_lines.len() != chunk.len() {
        return Err(anyhow!(
            "OpenAI returned the wrong number of translations: {}",
            translated_lines.len()
        ));
    }
    Ok(translated_lines)
}

/// Generate a prompt from a chunk of subtitles.
fn prompt_from_chunk(
    chunk: &[Subtitle],
    from_lang: Lang,
    to_lang: Lang,
) -> Result<String> {
    let template = ReportTranslationParameters {
        lines: chunk
            .iter()
            .map(LineTranslation::template_from_subtitle)
            .collect(),
    };
    let json_template =
        serde_json::to_string_pretty(&template).expect("failed to format JSON");
    Ok(format!(
        "Translate the following consecutive lines of dialog from {from} to {to}:

```json\n{template}```

Please call the function `report_translation` with your output, maintaing the right number of lines.",
        from = from_lang.english_names()?[0],
        to = to_lang.english_names()?[0],
        template = json_template,
    ))
}

/// "Parameters" for the `report_translation` function.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ReportTranslationParameters {
    /// The translated lines.
    pub lines: Vec<LineTranslation>,
}

/// Translation of a line.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LineTranslation {
    /// The original line.
    pub original: String,
    /// The translated line.
    pub translation: Option<String>,
}

impl LineTranslation {
    /// Construct a template from a [`Subtitle`].
    pub fn template_from_subtitle(sub: &Subtitle) -> LineTranslation {
        LineTranslation {
            original: sub.lines.join(" "),
            translation: None,
        }
    }
}

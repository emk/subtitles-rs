//! OpenAI client.

use std::{future::Future, time::Duration};

use anyhow::{anyhow, Context};
use async_openai::types::{
    ChatCompletionNamedToolChoice, ChatCompletionRequestMessage,
    ChatCompletionRequestSystemMessage, ChatCompletionRequestUserMessage,
    ChatCompletionRequestUserMessageContent, ChatCompletionTool,
    ChatCompletionToolChoiceOption, ChatCompletionToolType,
    CreateChatCompletionResponse, FunctionName, FunctionObject, Role,
};
use log::debug;
use tokio::time::sleep;

use crate::Result;

pub use self::{
    transcribe::{
        transcribe_subtitles_to_srt_file, transcribe_subtitles_to_whisper_json,
        TranscriptionFormat,
    },
    translate::translate_subtitle_file,
};

mod transcribe;
mod translate;

/// Retry an OpenAI request a few times.
async fn retry_openai_request<T, Func, Fut>(f: Func) -> Result<T>
where
    Func: Fn() -> Fut,
    Fut: Future<Output = Result<T>>,
    T: std::fmt::Debug + Send,
{
    let mut max_tries = 3;
    loop {
        let result = f().await;
        max_tries -= 1;
        match result {
            Ok(t) => return Ok(t),
            Err(e) if max_tries == 0 => return Err(e),
            Err(e) => {
                log::warn!("OpenAI request failed, retrying: {:?}", e);
                sleep(Duration::from_secs(2)).await;
            }
        }
    }
}

// let mut max_tries = 3;
// let translated_lines = loop {
//     let result = translate_chunk(&client, chunk, from_lang, to_lang).await;
//     max_tries -= 1;
//     match result {
//         Ok(lines) => break lines,
//         Err(e) if max_tries == 0 => {
//             return Err(e);
//         }
//         Err(e) => {
//             warn!("Failed to translate chunk, retrying: {}", e);
//             sleep(Duration::from_secs(2)).await;
//         }
//     }
// };

/// Generate a system message.
fn system_message(content: &str) -> ChatCompletionRequestMessage {
    ChatCompletionRequestMessage::System(ChatCompletionRequestSystemMessage {
        role: Role::System,
        content: content.to_owned(),
        name: None,
    })
}

/// Generate a user message.
fn user_message<S: Into<String>>(content: S) -> ChatCompletionRequestMessage {
    ChatCompletionRequestMessage::User(ChatCompletionRequestUserMessage {
        role: Role::User,
        content: ChatCompletionRequestUserMessageContent::Text(content.into()),
        name: None,
    })
}

/// Describe a "function" tool GPT can call.
fn function_tool(
    name: &str,
    description: &str,
    parameters: &serde_json::Value,
) -> ChatCompletionTool {
    ChatCompletionTool {
        r#type: ChatCompletionToolType::Function,
        function: FunctionObject {
            name: name.to_owned(),
            description: Some(description.to_owned()),
            parameters: Some(parameters.clone()),
        },
    }
}

/// Specify a "function" tool GPT should call.
fn function_tool_choice(name: &str) -> ChatCompletionToolChoiceOption {
    ChatCompletionToolChoiceOption::Named(ChatCompletionNamedToolChoice {
        r#type: ChatCompletionToolType::Function,
        function: FunctionName {
            name: name.to_owned(),
        },
    })
}

/// Extract a "tool call" from a chat response.
fn tool_call_response<T>(
    resp: &CreateChatCompletionResponse,
    expected_function: &str,
) -> Result<T>
where
    T: serde::de::DeserializeOwned,
{
    let choice = resp.choices.get(0).ok_or_else(|| {
        anyhow!("OpenAI did not return a response to our translation request")
    })?;
    let tool_calls =
        choice.message.tool_calls.as_ref().ok_or_else(|| {
            anyhow!("OpenAI did not return tool calls in its response")
        })?;
    let tool_call = tool_calls
        .get(0)
        .ok_or_else(|| anyhow!("OpenAI did not return a tool call in its response"))?;
    let f = &tool_call.function;
    if f.name != expected_function {
        return Err(anyhow!(
            "OpenAI returned a response, but it called the wrong function: {}",
            f.name
        ));
    }
    debug!("OpenAI called: {}({:?})", expected_function, f.arguments);
    serde_json::from_str::<T>(&f.arguments).context("Failed to parse OpenAPI response")
}

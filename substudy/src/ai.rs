//! Reusable interface to large language models, and to things which work
//! similarly to them.

use std::time::Duration;

use anyhow::anyhow;
use async_trait::async_trait;
use futures::{stream, StreamExt as _};
use log::warn;
use serde::{de::DeserializeOwned, Serialize};
use tokio::time::sleep;

use crate::{
    cache::Cache,
    ui::{ProgressConfig, Ui},
    Error, Result,
};

/// A request made to a potentially non-deteministic AI model, which may fail.
///
/// This is intended to support multiple AI services, not just OpenAI.
#[async_trait]
pub(crate) trait AiRequest:
    DeserializeOwned + Serialize + Send + Sync + 'static
{
    /// The response type.
    type Response: DeserializeOwned + Serialize + Send + Sync + 'static;

    /// Return the maximum number of times to try this request.
    fn max_tries(&self) -> u32 {
        3
    }

    /// Our progress increment, used to update a progress bar.
    fn progress_increment(&self) -> u64 {
        1
    }

    /// Invoke this request.
    ///
    /// If this request has been tried previously, `error_history` will contain
    /// the errors from those attempts.
    async fn perform(&self, error_history: &[Error]) -> Result<Self::Response>;

    /// Invoke this request, retrying errors, and caching successful requests
    /// using a persistent LRU cache.
    async fn perform_cached(
        &self,
        cache: &Cache<Self, Self::Response>,
    ) -> Result<Self::Response> {
        if let Some(cached) = cache
            .get(&self)
            .map_err(|e| anyhow!("could not read cache: {}", e))?
        {
            return Ok(cached);
        }
        let mut error_history = Vec::new();
        for _ in 0..self.max_tries() {
            match self.perform(&error_history).await {
                Ok(response) => {
                    cache
                        .put(&self, &response)
                        .map_err(|e| anyhow!("could not write cache: {}", e))?;
                    return Ok(response);
                }
                Err(e) => {
                    warn!("AI request failed, retrying: {:?}", e);
                    error_history.push(e);
                    sleep(Duration::from_secs(2)).await;
                }
            }
        }
        Err(error_history.pop().unwrap())
    }
}

/// Static methods for AI requests.
pub(crate) trait AiRequestStatic: AiRequest {
    /// Progress bar configuration.
    fn progress_config() -> &'static ProgressConfig<'static>;

    /// The basename for the cache file.
    fn cache_name() -> &'static str;

    /// The maximum number of items to cache.
    fn cache_size() -> u64;

    /// Concurrency limit for AI requests.
    fn concurrency_limit() -> usize;

    /// Perform a series of AI requests concurrently, handling caching and retries.
    async fn perform_requests(
        ui: &Ui,
        requests: Vec<Self>,
    ) -> Result<Vec<Self::Response>> {
        // Initialize our progress bar.
        let total_increments = requests.iter().map(|r| r.progress_increment()).sum();
        let pb = ui.new_progress_bar(Self::progress_config(), total_increments);

        // Open our cache.
        let file_name = format!("{}.cache", Self::cache_name());
        let cache_path = dirs::cache_dir()
            .ok_or_else(|| anyhow!("could not find cache directory"))?
            .join("substudy")
            .join("cache.sqlite3");
        let cache = Cache::new(cache_path, &file_name, Self::cache_size())?;

        //let pb2 = pb.clone();
        let pb_ref = &pb;
        let cache = &cache;
        let futures = stream::iter(requests)
            .map(move |r| async move {
                match r.perform_cached(&cache).await {
                    Ok(response) => {
                        pb_ref.inc(r.progress_increment());
                        Ok(response)
                    }
                    Err(e) => Err(e),
                }
            })
            .buffered(Self::concurrency_limit());
        let result = futures
            .collect::<Vec<_>>()
            .await
            .into_iter()
            .collect::<Result<Vec<_>>>();
        ui.finish(Self::progress_config(), pb);
        result
    }
}

#[cfg(test)]
mod tests {
    use serde::Deserialize;

    use super::*;

    #[derive(Debug, Deserialize, Serialize)]
    struct TestRequest {
        value: u32,
    }

    #[async_trait]
    impl AiRequest for TestRequest {
        type Response = u32;

        async fn perform(&self, _error_history: &[Error]) -> Result<Self::Response> {
            Ok(self.value)
        }
    }

    impl AiRequestStatic for TestRequest {
        fn progress_config() -> &'static ProgressConfig<'static> {
            &ProgressConfig {
                emoji: "🧪",
                msg: "Testing",
                done_msg: "Tested",
            }
        }

        fn cache_name() -> &'static str {
            "ai_request_test"
        }

        fn cache_size() -> u64 {
            2
        }

        fn concurrency_limit() -> usize {
            2
        }
    }

    #[tokio::test]
    async fn test_ai_request_static() {
        let _ = env_logger::builder().is_test(true).try_init();
        let requests = vec![
            TestRequest { value: 1 },
            TestRequest { value: 2 },
            TestRequest { value: 3 },
        ];
        let ui = Ui::init_for_tests();
        let responses = TestRequest::perform_requests(&ui, requests).await.unwrap();
        assert_eq!(responses, vec![1, 2, 3]);
    }
}

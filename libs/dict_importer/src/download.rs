//! Download a bilingual dictionary from [FreeDict](https://freedict.org/).

use std::collections::HashMap;

use isolang::Language;
use serde::{Deserialize, Deserializer};
use tokio::task::spawn_blocking;
use tracing::{debug, instrument};

use crate::{
    dictd,
    errors::{Error, Result},
    LanguagePair,
};

/// Information about a download.
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase", untagged)]
enum Download {
    Extra(ExtraInfo),
    Dictionary(DictionaryInfo),
}

impl Download {
    /// Download dictionary metadata using `reqwest`.
    #[instrument]
    async fn list_available() -> Result<Vec<Self>> {
        let url = "https://freedict.org/freedict-database.json";
        debug!(url, "Downloading dictionary list");
        let response = reqwest::get(url)
            .await
            .map_err(|e| Error::could_not_access_url(url, e))?;
        let dicts = response
            .json()
            .await
            .map_err(|e| Error::could_not_access_url(url, e))?;
        Ok(dicts)
    }
}

/// Information about a dictionary.
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
pub(crate) struct DictionaryInfo {
    date: String,
    edition: String,
    #[serde(deserialize_with = "deserialize_u64_from_string")]
    headwords: u64,
    maintainer_name: String,
    name: String,
    releases: Vec<Release>,
    #[serde(rename = "sourceURL")]
    source_url: Option<String>,
    status: Option<String>,
}

impl DictionaryInfo {
    /// Language codes for the foreign and native languages.
    fn language_pair(&self) -> Result<LanguagePair> {
        let parts = self
            .name
            .split_once('-')
            .ok_or_else(|| Error::UnknownLanguageCodes {})?;
        let foreign = Language::from_639_3(parts.0)
            .ok_or_else(|| Error::UnknownLanguageCodes {})?;
        let native = Language::from_639_3(parts.1)
            .ok_or_else(|| Error::UnknownLanguageCodes {})?;
        Ok(LanguagePair { foreign, native })
    }

    /// Get release information for a specific language pair and dictionary format.
    pub(crate) async fn for_language_pair(
        lang_pair: &LanguagePair,
    ) -> Result<Option<Self>> {
        let dicts = Download::list_available().await?;
        for d in dicts {
            if let Download::Dictionary(dict) = d {
                if dict.language_pair().as_ref().ok() == Some(lang_pair) {
                    return Ok(Some(dict));
                }
            }
        }
        Ok(None)
    }

    /// Get the latest release for a specific dictionary format.
    fn release_for_platform(&self, platform: &Platform) -> Option<&Release> {
        self.releases.iter().find(|r| &r.platform == platform)
    }

    /// Import the [`Platform::Dictd`] dictionary, if present.
    #[instrument(skip(self), fields(name = %self.name))]
    pub(crate) async fn import_dictd(&self) -> Result<Option<dictd::Dictionary>> {
        if let Some(release) = self.release_for_platform(&Platform::Dictd) {
            debug!(url = %release.url, "Downloading dictionary");
            let data = reqwest::get(&release.url)
                .await
                .map_err(|e| Error::could_not_access_url(&release.url, e))?
                .bytes()
                .await
                .map_err(|e| Error::could_not_access_url(&release.url, e))?
                .to_vec();
            let dictionary = spawn_blocking(move || {
                dictd::Dictionary::from_archive_reader(&data[..])
            })
            .await
            .map_err(|e| Error::could_not_load_dictionary(e))??;
            Ok(Some(dictionary))
        } else {
            Ok(None)
        }
    }
}

/// Information about a release of a dictionary.
#[derive(Clone, Debug, Deserialize, PartialEq)]
struct Release {
    #[serde(rename = "URL")]
    url: String,
    checksum: String,
    date: String,
    platform: Platform,
    #[serde(deserialize_with = "deserialize_u64_from_string")]
    size: u64,
    version: String,
}

/// A dictionary's platform.
#[derive(Clone, Debug, PartialEq)]
enum Platform {
    Stardict,
    Src,
    Dictd,
    Slob,
    Other(String),
}

impl<'de> Deserialize<'de> for Platform {
    fn deserialize<D>(deserializer: D) -> Result<Platform, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(match s.as_str() {
            "stardict" => Platform::Stardict,
            "src" => Platform::Src,
            "dictd" => Platform::Dictd,
            "slob" => Platform::Slob,
            _ => Platform::Other(s),
        })
    }
}

/// Deserialize a `u64` from a string.
fn deserialize_u64_from_string<'de, D>(deserializer: D) -> Result<u64, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    s.parse().map_err(serde::de::Error::custom)
}

/// Information about downloadable software.
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
struct ExtraInfo {
    software: HashMap<String, Software>,
}

/// Information about software.
#[derive(Clone, Debug, Deserialize, PartialEq)]
#[serde(rename_all = "camelCase")]
struct Software {
    #[serde(rename = "URL")]
    url: String,
    checksum: String,
    date: String,
    version: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_language_codes() {
        let dict = DictionaryInfo {
            date: "2020-01-01".to_string(),
            edition: "0.0.0".to_string(),
            headwords: 4000,
            maintainer_name: "FreeDict".to_string(),
            name: "afr-deu".to_string(),
            releases: vec![],
            source_url: Some("http://freedict.org/".to_string()),
            status: Some("too small".to_string()),
        };
        let LanguagePair { foreign, native } = dict.language_pair().unwrap();
        assert_eq!(foreign, Language::Afr);
        assert_eq!(native, Language::Deu);
    }
}

//! Error handling.

use std::{error::Error as StdError, result::Result as StdResult};

/// Our result type.
pub type Result<T, E = Error> = StdResult<T, E>;

/// An error that can occur when working with a dictionary.
#[derive(Debug, thiserror::Error)]
#[non_exhaustive]
pub enum Error {
    /// We encoutered a corrupted index.
    #[error("Corrupted dictionary index")]
    #[non_exhaustive]
    CorruptedIndex {},

    /// We could not fetch a list of dictionaries.
    #[error("Error accessing {url}")]
    #[non_exhaustive]
    CouldNotAccessUrl {
        /// The URL that we could not access.
        url: String,

        /// The original error.
        #[source]
        source: Box<dyn StdError + Send + Sync + 'static>,
    },

    /// Cannot load a dictionary.
    #[error("Could not load the dictionary")]
    #[non_exhaustive]
    CouldNotLoadDictionary {
        /// The original error.
        #[source]
        source: Box<dyn StdError + Send + Sync + 'static>,
    },

    /// No language codes found for a dictionary.
    #[error("No language codes found for the dictionary")]
    #[non_exhaustive]
    UnknownLanguageCodes {},
}

impl Error {
    #[cfg(feature = "download")]
    pub(crate) fn could_not_access_url<S: Into<String>>(
        url: S,
        e: reqwest::Error,
    ) -> Self {
        Self::CouldNotAccessUrl {
            url: url.into(),
            source: Box::new(e),
        }
    }

    pub(crate) fn could_not_load_dictionary<E>(e: E) -> Self
    where
        //E: StdError + Send + Sized + Sync + 'static,
        E: Into<Box<dyn StdError + Send + Sync + 'static>>,
    {
        Self::CouldNotLoadDictionary { source: e.into() }
    }
}

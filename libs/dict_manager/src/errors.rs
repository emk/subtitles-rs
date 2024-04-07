//! Error handling.

use dict_importer::LanguagePair;
use thiserror::Error;

/// Our error type.
#[derive(Debug, Error)]
pub enum Error {
    /// Could not access a dictionary.
    #[error("Could not access dictionary")]
    #[non_exhaustive]
    DictionaryAccessFailed {
        #[source]
        source: Box<dyn std::error::Error + Send + Sync + 'static>,
    },

    /// Could not create a dictionary.
    #[error("Could not create dictionary")]
    #[non_exhaustive]
    DictionaryCreationFailed {
        #[source]
        source: Box<dyn std::error::Error + Send + Sync + 'static>,
    },

    /// Could not find a matching dictionary.
    #[error("Could not find a matching dictionary")]
    #[non_exhaustive]
    DictionaryNotFound { lang_pair: LanguagePair },

    /// Incompatible database version.
    #[error("Incompatible database version: found {found_version}, supported {required_version}")]
    #[non_exhaustive]
    IncompatibleDatabaseVersion {
        found_version: String,
        required_version: String,
    },

    /// Could not find the local data directory for storing dictionaries.
    #[error("Could not find local data directory")]
    #[non_exhaustive]
    LocalDataDirectoryNotFound {},

    /// An error occurred importing the dictionary.
    #[error("Error importing dictionary")]
    ImportError(#[from] dict_importer::Error),
}

impl Error {
    /// Failed to access a dictionary.
    pub(crate) fn dictionary_access<E>(source: E) -> Self
    where
        E: Into<Box<dyn std::error::Error + Send + Sync>>,
    {
        Error::DictionaryAccessFailed {
            source: source.into(),
        }
    }

    /// Failed to create a dictionary.
    pub(crate) fn dictionary_creation<E>(source: E) -> Self
    where
        E: Into<Box<dyn std::error::Error + Send + Sync>>,
    {
        Error::DictionaryCreationFailed {
            source: source.into(),
        }
    }
}

/// Our result type.
pub type Result<T> = std::result::Result<T, Error>;

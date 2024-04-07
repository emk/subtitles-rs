//! Import dictionaries.
//!
//! We primarily focus on dictionaries from [FreeDict](https://freedict.org/),
//! but we might be interested in patches to support other major sources of
//! dictionaries.
//!
//! Right now, we support `dictd`-format dictionaries.
//!
//! # Features
//!
//! - `download`: Download dictionaries from the internet.
//! - `unpack`: Unpack dictionaries from archives.
//!
//! # Example
//!
//! This crate is intended to be used for bulk imports of dictionaries. Here's
//! an example of how to download a dictionary and read the metadata and some
//! definitions from it.
//!
//! ```no_run
//! # #[cfg(feature = "download")]
//! # async fn run() -> Result<(), Box<dyn std::error::Error>> {
//! use dict_import::{dictd, LanguagePair};
//!
//! let lang_pair = LanguagePair::from_639_1("es", "en")
//!     .ok_or("Invalid language codes")?;
//! let dict = dictd::Dictionary::from_freedict(&lang_pair).await?
//!     .ok_or("Dictionary not found")?;
//! for entry in dict.entries().take(20) {
//!     let entry = entry?;
//!     match entry {
//!         dictd::Entry::Metadata { key, value } => println!("META: {}={}", key, value),
//!         dictd::Entry::Word { headword, definition } => println!("{}: {}", headword, definition),
//!         _ => { /* Ignore other entry types */ }
//!     }
//! }
//! # Ok(()) }
//! ```

use isolang::Language;

pub use errors::{Error, Result};

pub mod dictd;
#[cfg(feature = "download")]
mod download;
mod errors;

/// A pair of language codes.
///
/// We use the terminology "foreign" and "native" because it's less
/// ambiguous than "source" and "target". Language learners often refer to
/// the language they're learning as the "target" language, for example.
///
/// Technically, the "native" may not be the user's actual native language,
/// but a language in which they can comfortably read definitions.
#[derive(Clone, Debug, PartialEq)]
pub struct LanguagePair {
    pub foreign: Language,
    pub native: Language,
}

impl LanguagePair {
    /// Create a new `LanguagePair` from ISO 639-1 language codes.
    pub fn from_639_1(foreign: &str, native: &str) -> Option<Self> {
        let foreign = Language::from_639_1(foreign)?;
        let native = Language::from_639_1(native)?;
        Some(Self { foreign, native })
    }

    /// Create a new `LanguagePair` from ISO 639-3 language codes.
    pub fn from_639_3(foreign: &str, native: &str) -> Option<Self> {
        let foreign = Language::from_639_3(foreign)?;
        let native = Language::from_639_3(native)?;
        Some(Self { foreign, native })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn language_pair_from_639_1() {
        let pair = LanguagePair::from_639_1("en", "es").unwrap();
        assert_eq!(pair.foreign, Language::from_639_1("en").unwrap());
        assert_eq!(pair.native, Language::from_639_1("es").unwrap());
    }

    #[test]
    fn language_pair_from_639_3() {
        let pair = LanguagePair::from_639_3("eng", "spa").unwrap();
        assert_eq!(pair.foreign, Language::from_639_3("eng").unwrap());
        assert_eq!(pair.native, Language::from_639_3("spa").unwrap());
    }
}

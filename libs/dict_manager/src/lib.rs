//! Manage bilingual dictionaries.
//!
//! This library provides functions to download and query bilingual
//! dictionaries. We focus on dictionaries supplied by the [FreeDict
//! project](https://freedict.org/).
//!
//! We store our dictionaries in a SQLite database. This is relatively simple,
//! and fast enough for our needs.

use std::{fs, path::PathBuf};

use dict_importer::dictd;
pub use dict_importer::LanguagePair;
pub use isolang::Language;
use semver::{Version, VersionReq};
use tokio::task::spawn_blocking;
use tracing::{debug, instrument, trace, warn};

pub use crate::errors::{Error, Result};

mod errors;

/// What version of the database format do we produce?
static DATABASE_FORMAT_VERSION: Version = Version::new(1, 0, 0);

/// What versions of the database format do we accept?
static SUPPORTED_DATABASE_FORMAT_VERSIONS: &str = "^1.0.0";

/// What kind of matches should we return?
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum MatchType {
    /// Return only exact headword matches.
    Exact,
    /// Return any exact headword matches, followed by any other words which
    /// match the stem. Stems are computed using
    /// [Snowball](https://crates.io/crates/rust-stemmers) where available,
    /// though we might use other algorithms in the future.
    IncludeStemMatches,
}

/// Check to see if we have a dictionary. If not, try to download one. Return
/// `false` if no dictionary can be found.
#[instrument(skip(lang_pair))]
pub async fn dictionary_can_be_found(lang_pair: &LanguagePair) -> Result<bool> {
    let db_path = dictionary_path(lang_pair)?;
    if db_path.exists() {
        trace!(?lang_pair, "Dictionary found");
        return Ok(true);
    }
    match fetch_dictionary(lang_pair).await {
        Ok(_) => Ok(true),
        Err(Error::DictionaryNotFound { .. }) => {
            debug!(?lang_pair, "Dictionary not found");
            Ok(false)
        }
        Err(e) => Err(e),
    }
}

/// Look up a word in the dictionary. Returns matching entries, as specified by
/// `match_type`.
#[instrument(skip(lang_pair, match_type))]
pub fn look_up_dictionary_entry(
    lang_pair: &LanguagePair,
    word: &str,
    match_type: MatchType,
) -> Result<Vec<Entry>> {
    let db_path = dictionary_path(lang_pair)?;
    debug!(?db_path, %word, "Looking up word");
    let conn =
        rusqlite::Connection::open(&db_path).map_err(Error::dictionary_access)?;

    // Begin a transaction, so we see an atomic view of the dictionary.
    conn.execute("BEGIN", [])
        .map_err(Error::dictionary_access)?;

    // Check that the database format is compatible.
    check_database_format_version(&conn)?;

    // Look up the word in the dictionary.
    let mut matches = conn
        .prepare(
            "SELECT headword, definition FROM entries WHERE headword = ? AND definition_format = 'text'",
        )
        .map_err(Error::dictionary_access)?
        .query_map([word], |row| {
            Ok(Entry {
                headword: row.get(0)?,
                is_exact_match: true,
                text: row.get(1)?,
            })
        })
        .map_err(Error::dictionary_access)?
        .map(|r| r.map_err(Error::dictionary_access))
        .collect::<Result<Vec<_>>>()?;
    trace!(matches = matches.len(), "Found exact matches");

    // Also look up the stem, if applicable.
    if match_type == MatchType::IncludeStemMatches {
        let stem = word_stem(&lang_pair.foreign, word);
        if let Some(stem) = stem {
            let stem_matches = conn
                .prepare(
                    "SELECT headword, definition FROM entries WHERE stem = ? AND definition_format = 'text'",
                )
                .map_err(Error::dictionary_access)?
                .query_map([&stem], |row| {
                    Ok(Entry {
                        headword: row.get(0)?,
                        is_exact_match: false,
                        text: row.get(1)?,
                    })
                })
                .map_err(Error::dictionary_access)?
                .map(|r| r.map_err(Error::dictionary_access))
                .collect::<Result<Vec<_>>>()?;
            trace!(matches = stem_matches.len(), "Found steam matches");
            matches.extend(stem_matches);
        }
    }
    Ok(matches)
}

/// A dictionary entry.
#[derive(Debug)]
pub struct Entry {
    pub headword: String,
    pub is_exact_match: bool,
    text: String,
}

impl Entry {
    /// Return the complete dictionary entry as text.
    pub fn text(&self) -> &str {
        &self.text
    }
}

/// Get our data directory.
fn data_local_dir() -> Result<PathBuf> {
    Ok(dirs::data_local_dir()
        .ok_or(Error::LocalDataDirectoryNotFound {})?
        .join("dict_manager"))
}

/// Build a dictionary name.
fn dictionary_name(lang_pair: &LanguagePair) -> String {
    format!(
        "{}-{}",
        lang_pair.foreign.to_639_3(),
        lang_pair.native.to_639_3()
    )
}

/// Path to a dictionary.
fn dictionary_path(lang_pair: &LanguagePair) -> Result<PathBuf> {
    let data_dir = data_local_dir()?;
    Ok(data_dir.join(format!("{}.sqlite3", dictionary_name(lang_pair))))
}

/// Fetch a dictionary.
#[instrument(skip(lang_pair))]
async fn fetch_dictionary(lang_pair: &LanguagePair) -> Result<()> {
    let dict = dictd::Dictionary::from_freedict(lang_pair).await?.ok_or(
        Error::DictionaryNotFound {
            lang_pair: lang_pair.clone(),
        },
    )?;
    let lang_pair = lang_pair.to_owned();
    spawn_blocking(move || write_dictionary(&lang_pair, dict))
        .await
        .map_err(Error::dictionary_creation)??;
    Ok(())
}

/// Check the database format version for compatibility. Call this from within a
/// transaction.
fn check_database_format_version(conn: &rusqlite::Connection) -> Result<()> {
    let version: String = conn
        .query_row("SELECT version FROM db_format", [], |row| row.get(0))
        .map_err(Error::dictionary_creation)?;
    let version =
        Version::parse(&version).map_err(|_| Error::IncompatibleDatabaseVersion {
            found_version: version,
            required_version: SUPPORTED_DATABASE_FORMAT_VERSIONS.to_string(),
        })?;
    let req = VersionReq::parse(SUPPORTED_DATABASE_FORMAT_VERSIONS)
        .expect("Invalid version requirement");
    if req.matches(&version) {
        Ok(())
    } else {
        Err(Error::IncompatibleDatabaseVersion {
            found_version: version.to_string(),
            required_version: req.to_string(),
        })
    }
}

/// Write a dictionary to disk.
#[instrument(skip(lang_pair, dict))]
fn write_dictionary(lang_pair: &LanguagePair, dict: dictd::Dictionary) -> Result<()> {
    let db_path = dictionary_path(lang_pair)?;
    debug!(?db_path, ?lang_pair, "Writing dictionary to disk");
    let parent = db_path
        .parent()
        .ok_or(Error::LocalDataDirectoryNotFound {})?;
    fs::create_dir_all(parent).map_err(Error::dictionary_creation)?;
    let conn =
        rusqlite::Connection::open(&db_path).map_err(Error::dictionary_creation)?;

    // Begin a transaction, so that we can't leave a dictionary in a
    // half-created state.
    conn.execute("BEGIN", [])
        .map_err(Error::dictionary_creation)?;

    // Create our tables and indices.
    create_tables(&conn)?;

    // Iterate over our entries, inserting them into the database.
    for entry in dict.entries() {
        let entry = entry.map_err(Error::ImportError)?;
        match entry {
            dictd::Entry::Metadata { key, value } => {
                conn.execute(
                    "INSERT INTO metadata (key, value) VALUES (?, ?)
                     ON CONFLICT (key) DO UPDATE SET value = excluded.value",
                    (key, value),
                )
                .map_err(Error::dictionary_creation)?;
            }
            dictd::Entry::Word {
                headword,
                definition,
            } => {
                let stem = word_stem(&lang_pair.foreign, &headword);
                conn.execute(
                    "INSERT INTO entries (stem, headword, definition_format, definition)
                     VALUES (?, ?, 'text', ?)",
                    (stem, headword, definition),
                )
                .map_err(Error::dictionary_creation)?;
            }
            _ => { /* Ignore other entry types */ }
        }
    }

    // Finally, commit the transaction.
    conn.execute("COMMIT", [])
        .map_err(Error::dictionary_creation)?;
    Ok(())
}

/// Create tables and indices.
fn create_tables(conn: &rusqlite::Connection) -> Result<()> {
    // Create our database schema.
    conn.execute(
        "CREATE TABLE IF NOT EXISTS db_format (version TEXT NOT NULL PRIMARY KEY)",
        [],
    )
    .map_err(Error::dictionary_creation)?;

    // Insert the database format version, but only if it doesn't already exist. Then
    // check that the version is compatible.
    conn.execute(
        "INSERT INTO db_format (version) VALUES (?) ON CONFLICT (version) DO NOTHING",
        [DATABASE_FORMAT_VERSION.to_string()],
    )
    .map_err(Error::dictionary_creation)?;
    check_database_format_version(conn)?;

    conn.execute(
        "CREATE TABLE IF NOT EXISTS metadata (key TEXT NOT NULL PRIMARY KEY, value TEXT NOT NULL)",
        [],
    ).map_err(Error::dictionary_creation)?;
    conn.execute(
        "CREATE TABLE IF NOT EXISTS entries (
            stem TEXT,
            headword TEXT NOT NULL,
            definition_format TEXT NOT NULL,
            definition TEXT NOT NULL
        )",
        [],
    )
    .map_err(Error::dictionary_creation)?;
    conn.execute("CREATE INDEX IF NOT EXISTS idx_stem ON entries (stem)", [])
        .map_err(Error::dictionary_creation)?;
    conn.execute(
        "CREATE INDEX IF NOT EXISTS idx_headword ON entries (headword)",
        [],
    )
    .map_err(Error::dictionary_creation)?;

    Ok(())
}

/// Compute the stem for a word.
fn word_stem<'w>(lang: &Language, word: &'w str) -> Option<String> {
    let word = word.to_lowercase();
    let algorithm = stemmer_algorithm(lang)?;
    let stemmer = rust_stemmers::Stemmer::create(algorithm);
    let stem = stemmer.stem(&word).into_owned();
    trace!(?word, ?stem, "Computed stem");
    Some(stem)
}

/// Stemmer algorithm for a language.
fn stemmer_algorithm(lang: &Language) -> Option<rust_stemmers::Algorithm> {
    use rust_stemmers::Algorithm;
    match lang.to_639_3() {
        "ara" => Some(Algorithm::Arabic),
        "dan" => Some(Algorithm::Danish),
        "nld" => Some(Algorithm::Dutch),
        "eng" => Some(Algorithm::English),
        "fin" => Some(Algorithm::Finnish),
        "fra" => Some(Algorithm::French),
        "deu" => Some(Algorithm::German),
        "ell" => Some(Algorithm::Greek),
        "hun" => Some(Algorithm::Hungarian),
        "ita" => Some(Algorithm::Italian),
        "nor" => Some(Algorithm::Norwegian),
        "por" => Some(Algorithm::Portuguese),
        "ron" => Some(Algorithm::Romanian),
        "rus" => Some(Algorithm::Russian),
        "spa" => Some(Algorithm::Spanish),
        "swe" => Some(Algorithm::Swedish),
        "tam" => Some(Algorithm::Tamil),
        "tur" => Some(Algorithm::Turkish),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_word_stem() {
        let lang = Language::from_639_3("eng").unwrap();
        assert_eq!(word_stem(&lang, "run"), word_stem(&lang, "runs"));
        assert_eq!(word_stem(&lang, "run"), word_stem(&lang, "running"));
    }

    #[test]
    fn test_stemmer_algorithm() {
        use rust_stemmers::Algorithm;
        let expected = &[
            (Language::from_name("Arabic"), Some(Algorithm::Arabic)),
            (Language::from_name("Danish"), Some(Algorithm::Danish)),
            (Language::from_name("Dutch"), Some(Algorithm::Dutch)),
            (Language::from_name("English"), Some(Algorithm::English)),
            (Language::from_name("Finnish"), Some(Algorithm::Finnish)),
            (Language::from_name("French"), Some(Algorithm::French)),
            (Language::from_name("German"), Some(Algorithm::German)),
            // The name here is probably "Greek (modern)" or something.
            (Language::from_639_1("el"), Some(Algorithm::Greek)),
            (Language::from_name("Hungarian"), Some(Algorithm::Hungarian)),
            (Language::from_name("Italian"), Some(Algorithm::Italian)),
            (Language::from_name("Norwegian"), Some(Algorithm::Norwegian)),
            (
                Language::from_name("Portuguese"),
                Some(Algorithm::Portuguese),
            ),
            (Language::from_name("Romanian"), Some(Algorithm::Romanian)),
            (Language::from_name("Russian"), Some(Algorithm::Russian)),
            (Language::from_name("Spanish"), Some(Algorithm::Spanish)),
            (Language::from_name("Swedish"), Some(Algorithm::Swedish)),
            (Language::from_name("Tamil"), Some(Algorithm::Tamil)),
            (Language::from_name("Turkish"), Some(Algorithm::Turkish)),
        ];
        for (lang, expected) in expected {
            let lang = lang.expect("Language name not found");
            assert_eq!(stemmer_algorithm(&lang), *expected);
        }
    }

    #[ignore]
    #[tokio::test]
    async fn test_look_up_dictionary_entry() {
        let lang_pair =
            LanguagePair::from_639_3("spa", "eng").expect("Invalid language pair");
        assert!(dictionary_can_be_found(&lang_pair).await.unwrap());
        let entries =
            look_up_dictionary_entry(&lang_pair, "acero", MatchType::Exact).unwrap();
        assert!(!entries.is_empty());
    }
}

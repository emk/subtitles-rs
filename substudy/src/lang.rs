//! Naming and identifying languages.  We use

use std::{collections::HashMap, fmt, result, str::from_utf8};

use anyhow::anyhow;
use lazy_static::lazy_static;
use log::debug;
use serde::{Serialize, Serializer};
use whatlang;

use crate::Result;

/// External CSV data from the LoC.
///
/// This is a CSV file which looks like:
///
/// ```csv
/// alpha3-b,alpha3-t,alpha2,English,French
/// aar,null,aa,Afar,afar
/// ```
static ISO_639_CODES: &str = include_str!("data/language-codes-full.csv");

/// Maps related to ISO 639 language codes.
struct LangMaps {
    canonical_codes: HashMap<String, String>,
    names: HashMap<String, String>,
}

/// Helper function called to build language maps.
fn iso_689_canonical_codes_and_names() -> LangMaps {
    let mut canonical_codes = HashMap::new();
    let mut names = HashMap::new();

    // Parse using `csv` crate.
    let mut rdr = csv::Reader::from_reader(ISO_639_CODES.as_bytes());
    let mut r = csv::StringRecord::new();
    while rdr.read_record(&mut r).expect("error reading embedded CSV") {
        let (a3b, a3t, a2, en, _fr) = (&r[0], &r[1], &r[2], &r[3], &r[4]);
        if a2 != "null" {
            if a3b != "null" {
                canonical_codes.insert(a3b.to_owned(), a2.to_owned());
            }
            if a3t != "null" {
                canonical_codes.insert(a3t.to_owned(), a2.to_owned());
            }
            names.insert(a2.to_owned(), en.to_owned());
        } else {
            if a3b != "null" {
                names.insert(a3b.to_owned(), en.to_owned());
            }
            if a3t != "null" {
                names.insert(a3t.to_owned(), en.to_owned());
            }
        }
    }
    LangMaps {
        canonical_codes,
        names,
    }
}

// Use the third-party `lazy_static!` macro to declare variables that will
// initialized the first time we use them.
lazy_static! {
    static ref LANG_MAPS: LangMaps = iso_689_canonical_codes_and_names();
}

/// A language identifier.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Lang {
    code: [u8; 3],
}

impl Lang {
    /// Specify a language using an ISO 639-1, -2/T or -2/B code.  We know
    /// that the same language is sometimes represented by more than one
    /// code, and we do our best to treat equivalent codes as the same
    /// language.
    ///
    /// ```
    /// use substudy::lang::Lang;
    /// assert_eq!(Lang::iso639("en").unwrap(), Lang::iso639("eng").unwrap());
    /// assert!(Lang::iso639("en").unwrap() != Lang::iso639("fr").unwrap());
    /// assert!(Lang::iso639("abcd").is_err());
    /// ```
    pub fn iso639(code: &str) -> Result<Lang> {
        let canon = LANG_MAPS
            .canonical_codes
            .get(code)
            .cloned()
            .unwrap_or_else(|| code.to_owned());
        let c = canon.as_bytes();
        match (canon.is_ascii(), c.len()) {
            (true, 2) => Ok(Lang {
                code: [c[0], c[1], b' '],
            }),
            (true, 3) => Ok(Lang {
                code: [c[0], c[1], c[2]],
            }),
            _ => Err(anyhow!("Unsupported language code: {}", code)),
        }
    }

    /// Get the normalized language code as a `&str`.  Prefers ISO 639-1
    /// codes when possible, and -2/T if that's the best it can do.
    ///
    /// ```
    /// use substudy::lang::Lang;
    /// assert_eq!("en", Lang::iso639("en").unwrap().as_str());
    /// assert_eq!("en", Lang::iso639("eng").unwrap().as_str());
    /// ```
    pub fn as_str(&self) -> &str {
        // We could actually use the unsafe from_utf8_unchecked here.
        if self.code[2] == b' ' {
            from_utf8(&self.code[..2]).unwrap()
        } else {
            from_utf8(&self.code).unwrap()
        }
    }

    /// Try to determine the language of `text`.  We return `None` unless
    /// we're pretty sure.
    ///
    /// ```
    /// use substudy::lang::Lang;
    /// let text = "Pour que le caractère d’un être humain dévoile des qualités";
    /// assert_eq!(Lang::for_text(text).unwrap(), Lang::iso639("fr").unwrap());
    /// ```
    pub fn for_text(text: &str) -> Option<Lang> {
        if let Some(info) = whatlang::detect(text) {
            debug!("detected language: {:?}", info);
            if info.is_reliable() {
                return Lang::iso639(info.lang().code()).ok();
            }
        }
        None
    }

    /// Names of the language (or related languages) in English. These
    /// may be separated by semi-colons.
    ///
    /// ```
    /// use substudy::lang::Lang;
    /// assert_eq!(
    ///     vec!["English".to_owned()],
    ///     Lang::iso639("en").unwrap().english_names().unwrap(),
    /// );
    /// ```
    pub fn english_names(&self) -> Result<Vec<&'static str>> {
        let name_str = LANG_MAPS
            .names
            .get(self.as_str())
            .map(|s| s.as_str())
            .ok_or_else(|| {
                anyhow!("No English name for language code: {:?}", self.as_str())
            })?;
        Ok(name_str.split("; ").collect())
    }
}

impl fmt::Debug for Lang {
    fn fmt(&self, f: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Display for Lang {
    fn fmt(&self, f: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        write!(f, "{}", self.as_str())
    }
}

impl<'de> serde::Deserialize<'de> for Lang {
    fn deserialize<D>(deserializer: D) -> result::Result<Lang, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Lang::iso639(&s).map_err(serde::de::Error::custom)
    }
}

impl Serialize for Lang {
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_str().serialize(serializer)
    }
}

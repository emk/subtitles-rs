//! Naming and identifying languages.  We use

use serde::{Serialize, Serializer};
use std::ascii::AsciiExt;
use std::collections::HashMap;
use std::fmt;
use std::iter::FromIterator;
use std::str::from_utf8;
use std::result;
use whatlang;

use errors::*;

// Use the third-party `lazy_static!` macro to declare variables that will
// initialized the first time we use them.
lazy_static! {
    /// Maps ISO 639 codes to their preferred internal forms.  Based on
    /// http://www.loc.gov/standards/iso639-2/ISO-639-2_utf-8.txt
    static ref CANONICAL_CODE: HashMap<&'static str, &'static str> = {
        HashMap::from_iter([
            ("aar", "aa"), ("abk", "ab"), ("afr", "af"), ("aka", "ak"),
            ("alb", "sq"), ("sqi", "sq"), ("amh", "am"), ("ara", "ar"),
            ("arg", "an"), ("arm", "hy"), ("hye", "hy"), ("asm", "as"),
            ("ava", "av"), ("ave", "ae"), ("aym", "ay"), ("aze", "az"),
            ("bak", "ba"), ("bam", "bm"), ("baq", "eu"), ("eus", "eu"),
            ("bel", "be"), ("ben", "bn"), ("bih", "bh"), ("bis", "bi"),
            ("bos", "bs"), ("bre", "br"), ("bul", "bg"), ("bur", "my"),
            ("mya", "my"), ("cat", "ca"), ("cha", "ch"), ("che", "ce"),
            ("chi", "zh"), ("zho", "zh"), ("chu", "cu"), ("chv", "cv"),
            ("cor", "kw"), ("cos", "co"), ("cre", "cr"), ("ces", "cs"),
            ("cze", "cs"), ("dan", "da"), ("div", "dv"), ("dut", "nl"),
            ("nld", "nl"), ("dzo", "dz"), ("eng", "en"), ("epo", "eo"),
            ("est", "et"), ("ewe", "ee"), ("fao", "fo"), ("fij", "fj"),
            ("fin", "fi"), ("fra", "fr"), ("fre", "fr"), ("fry", "fy"),
            ("ful", "ff"), ("geo", "ka"), ("kat", "ka"), ("deu", "de"),
            ("ger", "de"), ("gla", "gd"), ("gle", "ga"), ("glg", "gl"),
            ("glv", "gv"), ("ell", "el"), ("gre", "el"), ("grn", "gn"),
            ("guj", "gu"), ("hat", "ht"), ("hau", "ha"), ("heb", "he"),
            ("her", "hz"), ("hin", "hi"), ("hmo", "ho"), ("hrv", "hr"),
            ("hun", "hu"), ("ibo", "ig"), ("ice", "is"), ("isl", "is"),
            ("ido", "io"), ("iii", "ii"), ("iku", "iu"), ("ile", "ie"),
            ("ina", "ia"), ("ind", "id"), ("ipk", "ik"), ("ita", "it"),
            ("jav", "jv"), ("jpn", "ja"), ("kal", "kl"), ("kan", "kn"),
            ("kas", "ks"), ("kau", "kr"), ("kaz", "kk"), ("khm", "km"),
            ("kik", "ki"), ("kin", "rw"), ("kir", "ky"), ("kom", "kv"),
            ("kon", "kg"), ("kor", "ko"), ("kua", "kj"), ("kur", "ku"),
            ("lao", "lo"), ("lat", "la"), ("lav", "lv"), ("lim", "li"),
            ("lin", "ln"), ("lit", "lt"), ("ltz", "lb"), ("lub", "lu"),
            ("lug", "lg"), ("mac", "mk"), ("mkd", "mk"), ("mah", "mh"),
            ("mal", "ml"), ("mao", "mi"), ("mri", "mi"), ("mar", "mr"),
            ("may", "ms"), ("msa", "ms"), ("mlg", "mg"), ("mlt", "mt"),
            ("mon", "mn"), ("nau", "na"), ("nav", "nv"), ("nbl", "nr"),
            ("nde", "nd"), ("ndo", "ng"), ("nep", "ne"), ("nno", "nn"),
            ("nob", "nb"), ("nor", "no"), ("nya", "ny"), ("oci", "oc"),
            ("oji", "oj"), ("ori", "or"), ("orm", "om"), ("oss", "os"),
            ("pan", "pa"), ("fas", "fa"), ("per", "fa"), ("pli", "pi"),
            ("pol", "pl"), ("por", "pt"), ("pus", "ps"), ("que", "qu"),
            ("roh", "rm"), ("ron", "ro"), ("rum", "ro"), ("run", "rn"),
            ("rus", "ru"), ("sag", "sg"), ("san", "sa"), ("sin", "si"),
            ("slk", "sk"), ("slo", "sk"), ("slv", "sl"), ("sme", "se"),
            ("smo", "sm"), ("sna", "sn"), ("snd", "sd"), ("som", "so"),
            ("sot", "st"), ("spa", "es"), ("srd", "sc"), ("srp", "sr"),
            ("ssw", "ss"), ("sun", "su"), ("swa", "sw"), ("swe", "sv"),
            ("tah", "ty"), ("tam", "ta"), ("tat", "tt"), ("tel", "te"),
            ("tgk", "tg"), ("tgl", "tl"), ("tha", "th"), ("bod", "bo"),
            ("tib", "bo"), ("tir", "ti"), ("ton", "to"), ("tsn", "tn"),
            ("tso", "ts"), ("tuk", "tk"), ("tur", "tr"), ("twi", "tw"),
            ("uig", "ug"), ("ukr", "uk"), ("urd", "ur"), ("uzb", "uz"),
            ("ven", "ve"), ("vie", "vi"), ("vol", "vo"), ("cym", "cy"),
            ("wel", "cy"), ("wln", "wa"), ("wol", "wo"), ("xho", "xh"),
            ("yid", "yi"), ("yor", "yo"), ("zha", "za"), ("zul", "zu"),
        ].iter().cloned())
    };
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
        let canon = CANONICAL_CODE.get(code).cloned().unwrap_or(code);
        let c = canon.as_bytes();
        match (canon.is_ascii(), c.len()) {
            (true, 2) => Ok(Lang {
                code: [c[0], c[1], b' '],
            }),
            (true, 3) => Ok(Lang {
                code: [c[0], c[1], c[2]],
            }),
            _ => Err(format_err!("Unsupported language code: {}", code)),
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

impl Serialize for Lang {
    fn serialize<S>(&self, serializer: S) -> result::Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_str().serialize(serializer)
    }
}

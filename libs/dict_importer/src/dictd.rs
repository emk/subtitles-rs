use std::{
    io::{BufRead as _, BufReader, Read},
    str::FromStr,
};

use crate::errors::{Error, Result};

/// The numeric value for each character in the custom base64 encoding.
static NUMERIC_VALUE_FOR_CHAR: &'static [u8; 128] = &[
    255u8, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 62, 255, 255, 255, 63, 52,
    53, 54, 55, 56, 57, 58, 59, 60, 61, 255, 255, 255, 255, 255, 255, 255, 0, 1, 2, 3,
    4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    255, 255, 255, 255, 255, 255, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38,
    39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 255, 255, 255, 255, 255,
];

/// Decode a numeric value from the index.
///
/// The index uses a custom base64 encoding, where the entries are
/// `ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/`.
///
/// Values are decoded as `sum += x * 64.pow(i)`, where `x` is the index of the
/// character in the array, and `i` is the position in the string, starting from
/// the right, with the rightmost character being at position 0.
fn decode_index_value(s: &str) -> Result<usize> {
    let mut sum = 0;
    for (i, c) in s.chars().rev().enumerate() {
        let x = *NUMERIC_VALUE_FOR_CHAR.get(c as usize).unwrap_or(&255);
        if x == 255 {
            return Err(Error::CorruptedIndex {});
        }
        sum += x as usize * 64usize.pow(i as u32);
    }
    Ok(sum)
}

/// A dictionary in the `dictd` format.
pub struct Dictionary {
    /// The index entries.
    index: Vec<IndexEntry>,

    /// The raw dictionary data.
    data: Vec<u8>,
}

impl Dictionary {
    /// Download a dictionary from [FreeDict](https://freedict.org/).
    #[cfg(feature = "download")]
    pub async fn from_freedict(
        lang_pair: &crate::LanguagePair,
    ) -> Result<Option<Self>> {
        use crate::download::DictionaryInfo;
        if let Some(dict) = DictionaryInfo::for_language_pair(lang_pair).await? {
            dict.import_dictd().await
        } else {
            Ok(None)
        }
    }

    /// Given a compressed dictionary file, extract the index and data, and
    /// return them as a [`Dictionary`].
    ///
    /// This supports dictionary files stored as `*.dictd.tar.xz`, and
    /// containing at least two files named:
    ///
    /// - `*.index` (the index)
    /// - `*.dict.dz` or `*.dict` (the data)
    ///
    /// If the file ends in `dz`, we assume it is `gzip`-compressed.
    ///
    /// Note that this may take a long time to run. If you call it from `async`
    /// code, you should use [`tokio::task::spawn_blocking`].
    #[cfg(feature = "unpack")]
    #[tracing::instrument(skip(rdr))]
    pub fn from_archive_reader<R>(rdr: R) -> Result<Self>
    where
        R: Read,
    {
        use std::{
            ffi::OsStr,
            io::{self, Seek},
        };

        use tracing::{debug, trace};

        let mkerr = Error::could_not_load_dictionary;
        let mut compressed_file = io::BufReader::new(rdr);
        let mut uncompressed_file = tempfile::tempfile().map_err(mkerr)?;
        lzma_rs::xz_decompress(&mut compressed_file, &mut uncompressed_file)
            .map_err(Error::could_not_load_dictionary)?;
        uncompressed_file
            .seek(io::SeekFrom::Start(0))
            .map_err(mkerr)?;
        let mut tar = tar::Archive::new(uncompressed_file);
        let entries = tar.entries_with_seek().map_err(mkerr)?;
        let mut index = Vec::new();
        let mut data = Vec::new();
        for entry in entries {
            let mut entry = entry.map_err(mkerr)?;
            let path = entry.path().map_err(mkerr)?.to_owned();
            trace!("Found tar entry: {}", path.display());
            if let Some(ext) = path.extension() {
                if ext == OsStr::new("index") {
                    debug!("Reading dictionary index from {}", path.display());
                    entry.read_to_end(&mut index).map_err(mkerr)?;
                } else if ext == OsStr::new("dict") {
                    debug!("Reading dictionary data from {}", path.display());
                    entry.read_to_end(&mut data).map_err(mkerr)?;
                } else if ext == OsStr::new("dz") {
                    debug!(
                        "Reading compressed dictionary data from {}",
                        path.display()
                    );
                    let mut decompressor = flate2::read::GzDecoder::new(entry);
                    decompressor.read_to_end(&mut data).map_err(mkerr)?;
                }
            }
        }
        if index.is_empty() {
            return Err(Error::could_not_load_dictionary("No index found"));
        }
        if data.is_empty() {
            return Err(Error::could_not_load_dictionary("No data found"));
        }
        Self::from_readers(index.as_slice(), data.as_slice())
    }

    /// Create a new dictionary from files containing the index and data.
    pub fn from_readers<R>(index_rdr: R, data_rdr: R) -> Result<Self>
    where
        R: Read,
    {
        let index_rdr = BufReader::new(index_rdr);
        let mut data_rdr = BufReader::new(data_rdr);

        // The index has one entry per line.
        let index: Vec<IndexEntry> = index_rdr
            .lines()
            .map(|line| Ok(line.map_err(Error::could_not_load_dictionary)?.parse()?))
            .collect::<Result<_>>()?;

        // The data should be read in as a binary blob.
        let mut data = Vec::new();
        data_rdr
            .read_to_end(&mut data)
            .map_err(Error::could_not_load_dictionary)?;

        Ok(Dictionary { index, data })
    }

    /// Iterate over the entries in the dictionary.
    pub fn entries(&self) -> EntriesIter {
        EntriesIter {
            dict: self,
            index: 0,
        }
    }
}

/// A dictionary index entry, mapping a headword to a byte offset and size.
#[derive(Debug, Clone, PartialEq)]
struct IndexEntry {
    headword: String,
    offset: usize,
    size: usize,
}

impl FromStr for IndexEntry {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let parts: Vec<&str> = s.split('\t').collect();
        if parts.len() != 3 {
            return Err(Error::CorruptedIndex {});
        }
        Ok(IndexEntry {
            headword: parts[0].to_string(),
            offset: decode_index_value(parts[1])?,
            size: decode_index_value(parts[2])?,
        })
    }
}

/// An iterator over the entries in a dictionary.
pub struct EntriesIter<'dict> {
    /// The dictionary.
    dict: &'dict Dictionary,

    /// The current index.
    index: usize,
}

impl<'dict> Iterator for EntriesIter<'dict> {
    type Item = Result<Entry<'dict>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.dict.index.len() {
            return None;
        }

        let entry = &self.dict.index[self.index];
        let data = &self.dict.data[entry.offset..(entry.offset + entry.size)];
        let definition = std::str::from_utf8(data).unwrap(); // TODO Err
        self.index += 1;

        if entry.headword.starts_with("00database") {
            Some(Ok(Entry::Metadata {
                key: &entry.headword,
                value: definition,
            }))
        } else {
            Some(Ok(Entry::Word {
                headword: &entry.headword,
                definition,
            }))
        }
    }
}

/// A dictionary entry.
#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum Entry<'dict> {
    Metadata {
        /// The key.
        key: &'dict str,

        /// The value.
        value: &'dict str,
    },

    Word {
        /// The headword.
        headword: &'dict str,

        /// The definition.
        definition: &'dict str,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decode_index_value() {
        assert_eq!(decode_index_value("vew").unwrap(), 194480);
        assert_eq!(decode_index_value("s").unwrap(), 44);
        assert_eq!(decode_index_value("mq").unwrap(), 2474);
        assert_eq!(decode_index_value("v").unwrap(), 47);
    }

    #[test]
    fn test_index_entry_from_str() {
        assert_eq!(
            "hello\tvew\ts".parse::<IndexEntry>().unwrap(),
            IndexEntry {
                headword: "hello".to_string(),
                offset: 194480,
                size: 44,
            }
        );
    }

    #[test]
    fn test_dictionary_from_readers() {
        let index = b"a\tA\tC\nb\tC\tC\n";
        let data = b"1\n2\n";
        let dict = Dictionary::from_readers(index.as_ref(), data.as_ref()).unwrap();

        let entries = dict.entries().map(Result::unwrap).collect::<Vec<_>>();
        assert_eq!(
            entries,
            vec![
                Entry::Word {
                    headword: "a",
                    definition: "1\n"
                },
                Entry::Word {
                    headword: "b",
                    definition: "2\n"
                }
            ]
        );
    }

    #[cfg(feature = "unpack")]
    #[test]
    fn test_dictionary_from_archive_reader() {
        let _ = tracing_subscriber::fmt::try_init();

        let fixtures: &[&[u8]] = &[
            include_bytes!("../fixtures/tinydict-compressed.dictd.tar.xz"),
            include_bytes!("../fixtures/tinydict-uncompressed.dictd.tar.xz"),
        ];
        for fixture in fixtures {
            let dict = Dictionary::from_archive_reader(fixture.as_ref()).unwrap();
            let entries = dict.entries().map(Result::unwrap).collect::<Vec<_>>();
            assert_eq!(
                entries,
                vec![
                    Entry::Word {
                        headword: "a",
                        definition: "1\n"
                    },
                    Entry::Word {
                        headword: "b",
                        definition: "2\n"
                    }
                ]
            );
        }
    }

    #[cfg(feature = "download")]
    #[ignore]
    #[tokio::test]
    async fn test_dictionary_from_freedict() {
        let _ = tracing_subscriber::fmt::try_init();

        let lang_pair = crate::LanguagePair::from_639_3("spa", "eng").unwrap();
        let dict = Dictionary::from_freedict(&lang_pair)
            .await
            .unwrap()
            .unwrap();
        dict.entries()
            .map(Result::unwrap)
            .find(|e| matches!(e, Entry::Word { headword, .. } if *headword == "a"))
            .expect("Could not find entry for 'a'");
    }
}

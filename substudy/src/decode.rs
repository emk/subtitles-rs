//! Decode text in a wide variety of character encodings.

use anyhow::anyhow;
use chardet;
use encoding::{label::encoding_from_whatwg_label, types::DecoderTrap};
use log::debug;

use crate::{Error, Result};

/// Guess the encoding of a byte buffer and decode it to a string.
pub fn smart_decode(bytes: &[u8]) -> Result<String> {
    let (name, confidence, _lang) = chardet::detect(bytes);
    debug!(
        "detected encoding name {:?} with confidence {}",
        name, confidence
    );
    if confidence < 0.5 {
        return Err(anyhow!("cannot detect language with sufficient confidence"));
    }
    let encoding = encoding_from_whatwg_label(&name)
        .ok_or_else(|| -> Error { anyhow!("Unknown encoding: {}", &name) })?;
    match encoding.decode(bytes, DecoderTrap::Strict) {
        Ok(result) => Ok(result),
        Err(msg) => Err(anyhow!("{}", msg)),
    }
}

#[test]
fn test_smart_decode() {
    assert_eq!("ascii", &smart_decode("ascii".as_bytes()).unwrap());
    assert_eq!(
        "élèves français",
        &smart_decode("élèves français".as_bytes()).unwrap()
    );
    assert_eq!(
        "une idée française",
        &smart_decode(&[
            0x75u8, 0x6e, 0x65, 0x20, 0x69, 0x64, 0xe9, 0x65, 0x20, 0x66, 0x72, 0x61,
            0x6e, 0xe7, 0x61, 0x69, 0x73, 0x65
        ])
        .unwrap()
    );
}

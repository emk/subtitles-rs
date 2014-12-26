//! Decode text in a wide variety of character encodings.

use encoding::label::encoding_from_whatwg_label;
use encoding::types::DecoderTrap;
use uchardet::detect_encoding_name;
use err::{SubStudyError, SubStudyResult};

/// Guess the encoding of a byte buffer and decode it to a string.
pub fn smart_decode(bytes: &[u8]) -> SubStudyResult<String> {
    // If no matching encoding is found, that means we either have
    // valid ASCII data, or something hopelessly unsalvageable.
    let name = try!(detect_encoding_name(bytes))
        .unwrap_or("ascii".to_string());
    let encoding = try!(encoding_from_whatwg_label(name.as_slice())
        .ok_or_else(|| {
            SubStudyError::Decode(format!("Unknown encoding: {}", name))
        }));
    match encoding.decode(bytes, DecoderTrap::Strict) {
        Ok(result) => Ok(result),
        Err(msg) => Err(SubStudyError::Decode(msg.into_owned()))
    }
}

#[test]
fn test_smart_decode() {
    assert_eq!("ascii", smart_decode("ascii".as_bytes()).unwrap().as_slice());
    assert_eq!("français",
               smart_decode("français".as_bytes()).unwrap().as_slice());
    assert_eq!("français",
               smart_decode(&[0x66u8, 0x72, 0x61, 0x6e, 0xe7,
                              0x61, 0x69, 0x73]).unwrap().as_slice());
}

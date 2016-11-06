//! Decode text in a wide variety of character encodings.

use encoding::label::encoding_from_whatwg_label;
use encoding::types::DecoderTrap;
use uchardet::detect_encoding_name;

use err::{err_str, Error, Result};

/// Guess the encoding of a byte buffer and decode it to a string.
pub fn smart_decode(bytes: &[u8]) -> Result<String> {
    let name = try!(detect_encoding_name(bytes)
        .map_err(|err| {
            let err: Error = format!("{}", err).into();
            err
        }));
    let encoding = try!(encoding_from_whatwg_label(&name)
        .ok_or_else(|| {
            err_str(format!("Unknown encoding: {}", name))
        }));
    match encoding.decode(bytes, DecoderTrap::Strict) {
        Ok(result) => Ok(result),
        Err(msg) => Err(err_str(msg.into_owned()))
    }
}

#[test]
fn test_smart_decode() {
    assert_eq!("ascii", &smart_decode("ascii".as_bytes()).unwrap());
    assert_eq!("élèves français",
               &smart_decode("élèves français".as_bytes()).unwrap());
    assert_eq!("“Françoisé”",
               &smart_decode(&[0x93, 0x46, 0x72, 0x61, 0x6e, 0xe7, 0x6f,
                               0x69, 0x73, 0xe9, 0x94]).unwrap());
}

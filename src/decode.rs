//! Decode text in a wide variety of character encodings.

use encoding::label::encoding_from_whatwg_label;
use encoding::types::DecoderTrap;
use uchardet::detect_encoding_name;

use errors::*;

/// Guess the encoding of a byte buffer and decode it to a string.
pub fn smart_decode(bytes: &[u8]) -> Result<String> {
    let name = try!(detect_encoding_name(bytes));
    debug!("detected encoding name: {}", name);
    let encoding = try!(encoding_from_whatwg_label(&name)
        .ok_or_else(|| -> Error {
            format!("Unknown encoding: {}", &name).into()
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
    assert_eq!("une idée française",
               &smart_decode(&[0x75u8, 0x6e, 0x65, 0x20, 0x69, 0x64,
                               0xe9, 0x65, 0x20, 0x66, 0x72, 0x61,
                               0x6e, 0xe7, 0x61, 0x69, 0x73, 0x65]).unwrap());
}

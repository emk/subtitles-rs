//! Miscellaneous utilities.

use std::fmt;

/// Wrapper to force a `&[u8]` to display as nicely-formatted hexadecimal
/// bytes with only the the first line or so of bytes shown.
pub struct BytesFormatter<'a>(pub &'a [u8]);

impl<'a> fmt::Debug for BytesFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let BytesFormatter(bytes) = *self;
        for byte in bytes.iter().take(16) {
            write!(f, "{:02x} ", byte)?;
        }
        write!(f, "({} bytes)", bytes.len())?;
        Ok(())
    }
}

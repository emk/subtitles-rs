extern crate aligned_media;
extern crate common_failures;
extern crate failure;
#[macro_use]
extern crate stdweb;

use common_failures::prelude::*;

/// Called from JavaScript to parse and validate metadata. Returns `null` if the
/// data is valid, or a string containing an error message if the validation
/// fails.
pub fn validate_metadata(json: String) -> Option<String> {
    match aligned_media::Metadata::from_str(&json) {
        Ok(_) => None,
        Err(err) => {
            Some(format!("{}", err.display_causes_and_backtrace()))
        }
    }
}

fn main() {
    stdweb::initialize();

    // Export our API to JavaScript using the `stdweb` crate's `js!` macro.
    js! {
        Module.exports.validate_metadata = @{validate_metadata};
    }
}

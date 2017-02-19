//! Error types defined using `error-chain`.

// We can't document enum members declared in `links` or `foreign_links` yet.
#![allow(missing_docs)]

use cast;
#[cfg(test)]
use vobsub;

error_chain! {
    foreign_links {
        Cast(cast::Error);
    }

    links {
        // An error from the `vobsub` library.
        Vobsub(vobsub::Error, vobsub::ErrorKind) #[cfg(test)];
    }
}

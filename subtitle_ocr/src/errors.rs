//! Error types defined using `error-chain`.

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

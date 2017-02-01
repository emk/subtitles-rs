/// Custom `Error` and `Result` types, declared using `error-chain`.

use std::io;

error_chain! {
    foreign_links {
        Io(io::Error);
    }
}

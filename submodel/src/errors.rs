//! Our error-handling machinery.

use csv;
use std::io;

error_chain! {
    foreign_links {
        Csv(csv::Error);
        Io(io::Error);
    }
}

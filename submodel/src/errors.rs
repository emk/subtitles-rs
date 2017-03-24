//! Our error-handling machinery.

use csv;

error_chain! {
    foreign_links {
        Csv(csv::Error);
    }
}

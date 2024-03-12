//! Progress bar support.

use indicatif::ProgressStyle;

/// Our default progress style.
pub(crate) fn default_progress_style() -> ProgressStyle {
    ProgressStyle::default_bar()
        .template("{prefix} {pos:>4}/{len:4} {wide_bar:.cyan/blue} {eta_precise}")
        .expect("bad progress bar template")
}

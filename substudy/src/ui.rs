//! Application UI. For now, this is mostly progress bars.

use std::sync::Arc;

use indicatif::{MultiProgress, ProgressBar, ProgressStyle};

/// Application UI state.
#[derive(Clone)]
pub struct Ui {
    /// Our progress bars. I'm not actually sure that this `Arc` is useful, but
    /// I'm playing it safe until I understand `MultiProgress` and `tokio`
    /// interactions better.
    multi_progress: Arc<MultiProgress>,
}

impl Ui {
    /// Create a new UI. This sets up logging and and progress bars.
    pub fn init() -> Ui {
        // We'd like to wrap our logger so that it works well with our progress
        // bars.  Unfortunately, the obvious approach does not work and the
        // crate `indicatif-log-bridge` just supresses all messages.
        env_logger::init();

        let multi_progress = Arc::new(MultiProgress::new());
        Ui { multi_progress }
    }

    /// Get a reference to our progress bars.
    pub fn multi_progress(&self) -> &MultiProgress {
        &self.multi_progress
    }

    /// Create a new progress bar with default settings.
    pub fn new_progress_bar(&self, len: u64) -> ProgressBar {
        let pb = ProgressBar::new(len).with_style(default_progress_style());
        self.multi_progress.add(pb)
    }

    /// Create a new spinner with default settings.
    pub fn new_spinner(&self) -> ProgressBar {
        let sp = ProgressBar::new_spinner().with_style(default_spinner_style());
        self.multi_progress.add(sp)
    }
}

pub(crate) fn default_progress_style() -> ProgressStyle {
    ProgressStyle::default_bar()
        .template("  {prefix:3}{msg:25} {pos:>4}/{len:4} {elapsed_precise} {wide_bar:.cyan/blue} {eta_precise}")
        .expect("bad progress bar template")
}

pub(crate) fn default_spinner_style() -> ProgressStyle {
    ProgressStyle::default_spinner()
        .template("{spinner} {prefix:3}{msg}")
        .expect("bad progress bar template")
}

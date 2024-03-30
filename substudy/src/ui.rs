//! Application UI. For now, this is mostly progress bars.

use std::{sync::Arc, time::Duration};

#[cfg(test)]
use indicatif::ProgressDrawTarget;
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

    /// Create a new UI for unit tests.returns_right_number_of_subs
    #[cfg(test)]
    pub fn init_for_tests() -> Ui {
        let multi_progress =
            Arc::new(MultiProgress::with_draw_target(ProgressDrawTarget::hidden()));
        Ui { multi_progress }
    }

    /// Get a reference to our progress bars.
    pub fn multi_progress(&self) -> &MultiProgress {
        &self.multi_progress
    }

    /// Create a new progress bar with default settings.
    pub fn new_progress_bar(
        &self,
        config: &ProgressConfig<'_>,
        len: u64,
    ) -> ProgressBar {
        let pb = ProgressBar::new(len).with_style(default_progress_style());
        let pb = self.multi_progress.add(pb);
        #[cfg(test)]
        pb.set_draw_target(ProgressDrawTarget::hidden());
        pb.set_prefix(config.emoji.to_owned());
        pb.set_message(config.msg.to_owned());
        pb.enable_steady_tick(Duration::from_millis(250));
        pb
    }

    /// Create a new spinner with default settings.
    pub fn new_spinner(&self, config: &ProgressConfig<'_>) -> ProgressBar {
        let sp = ProgressBar::new_spinner().with_style(default_spinner_style());
        let sp = self.multi_progress.add(sp);
        #[cfg(test)]
        sp.set_draw_target(ProgressDrawTarget::hidden());
        sp.set_prefix(config.emoji.to_owned());
        sp.set_message(config.msg.to_owned());
        sp.enable_steady_tick(Duration::from_millis(250));
        sp
    }

    /// Finish a progress bar.
    pub fn finish(&self, config: &ProgressConfig<'_>, pb: ProgressBar) {
        pb.finish_with_message(config.done_msg.to_owned());
    }
}

/// Configuration for a progress bar.
pub struct ProgressConfig<'a> {
    /// Emoji to display in the progress bar.
    pub emoji: &'a str,
    /// Message to display in a running progress bar.
    pub msg: &'a str,
    /// Message to display in a progress bar when it is done.
    pub done_msg: &'a str,
}

fn default_progress_style() -> ProgressStyle {
    ProgressStyle::default_bar()
        .template("  {prefix:3}{msg:25} {pos:>4}/{len:4} {elapsed_precise} {wide_bar:.cyan/blue} {eta_precise}")
        .expect("bad progress bar template")
}

fn default_spinner_style() -> ProgressStyle {
    ProgressStyle::default_spinner()
        .template("{spinner} {prefix:3}{msg}")
        .expect("bad progress bar template")
}

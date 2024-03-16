//! Voice activity detection. Used to find likely breaks between lines of dialog
//! before attempting speech recognition, to avoid chopping lines in half.

use std::{io::ErrorKind, mem::size_of};

use anyhow::Result;
use bytemuck::{self, cast_slice};
use log::trace;
use tokio::io::AsyncReadExt as _;
use webrtc_vad::{Vad, VadMode};

use crate::{time::Period, ui::Ui, video::Video};

/// Sampling rate for voice detection.
const SAMPLES_PER_SECOND: usize = 8000;

/// How many samples per frame. This should be 80, 160 or 240.
const SAMPLES_PER_FRAME: usize = 240;

/// How many bytes per sample?
const BYTES_PER_SAMPLE: usize = size_of::<i16>();

/// Window for measuring voice presence (in seconds).
const WINDOW_SECONDS: f32 = 1.0;

/// Window for measuring voice presence (in frames)
const WINDOW_FRAMES: usize =
    (WINDOW_SECONDS * SAMPLES_PER_SECOND as f32 / SAMPLES_PER_FRAME as f32) as usize;

/// Find likely breaks between lines of dialog at roughly the specified spacing.
pub async fn segment_on_dialog_breaks(
    ui: &Ui,
    video: &Video,
    track: usize,
    spacing: f32,
) -> Result<Vec<Period>> {
    let spinner = ui.new_spinner();
    spinner.set_prefix("💬");
    spinner.set_message("Finding dialog breaks");

    let (mut rdr, join_handle) =
        video.open_audio_stream(track, SAMPLES_PER_SECOND).await?;

    let mut vad = Vad::new();
    vad.set_mode(VadMode::Aggressive);

    let mut periods = vec![];
    let mut last_break = 0.0;
    let mut samples = vec![0; SAMPLES_PER_FRAME * BYTES_PER_SAMPLE];
    let mut current_frame = 0;
    let mut avg = ExpMovingAverage::new(WINDOW_FRAMES, 1.0);
    loop {
        if current_frame % 20 == 0 {
            spinner.tick();
        }

        // Compute our current time.
        let current_time = current_frame as f32 * SAMPLES_PER_FRAME as f32
            / SAMPLES_PER_SECOND as f32;
        current_frame += 1;

        match rdr.read_exact(&mut samples).await {
            // Clean end-of-file.
            Ok(0) => {
                trace!("Last frame of audio stream at {:.2}", current_time);
                periods.push(Period::new(last_break, current_time)?);
                break;
            }
            Err(err) if err.kind() == ErrorKind::UnexpectedEof => {
                trace!("Last partial frame of audio stream at {:.2}", current_time);
                periods.push(Period::new(last_break, current_time)?);
                break;
            }
            Ok(_) => {
                if current_time - last_break >= spacing {
                    let samples_i16: &[i16] = cast_slice(&samples);
                    let is_voice = vad
                        .is_voice_segment(samples_i16)
                        .expect("Invalid VAD parameters");
                    avg.update(if is_voice { 1.0 } else { 0.0 });
                    if avg.value() < 0.2 {
                        trace!("Break end at {:.2}", current_time);
                        let break_middle =
                            f32::max(current_time - WINDOW_SECONDS / 2.0, last_break);
                        periods.push(Period::new(last_break, break_middle)?);
                        last_break = break_middle;
                        avg = ExpMovingAverage::new(WINDOW_FRAMES, 1.0);
                        vad.reset();
                        vad.set_mode(VadMode::Aggressive);
                    }
                }
            }
            // A real error.
            Err(err) => return Err(err.into()),
        }
    }
    // Wait for the audio stream to finish and report any errors.
    join_handle.await?;
    spinner.finish_with_message("Found dialog breaks");
    Ok(periods)
}

/// An [exponential moving average](https://en.wikipedia.org/wiki/Moving_average#Expo2nential_moving_average).
struct ExpMovingAverage {
    length: usize,
    value: f32,
}

impl ExpMovingAverage {
    /// Create a new moving average with the given length and initial value.
    fn new(length: usize, initial: f32) -> Self {
        Self {
            length,
            value: initial,
        }
    }

    /// Update the moving average with a new value.
    fn update(&mut self, new_value: f32) {
        self.value -= self.value / self.length as f32;
        self.value += new_value / self.length as f32;
    }

    /// Get the current value of the moving average.
    fn value(&self) -> f32 {
        self.value
    }
}

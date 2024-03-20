//! Exporting to CSV (compatible with Anki import).

use anyhow::Context as _;
use csv;
use serde::Serialize;

use crate::{export::Exporter, ui::Ui, Result};

use super::anki::{export_anki_helper, AudioNote};

/// Data in a CSV row. This is identical to [`AudioNote`], but columns will
/// never be omitted and we use snake_case for field names.
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) struct AudioNoteForCsv {
    pub(crate) sound: String,
    pub(crate) time: String,
    pub(crate) source: String,
    pub(crate) image: Option<String>,
    pub(crate) foreign_curr: Option<String>,
    pub(crate) native_curr: Option<String>,
    pub(crate) foreign_prev: Option<String>,
    pub(crate) native_prev: Option<String>,
    pub(crate) foreign_next: Option<String>,
    pub(crate) native_next: Option<String>,
    pub(crate) hint: Option<String>,
    pub(crate) notes: Option<String>,
}

impl From<AudioNote> for AudioNoteForCsv {
    fn from(note: AudioNote) -> Self {
        Self {
            sound: note.sound,
            time: note.time,
            source: note.source,
            image: note.image,
            foreign_curr: note.foreign_curr,
            native_curr: note.native_curr,
            foreign_prev: note.foreign_prev,
            native_prev: note.native_prev,
            foreign_next: note.foreign_next,
            native_next: note.native_next,
            hint: note.hint,
            notes: note.notes,
        }
    }
}

/// Export the video and subtitles as a CSV file with accompanying media
/// files, for import into Anki.
pub async fn export_csv(ui: &Ui, exporter: &mut Exporter) -> Result<()> {
    // Call our helper to do most of the work. We're just a thin wrapper now.
    let exporting = export_anki_helper(exporter, false)?;

    // Create our CSV writer and export our notes as a CSV file.
    let mut buffer = Vec::<u8>::new();
    {
        // TODO: Anki no longer likes having a header row. Do we want to keep it
        // for non-Anki users, or switch to something that works with Anki?
        let mut wtr = csv::Writer::from_writer(&mut buffer);
        for note in exporting.notes {
            let note = AudioNoteForCsv::from(note);
            wtr.serialize(&note).context("error serializing to RAM")?;
        }
    }

    // Write out our CSV file.
    exporter.export_data_file("cards.csv", &buffer)?;

    // Extract our media files.
    exporter.finish_exports(ui).await?;

    Ok(())
}

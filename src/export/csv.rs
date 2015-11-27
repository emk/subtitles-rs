//! Exporting to CSV (compatible with Anki import).

use csv;

use contexts::ItemsInContextExt;
use err::Result;
use export::Exporter;

#[derive(RustcEncodable)]
struct AnkiNote {
    sound: String,
    time: String,
    source: String,
    image: String,
    foreign_curr: Option<String>,
    native_curr: Option<String>,
    foreign_prev: Option<String>,
    native_prev: Option<String>,
    foreign_next: Option<String>,
    native_next: Option<String>,
}

/// Export the video and subtitles as a CSV file with accompanying media
/// files, for import into Anki.
pub fn export_csv(exporter: &mut Exporter) -> Result<()> {
    let foreign_lang = exporter.foreign().language;

    // Create our CSV writer.
    let mut wtr = csv::Writer::from_memory();

    // Align our input files and iterate.
    let aligned = exporter.align();
    for ctx in aligned.items_in_context() {
        // We have a `Context<&(Option<Subtitle>, Option<Subtitle>)>`
        // containing the previous subtitle pair, the current subtitle
        // pair, and the next subtitle pair.  We want to split apart that
        // tuple and flatten any nested `Option<&Option<T>>` types into
        // `Option<&T>`.
        let foreign = ctx.map(|&(ref f, _)| f).flatten();
        let native = ctx.map(|&(_, ref n)| n).flatten();

        if let Some(curr) = foreign.curr {
            let period = curr.period.grow(1.5, 1.5);

            let image_path = exporter.schedule_image_export(period.midpoint());
            let audio_path =
                exporter.schedule_audio_export(foreign_lang, period);

            let note = AnkiNote {
                sound: format!("[sound:{}]", &audio_path),
                time: "".to_owned(),
                source: exporter.title().to_owned(),
                image: format!("<img src=\"{}\" />", &image_path),
                foreign_curr: foreign.curr.map(|s| s.plain_text()),
                native_curr:  native.curr.map(|s| s.plain_text()),
                foreign_prev: foreign.prev.map(|s| s.plain_text()),
                native_prev:  native.prev.map(|s| s.plain_text()),
                foreign_next: foreign.next.map(|s| s.plain_text()),
                native_next:  native.next.map(|s| s.plain_text()),
            };
            try!(wtr.encode(&note));
        }
    }

    // Write out our CSV file.
    try!(exporter.export_data_file("cards.csv", wtr.as_bytes()));

    // Extract our media files.
    try!(exporter.finish_exports());

    Ok(())
}

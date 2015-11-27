//! Exporting to CSV (compatible with Anki import).

use contexts::ItemsInContextExt;
use err::Result;
use export::Exporter;

/// Export the video and subtitles as a CSV file with accompanying media
/// files, for import into Anki.
pub fn export_csv(exporter: &mut Exporter) -> Result<()> {

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

    }

    Ok(())
}

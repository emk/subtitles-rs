//! Export to web-page based "review" format.

use common_failures::prelude::*;
use failure::SyncFailure;
use handlebars::Handlebars;

use export::Exporter;
use lang::Lang;
use time::Period;

/// Information about a subtitle for use by our Handlebars HTML template.
#[derive(Debug, Serialize)]
struct SubtitleInfo {
    index: usize,
    image_path: String,
    audio_path: String,
    foreign_text: Option<String>,
    native_text: Option<String>,
}

/// Export-related information for use by our Handlebars HTML template.
#[derive(Debug, Serialize)]
struct ExportInfo {
    filename: String,
    subtitles: Vec<SubtitleInfo>,
    foreign_lang: Option<Lang>,
    native_lang: Option<Lang>,
}

/// Export the video and subtitles as a web page in "reviewable" format.
pub fn export_review(exporter: &mut Exporter) -> Result<()> {
    let foreign_lang = exporter.foreign().language;
    // Start preparing information we'll pass to our HTML template.
    let mut bindings = ExportInfo {
        filename: exporter.title().to_owned(),
        subtitles: vec![],
        foreign_lang: foreign_lang,
        native_lang: exporter.native().and_then(|n| n.language),
    };

    // Align our input files and iterate.
    let aligned = exporter.align();
    for sub in aligned.iter().enumerate() {
        let (i, &(ref foreign, ref native)) = sub;
        let index = i + 1;
        let period = Period::from_union_opt(
            foreign.as_ref().map(|s| s.period),
            native.as_ref().map(|s| s.period),
        ).expect("subtitle pair must not be empty")
            .grow(0.5, 0.5);

        let image_path = exporter.schedule_image_export(period.midpoint());
        let audio_path = exporter.schedule_audio_export(foreign_lang, period);

        bindings.subtitles.push(SubtitleInfo {
            index: index,
            image_path: image_path,
            audio_path: audio_path,
            foreign_text: foreign.as_ref().map(|s| s.plain_text()),
            native_text: native.as_ref().map(|s| s.plain_text()),
        });
    }

    // Write out our support files.
    exporter.export_data_file("style.css", include_bytes!("style.css"))?;
    exporter.export_data_file("play.svg", include_bytes!("play.svg"))?;

    // Render and write out our HTML.
    let template = include_str!("review.html.hbs");
    let mut handlebars = Handlebars::new();
    handlebars.register_template_string("review", template.to_owned())?;
    let html = handlebars
        .render("review", &bindings)
        .map_err(SyncFailure::new)?;
    exporter.export_data_file("index.html", html.as_bytes())?;

    // Extract our media files.
    exporter.finish_exports()?;

    Ok(())
}

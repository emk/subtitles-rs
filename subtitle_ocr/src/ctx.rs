//! The main OCR driver.

use common_failures::prelude::*;
use image::RgbaImage;
use std::path::Path;

use binarization::binarize;
use pixmap::Pixmap;
use segmentation::{group_into_glyphs, group_into_lines, segment};

/// An `OcrContext` represents a single movie's or episode's worth of
/// subtitles that we want to OCR.  To use it, create a new context, and
/// then add each individual subtitle image.
pub struct OcrContext {
    file_stem: String,
    next_id: usize,
}

impl OcrContext {
    /// Create a new `OcrContext`. The `base_path` is the name of the file
    /// that was used as input, which is used to make debug logging
    /// clearer.
    pub fn new(source_path: &Path) -> Result<OcrContext> {
        let file_stem = source_path.file_stem()
            .ok_or_else(|| {
                format_err!("expected a filename, not {}",
                            source_path.display())
            })?
            .to_string_lossy()
            .into_owned();
        Ok(OcrContext {
            file_stem: file_stem,
            next_id: 0,
        })
    }

    /// Add a single subtitle to this `OcrContext`.
    pub fn add(&mut self, _start: f64, _end: f64, image: &RgbaImage) -> Result<()> {
        let id = self.next_id;
        self.next_id += 1;
        // TODO: Get rid of `to_owned()`.
        let pixmap = Pixmap::from(image.to_owned());
        debug_pixmap!(&pixmap, "{}_{:04}_input.png", &self.file_stem, id);
        let bitmap = binarize(&pixmap)?;
        trace_pixmap!(&bitmap, "{}_{:04}_binarized.png", &self.file_stem, id);
        let (segmented, segments) = segment(&bitmap)?;
        debug_pixmap!(&segmented, "{}_{:04}_segmented.png", &self.file_stem, id);
        let lines = group_into_lines(pixmap.height(), segments)?;
        lines.into_iter().map(|l| group_into_glyphs(l)).collect::<Vec<_>>();
        Ok(())
    }
}

//! Convert images to black and white.

use cast;
use common_failures::prelude::*;
use image::Rgba;
use std::collections::HashMap;

use pixmap::{Pixel, Pixmap};
#[cfg(test)]
use test_util::{idx_fixture_pixmaps, rgba_hex};

/// Different kinds of colors we might find in an image.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ColorType {
    /// This color is transparent.
    Transparent,
    /// This color appears to be a shadow color which we should treat as
    /// transparent to facilitate letter separation.
    Shadow,
    /// This color is opaque, and should be used for letter recognition.
    Opaque,
}

impl From<ColorType> for usize {
    /// Map `ColorType` to something we can use to index an array.
    fn from(ct: ColorType) -> usize {
        match ct {
            ColorType::Transparent => 0,
            ColorType::Shadow => 1,
            ColorType::Opaque => 2,
        }
    }
}

/// Information about the `ColorType` of pixels adjacent to a given color.
#[derive(Debug)]
struct AdjacentPixelInfo {
    data: [usize; 3],
}

impl AdjacentPixelInfo {
    fn new() -> AdjacentPixelInfo {
        AdjacentPixelInfo { data: [0; 3] }
    }

    fn count(&self, ct: ColorType) -> usize {
        self.data[usize::from(ct)]
    }

    fn incr_count(&mut self, ct: ColorType) {
        self.data[usize::from(ct)] += 1;
    }

    fn total(&self) -> usize {
        self.data[0] + self.data[1] + self.data[2]
    }

    fn fraction_adj_to(&self, ct: ColorType) -> f64 {
        cast::f64(self.count(ct)) / cast::f64(self.total())
    }

    fn looks_like_opaque_inside_shadow(&self) -> bool {
        self.fraction_adj_to(ColorType::Opaque) > 0.95
    }

    fn looks_like_shadow(&self) -> bool {
        self.fraction_adj_to(ColorType::Opaque) > 0.33 &&
            self.fraction_adj_to(ColorType::Transparent) > 0.33
    }
}

/// Classify the colors in an image as transparent or non-transparent.
fn classify_colors(image: &Pixmap) -> Result<HashMap<Rgba<u8>, ColorType>> {
    // First divide colors into transparent and opaque based on alpha.  We
    // ensure that we always have an entry for the default color to
    // simplify the logic later on.
    let mut classification = HashMap::new();
    classification.insert(Rgba::default_color(), ColorType::Transparent);
    for px in image.pixels() {
        if px.is_transparent() {
            classification.entry(px).or_insert(ColorType::Transparent);
        } else {
            classification.entry(px).or_insert(ColorType::Opaque);
        }
    }
    debug!("color classification (initial): {:?}", &classification);

    // Calculate which colors are adjacent to which color classifications.
    // We'll use this to detect "shadow" colors.
    let mut adjacent = HashMap::new();
    for c in classification.keys() {
        if !c.is_transparent() {
            adjacent.insert(*c, AdjacentPixelInfo::new());
        }
    }
    for (x, y, px) in image.enumerate_pixels() {
        // Don't compute adjacency info for transparent pxiels.
        if px.is_transparent() {
            continue;
        }
        // Look at the 3x3 grid around this pixel.
        for (x2, y2) in image.all_neighbors(x, y) {
            // Get our neighboring pixel, if it's in bounds.
            let px_adj = image.get_default(x2, y2);

            // Ignore adjacent pixels of the same color.
            if px == px_adj {
                continue;
            }

            // Classify our neighboring color an increment our counter.
            let ct_adj = *classification.get(&px_adj)
                .expect("unknown classification");
            adjacent.get_mut(&px)
                .expect("unknown adjacent color")
                .incr_count(ct_adj);
        }
    }
    debug!("color adjacency: {:?}", &adjacent);

    // Check to see whether we have any opaque colors which seem to be
    // _surrounded_ by a shadow.
    let total_adj: usize = adjacent.values().map(|adj| adj.total()).sum();
    let mut have_opaque_inside_shadow = false;
    for adj in adjacent.values() {
        // Only check colors which make up a reasonable fraction of our
        // total.
        if adj.total() >= total_adj / 4 && adj.looks_like_opaque_inside_shadow() {
            have_opaque_inside_shadow = true;
        }
    }

    // If we have colors _surrounded_ by shadows, look for the shadow
    // colors.
    if have_opaque_inside_shadow {
        for (c, adj) in adjacent.iter() {
            if adj.looks_like_shadow() {
                classification.insert(c.to_owned(), ColorType::Shadow);
            }
        }
    }

    debug!("color classification (final): {:?}", &classification);
    Ok(classification)
}

#[test]
fn classify_colors_as_transparent_and_opaque() {
    //use env_logger;
    //let _ = env_logger::init();

    let colors = classify_colors(&idx_fixture_pixmaps()[0]).unwrap();
    assert_eq!(colors.len(), 4);
    assert_eq!(*colors.get(&rgba_hex(0x00000000)).unwrap(), ColorType::Transparent);
    assert_eq!(*colors.get(&rgba_hex(0x000000ff)).unwrap(), ColorType::Shadow);
    assert_eq!(*colors.get(&rgba_hex(0x999999ff)).unwrap(), ColorType::Opaque);
    assert_eq!(*colors.get(&rgba_hex(0xf0f0f0ff)).unwrap(), ColorType::Opaque);
}

/// Reduce an image to two "colors": Areas inside letters, and transparent
/// background.
pub fn binarize(pixmap: &Pixmap) -> Result<Pixmap<bool>> {
    let classification = classify_colors(pixmap)?;
    Ok(pixmap.map(|px| {
        let ct = classification.get(&px).expect("Color wasn't classified");
        ct == &ColorType::Opaque
    }))
}

#[test]
fn binarize_reduces_to_transparent_and_black() {
    //use env_logger;
    //let _ = env_logger::init();
    let bitmap = binarize(&idx_fixture_pixmaps()[0]).unwrap();
    debug_pixmap!(&bitmap, "binarize.png");
}

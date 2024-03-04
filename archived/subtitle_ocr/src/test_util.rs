//! Test-only utilities.

use image::{self, Rgba};
use std::path::Path;
use vobsub;

use pixmap::Pixmap;

/// Construct an RGBA color.
pub fn rgba_hex(hex: u32) -> Rgba<u8> {
    Rgba {
        data: [
            // 'as' is safe here because we know this can't overflow.
            ((hex & 0xff000000) >> 24) as u8,
            ((hex & 0x00ff0000) >> 16) as u8,
            ((hex & 0x0000ff00) >> 8) as u8,
            (hex & 0x000000ff) as u8,
        ]
    }
}

#[test]
fn rgba_hex_contructs_rbga_color() {
    assert_eq!(rgba_hex(0xf0f0f0ff),
               Rgba { data: [240, 240, 240, 255] });
}

/// Fetch some test images that we'll use in many tests.
pub fn idx_fixture_pixmaps() -> Vec<Pixmap> {
    let idx = vobsub::Index::open("../fixtures/example.idx")
        .expect("could not open subtitle");
    let mut images = vec![];
    for sub in idx.subtitles() {
        let sub = sub.expect("could not read subtitle");
        images.push(Pixmap::from(sub.to_image(idx.palette())));
    }
    images
}

/// Load a PNG file from our fixtures directory and convert it to a
/// `Pixmap`.
pub fn png_fixture_pixmap(base_name: &str) -> Pixmap {
    let path_str = format!("../fixtures/{}.png", base_name);
    let path = Path::new(&path_str);
    let img = image::open(path).expect("could not load image").to_rgba();
    Pixmap::from(img)
}

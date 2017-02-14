//! Test-only utilities.

use image::{Rgba, RgbaImage};
use vobsub;

use errors::*;

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
pub fn test_images() -> Result<Vec<RgbaImage>> {
    let idx = vobsub::Index::open("../fixtures/example.idx").unwrap();
    let mut images = vec![];
    for sub in idx.subtitles() {
        let sub = sub?;
        images.push(sub.to_image(idx.palette()));
    }
    Ok(images)
}

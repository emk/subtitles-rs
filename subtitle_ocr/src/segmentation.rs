//! Segmentation of images into continguous shapes.

use cast;
use image::Rgba;
use palette;
use palette::FromColor;
use palette::pixel::RgbPixel;

use pixmap::Pixel;

/// A virtual `Pixel` type which is used to help us extract contiguous
/// segments from an image.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SegmentInfo {
    /// This pixel is transparent (or was originally a "shadow" pixel that
    /// we've removed).
    Transparent,
    /// This pixel is opaque, but it has not yet been assigned to a
    /// segment.
    Unassigned,
    /// This pixel belongs to the segment with the specified ID.
    Id(u16),
}

impl Pixel for SegmentInfo {
    fn default_color() -> Self {
        SegmentInfo::Transparent
    }

    fn is_transparent(self) -> bool {
        self == SegmentInfo::Transparent
    }

    fn to_rgba(self) -> Rgba<u8> {
        // Try to pick nice, reasonably distinct colors for the `id`
        // values.  Based on
        // http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
        match self {
            SegmentInfo::Transparent => Rgba { data: [0, 0, 0, 0] },
            SegmentInfo::Unassigned => Rgba { data: [0, 0, 0, 0xff] },
            SegmentInfo::Id(id) => {
                const GOLDEN_RATIO: f32 = 0.618033988749895;
                let hue = ((0.94 + GOLDEN_RATIO*cast::f32(id)) % 1.0) * 360.0;
                let rgb_hue: palette::RgbHue<f32> =
                    palette::RgbHue::from(hue);
                let hsv = palette::Hsv::new(rgb_hue, 0.5, 0.95);
                let rgb = palette::Rgb::from_hsv(hsv);
                Rgba {
                    data: <[u8; 4]>::from_rgba(rgb.red, rgb.blue, rgb.green, 1.0),
                }
            }
        }
    }
}

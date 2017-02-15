//! Custom pixmaps which can handle kinds of data that the `image` library
//! doesn't support.

use cast;
use image::Rgba;
use palette::{self, FromColor};
use palette::pixel::RgbPixel;
use std::fmt;

/// A fully generic image type, which can hold non-graphical data.
pub struct Pixmap<P: Pixel> {
    data: Vec<P>,
    width: usize,
    height: usize,
}

impl<P: Pixel> Pixmap<P> {
    /// Create a new `Pixmap` filled with `P::default()`.
    pub fn blank(width: usize, height: usize) -> Pixmap<P> {
        Pixmap {
            data: vec![P::default(); width * height],
            width: width,
            height: height,
        }
    }

    /// If `x` and `y` do not fit within the pixmap, panic.
    fn bounds_check(&self, x: usize, y: usize) {
        if x >= self.width {
            panic!("out of bounds x: {} width: {}", x, self.width);
        }
        if y >= self.height {
            panic!("out of bounds y: {} height: {}", y, self.height);
        }
    }

    /// The width of the `Pixmap`.
    pub fn width(&self) -> usize {
        self.width
    }

    /// The height of the `Pixmap`.
    pub fn height(&self) -> usize {
        self.height
    }

    /// Get the pixel at `x` and `y`, or panic if out of bounds.
    pub fn get(&self, x: usize, y: usize) -> P {
        self.bounds_check(x, y);
        self.data[y*self.width + x]
    }

    /// Get the pixel at `x` and `y`, or return `P::default()` if out of
    /// bounds.
    pub fn get_default(&self, x: isize, y: isize) -> P {
        if x < 0 || y < 0 {
            return P::default();
        }
        let x = cast::usize(x).expect("cast failed unexpectedly");
        let y = cast::usize(y).expect("cast failed unexpectedly");
        if x >= self.width || y >= self.height {
            P::default()
        } else {
            self.data[y*self.width + x]
        }
    }

    /// Get a mutable reference to the pixel at `x` and `y`, or panic if
    /// out of bounds.
    pub fn get_mut(&mut self,  x: usize, y: usize) -> &mut P {
        self.bounds_check(x, y);
        &mut self.data[y*self.width + x]
    }
}

/// A type which can be used as a pixel in a `Pixmap`.
pub trait Pixel: Clone + Copy + fmt::Debug + Default + 'static {
    /// Return a RGBA color for this pixel.  Used for visualizing images
    /// with pixel types that aren't ordinary colors.
    fn to_rgba(self) -> Rgba<u8>;
}

impl Pixel for bool {
    fn to_rgba(self) -> Rgba<u8> {
        match self {
            false => Rgba { data: [0, 0, 0, 0] },
            true => Rgba { data: [0, 0, 0, 0xff] },
        }
    }
}

/// A virtual "pixel" type which is used to extract contiguous segments
/// from an image.
#[derive(Clone, Copy, Debug)]
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

impl Default for SegmentInfo {
    fn default() -> SegmentInfo {
        SegmentInfo::Transparent
    }
}

impl Pixel for SegmentInfo {
    fn to_rgba(self) -> Rgba<u8> {
        // Based on
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

//! Segmentation of images into continguous shapes.

use cast;
use image::Rgba;
use palette;
use palette::FromColor;
use palette::pixel::RgbPixel;
use std::cmp::{max, min};
use std::collections::VecDeque;

#[cfg(test)]
use binarization::binarize;
use errors::*;
use pixmap::{Pixel, Pixmap};
#[cfg(test)]
use test_util::test_images;

/// A virtual `Pixel` type which is used to help us extract contiguous
/// segments from an image.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SegmentPixel {
    /// This pixel is transparent (or was originally a "shadow" pixel that
    /// we've removed).
    Transparent,
    /// This pixel is opaque, but it has not yet been assigned to a
    /// segment.
    Unassigned,
    /// This pixel belongs to the segment with the specified ID.
    Id(u16),
}

impl Pixel for SegmentPixel {
    fn default_color() -> Self {
        SegmentPixel::Transparent
    }

    fn is_transparent(self) -> bool {
        self == SegmentPixel::Transparent
    }

    fn to_rgba(self) -> Rgba<u8> {
        // Try to pick nice, reasonably distinct colors for the `id`
        // values.  Based on
        // http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
        match self {
            SegmentPixel::Transparent => Rgba { data: [0, 0, 0, 0] },
            SegmentPixel::Unassigned => Rgba { data: [0, 0, 0, 0xff] },
            SegmentPixel::Id(id) => {
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

/// Information about a segment found in the image.
#[derive(Debug)]
pub struct Segment {
    /// The left-most edge of this segment.
    pub left: usize,
    /// The top-most edge of this segment.
    pub top: usize,
    /// The pixels included in this segment.  The `width()` and `height()`
    /// methods can be used to get the width and hight of the segment.
    pub pixmap: Pixmap<bool>,
    _placeholder: (),
}

/// Split an image into continguous segments of non-transparent pixels.
pub fn segment(pixmap: &Pixmap<bool>)
               -> Result<(Pixmap<SegmentPixel>, Vec<Segment>)> {
    // Construct a scatch image buffer using `SegmentPixel` as our pixel
    // type.  We'll use this to keep track of the actual floodfill.
    let mut scratch = pixmap.map(|p| {
        match p {
            false => SegmentPixel::Transparent,
            true => SegmentPixel::Unassigned,
        }
    });

    // Scan over all the pixels in the image, looking for pixels in
    // previously-unseen seguments.
    let mut result = vec![];
    let mut next_id = 0;
    for y_base in 0..scratch.height() {
        for x_base in 0..scratch.width() {
            // Is this a new segment?
            if scratch.get(x_base, y_base) == SegmentPixel::Unassigned {
                // Assign an ID for this segment.
                let id = next_id;
                next_id += 1;

                // Keep track of the min and max bounds of this segment.
                let mut x_min = x_base;
                let mut x_max = x_base;
                let mut y_min = y_base;
                let mut y_max = y_base;

                // Push the first point in this segment onto a queue.
                // We'll use this segment to keep track of the pixels that
                // we still need to process.  We use a queue instead of a
                // stack because we want to get old pixels off quickly,
                // because they tend to be filled sooner, reducing the
                // number of times we touch a pixel.
                let mut queue = VecDeque::new();
                queue.push_back((x_base, y_base));

                // Get the next item off our queue.
                while let Some((x, y)) = queue.pop_front() {
                    // If we've already been processed, bail.
                    if scratch.get(x, y) != SegmentPixel::Unassigned {
                        continue;
                    }

                    // Update our min and max bounds.
                    x_min = min(x_min, x);
                    x_max = max(x_max, x);
                    y_min = min(y_min, y);
                    y_max = max(y_max, y);

                    // Assign our segment ID to this pixel and queue any
                    // neighbors.
                    *scratch.get_mut(x, y) = SegmentPixel::Id(id);
                    for (x2, y2) in scratch.real_neighbors(x, y) {
                        if scratch.get(x2, y2) == SegmentPixel::Unassigned {
                            queue.push_back((x2, y2));
                        }
                    }
                }

                // Extract just the pixels of this segment into a smaller
                // buffer.
                let width = x_max + 1 - x_min;
                let height = y_max + 1 - y_min;
                let mut segment = Pixmap::blank(width, height);
                for j in 0..height {
                    for i in 0..width {
                        let in_segment = scratch.get(x_min+i, y_min+j)
                            == SegmentPixel::Id(id);
                        *segment.get_mut(i, j) = in_segment;
                    }
                }

                // Record our segment.
                result.push(Segment {
                    left: x_min,
                    top: y_min,
                    pixmap: segment,
                    _placeholder: ()
                });
            }
        }
    }

    Ok((scratch, result))
}

#[test]
fn segment_extracts_contiguous_regions() {
    let images = test_images().unwrap();
    let binarized = binarize(&Pixmap::from(images[0].clone())).unwrap();
    let (_segmented, segments) = segment(&binarized).unwrap();
    assert_eq!(segments.len(), 18);
}

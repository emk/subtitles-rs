//! Segmentation of images into continguous shapes.

use cast;
use common_failures::prelude::*;
use ext::RangeExt;
use image::Rgba;
use palette;
use palette::FromColor;
use palette::pixel::RgbPixel;
use std::cmp::{max, min};
use std::collections::VecDeque;

#[cfg(test)]
use binarization::binarize;
use geom::Rect;
use pixmap::{Pixel, Pixmap};
#[cfg(test)]
use test_util::{idx_fixture_pixmaps, png_fixture_pixmap};

/// The maximum permissible gap between two vertically-separated sections
/// of a letter.
const INTRA_LETTER_GAP: usize = 6;

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

impl Segment {
    fn bounds(&self) -> Rect {
        Rect::ltwh(self.left, self.top, self.pixmap.width(), self.pixmap.height())
    }
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
    let binarized = binarize(&idx_fixture_pixmaps()[0]).unwrap();
    let (_segmented, segments) = segment(&binarized).unwrap();
    assert_eq!(segments.len(), 18);
}

/// Group a list of letter segments into lines.
pub fn group_into_lines(height: usize, segments: Vec<Segment>)
                        -> Result<Vec<Vec<Segment>>> {
    // Find the blank lines.
    let mut histogram: Vec<u16> = vec![0; height];
    for segment in &segments {
        for i in segment.top..segment.top+segment.pixmap.height() {
            histogram[i] += 1;
        }
    }
    debug!("line histogram: {:?}", &histogram);

    // Find the contiguous non-blank lines.  Note that the bottom edge of
    // these runs is exclusive.
    let mut non_zero_runs = Vec::with_capacity(4);
    let mut i = 0;
    while i < height {
        // Skip section of zeros.
        while i < height && histogram[i] == 0 {
            i += 1;
        }

        // Record a second of non-zeros.
        let begin_run = i;
        while i < height && histogram[i] != 0 {
            i += 1;
        }
        if i > begin_run {
            non_zero_runs.push((begin_run, i));
        }
    }

    // Merge runs that are suspicously close.
    let mut merged_runs: Vec<(usize, usize)> = Vec::with_capacity(4);
    for run in non_zero_runs {
        if let Some(prev) = merged_runs.last_mut() {
            if prev.1 + INTRA_LETTER_GAP >= run.0 {
                prev.1 = run.1;
                continue;
            }
        }
        merged_runs.push(run);
    }
    debug!("merged runs: {:?}", &merged_runs);

    // Sort segments into lines.
    let mut lines = (0..merged_runs.len()).map(|_| vec![]).collect::<Vec<_>>();
    for segment in segments {
        // Again, bottom is exclusive.
        let line = merged_runs.iter()
            .position(|&(top, bottom)| {
                top <= segment.top &&
                    segment.top + segment.pixmap.height() <= bottom
            })
            .expect("letter does not belong to any line");
        lines[line].push(segment);
    }
    Ok(lines)
}

#[test]
fn group_into_lines_divides_subtitle_into_lines() {
    use env_logger;
    let _ = env_logger::init();

    let binarized = binarize(&png_fixture_pixmap("two_line_subtitle")).unwrap();
    let (_segmented, segments) = segment(&binarized).unwrap();
    let lines = group_into_lines(binarized.height(), segments).unwrap();
    assert_eq!(lines.len(), 2);
}

/// One or more `Segment`s that we believe make up a single letter.  We
/// attempt to rejoin the dots on "i"s, the two dots of a colon, the
/// accents above and below Latin letters--but we don't necessarily combine
/// the two quotes in a double quote.
///
/// Technically, these aren't true typographic glyphs, because they may
/// also be ligatures: Two or more typographic glyphs that are visually
/// connected.
pub struct Glyph {
    bounds: Rect,
    segments: Vec<Segment>,
}

impl Glyph {
    /// Create a new `Glyph` containing a single segment.
    fn new(segment: Segment) -> Glyph {
        Glyph {
            bounds: segment.bounds(),
            segments: vec![segment],
        }
    }

    /// Add a new segment to this glyph.
    fn add(&mut self, segment: Segment) {
        self.bounds = self.bounds.union(&segment.bounds());
        self.segments.push(segment);
    }

    // Convert this `Glyph` into something we could draw.
    //fn to_segment_pixmap(&self, id: u16) -> Pixmap<SegmentPixel> {
    //}
}

pub fn group_into_glyphs(mut segments: Vec<Segment>) -> Vec<Glyph> {
    // Sort our segments from left to right (and throw in some secondary
    // sort keys just to make things a bit more predictable--not sure if
    // this helps).
    segments.sort_by_key(|s| (s.left, s.pixmap.width()));

    // Collect glyphs into segments.
    let mut result: Vec<Glyph> = vec![];
    for segment in segments {
        if let Some(prev_glyph) = result.last_mut() {
            let prev_range = prev_glyph.bounds.horizontal_range();
            let curr_range = segment.bounds().horizontal_range();
            let overlap = prev_range.intersection(&curr_range).len();
            // This will probably need tweaking for best results.
            if overlap >= prev_range.len() / 3 || overlap >= curr_range.len() / 3 {
                prev_glyph.add(segment);
                continue;
            }
        }
        result.push(Glyph::new(segment));
    }
    result
}

#[test]
fn combine_segments_attaches_dots_and_diacritics() {
    let binarized = binarize(&png_fixture_pixmap("ligature_2")).unwrap();
    let (_segmented, segments) = segment(&binarized).unwrap();
    let lines = group_into_lines(binarized.height(), segments).unwrap();
    lines.into_iter()
        .map(|line| group_into_glyphs(line))
        .collect::<Vec<_>>();
}

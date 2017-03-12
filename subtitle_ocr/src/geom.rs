//! Geometry-related types.

use std::cmp::{max, min};
use std::ops::Range;

use errors::*;

/// A rectangle.
///
/// TODO: Maybe merge with `vobsub::Coords`?  Or find a third-party library
/// for this?
#[derive(Clone, Debug)]
pub struct Rect {
    left: usize,
    top: usize,
    width: usize,
    height: usize,
}

// Allow some of these API methods to be unused.
#[allow(dead_code)]
impl Rect {
    /// Create a rectangle by specifying the left, top, width and height
    /// values.
    pub fn ltwh(l: usize, t: usize, w: usize, h: usize) -> Rect {
        Rect {
            left: l,
            top: t,
            width: w,
            height: h,
        }
    }

    /// Create a rectangle from left and top (inclusive) and right and
    /// bottom (exclusive) coordinates. Returns an error if the rectangle
    /// has negative height or width.
    pub fn ltrb(l: usize, t: usize, r: usize, b: usize) -> Result<Rect> {
        if r < l {
            return Err("rectangle has negative width".into());
        }
        if b < t {
            return Err("rectangle has negative height".into());
        }
        Ok(Rect {
            left: l,
            top: t,
            width: r-l,
            height: b-t,
        })
    }

    /// The left-most edge of the rectangle (inclusive).
    pub fn left(&self) -> usize {
        self.left
    }

    /// The top-most edge of the rectangle (inclusive).
    pub fn top(&self) -> usize {
        self.top
    }

    /// The right-most edge of the rectangle (exclusive).
    pub fn right(&self) -> usize {
        self.left + self.width
    }

    /// The bottom-most edge of the rectangle (exclusive).
    pub fn bottom(&self) -> usize {
        self.top + self.height
    }

    /// The width of the rectangle.
    pub fn width(&self) -> usize {
        self.width
    }

    /// The height of the rectangle.
    pub fn height(&self) -> usize {
        self.height
    }

    /// Does this rectangle have area zero?
    pub fn is_empty(&self) -> bool {
        self.width == 0 || self.height == 0
    }

    /// Return a rectangle including all the area included by this
    /// rectangle and another.  If either rectangle has zero area, it will
    /// be excluded.
    pub fn union(&self, other: &Rect) -> Rect {
        if other.is_empty() {
            self.to_owned()
        } else if self.is_empty() {
            other.to_owned()
        } else {
            Rect::ltrb(min(self.left, other.left),
                       min(self.top, other.top),
                       max(self.right(), other.right()),
                       max(self.bottom(), other.bottom()))
                .expect("rectangle must have non-negative width and height")
        }
    }

    /// Get a range from `left..right`.
    pub fn horizontal_range(&self) -> Range<usize> {
        self.left..self.right()
    }
}

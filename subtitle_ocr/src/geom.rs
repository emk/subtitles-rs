//! Geometry-related types.

use std::cmp::{max, min};
use std::ops::Range;

/// A rectangle.
///
/// TODO: Maybe merge with `vobsub::Coords`?  Or find a third-party library
/// for this?
#[derive(Clone, Debug, Eq, PartialEq)]
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
    /// values.  Panics if the rectangles right or bottom coordinates are
    /// out-of-bounds.
    pub fn ltwh(l: usize, t: usize, w: usize, h: usize) -> Rect {
        l.checked_add(w).expect("rectangle right is larger than usize");
        t.checked_add(h).expect("rectangle bottom is larger than usize");
        Rect {
            left: l,
            top: t,
            width: w,
            height: h,
        }
    }

    /// Create a rectangle from left and top (inclusive) and right and
    /// bottom (exclusive) coordinates. Panics if the rectangle
    /// has negative height or width.
    pub fn ltrb(l: usize, t: usize, r: usize, b: usize) -> Rect {
        Rect {
            left: l,
            top: t,
            width: r.checked_sub(l).expect("rectangle has negative width"),
            height: b.checked_sub(t).expect("rectangle has negative height"),
        }
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

    /// Is the specified point in this rectangle?
    pub fn contains(&self, x: usize, y: usize) -> bool {
        self.left <= x && x < self.right() &&
            self.top <= y && y < self.bottom()
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
        }
    }

    /// Get a range from `left..right`.
    pub fn horizontal_range(&self) -> Range<usize> {
        self.left..self.right()
    }
}

#[cfg(test)]
mod test {
    use quickcheck::{Arbitrary, Gen, TestResult};
    use super::*;

    impl Arbitrary for Rect {
        fn arbitrary<G: Gen>(g: &mut G) -> Self {
            let s = g.size();
            Rect {
                left: g.gen_range(0, s),
                top: g.gen_range(0, s),
                width: g.gen_range(0, s),
                height: g.gen_range(0, s),
            }
        }

        fn shrink(&self) -> Box<Iterator<Item=Self>> {
            let tuple = (self.left, self.top, self.width, self.height);
            Box::new(tuple.shrink().map(|(l, t, w, h)| Rect::ltwh(l, t, w, h)))
        }
    }

    quickcheck! {
        fn rect_width_and_height_are_valid(r: Rect) -> bool {
            r.width() == r.right() - r.left() &&
                r.height() == r.bottom() - r.top()
        }

        fn rect_union_includes_all_points(r1: Rect, r2: Rect) -> bool {
            let u = r1.union(&r2);
            ((r1.is_empty() ||
              u.contains(r1.left, r1.top) &&
              u.contains(r1.right()-1, r1.bottom()-1)) &&
             (r2.is_empty() ||
              u.contains(r2.left, r2.top) &&
              u.contains(r2.right()-1, r2.bottom()-1)))
        }

        fn rect_union_with_zero_size_is_identity(r1: Rect, r2: Rect)
                                                 -> TestResult {
            if r2.is_empty() {
                TestResult::from_bool(r1.union(&r2) == r1)
            } else if r1.is_empty() {
                TestResult::from_bool(r1.union(&r2) == r2)
            } else {
                TestResult::discard()
            }
        }
    }
}

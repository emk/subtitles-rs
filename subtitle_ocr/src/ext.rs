//! Extensions to types provided by other libraries.

use std::cmp::{max, min};
use std::ops::Range;

/// Addition methods for the standard `Range` type.
pub trait RangeExt<Idx> {
    /// Returns a range contain all values that appear in _both_ ranges, or
    /// an empty range if there is no overlap.
    fn intersection(&self, other: &Range<Idx>) -> Range<Idx>;
}

impl RangeExt<usize> for Range<usize> {
    fn intersection(&self, other: &Range<usize>) -> Range<usize> {
        if self.len() == 0 {
            self.to_owned()
        } else if other.len() == 0 {
            other.to_owned()
        } else {
            let start = max(self.start, other.start);
            let mut end = min(self.end, other.end);
            if end < start {
                end = start
            }
            start..end
        }
    }
}

#[test]
fn range_intersection() {
    assert_eq!((0..1).intersection(&(0..1)), 0..1);
    assert_eq!((0..2).intersection(&(0..1)), 0..1);
    assert_eq!((0..1).intersection(&(0..2)), 0..1);
    assert!((0..1).intersection(&(1..2)).len() == 0);
    assert_eq!((0..2).intersection(&(1..2)), 1..2);
    assert_eq!((1..2).intersection(&(0..2)), 1..2);
    assert_eq!((0..1).intersection(&(10..10)), 10..10);
    assert_eq!((0..0).intersection(&(10..11)), 0..0);
}

#[cfg(test)]
mod test {
    use super::*;
    quickcheck! {
        fn intersection_points_in_both_ranges(r1: Range<usize>,
                                              r2: Range<usize>)
                                              -> bool {
            r1.intersection(&r2)
                .all(|i| {
                    r1.start <= i && i < r1.end && r2.start <= i && i < r2.end
                })
        }

        fn intersection_does_not_grow(r1: Range<usize>,
                                      r2: Range<usize>)
                                      -> bool {
            let result = r1.intersection(&r2);
            result.len() <= r1.len() && result.len() <= r2.len()
        }
    }
}

//! Tools for working with time.

use rustc_serialize::{Encodable, Encoder};
use std::result;

use errors::*;

/// The minimum spacing between two points in time to count as
/// unambiguously different.  This is related to the typical precision used
/// in *.srt subtitle files.
pub const MIN_SPACING: f32 = 0.001;

// Break seconds down into hours, minutes and seconds.
fn decompose_time(time: f32) -> (u32, u32, f32) {
    let mut seconds = time;
    let hours = (seconds / 3600.0).floor() as u32;
    seconds %= 3600.0;
    let mins = (seconds / 60.0).floor() as u32;
    seconds %= 60.0;
    (hours, mins, seconds)
}

/// Converts a time to a pretty, human-readable format, with second
/// precision.
///
/// ```
/// use substudy::time::seconds_to_hhmmss;
/// assert_eq!("3:02:01", seconds_to_hhmmss(3.0*3600.0+2.0*60.0+1.001));
/// ```
pub fn seconds_to_hhmmss(time: f32) -> String {
    let (hours, mins, seconds) = decompose_time(time);
    format!("{}:{:02}:{:02}", hours, mins, (seconds.floor() as u32))
}

/// Converts a time to a pretty, human-readable format, with millisecond
/// precision.
///
/// ```
/// use substudy::time::seconds_to_hhmmss_sss;
/// assert_eq!("3:02:01.001", seconds_to_hhmmss_sss(3.0*3600.0+2.0*60.0+1.001));
/// ```
pub fn seconds_to_hhmmss_sss(time: f32) -> String {
    let (hours, mins, seconds) = decompose_time(time);
    format!("{}:{:02}:{:06.3}", hours, mins, seconds)
}

/// A period of time, in seconds.  The beginning is guaranteed to be less
/// than the end, and all times are positive.  This is lightweight
/// structure which implements `Copy`, so it can be passed by value.
///
/// ```
/// use substudy::time::Period;
///
/// let period = Period::new(1.0, 5.0).unwrap();
/// assert_eq!(1.0, period.begin());
/// assert_eq!(5.0, period.end());
/// assert_eq!(4.0, period.duration());
/// assert_eq!(3.0, period.midpoint());
/// ```
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Period {
    begin: f32,
    end: f32,
}

impl Period {
    /// Create a new time period.
    pub fn new(begin: f32, end: f32) -> Result<Period> {
        if begin < end && begin >= 0.0 && end >= 0.0 {
            Ok(Period { begin: begin, end: end })
        } else {
            Err(err_str(format!("Beginning of range is before end: {}-{}",
                                begin, end)))
        }
    }

    /// Construct a time period from two optional time periods, taking the
    /// union if both are present.  This is normally used when working with
    /// aligned subtitle pairs, either of which might be missing.
    ///
    /// ```
    /// use substudy::time::Period;
    ///
    /// assert_eq!(None, Period::from_union_opt(None, None));
    ///
    /// let p1 = Period::new(1.0, 2.0).unwrap();
    /// let p2 = Period::new(2.5, 3.0).unwrap();
    /// assert_eq!(Some(p1),
    ///            Period::from_union_opt(Some(p1), None));
    /// assert_eq!(Some(Period::new(1.0, 3.0).unwrap()),
    ///            Period::from_union_opt(Some(p1), Some(p2)));
    /// ```
    pub fn from_union_opt(p1: Option<Period>, p2: Option<Period>) ->
        Option<Period>
    {
        match (p1, p2) {
            (None, None) => None,
            (Some(p), None) => Some(p),
            (None, Some(p)) => Some(p),
            (Some(p1), Some(p2)) => Some(p1.union(p2)),
        }
    }

    /// The beginning of this time period.
    pub fn begin(&self) -> f32 {
        self.begin
    }

    /// The end of this time period.
    pub fn end(&self) -> f32 {
        self.end
    }

    /// How long this time period lasts.
    pub fn duration(&self) -> f32 {
        self.end - self.begin
    }

    /// The midpoint of this time period.
    pub fn midpoint(&self) -> f32 {
        self.begin + self.duration()/2.0
    }

    /// Grow this time period by the specified amount, making any necessary
    /// adjustments to keep it valid.
    ///
    /// ```
    /// use substudy::time::Period;
    ///
    /// let period = Period::new(1.0, 5.0).unwrap();
    /// assert_eq!(Period::new(0.0, 7.0).unwrap(),
    ///            period.grow(1.5, 2.0));
    /// ```
    pub fn grow(&self, before: f32, after: f32) -> Period {
        let mid = self.midpoint();
        Period {
            begin: (self.begin - before).min(mid).max(0.0),
            end: (self.end + after).max(mid + MIN_SPACING),
        }
    }

    /// Calculate the smallest time period containing this time period and
    /// another.
    ///
    /// ```
    /// use substudy::time::Period;
    ///
    /// let p1 = Period::new(1.0, 2.0).unwrap();
    /// let p2 = Period::new(2.5, 3.0).unwrap();
    /// assert_eq!(Period::new(1.0, 3.0).unwrap(),
    ///            p1.union(p2));
    /// ```
    pub fn union(&self, other: Period) -> Period {
        Period {
            begin: self.begin.min(other.begin),
            end: self.end.max(other.end),
        }
    }

    /// Make sure this subtitle begins after `limit`.
    pub fn begin_after(&mut self, limit: f32) -> Result<()> {
        if limit > self.end - 2.0*MIN_SPACING {
            try!(Err(err_str(format!("Cannot begin time period {:?} after {}",
                                     self, limit))));
        }

        self.begin = self.begin.max(limit + MIN_SPACING);
        Ok(())
    }

    /// Truncate this subtitle before `limit`, which must be at least
    /// `2*MIN_SPACING` greater than the begin time.
    pub fn end_before(&mut self, limit: f32) -> Result<()> {
        if limit < self.begin + 2.0*MIN_SPACING {
            try!(Err(err_str(format!("Cannot truncate time period {:?} at {}",
                                     self, limit))));
        }

        self.end = self.end.min(limit - MIN_SPACING);
        Ok(())
    }

    /// Return the absolute value of the distance between two durations, or
    /// `None` if the durations overlap.
    ///
    /// ```
    /// use substudy::time::Period;
    ///
    /// let p1 = Period::new(1.0, 2.0).unwrap();
    /// let p2 = Period::new(2.0, 3.0).unwrap();
    /// let p3 = Period::new(2.5, 3.0).unwrap();
    /// assert_eq!(Some(0.5), p1.distance(p3));
    /// assert_eq!(Some(0.5), p3.distance(p1));
    /// assert_eq!(Some(0.0), p1.distance(p2));
    /// assert_eq!(None, p2.distance(p3));
    /// ```
    pub fn distance(&self, other: Period) -> Option<f32> {
        if self.end <= other.begin {
            Some((other.begin - self.end).abs())
        } else if other.end <= self.begin {
            Some((self.begin - other.end).abs())
        } else {
            None
        }
    }

    /// Return the total amount of time which appears in both durations.
    ///
    /// ```
    /// use substudy::time::Period;
    ///
    /// let p1 = Period::new(1.0, 2.0).unwrap();
    /// let p2 = Period::new(2.0, 3.0).unwrap();
    /// let p3 = Period::new(2.5, 3.0).unwrap();
    /// assert_eq!(0.0, p1.overlap(p3));
    /// assert_eq!(0.0, p3.overlap(p1));
    /// assert_eq!(0.0, p1.overlap(p2));
    /// assert_eq!(0.5, p2.overlap(p3));
    /// ```
    pub fn overlap(&self, other: Period) -> f32 {
        (self.end.min(other.end) - self.begin.max(other.begin)).max(0.0)
    }
}

impl Encodable for Period {
    fn encode<S: Encoder>(&self, s: &mut S) -> result::Result<(), S::Error> {
        s.emit_seq(2, |s1| {
            try!(s1.emit_seq_elt(0, |s2| s2.emit_f32(self.begin)));
            s1.emit_seq_elt(1, |s2| s2.emit_f32(self.end))
        })
    }
}

/// Convert a time to a timestamp string.  This is mostly used for giving
/// files semi-unique names so that we can dump files from multiple,
/// related export runs into a single directory without too much chance of
/// them overwriting each other unless they're basically the same file.
pub trait ToTimestamp {
    /// Convert to a string describing this time.
    fn to_timestamp(&self) -> String;

    /// Convert to a string describing this time, replacing periods with
    /// "_".
    fn to_file_timestamp(&self) -> String {
        self.to_timestamp().replace(".", "_")
    }
}

impl ToTimestamp for f32 {
    fn to_timestamp(&self) -> String {
        format!("{:09.3}", *self)
    }
}

impl ToTimestamp for Period {
    fn to_timestamp(&self) -> String {
        format!("{:09.3}-{:09.3}", self.begin(), self.end())
    }
}

#[test]
fn test_timestamp() {
    assert_eq!("00010.500", (10.5).to_timestamp());
    let period = Period::new(10.0, 20.0).unwrap();
    assert_eq!("00010.000-00020.000", period.to_timestamp());
    assert_eq!("00010_000-00020_000", period.to_file_timestamp());
}

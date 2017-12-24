//! Custom pixmaps which can handle kinds of data that the `image` library
//! doesn't support.

use cast;
use common_failures::prelude::*;
use image::{ImageBuffer, Rgba, RgbaImage};
use std::fmt;
use std::slice;

#[cfg(test)]
use test_util::rgba_hex;

/// A type which can be used as a pixel in a `Pixmap`.
pub trait Pixel: Clone + Copy + fmt::Debug + 'static {
    /// This is basically just `Default::default`. We would normally just
    /// base this trait on `Default`, but we also want to implement this
    /// trait for `image::Rgba`, which doesn't implement `Default`.
    fn default_color() -> Self;

    /// Is this pixel transparent?
    fn is_transparent(self) -> bool;

    /// Return a RGBA color for this pixel.  Used for visualizing images
    /// with pixel types that aren't ordinary colors.
    fn to_rgba(self) -> Rgba<u8>;
}

impl Pixel for bool {
    fn default_color() -> Self {
        false
    }

    fn is_transparent(self) -> bool {
        !self
    }

    fn to_rgba(self) -> Rgba<u8> {
        match self {
            false => Rgba { data: [0, 0, 0, 0] },
            true => Rgba { data: [0, 0, 0, 0xff] },
        }
    }
}

#[test]
fn bool_implements_pixel() {
    assert_eq!(bool::default_color(), false);
    assert_eq!(true.is_transparent(), false);
    assert_eq!(false.is_transparent(), true);
    assert_eq!(true.to_rgba(), rgba_hex(0x000000ff));
    assert_eq!(false.to_rgba(), rgba_hex(0x00000000));
}

impl Pixel for Rgba<u8> {
    fn default_color() -> Self {
        Rgba { data: [0, 0, 0, 0] }
    }

    fn is_transparent(self) -> bool {
        self.data[3] < 0xff
    }

    fn to_rgba(self) -> Rgba<u8> {
        self
    }
}

#[test]
fn rgba_implements_pixel() {
    assert_eq!(Rgba::<u8>::default_color(), rgba_hex(0x00000000));
    assert_eq!(rgba_hex(0x000000ff).is_transparent(), false);
    assert_eq!(rgba_hex(0x00000000).is_transparent(), true);
    assert_eq!(rgba_hex(0xff0000ff).to_rgba(), rgba_hex(0xff0000ff));
}

/// A fully generic image type, which can hold non-graphical data.
pub struct Pixmap<P: Pixel = Rgba<u8>> {
    data: Vec<P>,
    width: usize,
    height: usize,
}

impl<P: Pixel> Pixmap<P> {
    /// Make sure that these specified image size is legal.
    fn size_check(width: usize, height: usize) {
        let max_dim = isize::max_value() as usize;
        if width > max_dim || height > max_dim {
            panic!("image dimensions {}x{} are too large", width, height);
        }
        if width.checked_mul(height).is_none() {
            panic!("image area {}x{} is too large", width, height);
        }
    }

    /// Create a new `Pixmap` filled with `P::default_color()`.
    pub fn blank(width: usize, height: usize) -> Pixmap<P> {
        Pixmap::<P>::size_check(width, height);
        Pixmap {
            data: vec![P::default_color(); width * height],
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

    /// Get the pixel at `x` and `y`, or return `P::default_color()` if out
    /// of bounds.
    pub fn get_default(&self, x: isize, y: isize) -> P {
        if x < 0 || y < 0 {
            return P::default_color();
        }
        // `as usize` is safe here because we're verified x and y are
        // non-negative.
        let x = x as usize;
        let y = y as usize;
        if x >= self.width || y >= self.height {
            P::default_color()
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

    /// Iterate over all the pixels in an image.
    pub fn pixels(&self) -> Pixels<P> {
        Pixels { iter: self.data.iter() }
    }

    /// Iterate over `(x, y, value)` for all the pixels in an image.
    pub fn enumerate_pixels(&self) -> EnumeratePixels<P> {
        EnumeratePixels {
            pixmap: self,
            x: 0,
            y: 0,
        }
    }

    /// The coordinates `(x, y)` of all neighboring pixels, including
    /// out-of-bounds pixels.
    pub fn all_neighbors(&self, x: usize, y: usize) -> AllNeighbors {
        AllNeighbors {
            x: cast::isize(x).expect("x outside maximum image size"),
            y: cast::isize(y).expect("y outside maximum image size"),
            i: 0,
        }
    }

    /// The coordinates `(x, y)` of all neighboring pixels, including
    /// only in-bounds pixels.
    pub fn real_neighbors(&self, x: usize, y: usize) -> RealNeighbors {
        RealNeighbors {
            width: self.width,
            height: self.height,
            x: x,
            y: y,
            i: 0,
        }
    }

    /// Transform each pixel of the image.
    pub fn map<F, P2>(&self, f: F) -> Pixmap<P2>
        where F: Fn(P) -> P2, P2: Pixel
    {
        Pixmap {
            data: self.data.iter().map(|p| f(*p)).collect(),
            width: self.width,
            height: self.height,
        }
    }

    /// Convert this `Pixmap` into a `RgbaImage`, for easy output.
    pub fn to_image(&self) -> Result<RgbaImage> {
        let mut raw: Vec<_> = Vec::with_capacity(self.data.len() * 4);
        for px in &self.data {
            let rgba = px.to_rgba().data;
            raw.extend_from_slice(&[rgba[0], rgba[1], rgba[2], rgba[3]]);
        }
        Ok(ImageBuffer::from_raw(u32_from_usize(self.width)?,
                                 u32_from_usize(self.height)?,
                                 raw).expect("image bounds mismatch"))
    }
}

#[cfg(target_pointer_width = "64")]
fn u32_from_usize(i: usize) -> Result<u32> {
    // This can fail on 64-bit platforms.
    Ok(cast::u32(i)?)
}

#[cfg(target_pointer_width = "32")]
fn u32_from_usize(i: usize) -> Result<u32> {
    // This will always succeed on 32-bit platforms.
    Ok(cast::u32(i))
}

/// An iterator over all the pixels in an image.
pub struct Pixels<'a, P: Pixel> {
    iter: slice::Iter<'a, P>,
}

impl<'a, P: Pixel> Iterator for Pixels<'a, P> {
    type Item = P;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|v| *v)
    }
}

/// An iterator over `(x, y, value)` for all the pixels in an image.
pub struct EnumeratePixels<'a, P: Pixel> {
    pixmap: &'a Pixmap<P>,
    x: usize,
    y: usize,
}

impl<'a, P: Pixel> Iterator for EnumeratePixels<'a, P> {
    type Item = (usize, usize, P);
    fn next(&mut self) -> Option<Self::Item> {
        if self.pixmap.width == 0 || self.pixmap.height == 0 {
            return None;
        }
        if self.x >= self.pixmap.width {
            self.x = 0;
            self.y += 1;
        }
        if self.y >= self.pixmap.height {
            return None;
        }
        assert!(self.x < self.pixmap.width);
        assert!(self.y < self.pixmap.height);
        let px = self.pixmap.data[self.y*self.pixmap.width + self.x];
        let result = Some((self.x, self.y, px));
        self.x += 1;
        result
    }
}

/// Delta coordinates for neighboring pixels.
const NEIGHBORS: &'static [(isize, isize)] =
    &[(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)];


/// An iterator over the coordinates `(x, y)` of all neighboring pixels,
/// including out-of-bounds pixels.
pub struct AllNeighbors {
    x: isize,
    y: isize,
    i: usize,
}

impl Iterator for AllNeighbors {
    type Item = (isize, isize);
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(&(dx, dy)) = NEIGHBORS.get(self.i) {
            self.i += 1;
            Some((self.x + dx, self.y + dy))
        } else {
            None
        }
    }
}

/// An iterator over the coordinates `(x, y)` of all neighboring pixels,
/// including only in-bounds pixels.
pub struct RealNeighbors {
    width: usize,
    height: usize,
    x: usize,
    y: usize,
    i: usize,
}

impl Iterator for RealNeighbors {
    type Item = (usize, usize);
    fn next(&mut self) -> Option<Self::Item> {
        while let Some(&(dx, dy)) = NEIGHBORS.get(self.i) {
            self.i += 1;
            if self.x == 0 && dx == -1 || self.y == 0 && dy == -1 {
                continue;
            }
            // `as isize` is safe because of `size_check`, `as usize` is
            // safe because we eliminated sums of -1 above.
            let x = ((self.x as isize) + dx) as usize;
            let y = ((self.y as isize) + dy) as usize;
            if x == self.width || y == self.height {
                continue;
            }
            return Some((x, y))
        }
        None
    }
}

impl From<RgbaImage> for Pixmap<Rgba<u8>> {
    fn from(image: RgbaImage) -> Pixmap<Rgba<u8>> {
        let width = cast::usize(image.width());
        let height = cast::usize(image.height());
        let channels = image.into_raw();
        assert_eq!(channels.len(), width * height * 4);
        let mut data = Vec::with_capacity(width * height);
        for chunk in channels.chunks(4) {
            data.push(Rgba { data: [chunk[0], chunk[1], chunk[2], chunk[3]] });
        }
        Pixmap {
            data: data,
            width: width,
            height: height,
        }
    }
}

impl<P: Pixel> fmt::Debug for Pixmap<P> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Pixmap")
            .field("width", &self.width)
            .field("height", &self.height)
            .finish()
    }
}

#[test]
fn blank_fills_image_with_default() {
    let pixmap = Pixmap::<bool>::blank(1, 2);
    assert_eq!(pixmap.width(), 1);
    assert_eq!(pixmap.height(), 2);
    assert_eq!(pixmap.get(0, 0), false);
}

#[test]
fn get_and_get_mut_provide_access_to_pixels() {
    let mut pixmap = Pixmap::<bool>::blank(1, 1);
    assert_eq!(pixmap.get(0, 0), false);
    *pixmap.get_mut(0, 0) = true;
    assert_eq!(pixmap.get(0, 0), true);
}

#[test]
#[should_panic]
fn get_panics_when_out_of_bounds() {
    let pixmap = Pixmap::<bool>::blank(1, 1);
    pixmap.get(1, 1);
}

#[test]
fn get_default_returns_default_color_when_out_of_bounds() {
    let mut pixmap = Pixmap::<bool>::blank(1, 1);
    *pixmap.get_mut(0, 0) = true;
    assert_eq!(pixmap.get_default(0, -1), false);
    assert_eq!(pixmap.get_default(-1, 0), false);
    assert_eq!(pixmap.get_default(0, 1), false);
    assert_eq!(pixmap.get_default(1, 0), false);
}

#[test]
fn pixels_iterates_over_all_pixel_values() {
    let mut pixmap = Pixmap::<bool>::blank(2, 1);
    *pixmap.get_mut(0, 0) = true;
    let pixels = pixmap.pixels().collect::<Vec<_>>();
    assert_eq!(pixels, &[true, false]);
}


#[test]
fn enumerate_pixels_iterates_over_pixel_values_and_coordinates() {
    let mut pixmap = Pixmap::<bool>::blank(2, 1);
    *pixmap.get_mut(0, 0) = true;
    let output = pixmap.enumerate_pixels().collect::<Vec<_>>();
    assert_eq!(output, &[(0, 0, true), (1, 0, false)]);
}

#[test]
fn all_neighbors_iterates_over_all_neighbor_coordinates() {
    let pixmap = Pixmap::<bool>::blank(2, 1);
    let output = pixmap.all_neighbors(1, 0).collect::<Vec<_>>();
    assert_eq!(output,
               &[(0, -1), (1, -1), (2, -1),
                 (0, 0), (2, 0),
                 (0, 1), (1, 1), (2, 1)]);

}

#[test]
fn real_neighbors_iterates_over_in_bounds_coordinates() {
    let pixmap = Pixmap::<bool>::blank(2, 1);
    let output = pixmap.real_neighbors(1, 0).collect::<Vec<_>>();
    assert_eq!(output, &[(0, 0)]);
}

#[test]
fn map_creates_a_new_image_by_applying_a_function() {
    let mut pixmap = Pixmap::<bool>::blank(1, 2);
    *pixmap.get_mut(0, 0) = true;
    let mapped = pixmap.map(|p| {
        match p {
            true => rgba_hex(0xff0000ff),
            false => rgba_hex(0x00ff00ff),
        }
    });
    assert_eq!(mapped.get(0, 0), rgba_hex(0xff0000ff));
    assert_eq!(mapped.get(0, 1), rgba_hex(0x00ff00ff));
}

#[test]
fn to_image_converts_to_image() {
    let mut pixmap = Pixmap::<bool>::blank(1, 2);
    *pixmap.get_mut(0, 0) = true;
    let image = pixmap.to_image().unwrap();
    assert_eq!(image.width(), 1);
    assert_eq!(image.height(), 2);
    assert_eq!(image[(0, 0)], rgba_hex(0x000000ff));
    assert_eq!(image[(0, 1)], rgba_hex(0x00000000));
}

#[test]
fn from_image_constructs_from_rgba_image() {
    let image: RgbaImage = ImageBuffer::from_pixel(2, 1, rgba_hex(0xff0000ff));
    let pixmap = Pixmap::from(image);
    assert_eq!(pixmap.width(), 2);
    assert_eq!(pixmap.height(), 1);
    assert_eq!(pixmap.get(0, 0), rgba_hex(0xff0000ff));
}

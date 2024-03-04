//! Support for logging image files to a specified directory using the same
//! log levels as the `log` framework.

#![allow(unused_macros)]

use std::env;
use std::fs;
use std::thread;
use std::time;
use std::path::{Path, PathBuf};

use pixmap::{Pixel, Pixmap};

/// Get the path to which we are supposed to log images.
#[allow(dead_code)]
pub fn log_image_dir() -> Option<&'static Path> {
    lazy_static! {
        static ref PATH: Option<PathBuf> = {
            env::var("RUST_LOG_IMAGE_DIR")
                .ok()
                .map(|v| Path::new(&v).to_owned())
        };
    }
    match PATH.as_ref() {
        Some(p) => Some(&*p),
        None => None,
    }
}

/// Write an image to the specified directory.
#[allow(dead_code)]
pub fn write_pixmap<P: Pixel>(path: &Path, pixmap: &Pixmap<P>) {
    let parent = path.parent().expect("Cannot get parent dir");
    let mut create_dir_result = Ok(());
    for _ in 0..10 {
        create_dir_result = fs::create_dir_all(&parent);
        if create_dir_result.is_ok() {
            break;
        }
        thread::sleep(time::Duration::from_millis(100));
    }
    create_dir_result.expect("Unable to create RUST_LOG_IMAGE_DIR");
    pixmap.to_image().unwrap().save(path).expect("Unable to log image");
}

/// If `lvl` would normally be logged by the `log` framework, and
/// `RUST_LOG_IMAGE_DIR` is set, then use `$($path_arg)+` as a format
/// pattern to construct a file name, and write out the image.
macro_rules! log_pixmap {
    ($lvl:expr, $image:expr, $($path_arg:tt)+) => ({
        if log_enabled!($lvl) {
            if let Some(dir) = $crate::logimg::log_image_dir() {
                let file_path = dir.join(format!($($path_arg)+));
                log!($lvl, "image: {}", file_path.display());
                $crate::logimg::write_pixmap(&file_path, $image);
            }
        }
    })
}

/// See `log_pixmap!`.  Uses the log level `log::LogLevel::Error`.
macro_rules! error_pixmap {
    ($image:expr, $($path_arg:tt)+) => ({
        log_pixmap!($crate::log::LogLevel::Error, $image, $($path_arg)+);
    })
}

/// See `log_pixmap!`.  Uses the log level `log::LogLevel::Warn`.
macro_rules! warn_pixmap {
    ($image:expr, $($path_arg:tt)+) => ({
        log_pixmap!($crate::log::LogLevel::Warn, $image, $($path_arg)+);
    })
}

/// See `log_pixmap!`.  Uses the log level `log::LogLevel::Info`.
macro_rules! info_pixmap {
    ($image:expr, $($path_arg:tt)+) => ({
        log_pixmap!($crate::log::LogLevel::Info, $image, $($path_arg)+);
    })
}

/// See `log_pixmap!`.  Uses the log level `log::LogLevel::Debug`.
macro_rules! debug_pixmap {
    ($image:expr, $($path_arg:tt)+) => ({
        log_pixmap!($crate::log::LogLevel::Debug, $image, $($path_arg)+);
    })
}

/// See `log_pixmap!`.  Uses the log level `log::LogLevel::Trace`.
macro_rules! trace_pixmap {
    ($image:expr, $($path_arg:tt)+) => ({
        log_pixmap!($crate::log::LogLevel::Trace, $image, $($path_arg)+);
    })
}

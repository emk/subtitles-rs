# `subtitle_ocr`: Convert subtitles images to text, mostly automatically

**This library is still a work in progress!**

## Testing notes

If you have a larger private corpus of subtitles available in `../private`,
you can run an OCR test over them using `cargo test --ignored`.

The following options can be used to control logging:

```sh
# Standard Rust `log` options (lots more available, including log levels
# and per-module logging).
export RUST_LOG=subtitle_ocr=debug

# Log intermediate images to a directory.
export RUST_LOG_IMAGE_DIR=logimg
```

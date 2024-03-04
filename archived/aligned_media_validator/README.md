# Web-based `aligned_media` validator

This project uses [WebAssembly][] and [stdweb][] to implement a web-based validator for the [aligned media specification][spec].

## Compiling and running

If you have [rustup][] installed, this should be pretty easy:

```sh
cd aligned_media_validator
rustup install nightly-2017-12-21
rustup override add nightly-2017-12-21
rustup target add wasm32-unknown-unknown
cargo install -f cargo-web
cargo web start --target-webasm
```

[WebAssembly]: http://webassembly.org/
[stdweb]: https://github.com/koute/stdweb
[spec]: https://github.com/language-learners/aligned-media-spec
[rustup]: https://www.rustup.rs/

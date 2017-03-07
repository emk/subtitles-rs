# vobsub: A Rust library for decoding VobSub-format (sub/idx) subtitles

[![Latest version](https://img.shields.io/crates/v/vobsub.svg)](https://crates.io/crates/vobsub) [![License](https://img.shields.io/crates/l/vobsub.svg)](https://opensource.org/licenses/MIT) [![Build Status](https://travis-ci.org/emk/subtitles-rs.svg?branch=master)](https://travis-ci.org/emk/subtitles-rs) [![Build status](https://ci.appveyor.com/api/projects/status/3hn8cwckcdhpcasm/branch/master?svg=true)](https://ci.appveyor.com/project/emk/subtitles-rs/branch/master) [![Documentation](https://img.shields.io/badge/documentation-docs.rs-yellow.svg)](https://docs.rs/vobsub/)

For documentation and example code, please see
the [API docs](https://docs.rs/vobsub/).

## Contributing

Your feedback and contributions are welcome!  For more information, see
the [subtitles-rs][] project.

### Fuzz testing

We test some portions of this crate using the
excellent [`cargo fuzz`][fuzz] tool.  To run these tests, install `cargo
fuzz` according to its documentation.  Then run:

```sh
env RUST_BACKTRACE=1 rustup run nightly cargo fuzz run fuzzer_script_1 -- \
    -dict=dictionary.txt -detect_leaks=0 -max_len=250
```

You can also pass `-jobs N` to run multiple jobs in parallel.  By default,
this is limited to half the number of available CPU cores.

If it finds a crash, then copy the test case it produces back into our
standard test suite and run the tests:

```sh
cp fuzz/artifacts/* ../fixtures/invalid
cargo test
```

This will allow us to tell whether the bug is fixed, and to detect any
regressions.  Once the bug is fixed, call `cargo fuzz run` again (as shown
above).

[subtitles-rs]: https://github.com/emk/subtitles-rs
[fuzz]: https://github.com/rust-fuzz/cargo-fuzz

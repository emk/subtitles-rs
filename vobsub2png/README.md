# `vobsub2png`: Convert sub/idx files to PNGs & JSON

[![Latest version](https://img.shields.io/crates/v/vobsub2png.svg)](https://crates.io/crates/vobsub2png) [![License](https://img.shields.io/crates/l/vobsub2png.svg)](https://opensource.org/licenses/MIT) [![Build Status](https://travis-ci.org/emk/subtitles-rs.svg?branch=master)](https://travis-ci.org/emk/subtitles-rs) [![Build status](https://ci.appveyor.com/api/projects/status/3hn8cwckcdhpcasm/branch/master?svg=true)](https://ci.appveyor.com/project/emk/subtitles-rs/branch/master)

This is a small utility for people who want to look at subtitles in sub/idx
format, but who don't want to mess around with MPEG-2 Program Stream
decoding.  To install:

```sh
curl https://sh.rustup.rs -sSf | sh
cargo install vobsub2png
```

Assuming you have two files `movie.idx` and `movie.sub`, you can then run:

```sh
vobsub2png movie.idx
```

This will create a directory `movie_subtitles` containing one PNG for each
subtitle, plus an `index.json` file with various metadata, including the
time that each subtitle is displayed.

For more options, run:

```sh
vobsub2png --help
```

## Contributing

Your feedback and contributions are welcome!  For more information, see
the [subtitles-rs][] project.

[subtitles-rs]: https://github.com/emk/subtitles-rs

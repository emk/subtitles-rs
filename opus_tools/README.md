# `opus_tools`: Miscellaneous tools for working with OPUS parallel corpus

[![Latest version](https://img.shields.io/crates/v/opus_tools.svg)](https://crates.io/crates/opus_tools) [![License](https://img.shields.io/crates/l/opus_tools.svg)](https://opensource.org/licenses/MIT) [![Build Status](https://travis-ci.org/emk/subtitles-rs.svg?branch=master)](https://travis-ci.org/emk/subtitles-rs) [![Build status](https://ci.appveyor.com/api/projects/status/3hn8cwckcdhpcasm/branch/master?svg=true)](https://ci.appveyor.com/project/emk/subtitles-rs/branch/master)

These are small utilties for working with the [OPUS][] parallel corpus,
which is normally used for machine translation research.  To install:

```sh
curl https://sh.rustup.rs -sSf | sh
cargo install opus_tools
```

## `opusraw2txt`: Extract raw text from raw, monolingual file

Download the file `ca.raw.tar.gz` from the right-hand column of
the [subtitle page][subs] and run:

```sh
opusraw2txt ca.raw.tar.gz
```

This will print a huge number of sentences on standard output in UTF-8
format for further processing.

[subs]: http://opus.lingfil.uu.se/OpenSubtitles2016.php

## Contributing

Your feedback and contributions are welcome!  For more information, see
the [subtitles-rs][] project.

[subtitles-rs]: https://github.com/emk/subtitles-rs

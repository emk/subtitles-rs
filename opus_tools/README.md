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

If you want to process an entire directory of files, you could install GNU
`parallel` and [`szip`][szip], and run:

```sh
ls *.raw.tar.gz |
    sed 's/\.raw\.tar\.gz$//' |
    parallel --joblog out.log 'opusraw2txt {}.raw.tar.gz | szip > {}.sz'
```

This will rapidly extract a huge number of sentences:

```txt
Extracted 26782811 sentences from 27605 files.
Extracted 80140630 sentences from 90319 files.
Extracted 79320 sentences from 89 files.
Extracted 112360292 sentences from 124815 files.
Extracted 22917237 sentences from 23492 files.
Extracted 229583 sentences from 188 files.
Extracted 7335505 sentences from 6438 files.
Extracted 38677592 sentences from 44584 files.
Extracted 101502145 sentences from 114150 files.
```

...and so on.

[subs]: http://opus.lingfil.uu.se/OpenSubtitles2016.php
[szip]: https://github.com/BurntSushi/rust-snappy

## Contributing

Your feedback and contributions are welcome!  For more information, see
the [subtitles-rs][] project.

[subtitles-rs]: https://github.com/emk/subtitles-rs

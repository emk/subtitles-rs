# Rust subtitle utilities

[![Build Status](https://travis-ci.org/emk/subtitles-rs.svg?branch=master)](https://travis-ci.org/emk/subtitles-rs) [![Build status](https://ci.appveyor.com/api/projects/status/3hn8cwckcdhpcasm/branch/master?svg=true)](https://ci.appveyor.com/project/emk/subtitles-rs/branch/master)

**Are you looking for `substudy`? Try [here][substudy].** (`substudy` has been
merged into the `subtitles-rs` project.)

This repository contains a number of related tools and libraries for manipulating subtitles.  See the `README.md` files in this individual subdirectories for more details.

- [substudy][]: Learn foreign languages using audio and subtitles extracted from video files.
- [aligned_media][]: A Rust library for working with aligned audio, video and text. This was originally designed to be a potential interchange format, but it may become the internal format for `substudy`.
- [cli_test_dir][]: A simple integration testing harness for CLI tools.
- [vobsub][]: A Rust library for parsing subtitles in sub/idx format.
- [vobsub2png][]: A command-line tool for converting sub/idx subtitles to PNGs with JSON metadata.

### Deprecated libraries

- [common_failures][]: Useful `Fail` implementations and error-handling tools.

### Historical interest

The following assorted libraries have been moved into the `archive` sub-directory, and are no longer maintained. `vobsub2png` might be worth updating at some point, although fewer language learners rely on DVD subtitles these days.

- [opus_tools][]: Utilities for parsing subtitle data from the OPUS project, for use as input to various language models.

The following subtitle-related projects can be found in other repositories:

- [aligner][]: This GPLed library by kaegi uses dynamic programming to re-align out-of-sync subtitles using another subtitle file with known-good timing.
- [subparse][]: This library by kaegi parses many common subtitle formats.

[aligned_media]: ./aligned_media/README.md
[vobsub]: ./archive/vobsub/README.md
[vobsub2png]: ./vobsub2png/README.md
[opus_tools]: ./archive/opus_tools/README.md
[common_failures]: ./deprecated/common_failures/README.md
[cli_test_dir]: ./cli_test_dir/README.md
[substudy]: ./substudy/README.md
[aligner]: https://github.com/kaegi/aligner
[subparse]: https://github.com/kaegi/subparse

## License

This code is distributed under the Apache 2.0 license, with the exception of some data in the `fixtures` directory, which contains a few individual frames of subtitle data used in tests. Note that none of the individual crates include that data.

Earlier versions of this code were distributed under the CC0 1.0 Universal public domain grant
(plus fallback license). This may give you additional rights in certain jurisdictions, but you'd have to check with a legal professional.

## Contributions

Your feedback and contributions are welcome!  Please feel free to submit issues and pull requests using GitHub.

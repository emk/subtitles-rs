# Rust subtitle utilities

This repository contains a number of related crates for manipulating
subtitles.  See the `README.md` files in this individual subdirectories for
more details.

- [vobsub][]: A Rust library for parsing subtitles in sub/idx format.
- [vobsub2png][]: A command-line tool for converting sub/idx subtitles to
  PNGs with JSON metadata.

[vobsub]: ./vobsub/README.md
[vobsub2png]: ./vobsub2png/README.md

## License

This code is distributed under the CC0 1.0 Universal public domain grant
(plus fallback license), with the exception of some data in the `fixtures`
directory, which contains a few indivual frames of subtitle data used in
tests.  Note that none of the indvidual crates include that data.

## Contributions

Your feedback and contributions are welcome!  Please feel free to submit
issues and pull requests using GitHub.

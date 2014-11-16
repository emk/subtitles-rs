This is an experimental tool to help language-learners exploit parallel
subtitles in various ways.  It's written using Rust, a programming language
that's still under heavy development.  So it may be hard to get this
working on any given day.

## Building `substudy`

Installing Rust and Cargo:

```sh
curl https://static.rust-lang.org/rustup.sh | sudo bash
```

Clone, compiling and building `substudy`, assuming it builds with the most
recent nightly build of Rust:

```sh
git clone https://github.com/emk/substudy.git
cd substudy
cargo build
target/substudy foreign.srt native.srt > bilingual.srt
```

Note that the command-line interface will change as more features are
added.

## Finding & preparing subtitles

The simplest tool for extracting subtitles from DVDs is [Handbrake][].  If
you can't find what you need on the DVD, another good source of subtitles
is [opensubtitles.org][].  To OCR, convert, realign and otherwise clean up
subtitles, the open source Windows application [Subtitle Edit][] is an
excellent choice, and it runs fine in a Windows VM.

[Handbrake]: https://handbrake.fr/
[opensubtitles.org]: http://www.opensubtitles.org/en/search
[Subtitle Edit]: http://www.nikse.dk/subtitleedit/

## Contributing

Please feel welcome to send me a pull request or submit an issue!

Things which I'd love to see `subs2srs` support include:

- Working with clean `*.srt` files.
- Creating various sorts of parallel media: subtitles, Anki cards, etc.

Things which I'll probably merge if they come with clean code and solid
test suites:

- Character set conversion.
- Various sorts of subtitle cleanups.
- Formats other than `*.srt`.

## License

This program is released into the public domain using the [Unlicense][].
Our test suites contain a half-dozen lines of subtitles from copyrighted TV
shows, which should presumably fall under _de minimis_, fair use or
equivalent exceptions in most jurisdictions.

[Unlicense]: http://unlicense.org/

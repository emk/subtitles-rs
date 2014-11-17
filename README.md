This is an experimental tool to help language-learners exploit parallel
subtitles in various ways.

It's written using Rust, a programming language that's still under heavy
development.  So it may be hard to get this working on any given day.  But
if the following badge is green, there's a good chance everything will
work:

[![Build Status](https://travis-ci.org/emk/substudy.svg?branch=master)](https://travis-ci.org/emk/substudy)

## Building `substudy`

You'll need to install Rust and Cargo.  Note that Rust is currently
undergoing library stabilization before version 1.0, and things are
changing on an almost nighly basis.

```sh
curl https://static.rust-lang.org/rustup.sh | sudo bash
```

Next, you'll need to clone and build `substudy`:

```sh
git clone https://github.com/emk/substudy.git
cd substudy
cargo build
```

If this fails with the latest Rust, please feel free to submit an issue.

## Running `substudy`

To get a list of supported commands, run `target/substudy --help`:

```
Subtitle processing tools for students of foreign languages

Usage: substudy clean <subtitles>
       substudy combine <foreign-subtitles> <native-subtitles>
       substudy --help

For now, all subtitles must be in *.srt format and encoded as UTF-8.
```

So, for example, you could run:

``` sh
target/substudy combine foreign.srt native.srt > bilingual.srt
```

## Finding & preparing subtitles

The simplest tool for extracting subtitles from DVDs is [Handbrake][].  If
you can't find what you need on the DVD, another good source of subtitles
is [opensubtitles.org][].  To OCR, convert, realign and otherwise clean up
subtitles, the open source Windows application [Subtitle Edit][] is an
excellent choice, and it runs fine in a Windows VM.

[Handbrake]: https://handbrake.fr/
[opensubtitles.org]: http://www.opensubtitles.org/en/search
[Subtitle Edit]: http://www.nikse.dk/subtitleedit/

## Watching video with bilingual SRT subtitles

My favorite tools are [VLC][], for watching on my computer, and
[Videostream][], for streaming videos to my TV using a [Chromecast][].  The
same subtitle file should work fine with both.

[VLC]: http://www.videolan.org/vlc/
[Videostream]: http://www.getvideostream.com/
[Chromecast]: http://www.google.com/chrome/devices/chromecast/

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

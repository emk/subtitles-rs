[![Latest version](https://img.shields.io/crates/v/substudy.svg)](https://crates.io/crates/substudy) [![License](https://img.shields.io/crates/l/substudy.svg)](https://creativecommons.org/publicdomain/zero/1.0/) [![Build Status](https://travis-ci.org/emk/substudy.svg?branch=master)](https://travis-ci.org/emk/substudy) [![Build status (AppVeyor)](https://ci.appveyor.com/api/projects/status/188eau91m9umve8u/branch/master?svg=true)](https://ci.appveyor.com/project/emk/substudy/branch/master)

This is an experimental tool to help language-learners exploit parallel
subtitles in various ways.  Among other things, it can generate bilingual
subtitles, review pages, and decks of Anki cards:

<a href="http://www.randomhacks.net/substudy/#bilingual">
<img src="http://www.randomhacks.net/images/substudy/bilingual-subtitles.jpg"
     width="320" height="240"
     alt="Fleet of ships on TV, with subtitles in English and Spanish"
     title="TV with bilingual subtitles">
</a>
<a href="http://www.randomhacks.net/substudy/#anki">
<img src="http://www.randomhacks.net/images/substudy/anki.png"
     width="320" height="240"
     alt="Flash card with image and audio on front, bilingual subtitles on back"
     title="Studying subtitles with Anki">
</a>

Here's the documentation:

- [Overview and user documentation][docs]
- [API documentation (unstable)][apidocs]

Example usage:

```sh
# Create a bilingual subtitle file.
substudy combine episode_01_01.es.srt episode_01_01.en.srt \
    > episode_01_01.bilingual.srt

# Export images, audio clips and subtitles as a web page.
substudy export review episode_01_01.mkv \
    episode_01_01.es.srt episode_01_01.en.srt
```

[docs]: http://www.randomhacks.net/substudy/
[apidocs]: http://docs.randomhacks.net/substudy/substudy/index.html

## Installing `substudy`

The easiest way to install `substudy` is using the `cargo install` command.
To use this, you'll need Rust 1.15.1 or newer.  If you already
have [`rustup`][rustup] installed, you can run:

```sh
rustup update stable
```

If you've never heard of `rustup`, you can look at the instructions on
the [`rustup` page][rustup], or you can just run the following:

```sh
# Mac and Linux only.  Windows see above.
curl https://sh.rustup.rs -sSf | sh
```

You will also need to have a working copies of `cmake` and the
[latest version of `ffmpeg`][ffmpeg] (2.8.1 or newer), which you might be
able to install as follows:

```sh
# MacOS X with `brew` installed.
brew install cmake ffmpeg

# Ubuntu 16.04.
sudo apt-get install cmake ffmpeg

# Ubuntu 14.04.
sudo apt-add-repository ppa:mc3man/trusty-media
sudo apt-get update
sudo apt-get install cmake ffmpeg
sudo apt-add-repository -r ppa:mc3man/trusty-media
sudo apt-get update
```

Once all that is set up, you can then install `substudy` by running:

```sh
cargo install substudy
```

It should also be possible to get `substudy` working on Windows, but it's
more complicated.  See our [AppVeyor file](./appveyor.yml) for some
console-based scripts that you can use as a starting point.

[rustup]: https://www.rustup.rs/
[ffmpeg]: https://ffmpeg.org/download.html

## Building `substudy`

Assuming you have Rust and the other dependencies installed as described
above, you can run:

```sh
git clone https://github.com/emk/substudy.git
cd substudy
cargo build
```

If this fails, please feel free to submit an issue.

## Using substudy as a library

You can find [API documentation on the Rust CI site][apidocs].  Note that
all APIs are experimental and subject to change.  If you want to use
`substudy` as a library in your own tools, you're encouraged to do so, but
it might be worth letting me know which APIs you're using so that I can
stabilize them.

## Contributing

Please feel welcome to send me a pull request or submit an issue!

Make sure everything continues to work with your changes:

```sh
cargo test
```

Things which I'd love to see `substudy` support include:

- Creating various sorts of parallel media: subtitles, Anki cards, etc.
- Automatic vobsub to `*.srt` conversion, using OCR and character
  databases.  There are several open source Windows tools which tackle
  this, but it should be theoretically possible to do a lot better.

Things which I'll probably merge if they come with clean code and solid
test suites:

- Better character set conversion.
- Various sorts of subtitle cleanups.
- Formats other than `*.srt`.
- Better algorithms for repairing timings and alignment.

I'm happy to leave serious, interactive subtitle editing to
[Subtitle Edit][], and to focus on cases related to language learning, and
to things which are convenient to call from the command line.  I'd also be
happy to have implementations of the most useful [subs2srs][] features in
command-line form—it's a wonderful and useful program, but it has too many
configuration options and it requires too much work using external
utilities.

[Subtitle Edit]: http://www.nikse.dk/subtitleedit/
[subs2srs]: http://subs2srs.sourceforge.net/

## License

This program is released into the public domain using the
[CC0 public domain declaration][CC0].  Our test suites contain a half-dozen
lines of subtitles from copyrighted TV shows, which should presumably fall
under _de minimis_, fair use or equivalent exceptions in most
jurisdictions.

[CC0]: https://creativecommons.org/publicdomain/zero/1.0/

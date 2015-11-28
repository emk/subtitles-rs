[![Latest version](https://img.shields.io/crates/v/substudy.svg)](https://crates.io/crates/substudy) [![License](https://img.shields.io/crates/l/substudy.svg)](https://crates.io/crates/substudy) [![Build Status](https://travis-ci.org/emk/substudy.svg?branch=master)](https://travis-ci.org/emk/substudy)

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
To get access to this, you'll need a nightly build of Rust.  The easiest
way to to this is to install [`multirust`][multirust] by running:

```sh
curl -sf https://raw.githubusercontent.com/brson/multirust/master/blastoff.sh | sh
multirust update nightly
```

Follow any extra installation instructions printed by `multirust`.  You
will also need to have a working copy of `cmake`, which you might be able
to install as follows:

```sh
# MacOS X with `brew` installed.
brew install cmake

# Ubuntu.
sudo apt-get install cmake
```

Once all that is set up, you can then install `substudy` by running:

```sh
multirust run nightly cargo install substudy
```

[multirust]: https://github.com/brson/multirust

### Installing ffmpeg

To use the video-related features, you'll need to install
[version 2.8.1 or newer of `ffmpeg`][ffmpeg].  You may have problems with
older versions or with the `libav` fork.  If you're running Ubuntu 14.04
LTS, you could run:

```sh
sudo apt-add-repository ppa:mc3man/trusty-media
sudo apt-get update
sudo apt-get install ffmpeg
```

You might want to remove this external package repository before applying
other updates to your system, to avoid conflicts:

```sh
sudo apt-add-repository -r ppa:mc3man/trusty-media
sudo apt-get update
```

Once `ffmpeg` is installed, you should be able to access the video-related
features of `substudy`.

## Building `substudy`

Assuming you have `multirust` installed as described above, you can run:

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

## License

This program is released into the public domain using the
[CC0 public domain declaration][CC0].  Our test suites contain a half-dozen
lines of subtitles from copyrighted TV shows, which should presumably fall
under _de minimis_, fair use or equivalent exceptions in most
jurisdictions.

[CC0]: https://creativecommons.org/publicdomain/zero/1.0/

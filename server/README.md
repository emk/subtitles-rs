This is a work in progress.  To build it, you will need to install [Elm][]:

```sh
npm install -g elm
```

To run it, use a command like:

```sh
cargo run --release -- video.mp4 subs.es.srt subs.en.srt
```

...and visit the specified page in the browser.

[Elm]: http://elm-lang.org/

## License

The main `substudy/server` code is released under the same terms as
`substudy`.

The [svg4everybody][] library is by Jonathan Neal, and it's released into
the public domain using the CC0 public domain release.

[svg4everybody]: https://github.com/jonathantneal/svg4everybody

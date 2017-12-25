# `common_failures`: User-friendly `io::Error` wrappers, `quick_main!` and more

[![Latest version](https://img.shields.io/crates/v/common_failures.svg)](https://crates.io/crates/common_failures) [![License](https://img.shields.io/crates/l/common_failures.svg)](https://creativecommons.org/publicdomain/zero/1.0/) [![Build Status](https://travis-ci.org/emk/subtitles-rs.svg?branch=master)](https://travis-ci.org/emk/subtitles-rs) [![Build status (AppVeyor)](https://ci.appveyor.com/api/projects/status/188eau91m9umve8u/branch/master?svg=true)](https://ci.appveyor.com/project/emk/substudy/branch/master)

We provide support for:

- User-friendly `io::Error` wrappers with pathnames,
- Formatting errors for display to the user (with the entire cause chain!), and
- Handy helper utilities like `quick_main!`.

Basically, the goal is to make `failure` as ergonomic as possible, so that
everybody can stop re-inventing common bits of supporting code.

## User-friendly `io::Error` wrappers

By default, Rust's I/O errors do not include any information about the operation
that failed. This means that you'll often see errors like:

```txt
No such file or directory (os error 2)
```

But it's much nicer for users if we print something like:

```
Error: error reading the file no-such-file.txt
  caused by: No such file or directory (os error 2)
```

To do this, we can use `io_read_context` and related functions:

```rust
use common_failures::prelude::*;
use std::fs::File;
use std::path::Path;

fn open_example(path: &Path) -> Result<File> {
    Ok(File::open(path).io_read_context(path)?)
}
```

## Formatting errors for display to the user

We also provide a support for formatting errors as strings, including the entire
chain of "causes" of the error:

```rust
format!("{}", err.display_causes_and_backtrace());
```

## The `quick_main!` macro

This is a replacement for `quick_main!` from the `error-chain` crate. It
generates a `main` function that calls a second function returning `Result<()>`,
and prints out any errors.

```rust
#[macro_use]
extern crate common_failures;
#[macro_use]
extern crate failure;

// This imports `Result`, `Error`, `failure::ResultExt`, and possibly
// other useful extension traits, to get you a minimal useful API.
use common_failures::prelude::*;

// Uncomment this to define a `main` function that calls `run`, and prints
// any errors that it returns to standard error.
quick_main!(run);

fn run() -> Result<()> {
    if true {
        Ok(())
    } else {
        Err(format_err!("an error occurred"))
    }
}
```

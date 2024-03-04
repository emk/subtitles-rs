#[macro_use]
extern crate common_failures;
extern crate docopt;
extern crate env_logger;
#[macro_use]
extern crate failure;
extern crate flate2;
#[macro_use]
extern crate log;
extern crate quick_xml;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate tar;

use common_failures::prelude::*;
use failure::SyncFailure;
use flate2::bufread::GzDecoder;
use quick_xml::reader::Reader as XmlReader;
use quick_xml::events::Event as XmlEvent;
use std::fs;
use std::io;
use std::io::prelude::*;
use std::path::Path;

const USAGE: &'static str = "
Usage: opusraw2txt [options] <raw-tar-gz>

Options:
  --quiet, -q   Don't print out summary of what we did.

Given a `*.raw.tar.gz` file from the Opus project, attempt to extract as
many sentences as possible, and print them one-to-a-line on standard
output.

For example, see the files in the right-most column at
http://opus.lingfil.uu.se/OpenSubtitles2016.php
";

/// Command-line arguments.
#[derive(Debug, Deserialize)]
struct Args {
    arg_raw_tar_gz: String,
    flag_quiet: bool,
}

// Have `error-check` set up a boiler-plate entry function that handles
// errors.
quick_main!(run);

/// Our actual entry point.
fn run() -> Result<()> {
    env_logger::init().expect("can't init env_logger");

    // Parse our arguments.
    let args: Args = docopt::Docopt::new(USAGE)
        .and_then(|d| d.deserialize())
        .unwrap_or_else(|e| e.exit());
    trace!("Arguments: {:?}", args);

    // Take exclusive control of standard output and buffer it for maximum
    // performance.
    let stdout = io::stdout();
    let mut output = io::BufWriter::new(stdout.lock());

    Ok(process_tar_gz(Path::new(&args.arg_raw_tar_gz), &mut output, args.flag_quiet)
        .with_context(|_| {
            format_err!("couldn't process {}", &args.arg_raw_tar_gz)
        })?)
}

/// Handle an individual input file.
fn process_tar_gz<W: io::Write>(path: &Path, output: &mut W, quiet: bool)
                                -> Result<()> {
    // Some counters to help keep track of what we've processed.
    let mut file_count = 0;
    let mut sentence_count = 0;

    // Iterate over files in the `*.tar.gz`, being careful to always stream
    // data.
    let f = fs::File::open(path)?;
    let buffered = io::BufReader::new(f);
    let unzipped = GzDecoder::new(buffered);
    let mut tar = tar::Archive::new(unzipped);
    for file in tar.entries()? {
        let file = file?;
        let path = file.header().path()?.into_owned();
        trace!("Found file: {}", path.display());

        // Get the file name of this entry as a UTF-8 string.
        let file_name = match path.file_name() {
            Some(file_name) => file_name,
            None => {
                debug!("Skipping {}", path.display());
                continue;
            }
        };
        let utf8_file_name = file_name.to_string_lossy();

        // Decide how to decompress the file, and pass it to our sentence
        // extractor.
        let result = if utf8_file_name.ends_with(".xml.gz") {
            debug!("Decompressing and parsing {}", path.display());
            file_count += 1;
            let unzipped = GzDecoder::new(io::BufReader::new(file));
            extract_sentences(io::BufReader::new(unzipped), output)
        } else if utf8_file_name.ends_with(".xml") {
            debug!("Parsing {}", path.display());
            file_count += 1;
            extract_sentences(io::BufReader::new(file), output)
        } else {
            debug!("Skipping {}", path.display());
            Ok(0)
        };
        match result {
            Ok(sentences) => { sentence_count += sentences }
            Err(err) => {
                write!(io::stderr(),
                       "couldn't process {} (skipping):\n{}",
                       path.display(),
                       err.display_causes_and_backtrace())
                    .expect("Error writing to stderr");
            }
        }
    }

    // Print out how much work we did.
    if !quiet {
        writeln!(io::stderr(),
                 "Extracted {} sentences from {} files.",
                 sentence_count,
                 file_count)?;
    }

    Ok(())
}

/// Given a reader `rdr` which outputs XML text in OPUS raw format, write
/// the text of the sentences to the writer `wtr`.  For performance, `wtr`
/// should be a `BufWriter` or other writer than can handle many small
/// writes efficiently.  Returns the number of sentences found.
fn extract_sentences<R, W>(rdr: R, wtr: &mut W) -> Result<usize>
    where R: io::BufRead, W: io::Write
{
    let mut count = 0;
    let mut first_text = true;
    let mut depth: usize = 0;
    let mut buf = vec![];
    let mut xml = XmlReader::from_reader(rdr);
    loop {
        let event = xml.read_event(&mut buf).map_err(SyncFailure::new)?;
        match event {
            XmlEvent::Start(ref e) if e.name() == b"s" => {
                first_text = true;
                depth += 1;
                if depth > 1 {
                    warn!("<s> tags nested to depth {}", depth);
                }
            }
            XmlEvent::End(ref e) if e.name() == b"s" => {
                write!(wtr, "\n")?;
                if depth > 0 {
                    depth -= 1;
                } else {
                    warn!("unbalanced </s>");
                }
                count += 1;
            }
            XmlEvent::Text(ref e) => {
                if depth > 0 {
                    let raw = e.unescape_and_decode(&xml)
                        .map_err(SyncFailure::new)?;
                    let trimmed = raw.trim();

                    // We ignore pure-whitespace blocks.
                    if !trimmed.is_empty() {
                        if first_text {
                            first_text = false;
                        } else {
                            // Insert space between blocks.
                            write!(wtr, " ")?;
                        }
                        write!(wtr, "{}", trimmed)?;
                    }
                }
            }
            XmlEvent::Eof => break,
            _ => {}
        }
    }
    Ok(count)
}

//! Simple CLI for managing dictionaries.
//!
//! Normally, we expect `dict_manager` to be used as a library, but we're
//! providing a simple CLI for testing and debugging purposes.

use std::error::Error;

use dict_manager::{
    dictionary_can_be_found, look_up_dictionary_entry, LanguagePair, MatchType,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<_> = std::env::args().collect();
    if args.len() != 4 {
        eprintln!("Usage: {} <source_lang> <target_lang> <word>", args[0]);
        std::process::exit(1);
    }
    let lang_pair = LanguagePair::from_639_1(&args[1], &args[2])
        .ok_or("Invalid language codes")?;
    let word = &args[3];

    if !dictionary_can_be_found(&lang_pair).await? {
        eprintln!("Dictionary not found for language pair {:?}", lang_pair);
        std::process::exit(1);
    }
    let entries =
        look_up_dictionary_entry(&lang_pair, word, MatchType::IncludeStemMatches)?;
    if let Some(entry) = entries.first() {
        println!("{}", entry.text());
    } else {
        eprintln!("Word not found in dictionary");
        std::process::exit(1);
    }
    Ok(())
}

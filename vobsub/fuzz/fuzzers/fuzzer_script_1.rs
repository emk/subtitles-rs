#![no_main]

extern crate libfuzzer_sys;
extern crate vobsub;

#[export_name="rust_fuzzer_test_input"]
pub extern fn go(data: &[u8]) {
    for _ in vobsub::subtitles(data) {
        // Just parse and ignore.
    }
}

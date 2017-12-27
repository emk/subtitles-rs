// Build script which runs the `peg` parser generator for our HTML parser.

extern crate peg;

fn main() {
    peg::cargo_build("src/html/grammar.rustpeg");
}

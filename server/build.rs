use std::process::{Command, exit};

fn main() {
    // Build the command to run.
    let mut process = Command::new("elm");
    process
        .arg("make")
        .arg("Main.elm")
        .arg("--warn")
        .arg("--output").arg("../static/elm.js")
        .current_dir("site");

    // Try to run the command.
    let status = process.status().unwrap_or_else(|e| {
        println!("Error running process: {}", e);
        println!("You may need to install the elm tools.");
        exit(1);
    });

    // Fail if the build failed.
    if !status.success() {
        println!("Build failed");
        exit(1);
    }
}


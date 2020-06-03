// Copyright (c) The cargo-guppy Contributors
// SPDX-License-Identifier: MIT OR Apache-2.0

use std::env;
use std::fs;
use std::path::Path;

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("current_platform.rs");

    let target = env::var("TARGET").unwrap();

    let features = env::var("CARGO_CFG_TARGET_FEATURE").unwrap();
    // The features are in the format |foo,bar|. Convert to |&["foo", "bar", ]|;
    let mut out = vec!["&["];
    for feature in features.split(',') {
        out.push("\"");
        out.push(feature);
        out.push("\", ");
    }
    out.push("]");
    let features = out.join("");

    fs::write(
        &dest_path,
        format!(
            "static CURRENT_TARGET: &str = \"{}\";\n\
            \n\
            static CURRENT_TARGET_FEATURES: &[&str] = {};\
            ",
            target, features,
        ),
    )
    .unwrap();
    println!("cargo:rerun-if-changed=build.rs");
}
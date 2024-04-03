
use std::path::Path;

use xkb_rust_codegen::*;

fn main() {

    // converts all lalrpop files into .rs files
    lalrpop::process_root().unwrap();

    make_config(
        &Path::new("src").join("config.rs"));

    make_keywords_file(
        &Path::new("src").join("keywords.csv"),
        &Path::new("src").join("keywords.rs")
    );
}

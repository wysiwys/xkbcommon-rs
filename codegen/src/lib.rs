//! This crate is a subcrate of `xkbcommon-rs` and provides its codegen. It is not intended for use
//! as a standalone crate.

mod config;
mod keywords;

pub use config::make_config;
pub use keywords::make_keywords_file;

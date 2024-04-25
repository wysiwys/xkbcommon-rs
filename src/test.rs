// corresponds to part of test.h and common.c

mod common;

mod context;
mod filecomp;
mod fuzz;
mod keymap;
mod keyseq;
mod keysym;
mod modifiers;
mod rules_file;
mod rules_file_includes;
mod rulescomp;
mod stringcomp;

// use all the functions in `common`
#[cfg(test)]
pub(crate) use common::*;

pub mod xkbcomp;

// The keymap
mod keymap;

//for parsing the keymap
pub mod ast;

// for compiling the keymap
pub(crate) mod compat;
pub(crate) mod keycodes;
pub(crate) mod symbols;
pub(crate) mod types;
mod vmod;

pub(crate) mod rules;

//for dumping the keymap to string
mod keymap_dump;

// misc
pub(crate) mod action;
mod expr;
mod include;

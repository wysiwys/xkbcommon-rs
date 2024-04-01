
pub mod xkbcomp;

// The keymap
mod keymap;

//for parsing the keymap
pub mod ast;

// for compiling the keymap
pub(crate) mod keycodes;
pub(crate) mod types;
pub(crate) mod compat;
pub(crate) mod symbols;
mod vmod;

pub(crate) mod rules;

//for dumping the keymap to string
mod keymap_dump;

// misc
mod include;
mod expr;
pub(crate) mod action;

#![no_main]

use libfuzzer_sys::fuzz_target;

use xkbcommon_rs::{Context, Keymap, KeymapFormat};

fuzz_target!(|data: &str| {
    let _ = Keymap::new_from_string(Context::new(0).unwrap(), data, KeymapFormat::TextV1, 0);
});

#![no_main]

use libfuzzer_sys::fuzz_target;

use xkbcommon_rs::keysym::*;

fuzz_target!(|data: &str| {
    let _ = keysym_from_name(data, KeysymFlags::NO_FLAGS);
});

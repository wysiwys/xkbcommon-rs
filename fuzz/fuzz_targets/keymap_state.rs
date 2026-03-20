#![no_main]

use libfuzzer_sys::fuzz_target;
use std::sync::LazyLock;

use xkbcommon_rs::{xkb_state::KeyDirection, Context, Keymap, State};

// keymap to use for testing
static KEYMAP: LazyLock<Keymap> =
    LazyLock::new(|| Keymap::new_from_names(Context::new(0).unwrap(), None, 0).unwrap());

fuzz_target!(|data: [u32; 2]| {
    let [raw_keycode, level_index] = data;
    let mut state = State::new(KEYMAP.clone());
    let _ = state.update_key(raw_keycode, KeyDirection::Down);
    let _ = state.update_key(raw_keycode, KeyDirection::Down);
    let _ = state.key_get_utf8(raw_keycode);
    let _ = state.key_get_utf32(raw_keycode);
    let _ = state.key_get_one_sym(raw_keycode);
    let _ = state.key_get_syms(raw_keycode);
    let _ = state.key_get_layout(raw_keycode);
    let _ = state.key_get_level(raw_keycode, 0);
    let _ = state.key_get_level(raw_keycode, level_index as usize);
    let _ = state.update_key(raw_keycode, KeyDirection::Up);
    let _ = state.update_key(raw_keycode, KeyDirection::Up);
});


use crate::log_init;
use crate::test::*;
use crate::rust_xkbcommon::*;


use crate::context::Context;
use crate::errors::*;
use crate::keymap::Keymap;

use std::sync::{Arc, Mutex};


use evdev::Key;


const SHIFT_MASK: u32 = 1 << 0;
const LOCK_MASK: u32 = 1 << 1;
const CONTROL_MASK: u32 = 1 << 2;
const MOD_1_MASK: u32 = 1 << 3;
const MOD_2_MASK: u32 = 1 << 4;
const MOD_3_MASK: u32 = 1 << 5;
const MOD_4_MASK: u32 = 1 << 6;
const MOD_5_MASK: u32 = 1 << 7;
const NO_MODIFIER: u32 = 0;

// from modifiers.c
#[test]
fn test_modmap_none() {

    let context = test_get_context(TestContextFlags::empty()).unwrap();

    let keymap = test_compile_file(context, "keymaps/modmap-none.xkb").unwrap();

    let keycode = keymap.key_by_name("LVL3").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == NO_MODIFIER);

    let keycode = keymap.key_by_name("LFSH").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == NO_MODIFIER);
    
    let keycode = keymap.key_by_name("RTSH").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == NO_MODIFIER);
    
    let keycode = keymap.key_by_name("LWIN").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_4_MASK);
    
    let keycode = keymap.key_by_name("RWIN").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_4_MASK);
    
    let keycode = keymap.key_by_name("LCTL").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == CONTROL_MASK);
    
    let keycode = keymap.key_by_name("RCTL").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == CONTROL_MASK);
    
    let keycode = keymap.key_by_name("LALT").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_1_MASK);
    
    let keycode = keymap.key_by_name("RALT").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_2_MASK | MOD_5_MASK);
    
    let keycode = keymap.key_by_name("CAPS").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == LOCK_MASK);
    
    let keycode = keymap.key_by_name("AD01").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_1_MASK);
    
    let keycode = keymap.key_by_name("AD02").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == NO_MODIFIER);
    
    let keycode = keymap.key_by_name("AD03").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == NO_MODIFIER);
    
    let keycode = keymap.key_by_name("AD04").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_1_MASK);
    
    let keycode = keymap.key_by_name("AD05").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_2_MASK);
    
    let keycode = keymap.key_by_name("AD06").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_3_MASK);
    
    let keycode = keymap.key_by_name("AD07").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_1_MASK);
    
    let keycode = keymap.key_by_name("AD08").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_2_MASK);
    
    let keycode = keymap.key_by_name("AD09").unwrap();
    let key = keymap.xkb_key(keycode.raw()).unwrap();
    assert!(key.modmap == MOD_3_MASK);


}















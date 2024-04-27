// based on test/modifiers.c
/*
 * Copyright © 2023 Pierre Le Marre <dev@wismill.eu>
 * Copyright © 2024 wysiwys
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 */

use crate::test::*;

//const SHIFT_MASK: u32 = 1 << 0;
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

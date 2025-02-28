// based on test/keymap.c
/*
 * Copyright © 2016 Intel Corporation
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
 * Author: Mike Blumenkrantz <zmike@osg.samsung.com>
 */

use crate::log_init;
use crate::rust_xkbcommon::*;
use crate::test::*;

#[test]
fn test_garbage_key() {
    log_init!();

    let first_layout = 0;

    let context = test_get_context(TestContextFlags::empty()).unwrap();

    let keymap = test_compile_rules(context, None, None, Some("garbage"), None, None).unwrap();

    // TLDE uses the 'us' sym on the first level and is thus [grave, exclam]
    let kc = keymap.key_by_name("TLDE").unwrap();

    let n_levels = keymap.num_levels_for_key(kc, first_layout);
    assert_eq!(n_levels, 2);

    let syms = keymap.key_get_syms_by_level(kc, first_layout, 0).unwrap();

    assert_eq!(syms.len(), 1);
    assert_eq!(syms[0], Keysym::grave); //fallback from 'us'

    let syms = keymap.key_get_syms_by_level(kc, first_layout, 1).unwrap();

    assert_eq!(syms.len(), 1);
    assert_eq!(syms[0], Keysym::exclam);

    // AE13 has no 'us' fallback and ends up as
    // [NoSymbol, asciitilde]
    let kc = keymap.key_by_name("AE13").unwrap();
    let n_levels = keymap.num_levels_for_key(kc, first_layout);
    assert_eq!(n_levels, 2);
    let syms = keymap.key_get_syms_by_level(kc, first_layout, 0).unwrap();
    assert_eq!(syms.len(), 0);
    let syms = keymap.key_get_syms_by_level(kc, first_layout, 1).unwrap();

    assert_eq!(syms.len(), 1);
    assert_eq!(syms[0], Keysym::asciitilde);
}

#[test]
fn test_keymap() {
    let context = test_get_context(TestContextFlags::empty()).unwrap();

    let keymap = test_compile_rules(
        context,
        Some("evdev"),
        Some("pc104"),
        Some("us,ru"),
        None,
        Some("grp:menu_toggle"),
    )
    .unwrap();

    let kc = keymap.key_by_name("AE09").unwrap();
    let keyname = keymap.key_get_name(kc).unwrap();
    assert_eq!(keyname, "AE09");

    let kc = keymap.key_by_name("COMP").unwrap();
    let keyname = keymap.key_get_name(kc).unwrap();
    assert_eq!(keyname, "COMP");

    let kc = keymap.key_by_name("MENU").unwrap();
    let keyname = keymap.key_get_name(kc).unwrap();
    assert_eq!(keyname, "COMP");

    let kc = keymap.key_by_name("AC01").unwrap();

    // AC01 level 0 ('a') requires no modifiers on us-pc104
    // TODO: make this return Result or just vec![]
    let masks_out = keymap.key_get_mods_for_level(kc, 0, 0, 4).unwrap();
    assert_eq!(masks_out.len(), 1);
    assert_eq!(masks_out[0], 0);

    // TODO: what are the return values in original?
    let shift_mask = 1 << keymap.mod_get_index("Shift").unwrap();

    let lock_mask = 1 << keymap.mod_get_index("Lock").unwrap();

    let mod2_mask = 1 << keymap.mod_get_index("Mod2").unwrap();

    // AC01 level 1 ('A') requires either Shift or Lock
    // modifiers on us-pc104
    let masks_out = keymap.key_get_mods_for_level(kc, 0, 1, 4).unwrap();
    assert_eq!(masks_out.len(), 2);
    assert_eq!(masks_out[0], shift_mask);
    assert_eq!(masks_out[1], lock_mask);

    let kc = keymap.key_by_name("KP1").unwrap();

    // KP1 level 0 ('End') requires no modifiers
    // or Shift+Mod2 on us-pc104
    let masks_out = keymap.key_get_mods_for_level(kc, 0, 0, 4).unwrap();
    assert_eq!(masks_out[0], 0);
    assert_eq!(masks_out[1], shift_mask | mod2_mask);

    // KP level 1 ('1') requires either Shift or Mod2
    // modifiers on us-pc104
    let masks_out = keymap.key_get_mods_for_level(kc, 0, 1, 4).unwrap();
    assert_eq!(masks_out.len(), 2);
    assert_eq!(masks_out[0], shift_mask);
    assert_eq!(masks_out[1], mod2_mask);

    // Return key is not affected by modifiers
    // on us-pc104
    let kc = keymap.key_by_name("RTRN").unwrap();
    let masks_out = keymap.key_get_mods_for_level(kc, 0, 0, 4).unwrap();
    assert_eq!(masks_out.len(), 1);
    assert_eq!(masks_out[0], 0);
}

const MOD_1_MASK: u32 = 1 << 3;
const MOD_2_MASK: u32 = 1 << 4;
const MOD_3_MASK: u32 = 1 << 5;
#[test]
fn test_numeric_keysyms() {
    let first_layout = 0;

    let context = test_get_context(TestContextFlags::empty()).unwrap();

    let keymap = test_compile_rules(
        context,
        Some("evdev"),
        Some("pc104"),
        Some("numeric_keysyms"),
        None,
        None,
    )
    .unwrap();

    let kc = keymap.key_by_name("AD01").unwrap();
    let syms = keymap.key_get_syms_by_level(kc, first_layout, 0).unwrap();
    assert_eq!(syms.len(), 1);
    assert_eq!(syms[0].raw(), 0x1ffffffd);

    let key = keymap.xkb_key(kc.raw()).unwrap();
    assert_eq!(key.modmap, MOD_1_MASK);

    let kc = keymap.key_by_name("AD02").unwrap();
    let syms = keymap.key_get_syms_by_level(kc, first_layout, 0).unwrap();
    assert_eq!(syms.len(), 1);
    assert_eq!(syms[0].raw(), 0x1ffffffe);

    let key = keymap.xkb_key(kc.raw()).unwrap();
    assert_eq!(key.modmap, MOD_2_MASK);

    let kc = keymap.key_by_name("AD03").unwrap();
    let syms = keymap.key_get_syms_by_level(kc, first_layout, 0).unwrap();
    assert_eq!(syms.len(), 1);
    assert_eq!(syms[0].raw(), 0x1fffffff);

    // Invalid numeric keysym
    let syms = keymap.key_get_syms_by_level(kc, first_layout, 1).unwrap();
    let _key = keymap.xkb_key(kc.raw()).unwrap();
    assert_eq!(syms.len(), 0);
    let key = keymap.xkb_key(kc.raw()).unwrap();
    assert_eq!(key.modmap, MOD_3_MASK);
}

/*
#[test]
fn test_multiple_keysyms_per_level() {
    let context = test_get_context(TestContextFlags::empty()).unwrap();

    let first_layout = 0;

    let keymap = test_compile_rules(
        context,
        Some("evdev"),
        Some("pc104"),
        Some("awesome"),
        None,
        None,
    )
    .unwrap();

    let kc = keymap.key_by_name("AD01").unwrap();
    let keysyms = keymap.key_get_syms_by_level(kc, first_layout, 0).unwrap();
    assert_eq!(keysyms.len(), 3);
    assert_eq!(keysyms[0], Keysym::q);
    assert_eq!(keysyms[1], Keysym::a);
    assert_eq!(keysyms[2], Keysym::b);

    let kc = keymap.key_by_name("AD03").unwrap();
    let keysyms = keymap.key_get_syms_by_level(kc, first_layout, 1).unwrap();
    assert_eq!(keysyms.len(), 2);
    assert_eq!(keysyms[0], Keysym::E);
    assert_eq!(keysyms[1], Keysym::F);
}
*/

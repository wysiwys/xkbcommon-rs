/*
 * Copyright © 2012 Intel Corporation
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
 * Author: Daniel Stone <daniel@fooishbar.org>
*/

use crate::rust_xkbcommon::XKB_KEYCODE_INVALID;
use crate::test::*;
use crate::xkb_state::*;
use crate::*;
use evdev::Key;
use xkeysym::Keysym;

const EVDEV_OFFSET: u32 = 8;

impl State {
    fn print(&self) {
        eprintln!("TODO: print state");
    }
}
#[test]
#[cfg(feature = "server")]
fn test_state_update_key() {
    let keymap = get_keymap_1();

    let mut state = State::new(keymap.clone());

    //LCtrl down
    state.update_key(
        Key::KEY_LEFTCTRL.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    eprintln!("dumping state for LCtrl down:");
    state.print();
    assert!(state
        .mod_name_is_active(ModName::CTRL, StateComponent::MODS_DEPRESSED)
        .unwrap());

    // LCtrl + RAlt down
    state.update_key(
        Key::KEY_RIGHTALT.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    eprintln!("dumping state for LCtrl + RAlt down:");
    state.print();

    assert!(state
        .mod_name_is_active(ModName::CTRL.name(), StateComponent::MODS_DEPRESSED)
        .unwrap());
    assert!(state
        .mod_name_is_active(ModName::ALT.name(), StateComponent::MODS_DEPRESSED)
        .unwrap());
    assert!(state
        .mod_names_are_active(
            StateComponent::MODS_DEPRESSED,
            StateMatch::ALL,
            &[ModName::CTRL, ModName::ALT]
        )
        .unwrap());
    assert!(state
        .mod_indices_are_active(
            StateComponent::MODS_DEPRESSED,
            StateMatch::ALL,
            &[
                keymap.mod_get_index(ModName::CTRL).unwrap(),
                keymap.mod_get_index(ModName::ALT).unwrap()
            ]
        )
        .unwrap());
    // no NULL to end list
    assert_eq!(
        false,
        state
            .mod_names_are_active(
                StateComponent::MODS_DEPRESSED,
                StateMatch::ALL,
                &[ModName::ALT]
            )
            .unwrap()
    );
    assert!(state
        .mod_names_are_active(
            StateComponent::MODS_DEPRESSED,
            StateMatch::ALL | StateMatch::NON_EXCLUSIVE,
            &[ModName::ALT]
        )
        .unwrap());
    assert!(state
        .mod_names_are_active(
            StateComponent::MODS_DEPRESSED,
            StateMatch::ANY | StateMatch::NON_EXCLUSIVE,
            &[ModName::ALT]
        )
        .unwrap());

    // RAlt down
    state.update_key(Key::KEY_LEFTCTRL.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    eprintln!("dumping state for RAlt down:");
    state.print();
    assert_eq!(
        false,
        state
            .mod_name_is_active(ModName::CTRL, StateComponent::MODS_EFFECTIVE)
            .unwrap()
    );
    assert!(state
        .mod_name_is_active(ModName::ALT, StateComponent::MODS_EFFECTIVE)
        .unwrap());
    assert!(state
        .mod_names_are_active(
            StateComponent::MODS_DEPRESSED,
            StateMatch::ANY,
            &[ModName::CTRL, ModName::ALT]
        )
        .unwrap());
    assert_eq!(
        false,
        state
            .mod_names_are_active(
                StateComponent::MODS_LATCHED,
                StateMatch::ANY,
                &[ModName::CTRL, ModName::ALT]
            )
            .unwrap()
    );

    // none down
    state.update_key(Key::KEY_RIGHTALT.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    assert_eq!(
        false,
        state
            .mod_name_is_active(ModName::ALT, StateComponent::MODS_EFFECTIVE)
            .unwrap()
    );

    // Caps locked
    state.update_key(
        Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    assert!(state
        .mod_name_is_active(ModName::CAPS, StateComponent::MODS_DEPRESSED)
        .unwrap());
    state.update_key(Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    eprintln!("dumping state for Caps Lock:");
    state.print();
    assert_eq!(
        false,
        state
            .mod_name_is_active(ModName::CAPS, StateComponent::MODS_DEPRESSED)
            .unwrap()
    );
    assert!(state
        .mod_name_is_active(ModName::CAPS, StateComponent::MODS_LOCKED)
        .unwrap());
    assert!(state.led_name_is_active(LedName::CAPS).unwrap());
    let syms = state.key_get_syms(Key::KEY_Q.0 as u32 + EVDEV_OFFSET);
    assert!(syms.len() == 1 && syms[0] == Keysym::Q);

    // Num Lock locked
    state.update_key(Key::KEY_NUMLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    state.update_key(Key::KEY_NUMLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    eprintln!("dumping state for Caps Lock + Num Lock:");
    state.print();
    assert!(state
        .mod_name_is_active(ModName::CAPS, StateComponent::MODS_LOCKED)
        .unwrap());
    assert!(state
        .mod_name_is_active("Mod2", StateComponent::MODS_LOCKED)
        .unwrap());
    let syms = state.key_get_syms(Key::KEY_KP1.0 as u32 + EVDEV_OFFSET);
    assert!(syms.len() == 1 && syms[0] == Keysym::KP_1);

    // Num Lock unlocked
    state.update_key(Key::KEY_NUMLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    state.update_key(Key::KEY_NUMLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);

    // Switch to group 2
    state.update_key(Key::KEY_COMPOSE.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    state.update_key(Key::KEY_COMPOSE.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    assert!(state.led_name_is_active("Group 2").unwrap());
    assert_eq!(false, state.led_name_is_active(LedName::NUM).unwrap());

    // Switch back to group 1
    state.update_key(Key::KEY_COMPOSE.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    state.update_key(Key::KEY_COMPOSE.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);

    // Caps unlocked
    state.update_key(
        Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    state.update_key(Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    assert_eq!(
        false,
        state
            .mod_name_is_active(ModName::CAPS, StateComponent::MODS_EFFECTIVE)
            .unwrap()
    );
    assert_eq!(false, state.led_name_is_active(LedName::CAPS).unwrap());
    let syms = state.key_get_syms(Key::KEY_Q.0 as u32 + EVDEV_OFFSET);
    assert!(syms.len() == 1 && syms[0] == Keysym::q);

    // Multiple symbols
    let syms = state.key_get_syms(Key::KEY_6.0 as u32 + EVDEV_OFFSET);
    assert_eq!(
        syms,
        &[Keysym::H, Keysym::E, Keysym::L, Keysym::L, Keysym::O]
    );
    let one_sym = state.key_get_one_sym(Key::KEY_6.0 as u32 + EVDEV_OFFSET);
    assert!(one_sym.is_none());
    state.update_key(Key::KEY_6.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    state.update_key(Key::KEY_6.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    let one_sym = state.key_get_one_sym(Key::KEY_5.0 as u32 + EVDEV_OFFSET);
    assert_eq!(one_sym, Some(Keysym::_5));
}

#[test]
#[cfg(all(feature = "server", feature = "client"))]
fn test_state_serialization() {
    let keymap = get_keymap_1();

    let mut state = State::new(keymap.clone());

    let base_group = 0;
    let latched_group = 0;
    let locked_group = 0;

    let caps = keymap.mod_get_index(ModName::CAPS).unwrap();
    let shift = keymap.mod_get_index(ModName::SHIFT).unwrap();
    let ctrl = keymap.mod_get_index(ModName::CTRL).unwrap();

    state.update_key(
        Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    state.update_key(Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    let base_mods = state.serialize_mods(StateComponent::MODS_DEPRESSED);
    assert_eq!(base_mods, 0);
    let latched_mods = state.serialize_mods(StateComponent::MODS_LATCHED);
    assert_eq!(latched_mods, 0);
    let locked_mods = state.serialize_mods(StateComponent::MODS_LOCKED);
    assert_eq!(locked_mods, 1 << caps);

    let effective_mods = state.serialize_mods(StateComponent::MODS_EFFECTIVE);
    assert_eq!(effective_mods, locked_mods);

    state.update_key(
        Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    let mut base_mods = state.serialize_mods(StateComponent::MODS_DEPRESSED);
    assert_eq!(base_mods, 1 << shift);
    let latched_mods = state.serialize_mods(StateComponent::MODS_LATCHED);
    assert_eq!(latched_mods, 0);
    let locked_mods = state.serialize_mods(StateComponent::MODS_LOCKED);
    assert_eq!(locked_mods, 1 << caps);
    let effective_mods = state.serialize_mods(StateComponent::MODS_EFFECTIVE);
    assert_eq!(effective_mods, base_mods | locked_mods);

    base_mods |= 1 << ctrl;

    state.update_mask(
        base_mods,
        latched_mods,
        locked_mods,
        base_group,
        latched_group,
        locked_group,
    );

    assert!(state
        .mod_index_is_active(ctrl, StateComponent::MODS_DEPRESSED)
        .unwrap());
    assert!(state
        .mod_index_is_active(ctrl, StateComponent::MODS_EFFECTIVE)
        .unwrap());
}

#[test]
#[cfg(all(feature = "server", feature = "client"))]
fn test_state_update_mask_mods() {
    let keymap = get_keymap_1();

    let mut state = State::new(keymap.clone());

    let caps = keymap.mod_get_index(ModName::CAPS).unwrap();
    let shift = keymap.mod_get_index(ModName::SHIFT).unwrap();
    let num = keymap.mod_get_index("NumLock").unwrap();
    let alt = keymap.mod_get_index("Alt").unwrap();
    let mod1 = keymap.mod_get_index("Mod1").unwrap();
    let mod2 = keymap.mod_get_index("Mod2").unwrap();

    let changed = state.update_mask(1 << caps, 0, 0, 0, 0, 0);
    assert_eq!(
        changed,
        StateComponent::MODS_DEPRESSED | StateComponent::MODS_EFFECTIVE
    );
    assert_eq!(
        state.serialize_mods(StateComponent::MODS_EFFECTIVE),
        1 << caps
    );

    let changed = state.update_mask(1 << caps, 0, 1 << shift, 0, 0, 0);
    assert_eq!(
        changed,
        StateComponent::MODS_LOCKED | StateComponent::MODS_EFFECTIVE | StateComponent::LEDS
    );
    assert_eq!(
        state.serialize_mods(StateComponent::MODS_EFFECTIVE),
        (1 << caps | 1 << shift)
    );
    assert_eq!(
        state.serialize_mods(StateComponent::MODS_DEPRESSED),
        1 << caps
    );
    assert_eq!(state.serialize_mods(StateComponent::MODS_LATCHED), 0);
    assert_eq!(
        state.serialize_mods(StateComponent::MODS_LOCKED),
        1 << shift
    );

    let changed = state.update_mask(0, 0, 0, 0, 0, 0);
    assert_eq!(
        changed,
        StateComponent::MODS_DEPRESSED
            | StateComponent::MODS_LOCKED
            | StateComponent::MODS_EFFECTIVE
            | StateComponent::LEDS
    );
    assert_eq!(state.serialize_mods(StateComponent::MODS_EFFECTIVE), 0);

    let changed = state.update_mask(1 << alt, 0, 0, 0, 0, 0);
    assert_eq!(
        changed,
        StateComponent::MODS_DEPRESSED | StateComponent::MODS_EFFECTIVE
    );
    assert_eq!(
        state.serialize_mods(StateComponent::MODS_EFFECTIVE),
        1 << alt | 1 << mod1
    );

    let changed = state.update_mask(0, 0, 1 << num, 0, 0, 0);
    assert_eq!(
        changed,
        StateComponent::MODS_DEPRESSED
            | StateComponent::MODS_LOCKED
            | StateComponent::MODS_EFFECTIVE
            | StateComponent::LEDS
    );
    assert_eq!(
        state.serialize_mods(StateComponent::MODS_EFFECTIVE),
        1 << num | 1 << mod2
    );

    state.update_mask(0, 0, 0, 0, 0, 0);

    let changed = state.update_mask(1 << mod2, 0, 1 << num, 0, 0, 0);
    assert_eq!(
        changed,
        StateComponent::MODS_DEPRESSED
            | StateComponent::MODS_LOCKED
            | StateComponent::MODS_EFFECTIVE
            | StateComponent::LEDS
    );
    assert_eq!(
        state.serialize_mods(StateComponent::MODS_EFFECTIVE),
        1 << mod2 | 1 << num
    );
    assert_eq!(
        state.serialize_mods(StateComponent::MODS_DEPRESSED),
        1 << mod2
    );
    assert_eq!(
        state.serialize_mods(StateComponent::MODS_LOCKED),
        1 << num | 1 << mod2
    );
}

#[test]
fn test_state_repeat() {
    let keymap = get_keymap_1();
    assert!(!keymap.key_repeats(Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET));
    assert!(keymap.key_repeats(Key::KEY_A.0 as u32 + EVDEV_OFFSET));
    assert!(keymap.key_repeats(Key::KEY_8.0 as u32 + EVDEV_OFFSET));
    assert!(keymap.key_repeats(Key::KEY_DOWN.0 as u32 + EVDEV_OFFSET));
    assert!(keymap.key_repeats(Key::KEY_KBDILLUMDOWN.0 as u32 + EVDEV_OFFSET));
}

#[test]
#[cfg(feature = "server")]
fn test_state_consume() {
    let keymap = get_keymap_1();

    let mut state = State::new(keymap.clone());

    let alt = keymap.mod_get_index(ModName::ALT).unwrap();
    let shift = keymap.mod_get_index(ModName::SHIFT).unwrap();
    let caps = keymap.mod_get_index(ModName::CAPS).unwrap();
    let ctrl = keymap.mod_get_index(ModName::CTRL).unwrap();
    let mod5 = keymap.mod_get_index("Mod5").unwrap();

    // Test remove_consumed()
    state.update_key(Key::KEY_LEFTALT.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    state.update_key(
        Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    state.update_key(Key::KEY_EQUAL.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);

    eprintln!("Dumping state for Alt-Shift-+");
    state.print();

    let mask = state.serialize_mods(StateComponent::MODS_EFFECTIVE);
    assert_eq!(mask, 1 << alt | 1 << shift);
    #[allow(deprecated)]
    let mask = state.mod_mask_remove_consumed(Key::KEY_EQUAL.0 as u32 + EVDEV_OFFSET, mask);
    assert_eq!(mask, 1 << alt);

    // Test get_consumed_mods()
    let mask = state.key_get_consumed_mods(Key::KEY_EQUAL.0 as u32 + EVDEV_OFFSET);
    assert_eq!(mask, 1 << shift);

    let mask = state.key_get_consumed_mods(Key::KEY_ESC.0 as u32 + EVDEV_OFFSET);
    assert_eq!(mask, 0);

    // Test is_consumed() - simple ALPHABETIC type
    let mut state = State::new(keymap.clone());

    let mask = state.key_get_consumed_mods(Key::KEY_A.0 as u32 + EVDEV_OFFSET);
    assert_eq!(mask, 1 << shift | 1 << caps);

    assert!(state
        .mod_index_is_consumed(Key::KEY_A.0 as u32 + EVDEV_OFFSET, caps)
        .unwrap());
    assert!(state
        .mod_index_is_consumed(Key::KEY_A.0 as u32 + EVDEV_OFFSET, shift)
        .unwrap());
    state.update_key(
        Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    state.update_key(Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    assert!(state
        .mod_index_is_consumed(Key::KEY_A.0 as u32 + EVDEV_OFFSET, caps)
        .unwrap());
    assert!(state
        .mod_index_is_consumed(Key::KEY_A.0 as u32 + EVDEV_OFFSET, shift)
        .unwrap());
    state.update_key(
        Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    assert!(state
        .mod_index_is_consumed(Key::KEY_A.0 as u32 + EVDEV_OFFSET, caps)
        .unwrap());
    assert!(state
        .mod_index_is_consumed(Key::KEY_A.0 as u32 + EVDEV_OFFSET, shift)
        .unwrap());
    state.update_key(Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    state.update_key(
        Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    state.update_key(Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    assert!(state
        .mod_index_is_consumed(Key::KEY_A.0 as u32 + EVDEV_OFFSET, caps)
        .unwrap());
    assert!(state
        .mod_index_is_consumed(Key::KEY_A.0 as u32 + EVDEV_OFFSET, shift)
        .unwrap());

    // More complicated - CTRL+ALT
    let mut state = State::new(keymap.clone());
    let mask = state.key_get_consumed_mods(Key::KEY_F1.0 as u32 + EVDEV_OFFSET);
    assert_eq!(mask, 1 << shift | 1 << alt | 1 << ctrl | 1 << mod5);

    // Shift is preserved
    state.update_key(
        Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    let mask = state.key_get_consumed_mods(Key::KEY_F1.0 as u32 + EVDEV_OFFSET);
    assert_eq!(mask, 1 << alt | 1 << ctrl | 1 << mod5);
    state.update_key(Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);

    let mask = state.key_get_consumed_mods(Key::KEY_F1.0 as u32 + EVDEV_OFFSET);
    assert_eq!(mask, 1 << shift | 1 << alt | 1 << ctrl | 1 << mod5);

    // Test ConsumedMode::Gtk, CTRL+ALT
    let mut state = State::new(keymap.clone());

    let mask = state.key_get_consumed_mods2(Key::KEY_F1.0 as u32 + EVDEV_OFFSET, ConsumedMode::Gtk);

    assert_eq!(mask, 0);

    state.update_key(
        Key::KEY_LEFTCTRL.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    let mask = state.key_get_consumed_mods2(Key::KEY_F1.0 as u32 + EVDEV_OFFSET, ConsumedMode::Gtk);

    assert_eq!(mask, 0);

    state.update_key(Key::KEY_LEFTALT.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    let mask = state.key_get_consumed_mods2(Key::KEY_F1.0 as u32 + EVDEV_OFFSET, ConsumedMode::Gtk);

    assert_eq!(mask, 1 << alt | 1 << ctrl);

    // Test ConsumedMode::Gtk, Simple Shift
    let mut state = State::new(keymap.clone());

    let mask = state.key_get_consumed_mods2(Key::KEY_A.0 as u32 + EVDEV_OFFSET, ConsumedMode::Gtk);

    assert_eq!(mask, 1 << shift | 1 << caps);
    state.update_key(Key::KEY_LEFTALT.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    let mask = state.key_get_consumed_mods2(Key::KEY_A.0 as u32 + EVDEV_OFFSET, ConsumedMode::Gtk);
    assert_eq!(mask, 1 << shift | 1 << caps);
}

#[test]
fn test_state_range() {
    let keymap = get_keymap_1();

    let min_keycode = keymap.min_keycode().raw();
    let max_keycode = keymap.max_keycode().raw();

    assert_eq!(min_keycode, 9);
    assert_eq!(max_keycode, 569);

    // TODO: due to different data structure (BTreeMap), cannot test unassigned keycodes as in
    // original
}

#[test]
#[cfg(feature = "server")]
fn test_state_caps_keysym_transformation() {
    let keymap = get_keymap_2();

    let mut state = State::new(keymap.clone());

    /* See xkb_state_key_get_one_sym() for what's this all about. */
    let caps = keymap.mod_get_index(ModName::CAPS).unwrap();
    let shift = keymap.mod_get_index(ModName::SHIFT).unwrap();

    assert_eq!(
        state
            .key_get_layout(Key::KEY_A.0 as u32 + EVDEV_OFFSET)
            .unwrap(),
        0
    );
    assert_eq!(
        state
            .key_get_layout(Key::KEY_SEMICOLON.0 as u32 + EVDEV_OFFSET)
            .unwrap(),
        0
    );

    // Without caps, no transformation
    assert_eq!(
        state
            .mod_index_is_active(caps, StateComponent::MODS_EFFECTIVE)
            .unwrap(),
        false
    );
    assert_eq!(
        state
            .mod_index_is_active(shift, StateComponent::MODS_EFFECTIVE)
            .unwrap(),
        false
    );
    assert_eq!(
        state
            .key_get_level(Key::KEY_A.0 as u32 + EVDEV_OFFSET, 0)
            .unwrap(),
        0
    );
    let sym = state
        .key_get_one_sym(Key::KEY_A.0 as u32 + EVDEV_OFFSET)
        .unwrap();
    assert_eq!(sym, Keysym::a);
    assert_eq!(
        state
            .key_get_level(Key::KEY_SEMICOLON.0 as u32 + EVDEV_OFFSET, 0)
            .unwrap(),
        0
    );
    let sym = state
        .key_get_one_sym(Key::KEY_SEMICOLON.0 as u32 + EVDEV_OFFSET)
        .unwrap();
    assert_eq!(sym, Keysym::eacute);
    let syms = state.key_get_syms(Key::KEY_SEMICOLON.0 as u32 + EVDEV_OFFSET);
    assert!(syms.len() == 1 && syms[0] == Keysym::eacute);

    state.update_key(
        Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    assert_eq!(
        state
            .mod_index_is_active(caps, StateComponent::MODS_EFFECTIVE)
            .unwrap(),
        false
    );
    assert_eq!(
        state
            .mod_index_is_active(shift, StateComponent::MODS_EFFECTIVE)
            .unwrap(),
        true
    );
    assert_eq!(
        state
            .key_get_level(Key::KEY_A.0 as u32 + EVDEV_OFFSET, 0)
            .unwrap(),
        1
    );
    let sym = state
        .key_get_one_sym(Key::KEY_A.0 as u32 + EVDEV_OFFSET)
        .unwrap();
    assert_eq!(sym, Keysym::A);
    let sym = state
        .key_get_one_sym(Key::KEY_SEMICOLON.0 as u32 + EVDEV_OFFSET)
        .unwrap();
    assert_eq!(sym, Keysym::odiaeresis);
    let syms = state.key_get_syms(Key::KEY_SEMICOLON.0 as u32 + EVDEV_OFFSET);
    assert!(syms.len() == 1 && syms[0] == Keysym::odiaeresis);
    state.update_key(Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    assert_eq!(
        state
            .mod_index_is_active(shift, StateComponent::MODS_EFFECTIVE)
            .unwrap(),
        false
    );

    // With caps, transform in same level, only with _get_one_sym()
    state.update_key(
        Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    state.update_key(Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    assert_eq!(
        state
            .mod_index_is_active(caps, StateComponent::MODS_EFFECTIVE)
            .unwrap(),
        true
    );
    assert_eq!(
        state
            .mod_index_is_active(shift, StateComponent::MODS_EFFECTIVE)
            .unwrap(),
        false
    );
    assert_eq!(
        state
            .key_get_level(Key::KEY_A.0 as u32 + EVDEV_OFFSET, 0)
            .unwrap(),
        1
    );
    let sym = state
        .key_get_one_sym(Key::KEY_A.0 as u32 + EVDEV_OFFSET)
        .unwrap();
    assert_eq!(sym, Keysym::A);
    assert_eq!(
        state
            .key_get_level(Key::KEY_SEMICOLON.0 as u32 + EVDEV_OFFSET, 0)
            .unwrap(),
        0
    );
    let sym = state
        .key_get_one_sym(Key::KEY_SEMICOLON.0 as u32 + EVDEV_OFFSET)
        .unwrap();
    assert_eq!(sym, Keysym::Eacute);
    let syms = state.key_get_syms(Key::KEY_SEMICOLON.0 as u32 + EVDEV_OFFSET);
    assert!(syms.len() == 1 && syms[0] == Keysym::eacute);
    state.update_key(Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    assert_eq!(
        state
            .mod_index_is_active(shift, StateComponent::MODS_EFFECTIVE)
            .unwrap(),
        false
    );
    state.update_key(
        Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    state.update_key(Key::KEY_CAPSLOCK.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    // added
    assert_eq!(
        state
            .mod_index_is_active(caps, StateComponent::MODS_EFFECTIVE)
            .unwrap(),
        false
    );
}

fn test_key(state: &State, key: Key, expected_utf8_str: &str, expected_utf32: Option<u32>) {
    let keycode = key.0 as u32 + EVDEV_OFFSET;
    let got_utf8 = state.key_get_utf8(keycode).unwrap();
    assert_eq!(got_utf8, expected_utf8_str.as_bytes());

    let got_utf32 = state.key_get_utf32(keycode);
    assert_eq!(got_utf32, expected_utf32);
}
fn test_key_invalid(state: &State, keycode: u32) {
    let got_utf8 = state.key_get_utf8(keycode);
    let got_utf32 = state.key_get_utf32(keycode);

    assert!(got_utf8.is_none());
    assert!(got_utf32.is_none());
}
#[test]
#[cfg(feature = "server")]
fn test_state_get_utf8_utf32() {
    let keymap = get_keymap_1();
    let mut state = State::new(keymap.clone());

    // Simple ASCII
    test_key(&state, Key::KEY_A, "a", 0x61.into());
    test_key(&state, Key::KEY_ESC, "\x1B", 0x1B.into());
    test_key(&state, Key::KEY_1, "1", 0x31.into());

    // Invalid
    test_key_invalid(&state, XKB_KEYCODE_INVALID - 8);
    test_key_invalid(&state, 300);

    // Multiple keysyms
    test_key(&state, Key::KEY_6, "HELLO", None);
    test_key(&state, Key::KEY_7, "YES THIS IS DOG", None);

    // Check truncation
    // TODO: not implemented because rust function does not take buffer

    // Switch to ru layout
    state.update_key(Key::KEY_COMPOSE.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    state.update_key(Key::KEY_COMPOSE.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);

    // Non ASCII
    test_key(&state, Key::KEY_ESC, "\x1B", 0x1B.into());
    test_key(&state, Key::KEY_A, "ф", 0x0444.into());
    test_key(&state, Key::KEY_Z, "я", 0x044F.into());

    // Switch back to US layout
    state.update_key(Key::KEY_COMPOSE.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    state.update_key(Key::KEY_COMPOSE.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);

    assert_eq!(
        state.key_get_layout(Key::KEY_A.0 as u32 + EVDEV_OFFSET),
        Some(0)
    );

    state.update_key(
        Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    test_key(&state, Key::KEY_A, "A", 0x041.into());
    test_key(&state, Key::KEY_ESC, "\x1B", 0x1B.into());
    test_key(&state, Key::KEY_1, "!", 0x21.into());

    state.update_key(Key::KEY_LEFTSHIFT.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);

    test_key(&state, Key::KEY_6, "HELLO", None);
    test_key(&state, Key::KEY_7, "YES THIS IS DOG", None);
}

#[test]
#[cfg(feature = "server")]
fn test_state_ctrl_string_transformation() {
    let keymap = get_keymap_1();
    let mut state = State::new(keymap.clone());

    // see xkb_state_key_get_utf8()

    let ctrl = keymap.mod_get_index(ModName::CTRL).unwrap();

    // First without
    test_key(&state, Key::KEY_A, "a", 0x61.into());
    test_key(&state, Key::KEY_B, "b", 0x62.into());
    test_key(&state, Key::KEY_C, "c", 0x63.into());
    test_key(&state, Key::KEY_ESC, "\x1B", 0x1B.into());
    test_key(&state, Key::KEY_1, "1", 0x31.into());

    // And with.
    state.update_key(
        Key::KEY_RIGHTCTRL.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    assert!(state
        .mod_index_is_active(ctrl, StateComponent::MODS_EFFECTIVE)
        .unwrap());
    test_key(&state, Key::KEY_A, "\x01", 0x01.into());
    test_key(&state, Key::KEY_B, "\x02", 0x02.into());
    test_key(&state, Key::KEY_C, "\x03", 0x03.into());
    test_key(&state, Key::KEY_ESC, "\x1B", 0x1B.into());
    test_key(&state, Key::KEY_1, "1", 0x31.into());
    state.update_key(Key::KEY_RIGHTCTRL.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);

    // Switch to ru layout
    state.update_key(Key::KEY_COMPOSE.0 as u32 + EVDEV_OFFSET, KeyDirection::Down);
    state.update_key(Key::KEY_COMPOSE.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
    assert_eq!(
        state
            .key_get_layout(Key::KEY_A.0 as u32 + EVDEV_OFFSET)
            .unwrap(),
        1
    );

    // Non ASCII
    state.update_key(
        Key::KEY_RIGHTCTRL.0 as u32 + EVDEV_OFFSET,
        KeyDirection::Down,
    );
    assert!(state
        .mod_index_is_active(ctrl, StateComponent::MODS_EFFECTIVE)
        .unwrap());
    test_key(&state, Key::KEY_A, "\x01", 0x01.into());
    test_key(&state, Key::KEY_B, "\x02", 0x02.into());
    state.update_key(Key::KEY_RIGHTCTRL.0 as u32 + EVDEV_OFFSET, KeyDirection::Up);
}

fn get_keymap_1() -> Keymap {
    let context = test_get_context(TestContextFlags::empty()).unwrap();
    let keymap = test_compile_rules(
        context,
        Some("evdev"),
        Some("pc104"),
        Some("us,ru"),
        None,
        Some("grp:menu_toggle"),
    );

    keymap.unwrap()
}
fn get_keymap_2() -> Keymap {
    let context = test_get_context(TestContextFlags::empty()).unwrap();
    let keymap = test_compile_rules(context, Some("evdev"), None, Some("ch"), Some("fr"), None);

    keymap.unwrap()
}

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

use crate::keycode::Keycode;
use crate::test::*;
use crate::xkb_state::{KeyDirection, ModName, StateComponent, StateMatch};
use crate::*;
use evdev::Key;

const EVDEV_OFFSET: u32 = 8;

impl State {
    fn print(&self) {
        eprintln!("TODO: print state");
    }
}
#[test]
fn test_update_key() {
    let keymap = get_keymap_1();

    let mut state = State::new(keymap);

    //LCtrl down
    state.update_key(
        Keycode(Key::KEY_LEFTCTRL.0 as u32 + EVDEV_OFFSET),
        KeyDirection::Down,
    );
    eprintln!("dumping state for LCtrl down:");
    state.print();
    assert!(state
        .mod_name_is_active(ModName::CTRL, StateComponent::MODS_DEPRESSED)
        .unwrap());

    // LCtrl + RAlt down
    state.update_key(
        Keycode(Key::KEY_RIGHTALT.0 as u32 + EVDEV_OFFSET),
        KeyDirection::Down,
    );
    eprintln!("dumping state for LCtrl + RAlt down:");

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
            vec![ModName::CTRL, ModName::ALT]
        )
        .unwrap());
    state.print();
    todo!()
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

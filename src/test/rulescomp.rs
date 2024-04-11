/* ORIGINAL LICENSE of rulescomp.c:
 * ---------------------------------------------
 * Copyright © 2009 Dan Nicholson
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
 * --------------------------------------------
 */
use crate::log_init;
use crate::rust_xkbcommon::*;
use crate::test::*;

use crate::context::Context;
use crate::errors::*;
use crate::keymap::Keymap;
use crate::keysyms::keysym_from_name;

use std::env;

use evdev::Key;

fn test_rmlvo_va(
    ctx: Context,
    rules: Option<&str>,
    model: Option<&str>,
    layout: Option<&str>,
    variant: Option<&str>,
    options: Option<&str>,
    args: Vec<(evdev::Key, KeySeqState, Keysym)>,
) -> Result<(), TestErr> {
    let keymap = test_compile_rules(ctx, rules, model, layout, variant, options);

    let keymap = match keymap {
        Err(e) => return Err(TestErr::Keymap(e)),
        Ok(keymap) => keymap,
    };

    eprintln!(
        "Compiled '{}' '{}' '{}' '{}' '{}'",
        rules.unwrap_or_else(|| ""),
        model.unwrap_or_else(|| ""),
        layout.unwrap_or_else(|| ""),
        variant.unwrap_or_else(|| ""),
        options.unwrap_or_else(|| "")
    );

    test_key_seq(&keymap, args)
}

fn test_rmlvo(
    ctx: Context,
    rules: &str,
    model: &str,
    layout: &str,
    variant: &str,
    options: &str,
    args: Vec<(evdev::Key, KeySeqState, Keysym)>,
) -> Result<(), TestErr> {
    test_rmlvo_va(
        ctx,
        Some(rules),
        Some(model),
        Some(layout),
        Some(variant),
        Some(options),
        args,
    )?;

    Ok(())
}

fn test_rmlvo_env(
    ctx: Context,
    rules: &str,
    model: &str,
    layout: &str,
    variant: &str,
    options: &str,
    args: Vec<(evdev::Key, KeySeqState, Keysym)>,
) -> Result<(), TestErr> {
    match rules {
        "" => env::remove_var("XKB_DEFAULT_RULES"),
        _ => env::set_var("XKB_DEFAULT_RULES", rules),
    }

    match model {
        "" => env::remove_var("XKB_DEFAULT_MODEL"),
        _ => env::set_var("XKB_DEFAULT_MODEL", model),
    }

    match layout {
        "" => env::remove_var("XKB_DEFAULT_LAYOUT"),
        _ => env::set_var("XKB_DEFAULT_LAYOUT", layout),
    }

    match variant {
        "" => env::remove_var("XKB_DEFAULT_VARIANT"),
        _ => env::set_var("XKB_DEFAULT_VARIANT", variant),
    }

    match options {
        "" => env::remove_var("XKB_DEFAULT_OPTIONS"),
        _ => env::set_var("XKB_DEFAULT_OPTIONS", options),
    }

    test_rmlvo(ctx, "", "", "", "", "", args)
}

#[test]
fn rmlvo() {
    use KeySeqState::*;

    log_init!();

    let context = test_get_context(TestContextFlags::ALLOW_ENVIRONMENT_NAMES).unwrap();

    test_rmlvo(
        context.clone(),
        "evdev",
        "pc105",
        "us,il,ru,ca",
        ",,,multix",
        "grp:alts_toggle,ctrl:nocaps,compose:rwin",
        vec![
            (Key::KEY_Q, Both, Keysym::q),
            (Key::KEY_LEFTALT, Down, Keysym::Alt_L),
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Next_Group),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level3_Shift),
            (Key::KEY_LEFTALT, Up, Keysym::Alt_L),
            (Key::KEY_Q, Both, Keysym::slash),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_Q, Both, Keysym::Q),
            (Key::KEY_RIGHTMETA, Both, Keysym::Multi_key),
        ],
    )
    .unwrap();

    test_rmlvo(
        context.clone(),
        "evdev",
        "pc105",
        "us,in",
        "",
        "grp:alts_toggle",
        vec![
            (Key::KEY_A, Both, Keysym::a),
            (Key::KEY_LEFTALT, Down, Keysym::Alt_L),
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Next_Group),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level3_Shift),
            (Key::KEY_LEFTALT, Up, Keysym::Alt_L),
            (Key::KEY_A, Both, keysym_from_name("U094b", 0).unwrap()),
        ],
    )
    .unwrap();

    test_rmlvo(
        context.clone(),
        "evdev",
        "pc105",
        "us",
        "intl",
        "",
        vec![(Key::KEY_GRAVE, Both, Keysym::dead_grave)],
    )
    .unwrap();

    test_rmlvo(
        context.clone(),
        "evdev",
        "evdev",
        "us",
        "intl",
        "grp:alts_toggle",
        vec![(Key::KEY_GRAVE, Both, Keysym::dead_grave)],
    )
    .unwrap();

    // 20 is not a legal group; make sure this is handled gracefully.
    test_rmlvo(
        context.clone(),
        "evdev",
        "pc105",
        "us:20",
        "",
        "",
        vec![(Key::KEY_A, Both, Keysym::a)],
    )
    .unwrap();

    // Don't choke on missing values in RMLVO.
    // Should just skip them.
    // Currently generates us,us,ca.

    test_rmlvo(
        context.clone(),
        "evdev",
        "",
        "us,,ca",
        "",
        "grp:alts_toggle",
        vec![
            (Key::KEY_A, Both, Keysym::a),
            (Key::KEY_LEFTALT, Down, Keysym::Alt_L),
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Next_Group),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Next_Group),
            (Key::KEY_LEFTALT, Up, Keysym::Alt_L),
            (Key::KEY_LEFTALT, Down, Keysym::Alt_L),
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Next_Group),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level3_Shift),
            (Key::KEY_LEFTALT, Up, Keysym::Alt_L),
            (Key::KEY_APOSTROPHE, Both, Keysym::dead_grave),
        ],
    )
    .unwrap();

    test_rmlvo(
        context.clone(),
        "",
        "",
        "",
        "",
        "",
        vec![(Key::KEY_A, Both, Keysym::a)],
    )
    .unwrap();

    // TODO; something is wrong with the include paths here,
    //      so the compilation fails for the wrong reason.
    test_rmlvo(
        context.clone(),
        "does-not-exist",
        "",
        "",
        "",
        "",
        vec![(Key::KEY_A, Both, Keysym::a)],
    )
    .unwrap_err();

    test_rmlvo_env(
        context.clone(),
        "evdev",
        "",
        "us",
        "",
        "",
        vec![(Key::KEY_A, Both, Keysym::a)],
    )
    .unwrap();

    test_rmlvo_env(
        context.clone(),
        "evdev",
        "",
        "us",
        "",
        "ctrl:nocaps",
        vec![(Key::KEY_CAPSLOCK, Both, Keysym::Control_L)],
    )
    .unwrap();

    // Ignores multix and generates us,ca.

    test_rmlvo_env(
        context.clone(),
        "evdev",
        "",
        "us,ca",
        ",,,multix",
        "grp:alts_toggle",
        vec![
            (Key::KEY_A, Both, Keysym::a),
            (Key::KEY_LEFTALT, Down, Keysym::Alt_L),
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Next_Group),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level3_Shift),
            (Key::KEY_LEFTALT, Up, Keysym::Alt_L),
            (Key::KEY_GRAVE, Up, Keysym::numbersign),
        ],
    )
    .unwrap();

    // Ensure a keymap with an empty `xkb_keycodes` compiles fine.
    test_rmlvo_env(
        context.clone(),
        "base",
        "empty",
        "empty",
        "",
        "",
        vec![(Key::KEY_A, Both, xkeysym::NO_SYMBOL)],
    )
    .unwrap();

    // Has an illegal escape sequence, but shouldn't fail.
    test_rmlvo_env(
        context.clone(),
        "evdev",
        "",
        "cz",
        "bksl",
        "",
        vec![(Key::KEY_A, Both, Keysym::a)],
    )
    .unwrap();

    let context = test_get_context(TestContextFlags::empty()).unwrap();

    test_rmlvo_env(
        context.clone(),
        "broken",
        "but",
        "ignored",
        "per",
        "ctx flags",
        vec![(Key::KEY_A, Both, Keysym::a)],
    )
    .unwrap();

    // test response to invalid flags.
    let rmlvo = None;
    assert!(Keymap::new_from_names(context, rmlvo, 5453).is_err());
}

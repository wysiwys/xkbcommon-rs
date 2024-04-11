use crate::log_init;
use crate::rust_xkbcommon::*;
use crate::test::*;


use crate::keysyms::*;

use evdev::Key;

#[test]
fn keyseq() {
    log_init!();

    use KeySeqState::*;
    let ctx = test_get_context(TestContextFlags::empty()).unwrap();
    let keymap = test_compile_rules(
        ctx.clone(),
        Some("evdev"),
        Some("evdev"),
        Some("us,il,ru,de"),
        Some(",,phonetic,neo"),
        Some("grp:alt_shift_toggle,grp:menu_toggle"),
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_E, Both, Keysym::e),
            (Key::KEY_L, Both, Keysym::l),
            (Key::KEY_L, Both, Keysym::l),
            (Key::KEY_O, Both, Keysym::o),
        ],
    )
    .unwrap();

    // Simple shifted level
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_E, Both, Keysym::E),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
            (Key::KEY_L, Both, Keysym::l),
            (Key::KEY_O, Both, Keysym::o),
        ],
    )
    .unwrap();

    // Key repeat shifted and unshifted in the middle
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Down, Keysym::h),
            (Key::KEY_H, Repeat, Keysym::h),
            (Key::KEY_H, Repeat, Keysym::h),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_H, Repeat, Keysym::H),
            (Key::KEY_H, Repeat, Keysym::H),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
            (Key::KEY_H, Repeat, Keysym::h),
            (Key::KEY_H, Repeat, Keysym::h),
            (Key::KEY_H, Up, Keysym::h),
            (Key::KEY_H, Both, Keysym::h),
        ],
    )
    .unwrap();

    // Base modifier cleared on key release...
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_E, Both, Keysym::E),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_O, Both, Keysym::O),
        ],
    )
    .unwrap();

    // ...But only by the keycode that set it.
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_E, Both, Keysym::E),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_RIGHTSHIFT, Up, Keysym::Shift_R),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_O, Both, Keysym::O),
        ],
    )
    .unwrap();

    // A base modifier should only be cleared when no other
    // key affecting the modifier is down.
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_E, Both, Keysym::E),
            (Key::KEY_RIGHTSHIFT, Down, Keysym::Shift_R),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_RIGHTSHIFT, Up, Keysym::Shift_R),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
            (Key::KEY_O, Both, Keysym::o),
        ],
    )
    .unwrap();

    // Two key presses from the same key (e.g. if two keyboards
    // use the same State) should only be released after two releases.
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_H, Both, Keysym::H),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_H, Both, Keysym::H),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
            (Key::KEY_H, Both, Keysym::H),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
            (Key::KEY_H, Both, Keysym::h),
        ],
    )
    .unwrap();

    // Same as above with locked modifiers
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_CAPSLOCK, Down, Keysym::Caps_Lock),
            (Key::KEY_H, Both, Keysym::H),
            (Key::KEY_CAPSLOCK, Down, Keysym::Caps_Lock),
            (Key::KEY_H, Both, Keysym::H),
            (Key::KEY_CAPSLOCK, Up, Keysym::Caps_Lock),
            (Key::KEY_H, Both, Keysym::H),
            (Key::KEY_CAPSLOCK, Up, Keysym::Caps_Lock),
            (Key::KEY_H, Both, Keysym::H),
            (Key::KEY_CAPSLOCK, Both, Keysym::Caps_Lock),
            (Key::KEY_H, Both, Keysym::h),
        ],
    )
    .unwrap();

    // Group switching / locking
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_E, Both, Keysym::e),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_K, Both, Keysym::hebrew_lamed),
            (Key::KEY_F, Both, Keysym::hebrew_kaph),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_O, Both, Keysym::o),
        ],
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_LEFTALT, Down, Keysym::ISO_Next_Group),
            (Key::KEY_LEFTALT, Up, Keysym::ISO_Next_Group),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
        ],
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_LEFTALT, Down, Keysym::Alt_L),
            (Key::KEY_LEFTSHIFT, Down, Keysym::ISO_Next_Group),
            (Key::KEY_LEFTSHIFT, Up, Keysym::ISO_Next_Group),
            (Key::KEY_LEFTALT, Up, Keysym::Alt_L),
        ],
    )
    .unwrap();

    // Locked modifiers
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_CAPSLOCK, Both, Keysym::Caps_Lock),
            (Key::KEY_H, Both, Keysym::H),
            (Key::KEY_E, Both, Keysym::E),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_O, Both, Keysym::O),
        ],
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_E, Both, Keysym::e),
            (Key::KEY_CAPSLOCK, Both, Keysym::Caps_Lock),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_CAPSLOCK, Both, Keysym::Caps_Lock),
            (Key::KEY_O, Both, Keysym::o),
        ],
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_CAPSLOCK, Down, Keysym::Caps_Lock),
            (Key::KEY_E, Both, Keysym::E),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_L, Both, Keysym::L),
            (Key::KEY_CAPSLOCK, Up, Keysym::Caps_Lock),
            (Key::KEY_O, Both, Keysym::O),
        ],
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_E, Both, Keysym::e),
            (Key::KEY_CAPSLOCK, Up, Keysym::Caps_Lock),
            (Key::KEY_L, Both, Keysym::l),
            (Key::KEY_L, Both, Keysym::l),
            (Key::KEY_O, Both, Keysym::o),
        ],
    )
    .unwrap();

    // Commented out in original:
    // A key release affecting a locked modifier should clear it
    // regardless of the key press.

    /*
    test_key_seq(&keymap,
        vec![
        (Key::KEY_H,        Both,   Keysym::h),
        (Key::KEY_CAPSLOCK, Down,   Keysym::Caps_Lock),
        (Key::KEY_E,        Both,   Keysym::E),
        (Key::KEY_L,        Both,   Keysym::L),
        (Key::KEY_CAPSLOCK, Up,     Keysym::Caps_Lock),
        (Key::KEY_L,        Both,   Keysym::L),
        (Key::KEY_CAPSLOCK, Up,     Keysym::Caps_Lock),
        (Key::KEY_O,        Both,   Keysym::o),

        ]).unwrap();
    */

    // Simple Num Lock sanity check
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_KP1, Both, Keysym::KP_End),
            (Key::KEY_NUMLOCK, Both, Keysym::Num_Lock),
            (Key::KEY_KP1, Both, Keysym::KP_1),
            (Key::KEY_KP2, Both, Keysym::KP_2),
            (Key::KEY_NUMLOCK, Both, Keysym::Num_Lock),
            (Key::KEY_KP2, Both, Keysym::KP_Down),
        ],
    )
    .unwrap();

    // Test that the aliases in the ru(phonetic) symbols map work.
    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_1, Both, Keysym::_1),
            (Key::KEY_Q, Both, Keysym::Cyrillic_ya),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_1, Both, Keysym::exclam),
            (Key::KEY_Q, Both, Keysym::Cyrillic_YA),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
            (Key::KEY_V, Both, Keysym::Cyrillic_zhe),
            (Key::KEY_CAPSLOCK, Both, Keysym::Caps_Lock),
            (Key::KEY_1, Both, Keysym::_1),
            (Key::KEY_V, Both, Keysym::Cyrillic_ZHE),
            (Key::KEY_RIGHTSHIFT, Down, Keysym::Shift_R),
            (Key::KEY_V, Both, Keysym::Cyrillic_zhe),
            (Key::KEY_RIGHTSHIFT, Up, Keysym::Shift_R),
            (Key::KEY_V, Both, Keysym::Cyrillic_ZHE),
        ],
    )
    .unwrap();

    // Test that levels (1-5) in de(neo) symbols map work.
    test_key_seq(
        &keymap,
        vec![
            // Switch to the group.
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            // Level 1.
            (Key::KEY_1, Both, Keysym::_1),
            (Key::KEY_Q, Both, Keysym::x),
            (Key::KEY_KP7, Both, Keysym::KP_7),
            (Key::KEY_ESC, Both, Keysym::Escape),
            // Level 2 with Shift.
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_1, Both, Keysym::degree),
            (Key::KEY_Q, Both, Keysym::X),
            (Key::KEY_KP7, Both, keysym_from_name("U2714", 0).unwrap()),
            (Key::KEY_ESC, Both, Keysym::Escape),
            // XXX: de(neo) uses shift(both_capslock)
            // which causes the interesting result in the next
            // line. Since it's a key release, it doesn't
            // actually lock the modifier, and applications
            // by-and-large ignore the keysym on release(?).
            // Is this a problem?
            (Key::KEY_LEFTSHIFT, Up, Keysym::Caps_Lock),
            // Level 2 with the Lock modifier
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_RIGHTSHIFT, Both, Keysym::Caps_Lock),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Caps_Lock),
            (Key::KEY_6, Both, Keysym::_6),
            (Key::KEY_H, Both, Keysym::S),
            (Key::KEY_KP3, Both, Keysym::KP_3),
            (Key::KEY_ESC, Both, Keysym::Escape),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_RIGHTSHIFT, Both, Keysym::Caps_Lock),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Caps_Lock),
            // Level 3
            (Key::KEY_CAPSLOCK, Down, Keysym::ISO_Level3_Shift),
            (Key::KEY_6, Both, Keysym::cent),
            (Key::KEY_Q, Both, Keysym::ellipsis),
            (Key::KEY_KP7, Both, keysym_from_name("U2195", 0).unwrap()),
            (Key::KEY_ESC, Both, Keysym::Escape),
            (Key::KEY_CAPSLOCK, Up, Keysym::ISO_Level3_Shift),
            // Level 4
            (Key::KEY_CAPSLOCK, Down, Keysym::ISO_Level3_Shift),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_5, Both, Keysym::malesymbol),
            (Key::KEY_E, Both, Keysym::Greek_lambda),
            (Key::KEY_SPACE, Both, Keysym::nobreakspace),
            (Key::KEY_KP8, Both, Keysym::intersection),
            (Key::KEY_ESC, Both, Keysym::Escape),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Caps_Lock),
            (Key::KEY_CAPSLOCK, Up, Keysym::ISO_Level3_Shift),
            // Level 5
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Level5_Shift),
            // XXX: xkeyboard-config is borked when de(neo) is
            // not the first group - not our fault. We test
            // Level5 separately below with only de(neo).

            // Commented out in original
            /*
            (Key::KEY_5,            Both,   Keysym::periodcentered),
            (Key::KEY_E,            Both,   Keysym::Up),
            (Key::KEY_SPACE,        Both,   Keysym::KP_0),
            (Key::KEY_KP8,          Both,   Keysym::KP_Up),
            */
            (Key::KEY_ESC, Both, Keysym::Escape),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level5_Shift),
            (Key::KEY_V, Both, Keysym::p),
        ],
    )
    .unwrap();

    let keymap = test_compile_rules(
        ctx.clone(),
        Some("evdev"),
        Some(""),
        Some("de"),
        Some("neo"),
        Some(""),
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            // Level 5
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Level5_Shift),
            (Key::KEY_5, Both, Keysym::periodcentered),
            (Key::KEY_E, Both, Keysym::Up),
            (Key::KEY_SPACE, Both, Keysym::KP_0),
            (Key::KEY_KP8, Both, Keysym::KP_Up),
            (Key::KEY_ESC, Both, Keysym::Escape),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level5_Lock),
            // Level 6
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Level5_Shift),
            (Key::KEY_RIGHTSHIFT, Down, Keysym::Shift_R),
            (Key::KEY_5, Both, Keysym::NoSymbol),
            (Key::KEY_8, Both, Keysym::ISO_Left_Tab),
            (Key::KEY_E, Both, Keysym::Up),
            (Key::KEY_SPACE, Both, Keysym::KP_0),
            (Key::KEY_KP8, Both, Keysym::KP_Up),
            (Key::KEY_ESC, Both, Keysym::Escape),
            (Key::KEY_RIGHTSHIFT, Up, Keysym::Caps_Lock),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level5_Lock),
            // Level 7
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Level5_Shift),
            (Key::KEY_CAPSLOCK, Down, Keysym::ISO_Level3_Shift),
            (Key::KEY_5, Both, keysym_from_name("U2221", 0).unwrap()),
            (Key::KEY_E, Both, Keysym::Greek_LAMBDA),
            (Key::KEY_SPACE, Both, keysym_from_name("U202F", 0).unwrap()),
            (Key::KEY_KP8, Both, keysym_from_name("U22C2", 0).unwrap()),
            (Key::KEY_ESC, Both, Keysym::Escape),
            (Key::KEY_CAPSLOCK, Up, Keysym::ISO_Level3_Shift),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level5_Lock),
            // Level 8
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Level5_Shift),
            (Key::KEY_CAPSLOCK, Down, Keysym::ISO_Level3_Shift),
            (Key::KEY_RIGHTSHIFT, Down, Keysym::Shift_R),
            (Key::KEY_TAB, Both, Keysym::ISO_Level5_Lock),
            (Key::KEY_V, Both, Keysym::Greek_pi), //fails here
            (Key::KEY_RIGHTSHIFT, Up, Keysym::Caps_Lock),
            (Key::KEY_V, Both, Keysym::asciitilde),
            (Key::KEY_CAPSLOCK, Up, Keysym::ISO_Level3_Shift),
            (Key::KEY_V, Both, Keysym::p),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level5_Lock),
            // Locks Level 5.
            (Key::KEY_V, Both, Keysym::Return),
        ],
    )
    .unwrap();

    let keymap = test_compile_rules(
        ctx.clone(),
        Some("evdev"),
        Some(""),
        Some("us,il,ru"),
        Some(""),
        Some("grp:alt_shift_toggle_bidir,grp:menu_toggle"),
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_LEFTALT, Down, Keysym::ISO_Prev_Group),
            (Key::KEY_LEFTALT, Up, Keysym::ISO_Prev_Group),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
        ],
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_LEFTALT, Down, Keysym::Alt_L),
            (Key::KEY_LEFTSHIFT, Down, Keysym::ISO_Prev_Group),
            (Key::KEY_LEFTSHIFT, Up, Keysym::ISO_Prev_Group),
            (Key::KEY_LEFTALT, Up, Keysym::Alt_L),
        ],
    )
    .unwrap();

    // Check backwards (negative) group switching and wrapping

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_H, Both, Keysym::hebrew_yod),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_H, Both, Keysym::Cyrillic_er),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_LEFTALT, Both, Keysym::ISO_Prev_Group),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
            (Key::KEY_H, Both, Keysym::hebrew_yod),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_LEFTALT, Both, Keysym::ISO_Prev_Group),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_LEFTSHIFT, Down, Keysym::Shift_L),
            (Key::KEY_LEFTALT, Both, Keysym::ISO_Prev_Group),
            (Key::KEY_LEFTSHIFT, Up, Keysym::Shift_L),
            (Key::KEY_H, Both, Keysym::Cyrillic_er),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_H, Both, Keysym::h),
        ],
    )
    .unwrap();

    let keymap = test_compile_rules(
        ctx.clone(),
        Some("evdev"),
        Some(""),
        Some("us,il,ru"),
        Some(""),
        Some("grp:switch,grp:lswitch,grp:menu_toggle"),
    )
    .unwrap();

    // Test depressed group works (Mode_switch)

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H,        Both,   Keysym::h),
            (Key::KEY_RIGHTALT, Down,   Keysym::Mode_switch),
            (Key::KEY_H,        Both,   Keysym::hebrew_yod),
            (Key::KEY_RIGHTALT, Up,     Keysym::ISO_Level3_Shift),
            (Key::KEY_H,        Both,   Keysym::h),
            (Key::KEY_RIGHTALT, Down,   Keysym::Mode_switch),
            (Key::KEY_H,        Both,   Keysym::hebrew_yod),
            (Key::KEY_RIGHTALT, Up,     Keysym::ISO_Level3_Shift),
            (Key::KEY_H,        Both,   Keysym::h),
        ],
    )
    .unwrap();

    // Test locked+depressed group works,
    // with wrapping and accumulation.

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_LEFTALT, Down, Keysym::Mode_switch),
            (Key::KEY_H, Both, Keysym::Cyrillic_er),
            (Key::KEY_LEFTALT, Up, Keysym::Mode_switch),
            (Key::KEY_H, Both, Keysym::hebrew_yod),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_LEFTALT, Down, Keysym::Mode_switch),
            // Should wrap back to first group
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_LEFTALT, Up, Keysym::Mode_switch),
            (Key::KEY_H, Both, Keysym::Cyrillic_er),
            (Key::KEY_COMPOSE, Both, Keysym::ISO_Next_Group),
            (Key::KEY_H, Both, Keysym::h),
            // Two SetGroup(+1)'s should add up.
            (Key::KEY_RIGHTALT, Down, Keysym::Mode_switch),
            (Key::KEY_H, Both, Keysym::hebrew_yod),
            (Key::KEY_LEFTALT, Down, Keysym::Mode_switch),
            (Key::KEY_H, Both, Keysym::Cyrillic_er),
            (Key::KEY_LEFTALT, Up, Keysym::Mode_switch),
            (Key::KEY_H, Both, Keysym::hebrew_yod),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level3_Shift),
            (Key::KEY_H, Both, Keysym::h),
        ],
    )
    .unwrap();

    let keymap = test_compile_rules(
        ctx.clone(),
        Some("evdev"),
        Some(""),
        Some("us"),
        Some("euro"),
        Some(""),
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_5, Both, Keysym::_5),
            (Key::KEY_RIGHTALT, Down, Keysym::ISO_Level3_Shift),
            (Key::KEY_5, Both, Keysym::EuroSign),
            (Key::KEY_RIGHTALT, Up, Keysym::ISO_Level3_Shift),
        ],
    )
    .unwrap();

    let keymap = test_compile_file(ctx.clone(), "keymaps/unbound-vmod.xkb").unwrap();

    test_key_seq(
        &keymap,
        vec![
            (Key::KEY_H, Both, Keysym::h),
            (Key::KEY_Z, Down, Keysym::y),
            (Key::KEY_MINUS, Both, Keysym::ssharp),
            (Key::KEY_Z, Up, Keysym::y),
        ],
    )
    .unwrap();

    let keymap = test_compile_rules(
        ctx.clone(),
        Some("evdev"),
        Some("applealu_ansi"),
        Some("us"),
        Some(""),
        Some("terminate:ctrl_alt_bksp"),
    )
    .unwrap();

    test_key_seq(
        &keymap,
        vec![
            // Level 5
            (Key::KEY_5,            Both,   Keysym::_5),
            (Key::KEY_KP1,          Both,   Keysym::KP_1),
            (Key::KEY_NUMLOCK,      Both,   Keysym::Clear),
            (Key::KEY_LEFTSHIFT,    Down,   Keysym::Shift_L),
            (Key::KEY_KP1,          Both,   Keysym::KP_1),
            (Key::KEY_LEFTSHIFT,    Up,     Keysym::Shift_L),
            (Key::KEY_CAPSLOCK,     Both,   Keysym::Caps_Lock),
            (Key::KEY_KP1,          Both,   Keysym::KP_1),
            (Key::KEY_LEFTSHIFT,    Down,   Keysym::Shift_L),
            (Key::KEY_KP1,          Both,   Keysym::KP_1),
            (Key::KEY_LEFTSHIFT,    Up,     Keysym::Shift_L),
            (Key::KEY_CAPSLOCK,     Both,   Keysym::Caps_Lock),
            (Key::KEY_A,            Both,   Keysym::a),
        ],
    )
    .unwrap();
}

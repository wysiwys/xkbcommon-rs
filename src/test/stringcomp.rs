use crate::log_init;
use crate::rust_xkbcommon::*;
use crate::test::*;

use crate::keymap::Keymap;

#[test]
fn stringcomp() {
    log_init!();

    const DATA_PATH: &str = "keymaps/stringcomp.data";

    let context = test_get_context(TestContextFlags::empty()).unwrap();

    // Load in a prebuilt keymap, make sure we can compile it from a string,
    // then compare it to make sure we get the same result when dumping it to a string.

    let original = test_read_file(DATA_PATH).unwrap();

    let keymap = test_compile_string(context, original.clone()).unwrap();

    let dump = keymap.get_as_string(KeymapFormat::OriginalFormat).unwrap();

    if original != dump {
        eprintln!("round-trip test failed: dumped map differs from original\n");

        eprintln!("dumped map:\n");
        eprintln!("{}\n", dump);

        eprintln!("differences:\n");
        let original_lines: Vec<_> = original.split("\n").collect();
        let dumped_lines: Vec<_> = dump.split("\n").collect();

        for ((i, orig), new) in original_lines.iter().enumerate().zip(&dumped_lines) {
            if orig != new {
                eprintln!("Line {}:\n{}\n{}", i, orig, new);
            }
        }

        assert!(false);
    }

    // Make sure we can't (falsely claim to) compile an empty string.
    let empty_keymap = test_compile_string(keymap.context.clone(), "".to_owned());
    assert!(empty_keymap.is_none());

    let keymap = test_compile_rules(
        keymap.context.clone(),
        None,
        None,
        Some("ru,ca,de,us"),
        Some(",multix,neo,intl"),
        None,
    )
    .unwrap();

    let original2 = keymap.get_as_string(KeymapFormat::OriginalFormat).unwrap();

    let keymap = test_compile_string(keymap.context, original2.clone()).unwrap();

    // Now test that the dump of the dump is equal to the dump!
    let dump2 = keymap.get_as_string(KeymapFormat::OriginalFormat).unwrap();

    if original2 != dump2 {
        eprintln!("round-trip test failed: dumped map differs from original\n");

        eprintln!("differences:\n");
        let original_lines: Vec<_> = original2.split("\n").collect();
        let dumped_lines: Vec<_> = dump2.split("\n").collect();

        for ((i, orig), new) in original_lines.iter().enumerate().zip(&dumped_lines) {
            if orig != new {
                eprintln!("Line {}:\n{}\n{}", i, orig, new);
            }
        }

        assert!(false);
    }

    let dump = original2;

    // Test response to invalid formats and flags
    assert!(Keymap::new_from_string(keymap.context.clone(), dump.clone(), 0, 0).is_err());
    assert!(Keymap::new_from_string(keymap.context.clone(), dump.clone(), 42, 0).is_err());
    assert!(Keymap::new_from_string(keymap.context.clone(), dump.clone(), 2, 0).is_err());
    assert!(
        Keymap::new_from_string(keymap.context.clone(), dump, KeymapFormat::TextV1, 42).is_err()
    );
    assert!(keymap.get_as_string(0).is_err());
    assert!(keymap.get_as_string(4893).is_err());
}

use xkeysym::{Keysym, RawKeysym};

use crate::keysym::KeysymFlags;
use crate::keysym::XKB_KEYSYM_MAX;
use crate::keysyms::*;
use crate::keysyms_utf::*;
use icu_casemap::CaseMapper;

struct KsNamesEntry {
    keysym: Keysym,
    names: Vec<&'static str>,
}

const MODIFIER_KEYSYMS: [RawKeysym; 35] = [
    Keysym::ISO_Lock.raw(),
    Keysym::ISO_Level2_Latch.raw(),
    Keysym::ISO_Level3_Shift.raw(),
    Keysym::ISO_Level3_Latch.raw(),
    Keysym::ISO_Level3_Lock.raw(),
    /* Keysym::ISO_Group_Shift == Keysym::Mode_switch */
    Keysym::ISO_Group_Latch.raw(),
    Keysym::ISO_Group_Lock.raw(),
    Keysym::ISO_Next_Group.raw(),
    Keysym::ISO_Next_Group_Lock.raw(),
    Keysym::ISO_Prev_Group.raw(),
    Keysym::ISO_Prev_Group_Lock.raw(),
    Keysym::ISO_First_Group.raw(),
    Keysym::ISO_First_Group_Lock.raw(),
    Keysym::ISO_Last_Group.raw(),
    Keysym::ISO_Last_Group_Lock.raw(),
    0xfe10, /* Currently unassigned, but xkb_keysym_is_modifier returns true */
    Keysym::ISO_Level5_Shift.raw(),
    Keysym::ISO_Level5_Latch.raw(),
    Keysym::ISO_Level5_Lock.raw(),
    Keysym::Mode_switch.raw(),
    Keysym::Num_Lock.raw(),
    Keysym::Shift_L.raw(),
    Keysym::Shift_R.raw(),
    Keysym::Control_L.raw(),
    Keysym::Control_R.raw(),
    Keysym::Caps_Lock.raw(),
    Keysym::Shift_Lock.raw(),
    Keysym::Meta_L.raw(),
    Keysym::Meta_R.raw(),
    Keysym::Alt_L.raw(),
    Keysym::Alt_R.raw(),
    Keysym::Super_L.raw(),
    Keysym::Super_R.raw(),
    Keysym::Hyper_L.raw(),
    Keysym::Hyper_R.raw(),
];

#[test]
fn test_modifiers_table() {
    let mut ks: RawKeysym = xkeysym::NO_SYMBOL.raw();

    // Ensure ordered array
    for modifier in MODIFIER_KEYSYMS {
        assert!(
            ks < modifier,
            "MODIFIER_KEYSYMS is not ordered: {} >= {}",
            ks,
            modifier
        );

        ks = modifier;
    }

    // Unassigned keysym
    assert!(!keysym_is_assigned(&Keysym::from(0xfe10)));
}

fn test_modifier(ks: RawKeysym) -> bool {
    MODIFIER_KEYSYMS.contains(&ks)
}

fn test_keypad(_ks: RawKeysym, name: &str) -> bool {
    name.starts_with("KP_")
}

fn test_string(name: &str, expected: RawKeysym) -> bool {
    let keysym = keysym_from_name(name, 0).map(|ks| ks.raw()).unwrap_or(0);

    eprintln!("Expected string {} > {}", name, expected);
    eprintln!("Received string {} > {}", name, keysym);

    keysym == expected
}

fn test_casestring(name: &str, expected: RawKeysym) -> bool {
    let keysym = keysym_from_name(name, KeysymFlags::CASE_INSENSITIVE)
        .map(|sym| sym.raw())
        .unwrap_or(0);

    eprintln!("Expected casestring {} > {:?}", name, expected);
    eprintln!("Received casestring {} > {:?}", name, keysym);

    keysym == expected
}

fn test_ambiguous_icase_names(entry: &KsNamesEntry) {
    for name in entry.names.iter() {
        // Check expected result
        assert!(test_casestring(name, entry.keysym.raw()));

        // If the keysym is cased, then check the resuting keysym is lowercased
        let keysym = keysym_from_name(name, 0).unwrap_or(xkeysym::NO_SYMBOL);
        if keysym_is_lower(&keysym) || keysym_is_upper(&keysym) {
            assert!(keysym_is_lower(&entry.keysym));
        }
    }
}

fn test_keysym(keysym: u32, expected: &str) -> bool {
    let name: String = keysym_get_name(&Keysym::from(keysym)).unwrap_or("Invalid".into());

    eprintln!("Expected keysym {:?} -> {}", keysym, expected);
    eprintln!("Received keysym {:?} -> {}", keysym, name);

    name == expected
}

// TODO: return bool?
fn test_utf8(keysym: &Keysym, expected: &str) -> Option<bool> {
    let name = keysym_to_utf8(keysym).and_then(|bytes| String::from_utf8(bytes).ok())?;
    eprintln!("{:?}", name);

    assert!(expected != "");

    eprintln!("Expected keysym {:?} -> {}", keysym, expected);
    eprintln!("Received keysym {:?} -> {}", keysym, name);

    Some(name == expected)
}

fn to_simple_upper(cm: &CaseMapper, cp: u32) -> char {
    cm.simple_uppercase(char::try_from(cp).unwrap())
}

fn to_simple_lower(cm: &CaseMapper, cp: u32) -> char {
    cm.simple_lowercase(char::try_from(cp).unwrap())
}

fn test_icu_case_mappings(cm: &CaseMapper, ks: &Keysym) {
    // Check lower case mapping
    let ks_mapped = keysym_to_lower(ks);
    let cp = keysym_to_utf32(ks).unwrap_or(0);
    let expected = to_simple_lower(cm, cp);

    if ks_mapped.raw() > 0 && ks_mapped != *ks {
        let got = keysym_to_utf32(&ks_mapped).unwrap();
        assert_eq!(got, expected as u32);
        let got = keysym_is_upper(ks);

        let cp = char::try_from(cp).unwrap();

        // TODO: is titlecase check correct?
        let is_title = cm.simple_titlecase(cp) == cp;
        let expected = cp.is_uppercase() || is_title;

        assert!(got == expected || is_title);

        if is_title {
            eprintln!(
                "{} title case handling {:#06x} (U{} = {})",
                match got == expected {
                    true => "[INFO] valid",
                    false => "[WARNING] invalid",
                },
                ks.raw(),
                cp as u32,
                cp
            );
        }
    } else if expected as u32 != cp {
        eprintln!(
            "[WARNING] missing lower case mapping for {:#06x}: expected U{} ({}), got U{} ({})",
            ks.raw(),
            expected as u32,
            expected,
            cp,
            char::try_from(cp).unwrap()
        );
        assert!(!keysym_is_upper(ks));
    }

    // Check upper case mapping

    let ks_mapped = keysym_to_upper(ks);
    let expected = to_simple_upper(cm, cp);

    if ks_mapped.raw() > 0 && ks_mapped != *ks {
        let got = keysym_to_utf32(&ks_mapped).unwrap_or(0);
        assert_eq!(got, expected as u32);

        let cp = char::try_from(cp).unwrap();
        let got = keysym_is_lower(ks);
        let expected = cp.is_lowercase();
        assert_eq!(got, expected);
    } else if expected as u32 != cp {
        eprintln!(
            "[WARNING] missing upper case mapping for {:#06x}: expected U{} ({}), got U{} ({})",
            ks.raw(),
            expected as u32,
            expected,
            cp,
            char::try_from(cp).unwrap()
        );
        assert!(!keysym_is_lower(ks));
    }
}

#[test]
fn test_github_issue_42() {

    /*
        use gettextrs::setlocale;
        use gettextrs::LocaleCategory;

        // Verify we are not dependent on locale, Turkish-i problem in particular

        if setlocale(LocaleCategory::LcCType, "tr_TR.UTF-8").is_none() {
            // the locale is not available, probably; skip.
            return;
        }

        assert!(test_string("i", Keysym::i.raw()));
        assert!(test_string("I", Keysym::I.raw()));
        assert!(test_casestring("i", Keysym::i.raw()));
        assert!(test_casestring("I", Keysym::I.raw()));

        assert_eq!(keysym_to_upper(&Keysym::i), Keysym::I);
        assert_eq!(keysym_to_lower(&Keysym::I), Keysym::i);

        setlocale(LocaleCategory::LcCType, "C").unwrap();
    */
}

fn get_keysym_name(ks: Keysym) -> String {
    keysym_get_name(&ks).unwrap_or(format!("unknown: {:#?}", ks.raw()))
}

fn test_utf32_to_keysym(ucs: u32, expected: Keysym) -> bool {
    // TODO: if any, more allowed utf32s than chars
    let actual = match char::try_from(ucs) {
        Ok(c) => Keysym::from_char(c),
        _ => xkeysym::NO_SYMBOL,
    };
    let expected_name = get_keysym_name(expected);
    let actual_name = get_keysym_name(actual);

    eprintln!(
        "Code point {:#x}: expected keysym: {}, actual: {}\n",
        ucs, expected_name, actual_name
    );

    expected == actual
}

fn ambiguous_icase_names() -> [KsNamesEntry; 340] {
    [
        KsNamesEntry {
            keysym: Keysym::dead_a,
            names: vec!["dead_A", "dead_a"],
        },
        KsNamesEntry {
            keysym: Keysym::dead_e,
            names: vec!["dead_E", "dead_e"],
        },
        KsNamesEntry {
            keysym: Keysym::dead_i,
            names: vec!["dead_I", "dead_i"],
        },
        KsNamesEntry {
            keysym: Keysym::dead_o,
            names: vec!["dead_O", "dead_o"],
        },
        KsNamesEntry {
            keysym: Keysym::dead_u,
            names: vec!["dead_U", "dead_u"],
        },
        KsNamesEntry {
            keysym: Keysym::dead_schwa,
            names: vec!["dead_SCHWA", "dead_schwa"],
        },
        KsNamesEntry {
            keysym: Keysym::ch,
            names: vec!["CH", "Ch", "ch"],
        },
        KsNamesEntry {
            keysym: Keysym::c_h,
            names: vec!["C_H", "C_h", "c_h"],
        },
        KsNamesEntry {
            keysym: Keysym::a,
            names: vec!["A", "a"],
        },
        KsNamesEntry {
            keysym: Keysym::b,
            names: vec!["B", "b"],
        },
        KsNamesEntry {
            keysym: Keysym::c,
            names: vec!["C", "c"],
        },
        KsNamesEntry {
            keysym: Keysym::d,
            names: vec!["D", "d"],
        },
        KsNamesEntry {
            keysym: Keysym::e,
            names: vec!["E", "e"],
        },
        KsNamesEntry {
            keysym: Keysym::f,
            names: vec!["F", "f"],
        },
        KsNamesEntry {
            keysym: Keysym::g,
            names: vec!["G", "g"],
        },
        KsNamesEntry {
            keysym: Keysym::h,
            names: vec!["H", "h"],
        },
        KsNamesEntry {
            keysym: Keysym::i,
            names: vec!["I", "i"],
        },
        KsNamesEntry {
            keysym: Keysym::j,
            names: vec!["J", "j"],
        },
        KsNamesEntry {
            keysym: Keysym::k,
            names: vec!["K", "k"],
        },
        KsNamesEntry {
            keysym: Keysym::l,
            names: vec!["L", "l"],
        },
        KsNamesEntry {
            keysym: Keysym::m,
            names: vec!["M", "m"],
        },
        KsNamesEntry {
            keysym: Keysym::n,
            names: vec!["N", "n"],
        },
        KsNamesEntry {
            keysym: Keysym::o,
            names: vec!["O", "o"],
        },
        KsNamesEntry {
            keysym: Keysym::p,
            names: vec!["P", "p"],
        },
        KsNamesEntry {
            keysym: Keysym::q,
            names: vec!["Q", "q"],
        },
        KsNamesEntry {
            keysym: Keysym::r,
            names: vec!["R", "r"],
        },
        KsNamesEntry {
            keysym: Keysym::s,
            names: vec!["S", "s"],
        },
        KsNamesEntry {
            keysym: Keysym::t,
            names: vec!["T", "t"],
        },
        KsNamesEntry {
            keysym: Keysym::u,
            names: vec!["U", "u"],
        },
        KsNamesEntry {
            keysym: Keysym::v,
            names: vec!["V", "v"],
        },
        KsNamesEntry {
            keysym: Keysym::w,
            names: vec!["W", "w"],
        },
        KsNamesEntry {
            keysym: Keysym::x,
            names: vec!["X", "x"],
        },
        KsNamesEntry {
            keysym: Keysym::y,
            names: vec!["Y", "y"],
        },
        KsNamesEntry {
            keysym: Keysym::z,
            names: vec!["Z", "z"],
        },
        KsNamesEntry {
            keysym: Keysym::agrave,
            names: vec!["Agrave", "agrave"],
        },
        KsNamesEntry {
            keysym: Keysym::aacute,
            names: vec!["Aacute", "aacute"],
        },
        KsNamesEntry {
            keysym: Keysym::acircumflex,
            names: vec!["Acircumflex", "acircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::atilde,
            names: vec!["Atilde", "atilde"],
        },
        KsNamesEntry {
            keysym: Keysym::adiaeresis,
            names: vec!["Adiaeresis", "adiaeresis"],
        },
        KsNamesEntry {
            keysym: Keysym::aring,
            names: vec!["Aring", "aring"],
        },
        KsNamesEntry {
            keysym: Keysym::ae,
            names: vec!["AE", "ae"],
        },
        KsNamesEntry {
            keysym: Keysym::ccedilla,
            names: vec!["Ccedilla", "ccedilla"],
        },
        KsNamesEntry {
            keysym: Keysym::egrave,
            names: vec!["Egrave", "egrave"],
        },
        KsNamesEntry {
            keysym: Keysym::eacute,
            names: vec!["Eacute", "eacute"],
        },
        KsNamesEntry {
            keysym: Keysym::ecircumflex,
            names: vec!["Ecircumflex", "ecircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::ediaeresis,
            names: vec!["Ediaeresis", "ediaeresis"],
        },
        KsNamesEntry {
            keysym: Keysym::igrave,
            names: vec!["Igrave", "igrave"],
        },
        KsNamesEntry {
            keysym: Keysym::iacute,
            names: vec!["Iacute", "iacute"],
        },
        KsNamesEntry {
            keysym: Keysym::icircumflex,
            names: vec!["Icircumflex", "icircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::idiaeresis,
            names: vec!["Idiaeresis", "idiaeresis"],
        },
        KsNamesEntry {
            keysym: Keysym::eth,
            names: vec!["ETH", "Eth", "eth"],
        },
        KsNamesEntry {
            keysym: Keysym::ntilde,
            names: vec!["Ntilde", "ntilde"],
        },
        KsNamesEntry {
            keysym: Keysym::ograve,
            names: vec!["Ograve", "ograve"],
        },
        KsNamesEntry {
            keysym: Keysym::oacute,
            names: vec!["Oacute", "oacute"],
        },
        KsNamesEntry {
            keysym: Keysym::ocircumflex,
            names: vec!["Ocircumflex", "ocircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::otilde,
            names: vec!["Otilde", "otilde"],
        },
        KsNamesEntry {
            keysym: Keysym::odiaeresis,
            names: vec!["Odiaeresis", "odiaeresis"],
        },
        KsNamesEntry {
            keysym: Keysym::oslash,
            names: vec!["Oslash", "oslash"],
        },
        KsNamesEntry {
            keysym: Keysym::ooblique,
            names: vec!["Ooblique", "ooblique"],
        },
        KsNamesEntry {
            keysym: Keysym::ugrave,
            names: vec!["Ugrave", "ugrave"],
        },
        KsNamesEntry {
            keysym: Keysym::uacute,
            names: vec!["Uacute", "uacute"],
        },
        KsNamesEntry {
            keysym: Keysym::ucircumflex,
            names: vec!["Ucircumflex", "ucircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::udiaeresis,
            names: vec!["Udiaeresis", "udiaeresis"],
        },
        KsNamesEntry {
            keysym: Keysym::yacute,
            names: vec!["Yacute", "yacute"],
        },
        KsNamesEntry {
            keysym: Keysym::thorn,
            names: vec!["THORN", "Thorn", "thorn"],
        },
        KsNamesEntry {
            keysym: Keysym::ydiaeresis,
            names: vec!["Ydiaeresis", "ydiaeresis"],
        },
        KsNamesEntry {
            keysym: Keysym::aogonek,
            names: vec!["Aogonek", "aogonek"],
        },
        KsNamesEntry {
            keysym: Keysym::lstroke,
            names: vec!["Lstroke", "lstroke"],
        },
        KsNamesEntry {
            keysym: Keysym::lcaron,
            names: vec!["Lcaron", "lcaron"],
        },
        KsNamesEntry {
            keysym: Keysym::sacute,
            names: vec!["Sacute", "sacute"],
        },
        KsNamesEntry {
            keysym: Keysym::scaron,
            names: vec!["Scaron", "scaron"],
        },
        KsNamesEntry {
            keysym: Keysym::scedilla,
            names: vec!["Scedilla", "scedilla"],
        },
        KsNamesEntry {
            keysym: Keysym::tcaron,
            names: vec!["Tcaron", "tcaron"],
        },
        KsNamesEntry {
            keysym: Keysym::zacute,
            names: vec!["Zacute", "zacute"],
        },
        KsNamesEntry {
            keysym: Keysym::zcaron,
            names: vec!["Zcaron", "zcaron"],
        },
        KsNamesEntry {
            keysym: Keysym::zabovedot,
            names: vec!["Zabovedot", "zabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::racute,
            names: vec!["Racute", "racute"],
        },
        KsNamesEntry {
            keysym: Keysym::abreve,
            names: vec!["Abreve", "abreve"],
        },
        KsNamesEntry {
            keysym: Keysym::lacute,
            names: vec!["Lacute", "lacute"],
        },
        KsNamesEntry {
            keysym: Keysym::cacute,
            names: vec!["Cacute", "cacute"],
        },
        KsNamesEntry {
            keysym: Keysym::ccaron,
            names: vec!["Ccaron", "ccaron"],
        },
        KsNamesEntry {
            keysym: Keysym::eogonek,
            names: vec!["Eogonek", "eogonek"],
        },
        KsNamesEntry {
            keysym: Keysym::ecaron,
            names: vec!["Ecaron", "ecaron"],
        },
        KsNamesEntry {
            keysym: Keysym::dcaron,
            names: vec!["Dcaron", "dcaron"],
        },
        KsNamesEntry {
            keysym: Keysym::dstroke,
            names: vec!["Dstroke", "dstroke"],
        },
        KsNamesEntry {
            keysym: Keysym::nacute,
            names: vec!["Nacute", "nacute"],
        },
        KsNamesEntry {
            keysym: Keysym::ncaron,
            names: vec!["Ncaron", "ncaron"],
        },
        KsNamesEntry {
            keysym: Keysym::odoubleacute,
            names: vec!["Odoubleacute", "odoubleacute"],
        },
        KsNamesEntry {
            keysym: Keysym::rcaron,
            names: vec!["Rcaron", "rcaron"],
        },
        KsNamesEntry {
            keysym: Keysym::uring,
            names: vec!["Uring", "uring"],
        },
        KsNamesEntry {
            keysym: Keysym::udoubleacute,
            names: vec!["Udoubleacute", "udoubleacute"],
        },
        KsNamesEntry {
            keysym: Keysym::tcedilla,
            names: vec!["Tcedilla", "tcedilla"],
        },
        KsNamesEntry {
            keysym: Keysym::hstroke,
            names: vec!["Hstroke", "hstroke"],
        },
        KsNamesEntry {
            keysym: Keysym::hcircumflex,
            names: vec!["Hcircumflex", "hcircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::gbreve,
            names: vec!["Gbreve", "gbreve"],
        },
        KsNamesEntry {
            keysym: Keysym::jcircumflex,
            names: vec!["Jcircumflex", "jcircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::cabovedot,
            names: vec!["Cabovedot", "cabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::ccircumflex,
            names: vec!["Ccircumflex", "ccircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::gabovedot,
            names: vec!["Gabovedot", "gabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::gcircumflex,
            names: vec!["Gcircumflex", "gcircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::ubreve,
            names: vec!["Ubreve", "ubreve"],
        },
        KsNamesEntry {
            keysym: Keysym::scircumflex,
            names: vec!["Scircumflex", "scircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::rcedilla,
            names: vec!["Rcedilla", "rcedilla"],
        },
        KsNamesEntry {
            keysym: Keysym::itilde,
            names: vec!["Itilde", "itilde"],
        },
        KsNamesEntry {
            keysym: Keysym::lcedilla,
            names: vec!["Lcedilla", "lcedilla"],
        },
        KsNamesEntry {
            keysym: Keysym::emacron,
            names: vec!["Emacron", "emacron"],
        },
        KsNamesEntry {
            keysym: Keysym::gcedilla,
            names: vec!["Gcedilla", "gcedilla"],
        },
        KsNamesEntry {
            keysym: Keysym::tslash,
            names: vec!["Tslash", "tslash"],
        },
        KsNamesEntry {
            keysym: Keysym::eng,
            names: vec!["ENG", "eng"],
        },
        KsNamesEntry {
            keysym: Keysym::amacron,
            names: vec!["Amacron", "amacron"],
        },
        KsNamesEntry {
            keysym: Keysym::iogonek,
            names: vec!["Iogonek", "iogonek"],
        },
        KsNamesEntry {
            keysym: Keysym::eabovedot,
            names: vec!["Eabovedot", "eabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::imacron,
            names: vec!["Imacron", "imacron"],
        },
        KsNamesEntry {
            keysym: Keysym::ncedilla,
            names: vec!["Ncedilla", "ncedilla"],
        },
        KsNamesEntry {
            keysym: Keysym::omacron,
            names: vec!["Omacron", "omacron"],
        },
        KsNamesEntry {
            keysym: Keysym::kcedilla,
            names: vec!["Kcedilla", "kcedilla"],
        },
        KsNamesEntry {
            keysym: Keysym::uogonek,
            names: vec!["Uogonek", "uogonek"],
        },
        KsNamesEntry {
            keysym: Keysym::utilde,
            names: vec!["Utilde", "utilde"],
        },
        KsNamesEntry {
            keysym: Keysym::umacron,
            names: vec!["Umacron", "umacron"],
        },
        KsNamesEntry {
            keysym: Keysym::wcircumflex,
            names: vec!["Wcircumflex", "wcircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::ycircumflex,
            names: vec!["Ycircumflex", "ycircumflex"],
        },
        KsNamesEntry {
            keysym: Keysym::babovedot,
            names: vec!["Babovedot", "babovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::dabovedot,
            names: vec!["Dabovedot", "dabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::fabovedot,
            names: vec!["Fabovedot", "fabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::mabovedot,
            names: vec!["Mabovedot", "mabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::pabovedot,
            names: vec!["Pabovedot", "pabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::sabovedot,
            names: vec!["Sabovedot", "sabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::tabovedot,
            names: vec!["Tabovedot", "tabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::wgrave,
            names: vec!["Wgrave", "wgrave"],
        },
        KsNamesEntry {
            keysym: Keysym::wacute,
            names: vec!["Wacute", "wacute"],
        },
        KsNamesEntry {
            keysym: Keysym::wdiaeresis,
            names: vec!["Wdiaeresis", "wdiaeresis"],
        },
        KsNamesEntry {
            keysym: Keysym::ygrave,
            names: vec!["Ygrave", "ygrave"],
        },
        KsNamesEntry {
            keysym: Keysym::oe,
            names: vec!["OE", "oe"],
        },
        KsNamesEntry {
            keysym: Keysym::kana_a,
            names: vec!["kana_A", "kana_a"],
        },
        KsNamesEntry {
            keysym: Keysym::kana_i,
            names: vec!["kana_I", "kana_i"],
        },
        KsNamesEntry {
            keysym: Keysym::kana_u,
            names: vec!["kana_U", "kana_u"],
        },
        KsNamesEntry {
            keysym: Keysym::kana_e,
            names: vec!["kana_E", "kana_e"],
        },
        KsNamesEntry {
            keysym: Keysym::kana_o,
            names: vec!["kana_O", "kana_o"],
        },
        KsNamesEntry {
            keysym: Keysym::kana_ya,
            names: vec!["kana_YA", "kana_ya"],
        },
        KsNamesEntry {
            keysym: Keysym::kana_yu,
            names: vec!["kana_YU", "kana_yu"],
        },
        KsNamesEntry {
            keysym: Keysym::kana_yo,
            names: vec!["kana_YO", "kana_yo"],
        },
        KsNamesEntry {
            keysym: Keysym::kana_tsu,
            names: vec!["kana_TSU", "kana_tsu"],
        },
        KsNamesEntry {
            keysym: Keysym::kana_tu,
            names: vec!["kana_TU", "kana_tu"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ghe_bar,
            names: vec!["Cyrillic_GHE_bar", "Cyrillic_ghe_bar"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_zhe_descender,
            names: vec!["Cyrillic_ZHE_descender", "Cyrillic_zhe_descender"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ka_descender,
            names: vec!["Cyrillic_KA_descender", "Cyrillic_ka_descender"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ka_vertstroke,
            names: vec!["Cyrillic_KA_vertstroke", "Cyrillic_ka_vertstroke"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_en_descender,
            names: vec!["Cyrillic_EN_descender", "Cyrillic_en_descender"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_u_straight,
            names: vec!["Cyrillic_U_straight", "Cyrillic_u_straight"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_u_straight_bar,
            names: vec!["Cyrillic_U_straight_bar", "Cyrillic_u_straight_bar"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ha_descender,
            names: vec!["Cyrillic_HA_descender", "Cyrillic_ha_descender"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_che_descender,
            names: vec!["Cyrillic_CHE_descender", "Cyrillic_che_descender"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_che_vertstroke,
            names: vec!["Cyrillic_CHE_vertstroke", "Cyrillic_che_vertstroke"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_shha,
            names: vec!["Cyrillic_SHHA", "Cyrillic_shha"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_schwa,
            names: vec!["Cyrillic_SCHWA", "Cyrillic_schwa"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_i_macron,
            names: vec!["Cyrillic_I_macron", "Cyrillic_i_macron"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_o_bar,
            names: vec!["Cyrillic_O_bar", "Cyrillic_o_bar"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_u_macron,
            names: vec!["Cyrillic_U_macron", "Cyrillic_u_macron"],
        },
        KsNamesEntry {
            keysym: Keysym::Serbian_dje,
            names: vec!["Serbian_DJE", "Serbian_dje"],
        },
        KsNamesEntry {
            keysym: Keysym::Macedonia_gje,
            names: vec!["Macedonia_GJE", "Macedonia_gje"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_io,
            names: vec!["Cyrillic_IO", "Cyrillic_io"],
        },
        KsNamesEntry {
            keysym: Keysym::Ukrainian_ie,
            names: vec!["Ukrainian_IE", "Ukrainian_ie"],
        },
        KsNamesEntry {
            keysym: Keysym::Ukranian_je,
            names: vec!["Ukranian_JE", "Ukranian_je"],
        },
        KsNamesEntry {
            keysym: Keysym::Macedonia_dse,
            names: vec!["Macedonia_DSE", "Macedonia_dse"],
        },
        KsNamesEntry {
            keysym: Keysym::Ukrainian_i,
            names: vec!["Ukrainian_I", "Ukrainian_i"],
        },
        KsNamesEntry {
            keysym: Keysym::Ukranian_i,
            names: vec!["Ukranian_I", "Ukranian_i"],
        },
        KsNamesEntry {
            keysym: Keysym::Ukrainian_yi,
            names: vec!["Ukrainian_YI", "Ukrainian_yi"],
        },
        KsNamesEntry {
            keysym: Keysym::Ukranian_yi,
            names: vec!["Ukranian_YI", "Ukranian_yi"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_je,
            names: vec!["Cyrillic_JE", "Cyrillic_je"],
        },
        KsNamesEntry {
            keysym: Keysym::Serbian_je,
            names: vec!["Serbian_JE", "Serbian_je"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_lje,
            names: vec!["Cyrillic_LJE", "Cyrillic_lje"],
        },
        KsNamesEntry {
            keysym: Keysym::Serbian_lje,
            names: vec!["Serbian_LJE", "Serbian_lje"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_nje,
            names: vec!["Cyrillic_NJE", "Cyrillic_nje"],
        },
        KsNamesEntry {
            keysym: Keysym::Serbian_nje,
            names: vec!["Serbian_NJE", "Serbian_nje"],
        },
        KsNamesEntry {
            keysym: Keysym::Serbian_tshe,
            names: vec!["Serbian_TSHE", "Serbian_tshe"],
        },
        KsNamesEntry {
            keysym: Keysym::Macedonia_kje,
            names: vec!["Macedonia_KJE", "Macedonia_kje"],
        },
        KsNamesEntry {
            keysym: Keysym::Ukrainian_ghe_with_upturn,
            names: vec!["Ukrainian_GHE_WITH_UPTURN", "Ukrainian_ghe_with_upturn"],
        },
        KsNamesEntry {
            keysym: Keysym::Byelorussian_shortu,
            names: vec!["Byelorussian_SHORTU", "Byelorussian_shortu"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_dzhe,
            names: vec!["Cyrillic_DZHE", "Cyrillic_dzhe"],
        },
        KsNamesEntry {
            keysym: Keysym::Serbian_dze,
            names: vec!["Serbian_DZE", "Serbian_dze"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_yu,
            names: vec!["Cyrillic_YU", "Cyrillic_yu"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_a,
            names: vec!["Cyrillic_A", "Cyrillic_a"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_be,
            names: vec!["Cyrillic_BE", "Cyrillic_be"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_tse,
            names: vec!["Cyrillic_TSE", "Cyrillic_tse"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_de,
            names: vec!["Cyrillic_DE", "Cyrillic_de"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ie,
            names: vec!["Cyrillic_IE", "Cyrillic_ie"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ef,
            names: vec!["Cyrillic_EF", "Cyrillic_ef"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ghe,
            names: vec!["Cyrillic_GHE", "Cyrillic_ghe"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ha,
            names: vec!["Cyrillic_HA", "Cyrillic_ha"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_i,
            names: vec!["Cyrillic_I", "Cyrillic_i"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_shorti,
            names: vec!["Cyrillic_SHORTI", "Cyrillic_shorti"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ka,
            names: vec!["Cyrillic_KA", "Cyrillic_ka"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_el,
            names: vec!["Cyrillic_EL", "Cyrillic_el"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_em,
            names: vec!["Cyrillic_EM", "Cyrillic_em"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_en,
            names: vec!["Cyrillic_EN", "Cyrillic_en"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_o,
            names: vec!["Cyrillic_O", "Cyrillic_o"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_pe,
            names: vec!["Cyrillic_PE", "Cyrillic_pe"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ya,
            names: vec!["Cyrillic_YA", "Cyrillic_ya"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_er,
            names: vec!["Cyrillic_ER", "Cyrillic_er"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_es,
            names: vec!["Cyrillic_ES", "Cyrillic_es"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_te,
            names: vec!["Cyrillic_TE", "Cyrillic_te"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_u,
            names: vec!["Cyrillic_U", "Cyrillic_u"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_zhe,
            names: vec!["Cyrillic_ZHE", "Cyrillic_zhe"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ve,
            names: vec!["Cyrillic_VE", "Cyrillic_ve"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_softsign,
            names: vec!["Cyrillic_SOFTSIGN", "Cyrillic_softsign"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_yeru,
            names: vec!["Cyrillic_YERU", "Cyrillic_yeru"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_ze,
            names: vec!["Cyrillic_ZE", "Cyrillic_ze"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_sha,
            names: vec!["Cyrillic_SHA", "Cyrillic_sha"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_e,
            names: vec!["Cyrillic_E", "Cyrillic_e"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_shcha,
            names: vec!["Cyrillic_SHCHA", "Cyrillic_shcha"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_che,
            names: vec!["Cyrillic_CHE", "Cyrillic_che"],
        },
        KsNamesEntry {
            keysym: Keysym::Cyrillic_hardsign,
            names: vec!["Cyrillic_HARDSIGN", "Cyrillic_hardsign"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_alphaaccent,
            names: vec!["Greek_ALPHAaccent", "Greek_alphaaccent"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_epsilonaccent,
            names: vec!["Greek_EPSILONaccent", "Greek_epsilonaccent"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_etaaccent,
            names: vec!["Greek_ETAaccent", "Greek_etaaccent"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_iotaaccent,
            names: vec!["Greek_IOTAaccent", "Greek_iotaaccent"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_iotadieresis,
            names: vec!["Greek_IOTAdieresis", "Greek_iotadieresis"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_omicronaccent,
            names: vec!["Greek_OMICRONaccent", "Greek_omicronaccent"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_upsilonaccent,
            names: vec!["Greek_UPSILONaccent", "Greek_upsilonaccent"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_upsilondieresis,
            names: vec!["Greek_UPSILONdieresis", "Greek_upsilondieresis"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_omegaaccent,
            names: vec!["Greek_OMEGAaccent", "Greek_omegaaccent"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_alpha,
            names: vec!["Greek_ALPHA", "Greek_alpha"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_beta,
            names: vec!["Greek_BETA", "Greek_beta"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_gamma,
            names: vec!["Greek_GAMMA", "Greek_gamma"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_delta,
            names: vec!["Greek_DELTA", "Greek_delta"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_epsilon,
            names: vec!["Greek_EPSILON", "Greek_epsilon"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_zeta,
            names: vec!["Greek_ZETA", "Greek_zeta"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_eta,
            names: vec!["Greek_ETA", "Greek_eta"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_theta,
            names: vec!["Greek_THETA", "Greek_theta"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_iota,
            names: vec!["Greek_IOTA", "Greek_iota"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_kappa,
            names: vec!["Greek_KAPPA", "Greek_kappa"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_lamda,
            names: vec!["Greek_LAMDA", "Greek_lamda"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_lambda,
            names: vec!["Greek_LAMBDA", "Greek_lambda"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_mu,
            names: vec!["Greek_MU", "Greek_mu"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_nu,
            names: vec!["Greek_NU", "Greek_nu"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_xi,
            names: vec!["Greek_XI", "Greek_xi"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_omicron,
            names: vec!["Greek_OMICRON", "Greek_omicron"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_pi,
            names: vec!["Greek_PI", "Greek_pi"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_rho,
            names: vec!["Greek_RHO", "Greek_rho"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_sigma,
            names: vec!["Greek_SIGMA", "Greek_sigma"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_tau,
            names: vec!["Greek_TAU", "Greek_tau"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_upsilon,
            names: vec!["Greek_UPSILON", "Greek_upsilon"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_phi,
            names: vec!["Greek_PHI", "Greek_phi"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_chi,
            names: vec!["Greek_CHI", "Greek_chi"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_psi,
            names: vec!["Greek_PSI", "Greek_psi"],
        },
        KsNamesEntry {
            keysym: Keysym::Greek_omega,
            names: vec!["Greek_OMEGA", "Greek_omega"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_ayb,
            names: vec!["Armenian_AYB", "Armenian_ayb"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_ben,
            names: vec!["Armenian_BEN", "Armenian_ben"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_gim,
            names: vec!["Armenian_GIM", "Armenian_gim"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_da,
            names: vec!["Armenian_DA", "Armenian_da"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_yech,
            names: vec!["Armenian_YECH", "Armenian_yech"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_za,
            names: vec!["Armenian_ZA", "Armenian_za"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_e,
            names: vec!["Armenian_E", "Armenian_e"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_at,
            names: vec!["Armenian_AT", "Armenian_at"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_to,
            names: vec!["Armenian_TO", "Armenian_to"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_zhe,
            names: vec!["Armenian_ZHE", "Armenian_zhe"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_ini,
            names: vec!["Armenian_INI", "Armenian_ini"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_lyun,
            names: vec!["Armenian_LYUN", "Armenian_lyun"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_khe,
            names: vec!["Armenian_KHE", "Armenian_khe"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_tsa,
            names: vec!["Armenian_TSA", "Armenian_tsa"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_ken,
            names: vec!["Armenian_KEN", "Armenian_ken"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_ho,
            names: vec!["Armenian_HO", "Armenian_ho"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_dza,
            names: vec!["Armenian_DZA", "Armenian_dza"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_ghat,
            names: vec!["Armenian_GHAT", "Armenian_ghat"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_tche,
            names: vec!["Armenian_TCHE", "Armenian_tche"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_men,
            names: vec!["Armenian_MEN", "Armenian_men"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_hi,
            names: vec!["Armenian_HI", "Armenian_hi"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_nu,
            names: vec!["Armenian_NU", "Armenian_nu"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_sha,
            names: vec!["Armenian_SHA", "Armenian_sha"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_vo,
            names: vec!["Armenian_VO", "Armenian_vo"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_cha,
            names: vec!["Armenian_CHA", "Armenian_cha"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_pe,
            names: vec!["Armenian_PE", "Armenian_pe"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_je,
            names: vec!["Armenian_JE", "Armenian_je"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_ra,
            names: vec!["Armenian_RA", "Armenian_ra"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_se,
            names: vec!["Armenian_SE", "Armenian_se"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_vev,
            names: vec!["Armenian_VEV", "Armenian_vev"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_tyun,
            names: vec!["Armenian_TYUN", "Armenian_tyun"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_re,
            names: vec!["Armenian_RE", "Armenian_re"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_tso,
            names: vec!["Armenian_TSO", "Armenian_tso"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_vyun,
            names: vec!["Armenian_VYUN", "Armenian_vyun"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_pyur,
            names: vec!["Armenian_PYUR", "Armenian_pyur"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_ke,
            names: vec!["Armenian_KE", "Armenian_ke"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_o,
            names: vec!["Armenian_O", "Armenian_o"],
        },
        KsNamesEntry {
            keysym: Keysym::Armenian_fe,
            names: vec!["Armenian_FE", "Armenian_fe"],
        },
        KsNamesEntry {
            keysym: Keysym::xabovedot,
            names: vec!["Xabovedot", "xabovedot"],
        },
        KsNamesEntry {
            keysym: Keysym::ibreve,
            names: vec!["Ibreve", "ibreve"],
        },
        KsNamesEntry {
            keysym: Keysym::zstroke,
            names: vec!["Zstroke", "zstroke"],
        },
        KsNamesEntry {
            keysym: Keysym::gcaron,
            names: vec!["Gcaron", "gcaron"],
        },
        KsNamesEntry {
            keysym: Keysym::ocaron,
            names: vec!["Ocaron", "ocaron"],
        },
        KsNamesEntry {
            keysym: Keysym::obarred,
            names: vec!["Obarred", "obarred"],
        },
        KsNamesEntry {
            keysym: Keysym::schwa,
            names: vec!["SCHWA", "schwa"],
        },
        KsNamesEntry {
            keysym: Keysym::ezh,
            names: vec!["EZH", "ezh"],
        },
        KsNamesEntry {
            keysym: Keysym::lbelowdot,
            names: vec!["Lbelowdot", "lbelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::abelowdot,
            names: vec!["Abelowdot", "abelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::ahook,
            names: vec!["Ahook", "ahook"],
        },
        KsNamesEntry {
            keysym: Keysym::acircumflexacute,
            names: vec!["Acircumflexacute", "acircumflexacute"],
        },
        KsNamesEntry {
            keysym: Keysym::acircumflexgrave,
            names: vec!["Acircumflexgrave", "acircumflexgrave"],
        },
        KsNamesEntry {
            keysym: Keysym::acircumflexhook,
            names: vec!["Acircumflexhook", "acircumflexhook"],
        },
        KsNamesEntry {
            keysym: Keysym::acircumflextilde,
            names: vec!["Acircumflextilde", "acircumflextilde"],
        },
        KsNamesEntry {
            keysym: Keysym::acircumflexbelowdot,
            names: vec!["Acircumflexbelowdot", "acircumflexbelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::abreveacute,
            names: vec!["Abreveacute", "abreveacute"],
        },
        KsNamesEntry {
            keysym: Keysym::abrevegrave,
            names: vec!["Abrevegrave", "abrevegrave"],
        },
        KsNamesEntry {
            keysym: Keysym::abrevehook,
            names: vec!["Abrevehook", "abrevehook"],
        },
        KsNamesEntry {
            keysym: Keysym::abrevetilde,
            names: vec!["Abrevetilde", "abrevetilde"],
        },
        KsNamesEntry {
            keysym: Keysym::abrevebelowdot,
            names: vec!["Abrevebelowdot", "abrevebelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::ebelowdot,
            names: vec!["Ebelowdot", "ebelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::ehook,
            names: vec!["Ehook", "ehook"],
        },
        KsNamesEntry {
            keysym: Keysym::etilde,
            names: vec!["Etilde", "etilde"],
        },
        KsNamesEntry {
            keysym: Keysym::ecircumflexacute,
            names: vec!["Ecircumflexacute", "ecircumflexacute"],
        },
        KsNamesEntry {
            keysym: Keysym::ecircumflexgrave,
            names: vec!["Ecircumflexgrave", "ecircumflexgrave"],
        },
        KsNamesEntry {
            keysym: Keysym::ecircumflexhook,
            names: vec!["Ecircumflexhook", "ecircumflexhook"],
        },
        KsNamesEntry {
            keysym: Keysym::ecircumflextilde,
            names: vec!["Ecircumflextilde", "ecircumflextilde"],
        },
        KsNamesEntry {
            keysym: Keysym::ecircumflexbelowdot,
            names: vec!["Ecircumflexbelowdot", "ecircumflexbelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::ihook,
            names: vec!["Ihook", "ihook"],
        },
        KsNamesEntry {
            keysym: Keysym::ibelowdot,
            names: vec!["Ibelowdot", "ibelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::obelowdot,
            names: vec!["Obelowdot", "obelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::ohook,
            names: vec!["Ohook", "ohook"],
        },
        KsNamesEntry {
            keysym: Keysym::ocircumflexacute,
            names: vec!["Ocircumflexacute", "ocircumflexacute"],
        },
        KsNamesEntry {
            keysym: Keysym::ocircumflexgrave,
            names: vec!["Ocircumflexgrave", "ocircumflexgrave"],
        },
        KsNamesEntry {
            keysym: Keysym::ocircumflexhook,
            names: vec!["Ocircumflexhook", "ocircumflexhook"],
        },
        KsNamesEntry {
            keysym: Keysym::ocircumflextilde,
            names: vec!["Ocircumflextilde", "ocircumflextilde"],
        },
        KsNamesEntry {
            keysym: Keysym::ocircumflexbelowdot,
            names: vec!["Ocircumflexbelowdot", "ocircumflexbelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::ohornacute,
            names: vec!["Ohornacute", "ohornacute"],
        },
        KsNamesEntry {
            keysym: Keysym::ohorngrave,
            names: vec!["Ohorngrave", "ohorngrave"],
        },
        KsNamesEntry {
            keysym: Keysym::ohornhook,
            names: vec!["Ohornhook", "ohornhook"],
        },
        KsNamesEntry {
            keysym: Keysym::ohorntilde,
            names: vec!["Ohorntilde", "ohorntilde"],
        },
        KsNamesEntry {
            keysym: Keysym::ohornbelowdot,
            names: vec!["Ohornbelowdot", "ohornbelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::ubelowdot,
            names: vec!["Ubelowdot", "ubelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::uhook,
            names: vec!["Uhook", "uhook"],
        },
        KsNamesEntry {
            keysym: Keysym::uhornacute,
            names: vec!["Uhornacute", "uhornacute"],
        },
        KsNamesEntry {
            keysym: Keysym::uhorngrave,
            names: vec!["Uhorngrave", "uhorngrave"],
        },
        KsNamesEntry {
            keysym: Keysym::uhornhook,
            names: vec!["Uhornhook", "uhornhook"],
        },
        KsNamesEntry {
            keysym: Keysym::uhorntilde,
            names: vec!["Uhorntilde", "uhorntilde"],
        },
        KsNamesEntry {
            keysym: Keysym::uhornbelowdot,
            names: vec!["Uhornbelowdot", "uhornbelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::ybelowdot,
            names: vec!["Ybelowdot", "ybelowdot"],
        },
        KsNamesEntry {
            keysym: Keysym::yhook,
            names: vec!["Yhook", "yhook"],
        },
        KsNamesEntry {
            keysym: Keysym::ytilde,
            names: vec!["Ytilde", "ytilde"],
        },
        KsNamesEntry {
            keysym: Keysym::ohorn,
            names: vec!["Ohorn", "ohorn"],
        },
        KsNamesEntry {
            keysym: Keysym::uhorn,
            names: vec!["Uhorn", "uhorn"],
        },
        KsNamesEntry {
            keysym: Keysym::XF86_Screensaver,
            names: vec!["XF86ScreenSaver", "XF86Screensaver"],
        },
    ]
}

#[test]
fn keysym_bounds() {
    // Bounds
    assert_eq!(XKB_KEYSYM_MIN, 0);
    assert!(XKB_KEYSYM_MIN < XKB_KEYSYM_MAX);
    assert!(XKB_KEYSYM_MAX <= u32::MAX);
    // ensure it fits in xkb_keysym_t
    assert!(XKB_KEYSYM_MAX <= i32::MAX as u32);
    // ensure correct cast to i32_t
    assert!(XKB_KEYSYM_MIN_ASSIGNED == XKB_KEYSYM_MIN);
    assert!(XKB_KEYSYM_MIN_ASSIGNED < XKB_KEYSYM_MAX_ASSIGNED);
    assert!(XKB_KEYSYM_MAX_ASSIGNED <= XKB_KEYSYM_MAX);
    assert!(XKB_KEYSYM_MIN_EXPLICIT == XKB_KEYSYM_MIN_ASSIGNED);
    assert!(XKB_KEYSYM_MIN_EXPLICIT < XKB_KEYSYM_MAX_EXPLICIT);
    assert!(XKB_KEYSYM_MAX_EXPLICIT <= XKB_KEYSYM_MAX_ASSIGNED);
    assert!(
        u32::try_from(XKB_KEYSYM_COUNT_EXPLICIT).unwrap()
            <= XKB_KEYSYM_MAX_EXPLICIT - XKB_KEYSYM_MIN_EXPLICIT + 1
    );
    assert!(XKB_KEYSYM_UNICODE_MIN >= XKB_KEYSYM_MIN_EXPLICIT);
    assert!(XKB_KEYSYM_UNICODE_MIN < XKB_KEYSYM_UNICODE_MAX);
    assert!(XKB_KEYSYM_UNICODE_MAX <= XKB_KEYSYM_MAX_EXPLICIT);
}

#[test]
fn assigned_keysyms() {
    assert!(keysym_is_assigned(&Keysym::from(XKB_KEYSYM_MIN)));
    assert!(keysym_is_assigned(&Keysym::from(XKB_KEYSYM_MIN_ASSIGNED)));
    assert!(keysym_is_assigned(&Keysym::space));
    assert!(keysym_is_assigned(&Keysym::nobreakspace));
    assert!(keysym_is_assigned(&Keysym::Aogonek));
    assert!(keysym_is_assigned(&Keysym::Hstroke));
    assert!(keysym_is_assigned(&Keysym::kra));
    assert!(keysym_is_assigned(&Keysym::braille_dot_1));
    assert!(keysym_is_assigned(&Keysym::XF86_KbdLcdMenu5));
    assert!(keysym_is_assigned(&Keysym::Shift_L));
    assert!(keysym_is_assigned(&Keysym::XF86_MonBrightnessUp));
    assert!(keysym_is_assigned(&Keysym::VoidSymbol));
    assert!(keysym_is_assigned(&Keysym::from(XKB_KEYSYM_UNICODE_MIN)));
    assert!(keysym_is_assigned(&Keysym::from(
        (XKB_KEYSYM_UNICODE_MIN + XKB_KEYSYM_UNICODE_MAX) / 2
    )));
    assert!(keysym_is_assigned(&Keysym::from(XKB_KEYSYM_UNICODE_MAX)));
    assert!(keysym_is_assigned(&Keysym::from(XKB_KEYSYM_MAX_ASSIGNED)));
    assert!(!keysym_is_assigned(&Keysym::from(XKB_KEYSYM_MAX)));
}

#[test]
fn test_keysyms() {
    let cm = CaseMapper::new();

    let mut ks_prev = XKB_KEYSYM_MIN;
    let mut count = 0;
    let mut count_non_unicode = 0;

    let mut iter = KeysymIterator::new(false);

    while let Some(ks) = iter.next() {
        // May have repeat syms
        if ks_prev == ks {
            continue;
        }

        count += 1;
        if ks < XKB_KEYSYM_UNICODE_MIN || ks > XKB_KEYSYM_UNICODE_MAX {
            count_non_unicode += 1;
        }
        assert!(ks > ks_prev || count == 1);
        ks_prev = ks;

        // Check assigned keysyms bounds
        assert!(XKB_KEYSYM_MIN_ASSIGNED as i32 <= ks as i32 && ks <= XKB_KEYSYM_MAX_ASSIGNED);

        // Check utf8
        let utf8 = keysym_to_utf8(&Keysym::from(ks)).unwrap_or(vec![]); //TODO: is this correct?
        let needed = utf8.len();
        assert!(needed <= 5);

        // Check maximum name length
        let name = iter.get_name().unwrap();
        let needed = name.len();
        assert!(0 < needed && needed <= XKB_KEYSYM_NAME_MAX_SIZE);

        // Test modifier keysyms
        let expected = test_modifier(ks);
        let got = keysym_is_modifier(&Keysym::from(ks));
        assert_eq!(got, expected);

        // Test keypad keysyms
        let expected = test_keypad(ks, name.as_str());
        let got = keysym_is_keypad(&Keysym::from(ks));

        assert_eq!(got, expected);

        test_icu_case_mappings(&cm, &Keysym::from(ks)); // TODO
    }
    assert_eq!(ks_prev, XKB_KEYSYM_MAX_ASSIGNED);
    assert_eq!(
        count,
        XKB_KEYSYM_UNICODE_MAX - XKB_KEYSYM_UNICODE_MIN + 1 + count_non_unicode
    );

    // Named keysyms
    assert!(test_string("NoSymbol", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("Undo", 0xFF65));
    assert!(test_string("UNDO", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string(
        "ThisKeyShouldNotExist",
        xkeysym::NO_SYMBOL.raw()
    ));
    assert!(test_string("XF86_Switch_VT_5", 0x1008FE05));
    assert!(test_string("VoidSymbol", 0xFFFFFF));
    assert!(test_string("0", 0x30));
    assert!(test_string("9", 0x39));
    assert!(test_string("a", 0x61));
    assert!(test_string("A", 0x41));
    assert!(test_string("ch", 0xfea0));
    assert!(test_string("Ch", 0xfea1));
    assert!(test_string("CH", 0xfea2));
    assert!(test_string("THORN", 0x00de));
    assert!(test_string("Thorn", 0x00de));
    assert!(test_string("thorn", 0x00fe));
    assert!(test_string(" thorn", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("thorn ", xkeysym::NO_SYMBOL.raw()));

    /* Decimal keysyms are not supported (digits are special cases) */
    assert!(test_string("-1", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("10", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("010", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("4567", xkeysym::NO_SYMBOL.raw()));

    /* Unicode: test various ranges */
    assert!(test_string("U0000", xkeysym::NO_SYMBOL.raw())); /* Min Unicode */
    assert!(test_string("U001f", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("U0020", 0x0000020));
    assert!(test_string("U007E", 0x000007e));
    assert!(test_string("U007f", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("U009f", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("U00a0", 0x00000a0));
    assert!(test_string("U00ff", 0x00000ff));
    assert!(test_string("U0100", XKB_KEYSYM_UNICODE_MIN));
    assert!(test_string("U4567", 0x1004567));
    assert!(test_string("U1F4A9", 0x0101F4A9));
    assert!(test_string("U10FFFF", XKB_KEYSYM_UNICODE_MAX)); /* Max Unicode */
    assert!(test_string("U110000", xkeysym::NO_SYMBOL.raw()));
    /* Unicode: test syntax */
    assert!(test_string("U00004567", 0x1004567)); /* OK:  8 digits */
    assert!(test_string("U000004567", xkeysym::NO_SYMBOL.raw())); /* ERR: 9 digits */
    assert!(test_string("U+4567", xkeysym::NO_SYMBOL.raw())); /* ERR: Standard Unicode notation */
    assert!(test_string("U+4567ffff", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("U+4567ffffff", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("U-456", xkeysym::NO_SYMBOL.raw())); /* No negative number */
    assert!(test_string("U456w", xkeysym::NO_SYMBOL.raw())); /* Not hexadecimal digit */
    assert!(test_string("U4567   ", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("   U4567", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("U   4567", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("U  +4567", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("u4567", xkeysym::NO_SYMBOL.raw())); /* Require XKB_KEYSYM_CASE_INSENSITIVE */

    /* Hexadecimal: test ranges */
    assert!(test_string(
        &get_unicode_name(XKB_KEYSYM_MIN)[1..],
        xkeysym::NO_SYMBOL.raw()
    )); /* Min keysym. */
    assert!(test_string("0x1", 0x00000001));
    assert!(test_string("0x01234567", 0x01234567));
    assert!(test_string("0x09abcdef", 0x09abcdef));
    assert!(test_string("0x01000100", XKB_KEYSYM_UNICODE_MIN)); /* Min Unicode. */
    assert!(test_string("0x0110ffff", XKB_KEYSYM_UNICODE_MAX)); /* Max Unicode. */
    assert!(test_string(
        &format!("{:#08x}", XKB_KEYSYM_MAX),
        XKB_KEYSYM_MAX
    )); /* Max keysym. */
    assert!(test_string("0x20000000", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("0xffffffff", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("0x100000000", xkeysym::NO_SYMBOL.raw()));
    /* Hexadecimal: test syntax */
    assert!(test_string("0x10203040", 0x10203040)); /* OK:  8 digits */
    assert!(test_string("0x102030400", xkeysym::NO_SYMBOL.raw())); /* ERR: 9 digits */
    assert!(test_string("0x01020304", 0x1020304)); /* OK:  8 digits, starts with 0 */
    assert!(test_string("0x010203040", xkeysym::NO_SYMBOL.raw())); /* ERR: 9 digits, starts with 0 */
    assert!(test_string("0x+10203040", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("0x01020304w", xkeysym::NO_SYMBOL.raw())); /* Not hexadecimal digit */
    assert!(test_string("0x102030  ", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("0x  102030", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("  0x102030", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("0x  +10203040", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("0x-10203040", xkeysym::NO_SYMBOL.raw()));
    assert!(test_string("0X10203040", xkeysym::NO_SYMBOL.raw())); /* Require XKB_KEYSYM_CASE_INSENSITIVE */
    assert!(test_string("10203040", xkeysym::NO_SYMBOL.raw())); /* Missing prefix/decimal not implemented */
    assert!(test_string("0b0101", xkeysym::NO_SYMBOL.raw())); /* Wrong prefix: binary not implemented */
    assert!(test_string("0o0701", xkeysym::NO_SYMBOL.raw())); /* Wrong prefix: octal not implemented */

    assert!(test_keysym(0x1008FF56, "XF86Close"));
    assert!(test_keysym(0x0, "NoSymbol"));
    assert!(test_keysym(0x1008FE20, "XF86Ungrab"));
    assert!(test_keysym(XKB_KEYSYM_UNICODE_OFFSET, "0x01000000"));
    /* Canonical names */
    assert!(test_keysym(Keysym::Henkan.into(), "Henkan_Mode"));
    assert!(test_keysym(Keysym::ISO_Group_Shift.into(), "Mode_switch"));
    assert!(test_keysym(Keysym::dead_perispomeni.into(), "dead_tilde"));
    assert!(test_keysym(Keysym::guillemetleft.into(), "guillemotleft"));
    assert!(test_keysym(Keysym::ordmasculine.into(), "masculine"));
    assert!(test_keysym(Keysym::Greek_lambda.into(), "Greek_lamda"));
    /* Min Unicode */
    assert!(test_keysym(XKB_KEYSYM_UNICODE_MIN, "U0100"));
    assert!(test_keysym(0x01001234, "U1234"));
    /* 16-bit unicode padded to width 4. */
    assert!(test_keysym(0x010002DE, "U02DE"));
    /* 32-bit unicode padded to width 8. */
    assert!(test_keysym(0x0101F4A9, "U0001F4A9"));
    /* Max Unicode */
    assert!(test_keysym(XKB_KEYSYM_UNICODE_MAX, "U0010FFFF"));
    /* Max Unicode + 1 */
    assert!(test_keysym(0x01110000, "0x01110000"));
    /* Min keysym. */
    assert!(test_keysym(XKB_KEYSYM_MIN, "NoSymbol"));
    /* Max keysym. */
    assert!(test_keysym(
        XKB_KEYSYM_MAX,
        &format!("{:#08x}", XKB_KEYSYM_MAX)
    ));
    /* Outside range. */
    assert!(test_keysym(XKB_KEYSYM_MAX + 1, "Invalid"));
    assert!(test_keysym(0xffffffff, "Invalid"));

    assert!(test_casestring("Undo", 0xFF65));
    assert!(test_casestring("UNDO", 0xFF65));
    assert!(test_casestring("A", 0x61));
    assert!(test_casestring("a", 0x61));
    assert!(test_casestring(
        "ThisKeyShouldNotExist",
        xkeysym::NO_SYMBOL.raw()
    ));
    assert!(test_casestring("XF86_Switch_vT_5", 0x1008FE05));
    assert!(test_casestring("xF86_SwitcH_VT_5", 0x1008FE05));
    assert!(test_casestring("xF86SwiTch_VT_5", 0x1008FE05));
    assert!(test_casestring("xF86Switch_vt_5", 0x1008FE05));
    assert!(test_casestring("VoidSymbol", 0xFFFFFF));
    assert!(test_casestring("vOIDsymBol", 0xFFFFFF));
    assert!(test_casestring("U4567", 0x1004567));
    assert!(test_casestring("u4567", 0x1004567));
    assert!(test_casestring("0x10203040", 0x10203040));
    assert!(test_casestring("0X10203040", 0x10203040));
    assert!(test_casestring("THORN", 0x00fe));
    assert!(test_casestring("Thorn", 0x00fe));
    assert!(test_casestring("thorn", 0x00fe));

    for entry in ambiguous_icase_names() {
        test_ambiguous_icase_names(&entry);
    }

    assert!(test_string("", xkeysym::NO_SYMBOL.raw()));
    assert!(test_casestring("", xkeysym::NO_SYMBOL.raw()));

    /* Latin-1 keysyms (1:1 mapping in UTF-32) */
    assert!(test_utf8(&Keysym::from(0x0020), "\x20").unwrap());
    assert!(test_utf8(&Keysym::from(0x007e), "\x7e").unwrap());

    assert!(test_utf8(
        &Keysym::from(0x00a0),
        &String::from_utf8(vec![0xc2, 0xa0]).unwrap()
    )
    .unwrap());
    assert!(test_utf8(
        &Keysym::from(0x00ff),
        &String::from_utf8(vec![0xc3, 0xbf]).unwrap()
    )
    .unwrap());

    assert!(test_utf8(&Keysym::y, "y").unwrap());
    assert!(test_utf8(&Keysym::u, "u").unwrap());
    assert!(test_utf8(&Keysym::m, "m").unwrap());
    assert!(test_utf8(&Keysym::Cyrillic_em, "м").unwrap());
    assert!(test_utf8(&Keysym::Cyrillic_u, "у").unwrap());
    assert!(test_utf8(&Keysym::exclam, "!").unwrap());
    assert!(test_utf8(&Keysym::oslash, "ø").unwrap());
    assert!(test_utf8(&Keysym::hebrew_aleph, "א").unwrap());
    assert!(test_utf8(&Keysym::Arabic_sheen, "ش").unwrap());

    /* Keysyms with special handling */
    assert!(test_utf8(&Keysym::space, " ").unwrap());
    assert!(test_utf8(&Keysym::KP_Space, " ").unwrap());
    // \u{8} is equivalent to \b
    assert!(test_utf8(&Keysym::BackSpace, "\u{8}").unwrap());
    assert!(test_utf8(&Keysym::Escape, "\x1b").unwrap());
    assert!(test_utf8(&Keysym::KP_Separator, ",").unwrap());
    assert!(test_utf8(&Keysym::KP_Decimal, ".").unwrap());
    assert!(test_utf8(&Keysym::Tab, "\t").unwrap());
    assert!(test_utf8(&Keysym::KP_Tab, "\t").unwrap());
    assert!(test_utf8(&Keysym::hyphen, "­").unwrap());
    assert!(test_utf8(&Keysym::Linefeed, "\n").unwrap());
    assert!(test_utf8(&Keysym::Return, "\r").unwrap());
    assert!(test_utf8(&Keysym::KP_Enter, "\r").unwrap());
    assert!(test_utf8(&Keysym::KP_Equal, "=").unwrap());
    assert!(test_utf8(&Keysym::_9, "9").unwrap());
    assert!(test_utf8(&Keysym::KP_9, "9").unwrap());
    assert!(test_utf8(&Keysym::KP_Multiply, "*").unwrap());
    assert!(test_utf8(&Keysym::KP_Subtract, "-").unwrap());

    /* Unicode keysyms */
    assert!(test_utf8(&Keysym::from(XKB_KEYSYM_UNICODE_OFFSET), "").is_none()); /* Min Unicode codepoint */
    assert!(test_utf8(&Keysym::from(0x1000001), "\x01").unwrap()); /* Currently accepted, but not intended (< 0x100100) */
    assert!(test_utf8(&Keysym::from(0x1000020), " ").unwrap()); /* Currently accepted, but not intended (< 0x100100) */
    assert!(test_utf8(&Keysym::from(0x100007f), "\x7f").unwrap()); /* Currently accepted, but not intended (< 0x100100) */
    assert!(test_utf8(
        &Keysym::from(0x10000a0),
        &String::from_utf8(vec![0xc2, 0xa0]).unwrap()
    )
    .unwrap()); /* Currently accepted, but not intended (< 0x100100) */
    assert!(test_utf8(&Keysym::from(XKB_KEYSYM_UNICODE_MIN), "Ā").unwrap()); /* Min Unicode keysym */
    assert!(test_utf8(&Keysym::from(0x10005d0), "א").unwrap());
    assert!(test_utf8(
        &Keysym::from(XKB_KEYSYM_UNICODE_MAX),
        &String::from_utf8(vec![0xf4, 0x8f, 0xbf, 0xbf]).unwrap()
    )
    .unwrap()); /* Max Unicode */
    assert!(test_utf8(&Keysym::from(0x0100d800), "").is_none()); // Unicode surrogates
    assert!(test_utf8(&Keysym::from(0x0100dfff), "").is_none()); // Unicode surrogates
    assert!(test_utf8(&Keysym::from(0x1110000), "").is_none());

    assert!(test_utf32_to_keysym('y' as u32, Keysym::y));
    assert!(test_utf32_to_keysym('u' as u32, Keysym::u));
    assert!(test_utf32_to_keysym('m' as u32, Keysym::m));
    assert!(test_utf32_to_keysym(0x43c, Keysym::Cyrillic_em));
    assert!(test_utf32_to_keysym(0x443, Keysym::Cyrillic_u));
    assert!(test_utf32_to_keysym('!' as u32, Keysym::exclam));
    assert!(test_utf32_to_keysym(0xF8, Keysym::oslash));
    assert!(test_utf32_to_keysym(0x5D0, Keysym::hebrew_aleph));
    assert!(test_utf32_to_keysym(0x634, Keysym::Arabic_sheen));
    assert!(test_utf32_to_keysym(0x1F609, Keysym::from(0x0101F609))); // ;) emoji

    assert!(test_utf32_to_keysym('\u{8}' as u32, Keysym::BackSpace));
    assert!(test_utf32_to_keysym('\t' as u32, Keysym::Tab));
    assert!(test_utf32_to_keysym('\n' as u32, Keysym::Linefeed));
    assert!(test_utf32_to_keysym(0x0b, Keysym::Clear));
    assert!(test_utf32_to_keysym('\r' as u32, Keysym::Return));
    assert!(test_utf32_to_keysym(0x1b, Keysym::Escape));
    assert!(test_utf32_to_keysym(0x7f, Keysym::Delete));

    assert!(test_utf32_to_keysym(' ' as u32, Keysym::space));
    assert!(test_utf32_to_keysym(',' as u32, Keysym::comma));
    assert!(test_utf32_to_keysym('.' as u32, Keysym::period));
    assert!(test_utf32_to_keysym('=' as u32, Keysym::equal));
    assert!(test_utf32_to_keysym('9' as u32, Keysym::_9));
    assert!(test_utf32_to_keysym('*' as u32, Keysym::asterisk));
    assert!(test_utf32_to_keysym(0xd7, Keysym::multiply));
    assert!(test_utf32_to_keysym('-' as u32, Keysym::minus));
    assert!(test_utf32_to_keysym(0x10fffd, Keysym::from(0x110fffd)));
    assert!(test_utf32_to_keysym(0x20ac, Keysym::EuroSign));

    // Unicode non-characters
    assert!(test_utf32_to_keysym(0xd800, Keysym::NoSymbol)); // Unicode surrogates
    assert!(test_utf32_to_keysym(0xdfff, Keysym::NoSymbol)); // Unicode surrogates
    assert!(test_utf32_to_keysym(0xfdd0, Keysym::NoSymbol));
    assert!(test_utf32_to_keysym(0xfdef, Keysym::NoSymbol));
    assert!(test_utf32_to_keysym(0xfffe, Keysym::NoSymbol));
    assert!(test_utf32_to_keysym(0xffff, Keysym::NoSymbol));
    assert!(test_utf32_to_keysym(0x7fffe, Keysym::NoSymbol));
    assert!(test_utf32_to_keysym(0x7ffff, Keysym::NoSymbol));
    assert!(test_utf32_to_keysym(0xafffe, Keysym::NoSymbol));
    assert!(test_utf32_to_keysym(0xaffff, Keysym::NoSymbol));

    // Codepoints outside the Unicode planes
    assert!(test_utf32_to_keysym(0x110000, Keysym::NoSymbol));
    assert!(test_utf32_to_keysym(0xdeadbeef, Keysym::NoSymbol));

    assert!(keysym_is_lower(&Keysym::a));
    assert!(keysym_is_lower(&Keysym::Greek_lambda));
    assert!(keysym_is_lower(&keysym_from_name("U03b1", 0).unwrap())); /* GREEK SMALL LETTER ALPHA */
    assert!(keysym_is_lower(&keysym_from_name("U03af", 0).unwrap())); /* GREEK SMALL LETTER IOTA WITH TONOS */

    assert!(keysym_is_upper(&Keysym::A));
    assert!(keysym_is_upper(&Keysym::Greek_LAMBDA));
    assert!(keysym_is_upper(&keysym_from_name("U0391", 0).unwrap())); /* GREEK CAPITAL LETTER ALPHA */
    assert!(keysym_is_upper(&keysym_from_name("U0388", 0).unwrap())); /* GREEK CAPITAL LETTER EPSILON WITH TONOS */

    assert!(!keysym_is_upper(&Keysym::a));
    assert!(!keysym_is_lower(&Keysym::A));
    assert!(!keysym_is_lower(&Keysym::Return));
    assert!(!keysym_is_upper(&Keysym::Return));
    assert!(!keysym_is_lower(&Keysym::hebrew_aleph));
    assert!(!keysym_is_upper(&Keysym::hebrew_aleph));
    assert!(!keysym_is_upper(&keysym_from_name("U05D0", 0).unwrap())); /* HEBREW LETTER ALEF */
    assert!(!keysym_is_lower(&keysym_from_name("U05D0", 0).unwrap())); /* HEBREW LETTER ALEF */
    assert!(!keysym_is_lower(&Keysym::_8));
    assert!(!keysym_is_upper(&Keysym::_8));

    assert!(Keysym::KP_Enter.is_keypad_key());
    assert!(Keysym::KP_6.is_keypad_key());
    assert!(Keysym::KP_Add.is_keypad_key());
    assert!(!Keysym::Num_Lock.is_keypad_key());
    assert!(!Keysym::_1.is_keypad_key());
    assert!(!Keysym::Return.is_keypad_key());

    assert!(keysym_to_upper(&Keysym::a) == Keysym::A);
    assert!(keysym_to_upper(&Keysym::A) == Keysym::A);
    assert!(keysym_to_lower(&Keysym::a) == Keysym::a);
    assert!(keysym_to_lower(&Keysym::A) == Keysym::a);
    assert!(keysym_to_upper(&Keysym::Return) == Keysym::Return);
    assert!(keysym_to_lower(&Keysym::Return) == Keysym::Return);
    assert!(keysym_to_upper(&Keysym::Greek_lambda) == Keysym::Greek_LAMBDA);
    assert!(keysym_to_upper(&Keysym::Greek_LAMBDA) == Keysym::Greek_LAMBDA);
    assert!(keysym_to_lower(&Keysym::Greek_lambda) == Keysym::Greek_lambda);
    assert!(keysym_to_lower(&Keysym::Greek_LAMBDA) == Keysym::Greek_lambda);
    assert!(keysym_to_upper(&Keysym::eacute) == Keysym::Eacute);
    assert!(keysym_to_lower(&Keysym::Eacute) == Keysym::eacute);

    test_github_issue_42();
}

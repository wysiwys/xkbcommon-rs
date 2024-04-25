#[cfg(test)]
use xkeysym::RawKeysym;

use crate::rust_xkbcommon::*;

use crate::keysyms_generated_phf::{KEYSYM_TO_NAME, NAME_TO_KEYSYM};

#[allow(dead_code)]
mod constants {
    pub const XKB_KEYSYM_MIN: u32 = 0;
    pub const XKB_KEYSYM_MIN_ASSIGNED: u32 = 0;
    pub const XKB_KEYSYM_MAX_ASSIGNED: u32 = 0x1008ffb8;
    pub const XKB_KEYSYM_MIN_EXPLICIT: u32 = 0;
    pub const XKB_KEYSYM_MAX_EXPLICIT: u32 = 0x1008ffb8;
    pub const XKB_KEYSYM_COUNT_EXPLICIT: usize = 2446;
    pub const XKB_KEYSYM_UNICODE_OFFSET: u32 = 0x01000000;
    pub const XKB_KEYSYM_UNICODE_MIN: u32 = 0x01000100;
    pub const XKB_KEYSYM_UNICODE_MAX: u32 = 0x0110ffff;
    pub const XKB_KEYSYM_NAME_MAX_SIZE: usize = 27;
}

pub use constants::*;

fn find_keysym_name(ks: &Keysym) -> Option<&'static str> {
    if ks.raw() > XKB_KEYSYM_MAX_EXPLICIT {
        return None;
    }

    let found_idx = KEYSYM_TO_NAME.binary_search_by_key(ks, |&(a, _)| a).ok()?;

    let found_sym = KEYSYM_TO_NAME[found_idx].0.raw();
    let mut found_name = KEYSYM_TO_NAME[found_idx].1;

    // Get the first sym that matches
    let mut try_idx = match found_idx {
        0 => return Some(found_name),
        idx => idx - 1,
    };

    while KEYSYM_TO_NAME[try_idx].0.raw() == found_sym {
        found_name = KEYSYM_TO_NAME[try_idx].1;
        try_idx = match try_idx {
            0 => return Some(found_name),
            idx => idx - 1,
        };
    }
    Some(found_name)
}
pub(crate) fn get_unicode_name(ks: u32) -> String {
    match (ks & 0xff0000) != 0 {
        true => format!("U{:08X}", ks & 0xffffff),
        false => format!("U{:04X}", ks & 0xffffff),
    }
}

pub fn keysym_get_name(ks: &Keysym) -> Option<String> {
    if let Some(sym) = find_keysym_name(ks) {
        return Some(sym.into());
    }
    let ks: u32 = ks.raw();

    if ks > XKB_KEYSYM_MAX {
        return None;
    }

    // Unnamed Unicode codepoint.
    if (XKB_KEYSYM_UNICODE_MIN..=XKB_KEYSYM_UNICODE_MAX).contains(&ks) {
        return Some(get_unicode_name(ks));
    }

    // Unnamed, non-unicode symbol (shouldn't generally happen)
    Some(format!("{:#010x}", ks))
}

// TODO: move test functions/structs to separate module
#[cfg(test)]
pub(crate) fn keysym_is_assigned(ks: &Keysym) -> bool {
    (XKB_KEYSYM_UNICODE_MIN..=XKB_KEYSYM_UNICODE_MAX).contains(&ks.raw())
        || find_keysym_name(ks).is_some()
}

#[cfg(test)]
pub(crate) struct KeysymIterator {
    explicit: bool,       // If true, traverse only explicitly named keysyms
    index: Option<usize>, // Current index in keysym_to_name
    keysym: RawKeysym,
}

#[cfg(test)]
impl KeysymIterator {
    pub(crate) fn new(iterate_only_explicit_keysyms: bool) -> Self {
        Self {
            explicit: iterate_only_explicit_keysyms,
            index: None,
            keysym: XKB_KEYSYM_UNICODE_MAX,
        }
    }

    /*
    pub(crate) fn is_explicitly_named(&self) -> bool {
        // ensure >= 0
        let index: usize = match self.index {
            Some(index) if index < KEYSYM_TO_NAME.len() => index,
            _ => return false,
        };

        self.explicit
            || KEYSYM_TO_NAME.get(index)
                .map(|(sym, _)| sym.raw() == self.keysym)
                .unwrap_or(false)
    }
    */

    pub(crate) fn get_name(&self) -> Option<String> {
        let index: usize = match self.index {
            Some(index) if index < KEYSYM_TO_NAME.len() => index,
            _ => return None,
        };
        if self.explicit
            || KEYSYM_TO_NAME
                .get(index)
                .map(|(sym, _)| sym.raw() == self.keysym)
                .unwrap_or(false)
        {
            return KEYSYM_TO_NAME.get(index).map(|(_, name)| name.to_string());
        }

        Some(get_unicode_name(self.keysym))
    }

    pub(crate) fn next(&mut self) -> Option<RawKeysym> {
        if let Some(index) = self.index {
            if index >= KEYSYM_TO_NAME.len() - 1 {
                return None;
            }
        }

        // Next keysym
        if self.explicit
            || self.keysym >= XKB_KEYSYM_UNICODE_MAX
            || self
                .index
                .and_then(|i| KEYSYM_TO_NAME.get(i + 1))
                .map(|(sym, _)| sym.raw() < XKB_KEYSYM_UNICODE_MIN)
                .unwrap_or(false)
        {
            // explicitly named keysyms only
            // increment
            if let Some(ref mut index) = self.index {
                *index += 1;
            } else {
                self.index = Some(0);
            }
            self.keysym = KEYSYM_TO_NAME.get(self.index.unwrap()).unwrap().0.raw();

            assert!(
                self.explicit
                    || self.keysym <= XKB_KEYSYM_UNICODE_MIN
                    || self.keysym >= XKB_KEYSYM_UNICODE_MAX,
                "{:?}",
                Keysym::from(self.keysym)
            );
        } else {
            // Unicode keysyms
            // NOTE: Unicode keysyms are within keysym_to_name keysyms range.
            if let Some(ref mut index) = self.index {
                if self.keysym >= KEYSYM_TO_NAME.get(*index).unwrap().0.raw() {
                    *index += 1;
                }
            }
            if self.keysym >= XKB_KEYSYM_UNICODE_MIN {
                // Continue Unicode keysyms
                self.keysym += 1;
            } else {
                self.keysym = XKB_KEYSYM_UNICODE_MIN;
            }
        }

        Some(self.keysym)
    }
}
fn parse_keysym_hex(s: &str) -> Option<u32> {
    let mut result: u32 = 0;

    let mut chars = s.chars();
    for _ in 0..8 {
        if let Some(c) = chars.next() {
            // TODO: is this needed?
            if c == '\0' {
                break;
            }
            let c_u32 = u32::from(c);

            result <<= 4;
            if c.is_ascii_digit() {
                result += c_u32 - u32::from('0');
            } else if ('a'..='f').contains(&c) {
                result += 10 + c_u32 - u32::from('a');
            } else if ('A'..='F').contains(&c) {
                result += 10 + c_u32 - u32::from('A');
            } else {
                return None;
            }
        }
    }

    // TODO: check this bound
    if (1..=8).contains(&s.len()) {
        return Some(result);
    }

    None
}

pub fn keysym_from_name<F>(name: &str, flags: F) -> Option<Keysym>
where
    F: TryInto<KeysymFlags>,
{
    // TODO: updated keysym_from_name and associated functions
    let flags: KeysymFlags = flags.try_into().ok()?;
    let icase = flags.intersects(KeysymFlags::CASE_INSENSITIVE);

    if flags.intersects(!KeysymFlags::CASE_INSENSITIVE) {
        return None;
    }

    if !icase {
        if let Some(sym) = NAME_TO_KEYSYM.get(name) {
            return Some(*sym);
        }
    } else {
        // TODO: clean this up
        let mut entry = None;
        let name_lower = name.to_lowercase();
        let mut lo = 0;
        let mut hi = NAME_TO_KEYSYM.len() - 1;

        // assumes name_to_keysym is sorted by istrcmp`

        while hi >= lo {
            let mid = (lo + hi) / 2;
            let (mid_name_lower, sym) = NAME_TO_KEYSYM
                .index(mid)
                .map(|(name, sym)| (name.to_lowercase(), sym.raw()))
                .unwrap();

            match (&name_lower, &mid_name_lower) {
                (a, b) if a > b => lo = mid + 1,
                (a, b) if a < b => {
                    hi = match mid {
                        0 => break,
                        mid => mid - 1,
                    }
                }
                _ => {
                    entry = Some((mid, sym));
                    break;
                }
            }
        }
        if let Some((ref mut idx, ref mut entry)) = entry {
            // Keep going until we reach end of array or non case-insensitive match
            while *idx < NAME_TO_KEYSYM.len()
                && NAME_TO_KEYSYM
                    .index(*idx)
                    .map(|(name, _)| name.to_lowercase())
                    == NAME_TO_KEYSYM
                        .index(*idx + 1)
                        .map(|(name, _)| name.to_lowercase())
            {
                *idx += 1;
                *entry = NAME_TO_KEYSYM.index(*idx).unwrap().1.raw();
            }
            return Some(Keysym::from(*entry));
        }
    }

    let mut chars = name.chars();
    let first_char = chars.next();
    let second_char = chars.next();
    let _third_char = chars.next();

    if first_char == Some('U') || (icase && first_char == Some('u')) {
        let val = parse_keysym_hex(&name[1..])?;

        if val < 0x20 || (val > 0x7e && val < 0xa0) {
            return None;
        }
        if val < 0x100 {
            return Some(Keysym::from(val));
        }
        if val > 0x10ffff {
            return None;
        }
        return Some(Keysym::from(val | XKB_KEYSYM_UNICODE_OFFSET));
    } else if first_char == Some('0')
        && (second_char == Some('x') || icase && second_char == Some('X'))
    {
        let val = parse_keysym_hex(&name[2..])?;
        if val > XKB_KEYSYM_MAX {
            return None;
        }
        return Some(Keysym::from(val));
    }

    // Try without the underscore,
    // due to inconsistency between the headers
    // and XKeysymDB, where
    // the former has no separating underscore,
    // while soem XF86* syms in the latter did.

    if name.starts_with("XF86_")
        || (icase && name.len() >= 5 && name[0..5].to_lowercase() == "XF86_".to_lowercase())
    {
        if name.is_empty() {
            return None;
        }

        let name = String::from(&name[..4]) + &name[5..];

        return keysym_from_name(&name, flags);
    }

    None
}

fn ucs_convert_case(code: u32) -> ConvertCase {
    static IPA_EXT_UPPER_MAPPING: [u16; 64] = [
        /* part only */
        0x0181, 0x0186, 0x0255, 0x0189, 0x018A, 0x0258, 0x018F, 0x025A, 0x0190, 0x025C, 0x025D,
        0x025E, 0x025F, 0x0193, 0x0261, 0x0262, 0x0194, 0x0264, 0x0265, 0x0266, 0x0267, 0x0197,
        0x0196, 0x026A, 0x026B, 0x026C, 0x026D, 0x026E, 0x019C, 0x0270, 0x0271, 0x019D, 0x0273,
        0x0274, 0x019F, 0x0276, 0x0277, 0x0278, 0x0279, 0x027A, 0x027B, 0x027C, 0x027D, 0x027E,
        0x027F, 0x01A6, 0x0281, 0x0282, 0x01A9, 0x0284, 0x0285, 0x0286, 0x0287, 0x01AE, 0x0289,
        0x01B1, 0x01B2, 0x028C, 0x028D, 0x028E, 0x028F, 0x0290, 0x0291, 0x01B7,
    ];

    static LATIN_EXT_B_UPPER_MAPPING: [u16; 77] = [
        /* first part only */
        0x0180, 0x0181, 0x0182, 0x0182, 0x0184, 0x0184, 0x0186, 0x0187, 0x0187, 0x0189, 0x018A,
        0x018B, 0x018B, 0x018D, 0x018E, 0x018F, 0x0190, 0x0191, 0x0191, 0x0193, 0x0194, 0x01F6,
        0x0196, 0x0197, 0x0198, 0x0198, 0x019A, 0x019B, 0x019C, 0x019D, 0x0220, 0x019F, 0x01A0,
        0x01A0, 0x01A2, 0x01A2, 0x01A4, 0x01A4, 0x01A6, 0x01A7, 0x01A7, 0x01A9, 0x01AA, 0x01AB,
        0x01AC, 0x01AC, 0x01AE, 0x01AF, 0x01AF, 0x01B1, 0x01B2, 0x01B3, 0x01B3, 0x01B5, 0x01B5,
        0x01B7, 0x01B8, 0x01B8, 0x01BA, 0x01BB, 0x01BC, 0x01BC, 0x01BE, 0x01F7, 0x01C0, 0x01C1,
        0x01C2, 0x01C3, 0x01C4, 0x01C4, 0x01C4, 0x01C7, 0x01C7, 0x01C7, 0x01CA, 0x01CA, 0x01CA,
    ];

    static LATIN_EXT_B_LOWER_MAPPING: [u16; 77] = [
        /* first part only */
        0x0180, 0x0253, 0x0183, 0x0183, 0x0185, 0x0185, 0x0254, 0x0188, 0x0188, 0x0256, 0x0257,
        0x018C, 0x018C, 0x018D, 0x01DD, 0x0259, 0x025B, 0x0192, 0x0192, 0x0260, 0x0263, 0x0195,
        0x0269, 0x0268, 0x0199, 0x0199, 0x019A, 0x019B, 0x026F, 0x0272, 0x019E, 0x0275, 0x01A1,
        0x01A1, 0x01A3, 0x01A3, 0x01A5, 0x01A5, 0x0280, 0x01A8, 0x01A8, 0x0283, 0x01AA, 0x01AB,
        0x01AD, 0x01AD, 0x0288, 0x01B0, 0x01B0, 0x028A, 0x028B, 0x01B4, 0x01B4, 0x01B6, 0x01B6,
        0x0292, 0x01B9, 0x01B9, 0x01BA, 0x01BB, 0x01BD, 0x01BD, 0x01BE, 0x01BF, 0x01C0, 0x01C1,
        0x01C2, 0x01C3, 0x01C6, 0x01C6, 0x01C6, 0x01C9, 0x01C9, 0x01C9, 0x01CC, 0x01CC, 0x01CC,
    ];

    static GREEK_UPPER_MAPPING: [u16; 144] = [
        0x0000, 0x0000, 0x0000, 0x0000, 0x0374, 0x0375, 0x0000, 0x0000, 0x0000, 0x0000, 0x037A,
        0x0000, 0x0000, 0x0000, 0x037E, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0384, 0x0385,
        0x0386, 0x0387, 0x0388, 0x0389, 0x038A, 0x0000, 0x038C, 0x0000, 0x038E, 0x038F, 0x0390,
        0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397, 0x0398, 0x0399, 0x039A, 0x039B,
        0x039C, 0x039D, 0x039E, 0x039F, 0x03A0, 0x03A1, 0x0000, 0x03A3, 0x03A4, 0x03A5, 0x03A6,
        0x03A7, 0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x0386, 0x0388, 0x0389, 0x038A, 0x03B0, 0x0391,
        0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397, 0x0398, 0x0399, 0x039A, 0x039B, 0x039C,
        0x039D, 0x039E, 0x039F, 0x03A0, 0x03A1, 0x03A3, 0x03A3, 0x03A4, 0x03A5, 0x03A6, 0x03A7,
        0x03A8, 0x03A9, 0x03AA, 0x03AB, 0x038C, 0x038E, 0x038F, 0x0000, 0x0392, 0x0398, 0x03D2,
        0x03D3, 0x03D4, 0x03A6, 0x03A0, 0x03D7, 0x03D8, 0x03D8, 0x03DA, 0x03DA, 0x03DC, 0x03DC,
        0x03DE, 0x03DE, 0x03E0, 0x03E0, 0x03E2, 0x03E2, 0x03E4, 0x03E4, 0x03E6, 0x03E6, 0x03E8,
        0x03E8, 0x03EA, 0x03EA, 0x03EC, 0x03EC, 0x03EE, 0x03EE, 0x039A, 0x03A1, 0x03F9, 0x03F3,
        0x03F4, 0x0395, 0x03F6, 0x03F7, 0x03F7, 0x03F9, 0x03FA, 0x03FA, 0x0000, 0x0000, 0x0000,
        0x0000,
    ];

    static GREEK_LOWER_MAPPING: [u16; 144] = [
        0x0000, 0x0000, 0x0000, 0x0000, 0x0374, 0x0375, 0x0000, 0x0000, 0x0000, 0x0000, 0x037A,
        0x0000, 0x0000, 0x0000, 0x037E, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0384, 0x0385,
        0x03AC, 0x0387, 0x03AD, 0x03AE, 0x03AF, 0x0000, 0x03CC, 0x0000, 0x03CD, 0x03CE, 0x0390,
        0x03B1, 0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7, 0x03B8, 0x03B9, 0x03BA, 0x03BB,
        0x03BC, 0x03BD, 0x03BE, 0x03BF, 0x03C0, 0x03C1, 0x0000, 0x03C3, 0x03C4, 0x03C5, 0x03C6,
        0x03C7, 0x03C8, 0x03C9, 0x03CA, 0x03CB, 0x03AC, 0x03AD, 0x03AE, 0x03AF, 0x03B0, 0x03B1,
        0x03B2, 0x03B3, 0x03B4, 0x03B5, 0x03B6, 0x03B7, 0x03B8, 0x03B9, 0x03BA, 0x03BB, 0x03BC,
        0x03BD, 0x03BE, 0x03BF, 0x03C0, 0x03C1, 0x03C2, 0x03C3, 0x03C4, 0x03C5, 0x03C6, 0x03C7,
        0x03C8, 0x03C9, 0x03CA, 0x03CB, 0x03CC, 0x03CD, 0x03CE, 0x0000, 0x03D0, 0x03D1, 0x03D2,
        0x03D3, 0x03D4, 0x03D5, 0x03D6, 0x03D7, 0x03D9, 0x03D9, 0x03DB, 0x03DB, 0x03DD, 0x03DD,
        0x03DF, 0x03DF, 0x03E1, 0x03E1, 0x03E3, 0x03E3, 0x03E5, 0x03E5, 0x03E7, 0x03E7, 0x03E9,
        0x03E9, 0x03EB, 0x03EB, 0x03ED, 0x03ED, 0x03EF, 0x03EF, 0x03F0, 0x03F1, 0x03F2, 0x03F3,
        0x03B8, 0x03F5, 0x03F6, 0x03F8, 0x03F8, 0x03F2, 0x03FB, 0x03FB, 0x0000, 0x0000, 0x0000,
        0x0000,
    ];

    static GREEK_EXT_LOWER_MAPPING: [u16; 256] = [
        0x1F00, 0x1F01, 0x1F02, 0x1F03, 0x1F04, 0x1F05, 0x1F06, 0x1F07, 0x1F00, 0x1F01, 0x1F02,
        0x1F03, 0x1F04, 0x1F05, 0x1F06, 0x1F07, 0x1F10, 0x1F11, 0x1F12, 0x1F13, 0x1F14, 0x1F15,
        0x0000, 0x0000, 0x1F10, 0x1F11, 0x1F12, 0x1F13, 0x1F14, 0x1F15, 0x0000, 0x0000, 0x1F20,
        0x1F21, 0x1F22, 0x1F23, 0x1F24, 0x1F25, 0x1F26, 0x1F27, 0x1F20, 0x1F21, 0x1F22, 0x1F23,
        0x1F24, 0x1F25, 0x1F26, 0x1F27, 0x1F30, 0x1F31, 0x1F32, 0x1F33, 0x1F34, 0x1F35, 0x1F36,
        0x1F37, 0x1F30, 0x1F31, 0x1F32, 0x1F33, 0x1F34, 0x1F35, 0x1F36, 0x1F37, 0x1F40, 0x1F41,
        0x1F42, 0x1F43, 0x1F44, 0x1F45, 0x0000, 0x0000, 0x1F40, 0x1F41, 0x1F42, 0x1F43, 0x1F44,
        0x1F45, 0x0000, 0x0000, 0x1F50, 0x1F51, 0x1F52, 0x1F53, 0x1F54, 0x1F55, 0x1F56, 0x1F57,
        0x0000, 0x1F51, 0x0000, 0x1F53, 0x0000, 0x1F55, 0x0000, 0x1F57, 0x1F60, 0x1F61, 0x1F62,
        0x1F63, 0x1F64, 0x1F65, 0x1F66, 0x1F67, 0x1F60, 0x1F61, 0x1F62, 0x1F63, 0x1F64, 0x1F65,
        0x1F66, 0x1F67, 0x1F70, 0x1F71, 0x1F72, 0x1F73, 0x1F74, 0x1F75, 0x1F76, 0x1F77, 0x1F78,
        0x1F79, 0x1F7A, 0x1F7B, 0x1F7C, 0x1F7D, 0x0000, 0x0000, 0x1F80, 0x1F81, 0x1F82, 0x1F83,
        0x1F84, 0x1F85, 0x1F86, 0x1F87, 0x1F80, 0x1F81, 0x1F82, 0x1F83, 0x1F84, 0x1F85, 0x1F86,
        0x1F87, 0x1F90, 0x1F91, 0x1F92, 0x1F93, 0x1F94, 0x1F95, 0x1F96, 0x1F97, 0x1F90, 0x1F91,
        0x1F92, 0x1F93, 0x1F94, 0x1F95, 0x1F96, 0x1F97, 0x1FA0, 0x1FA1, 0x1FA2, 0x1FA3, 0x1FA4,
        0x1FA5, 0x1FA6, 0x1FA7, 0x1FA0, 0x1FA1, 0x1FA2, 0x1FA3, 0x1FA4, 0x1FA5, 0x1FA6, 0x1FA7,
        0x1FB0, 0x1FB1, 0x1FB2, 0x1FB3, 0x1FB4, 0x0000, 0x1FB6, 0x1FB7, 0x1FB0, 0x1FB1, 0x1F70,
        0x1F71, 0x1FB3, 0x1FBD, 0x1FBE, 0x1FBF, 0x1FC0, 0x1FC1, 0x1FC2, 0x1FC3, 0x1FC4, 0x0000,
        0x1FC6, 0x1FC7, 0x1F72, 0x1F73, 0x1F74, 0x1F75, 0x1FC3, 0x1FCD, 0x1FCE, 0x1FCF, 0x1FD0,
        0x1FD1, 0x1FD2, 0x1FD3, 0x0000, 0x0000, 0x1FD6, 0x1FD7, 0x1FD0, 0x1FD1, 0x1F76, 0x1F77,
        0x0000, 0x1FDD, 0x1FDE, 0x1FDF, 0x1FE0, 0x1FE1, 0x1FE2, 0x1FE3, 0x1FE4, 0x1FE5, 0x1FE6,
        0x1FE7, 0x1FE0, 0x1FE1, 0x1F7A, 0x1F7B, 0x1FE5, 0x1FED, 0x1FEE, 0x1FEF, 0x0000, 0x0000,
        0x1FF2, 0x1FF3, 0x1FF4, 0x0000, 0x1FF6, 0x1FF7, 0x1F78, 0x1F79, 0x1F7C, 0x1F7D, 0x1FF3,
        0x1FFD, 0x1FFE, 0x0000,
    ];

    static GREEK_EXT_UPPER_MAPPING: [u16; 256] = [
        0x1F08, 0x1F09, 0x1F0A, 0x1F0B, 0x1F0C, 0x1F0D, 0x1F0E, 0x1F0F, 0x1F08, 0x1F09, 0x1F0A,
        0x1F0B, 0x1F0C, 0x1F0D, 0x1F0E, 0x1F0F, 0x1F18, 0x1F19, 0x1F1A, 0x1F1B, 0x1F1C, 0x1F1D,
        0x0000, 0x0000, 0x1F18, 0x1F19, 0x1F1A, 0x1F1B, 0x1F1C, 0x1F1D, 0x0000, 0x0000, 0x1F28,
        0x1F29, 0x1F2A, 0x1F2B, 0x1F2C, 0x1F2D, 0x1F2E, 0x1F2F, 0x1F28, 0x1F29, 0x1F2A, 0x1F2B,
        0x1F2C, 0x1F2D, 0x1F2E, 0x1F2F, 0x1F38, 0x1F39, 0x1F3A, 0x1F3B, 0x1F3C, 0x1F3D, 0x1F3E,
        0x1F3F, 0x1F38, 0x1F39, 0x1F3A, 0x1F3B, 0x1F3C, 0x1F3D, 0x1F3E, 0x1F3F, 0x1F48, 0x1F49,
        0x1F4A, 0x1F4B, 0x1F4C, 0x1F4D, 0x0000, 0x0000, 0x1F48, 0x1F49, 0x1F4A, 0x1F4B, 0x1F4C,
        0x1F4D, 0x0000, 0x0000, 0x1F50, 0x1F59, 0x1F52, 0x1F5B, 0x1F54, 0x1F5D, 0x1F56, 0x1F5F,
        0x0000, 0x1F59, 0x0000, 0x1F5B, 0x0000, 0x1F5D, 0x0000, 0x1F5F, 0x1F68, 0x1F69, 0x1F6A,
        0x1F6B, 0x1F6C, 0x1F6D, 0x1F6E, 0x1F6F, 0x1F68, 0x1F69, 0x1F6A, 0x1F6B, 0x1F6C, 0x1F6D,
        0x1F6E, 0x1F6F, 0x1FBA, 0x1FBB, 0x1FC8, 0x1FC9, 0x1FCA, 0x1FCB, 0x1FDA, 0x1FDB, 0x1FF8,
        0x1FF9, 0x1FEA, 0x1FEB, 0x1FFA, 0x1FFB, 0x0000, 0x0000, 0x1F88, 0x1F89, 0x1F8A, 0x1F8B,
        0x1F8C, 0x1F8D, 0x1F8E, 0x1F8F, 0x1F88, 0x1F89, 0x1F8A, 0x1F8B, 0x1F8C, 0x1F8D, 0x1F8E,
        0x1F8F, 0x1F98, 0x1F99, 0x1F9A, 0x1F9B, 0x1F9C, 0x1F9D, 0x1F9E, 0x1F9F, 0x1F98, 0x1F99,
        0x1F9A, 0x1F9B, 0x1F9C, 0x1F9D, 0x1F9E, 0x1F9F, 0x1FA8, 0x1FA9, 0x1FAA, 0x1FAB, 0x1FAC,
        0x1FAD, 0x1FAE, 0x1FAF, 0x1FA8, 0x1FA9, 0x1FAA, 0x1FAB, 0x1FAC, 0x1FAD, 0x1FAE, 0x1FAF,
        0x1FB8, 0x1FB9, 0x1FB2, 0x1FBC, 0x1FB4, 0x0000, 0x1FB6, 0x1FB7, 0x1FB8, 0x1FB9, 0x1FBA,
        0x1FBB, 0x1FBC, 0x1FBD, 0x0399, 0x1FBF, 0x1FC0, 0x1FC1, 0x1FC2, 0x1FCC, 0x1FC4, 0x0000,
        0x1FC6, 0x1FC7, 0x1FC8, 0x1FC9, 0x1FCA, 0x1FCB, 0x1FCC, 0x1FCD, 0x1FCE, 0x1FCF, 0x1FD8,
        0x1FD9, 0x1FD2, 0x1FD3, 0x0000, 0x0000, 0x1FD6, 0x1FD7, 0x1FD8, 0x1FD9, 0x1FDA, 0x1FDB,
        0x0000, 0x1FDD, 0x1FDE, 0x1FDF, 0x1FE8, 0x1FE9, 0x1FE2, 0x1FE3, 0x1FE4, 0x1FEC, 0x1FE6,
        0x1FE7, 0x1FE8, 0x1FE9, 0x1FEA, 0x1FEB, 0x1FEC, 0x1FED, 0x1FEE, 0x1FEF, 0x0000, 0x0000,
        0x1FF2, 0x1FFC, 0x1FF4, 0x0000, 0x1FF6, 0x1FF7, 0x1FF8, 0x1FF9, 0x1FFA, 0x1FFB, 0x1FFC,
        0x1FFD, 0x1FFE, 0x0000,
    ];

    let mut lower = code;
    let mut upper = code;

    // Basic Latin and Latin-1 supplement,
    // U+0000 to U+00FF
    if code <= 0x00ff {
        if (0x0041..=0x005a).contains(&code) {
            lower += 0x20;
        } else if (0x0061..=0x007a).contains(&code) {
            upper -= 0x20;
        } else if (0x00c0..=0x00d6).contains(&code) || (0x00d8..=0x00de).contains(&code) {
            lower += 0x20;
        } else if (0x00e0..=0x00f6).contains(&code) || (0x00f8..=0x00fe).contains(&code) {
            upper -= 0x20;
        } else if code == 0x00ff {
            upper = 0x0178;
        } else if code == 0x00b5 {
            //micro sign
            upper = 0x039c;
        }
        return ConvertCase { lower, upper };
    }
    /* Latin Extended-A, U+0100 to U+017F */
    if (0x0100..=0x017f).contains(&code) {
        if (0x0100..=0x012f).contains(&code)
            || (0x0132..=0x0137).contains(&code)
            || (0x014a..=0x0177).contains(&code)
        {
            upper = code & !1;
            lower = code | 1;
        } else if (0x0139..=0x0148).contains(&code) || (0x0179..=0x017e).contains(&code) {
            if (code & 1) != 0 {
                lower += 1;
            } else {
                upper -= 1;
            }
        } else if code == 0x0130 {
            lower = 0x0069;
        } else if code == 0x0131 {
            upper = 0x0049;
        } else if code == 0x0178 {
            lower = 0x00ff;
        } else if code == 0x017f {
            upper = 0x0053;
        }

        return ConvertCase { lower, upper };
    }

    /* Latin Extended-B, U+0180 to U+024F */
    if (0x0180..=0x024f).contains(&code) {
        if (0x01cd..=0x01dc).contains(&code) {
            if (code & 1) != 0 {
                lower += 1;
            } else {
                upper -= 1;
            }
        } else if (0x01de..=0x01ef).contains(&code)
            || (0x01f4..=0x01f5).contains(&code)
            || (0x01f8..=0x021f).contains(&code)
            || (0x0222..=0x0233).contains(&code)
        {
            lower |= 1;
            upper &= !1;
        } else if (0x0180..=0x01cc).contains(&code) {
            lower = LATIN_EXT_B_LOWER_MAPPING[code as usize - 0x0180].into();
            upper = LATIN_EXT_B_UPPER_MAPPING[code as usize - 0x0180].into();
        } else if code == 0x01dd {
            upper = 0x018e;
        } else if code == 0x01f1 || code == 0x01f2 {
            lower = 0x01f3;
            upper = 0x01f1;
        } else if code == 0x01f3 {
            upper = 0x01f1;
        } else if code == 0x01f6 {
            lower = 0x0195;
        } else if code == 0x01f7 {
            lower = 0x01bf;
        } else if code == 0x0220 {
            lower = 0x019e;
        }

        return ConvertCase { lower, upper };
    }

    /* IPA Extensions, U+0250 to U+02AF */
    if (0x0253..=0x0292).contains(&code) {
        upper = IPA_EXT_UPPER_MAPPING[code as usize - 0x0253].into();
    }

    /* Combining Diacritical Marks, U+0300 to U+036F */
    if code == 0x0345 {
        upper = 0x0399;
    }

    /* Greek and Coptic, U+0370 to U+03FF */
    if (0x0370..=0x03ff).contains(&code) {
        lower = GREEK_LOWER_MAPPING[code as usize - 0x0370].into();
        upper = GREEK_UPPER_MAPPING[code as usize - 0x0370].into();
        if upper == 0 {
            upper = code;
        }
        if lower == 0 {
            lower = code;
        }
    }

    /* Cyrillic and Cyrillic Supplementary, U+0400 to U+052F */
    if (0x0400..=0x04ff).contains(&code) || (0x0500..=0x052f).contains(&code) {
        if (0x0400..=0x040f).contains(&code) {
            lower += 0x50;
        } else if (0x0410..=0x042f).contains(&code) {
            lower += 0x20;
        } else if (0x0430..=0x044f).contains(&code) {
            upper -= 0x20;
        } else if (0x0450..=0x045f).contains(&code) {
            upper -= 0x50;
        } else if (0x0460..=0x0481).contains(&code)
            || (0x048a..=0x04bf).contains(&code)
            || (0x04d0..=0x04f5).contains(&code)
            || (0x04f8..=0x04f9).contains(&code)
            || (0x0500..=0x050f).contains(&code)
        {
            upper &= !1;
            lower |= 1;
        } else if (0x04c1..=0x04ce).contains(&code) {
            if (code & 1) != 0 {
                lower += 1;
            } else {
                upper -= 1;
            }
        }
    }

    /* Armenian, U+0530 to U+058F */
    if (0x0530..=0x058f).contains(&code) {
        if (0x0531..=0x0556).contains(&code) {
            lower += 0x30;
        } else if (0x0561..=0x0586).contains(&code) {
            upper -= 0x30;
        }
    }

    /* Latin Extended Additional, U+1E00 to U+1EFF */
    if (0x1e00..=0x1eff).contains(&code) {
        if (0x1e00..=0x1e95).contains(&code) || (0x1ea0..=0x1ef9).contains(&code) {
            upper &= !1;
            lower |= 1;
        } else if code == 0x1e9b {
            upper = 0x1e60;
        } else if code == 0x1e9e {
            lower = 0x00df; /* ssharp */
        }
    }

    /* Greek Extended, U+1F00 to U+1FFF */
    if (0x1f00..=0x1fff).contains(&code) {
        lower = GREEK_EXT_LOWER_MAPPING[code as usize - 0x1f00].into();
        upper = GREEK_EXT_UPPER_MAPPING[code as usize - 0x1f00].into();
        if upper == 0 {
            upper = code;
        }
        if lower == 0 {
            lower = code;
        }
    }

    /* Letterlike Symbols, U+2100 to U+214F */
    if (0x2100..=0x214f).contains(&code) {
        match code {
            0x2126 => lower = 0x03c9,
            0x212a => lower = 0x006b,
            0x212b => lower = 0x00e5,
            _ => {}
        }
    }
    /* Number Forms, U+2150 to U+218F */
    else if (0x2160..=0x216f).contains(&code) {
        lower += 0x10;
    } else if (0x2170..=0x217f).contains(&code) {
        upper -= 0x10;
    }
    /* Enclosed Alphanumerics, U+2460 to U+24FF */
    else if (0x24b6..=0x24cf).contains(&code) {
        lower += 0x1a;
    } else if (0x24d0..=0x24e9).contains(&code) {
        upper -= 0x1a;
    }
    /* Halfwidth and Fullwidth Forms, U+FF00 to U+FFEF */
    else if (0xff21..=0xff3a).contains(&code) {
        lower += 0x20;
    } else if (0xff41..=0xff5a).contains(&code) {
        upper -= 0x20;
    }
    /* Deseret, U+10400 to U+104FF */
    else if (0x10400..=0x10427).contains(&code) {
        lower += 0x28;
    } else if (0x10428..=0x1044f).contains(&code) {
        upper -= 0x28;
    }

    ConvertCase { lower, upper }
}

pub fn keysym_is_keypad(sym: &Keysym) -> bool {
    sym.is_keypad_key()
}

pub fn keysym_is_modifier(sym: &Keysym) -> bool {
    let sym = sym.raw();

    (sym >= Keysym::Shift_L.raw() && sym <= Keysym::Hyper_R.raw())
        // libX11 only goes up to Level5_Lock
        || (sym >= Keysym::ISO_Lock.raw() && sym <= Keysym::ISO_Level5_Lock.raw())
        || sym == Keysym::Mode_switch.raw()
        || sym == Keysym::Num_Lock.raw()
}

struct ConvertCase {
    upper: u32,
    lower: u32,
}

fn x_convert_case(sym: &Keysym) -> ConvertCase {
    let sym = *sym;
    let sym_raw: u32 = sym.into();
    // Latin 1 keysym (first part: fast path)
    if sym_raw < 0xb5 {
        return ucs_convert_case(sym_raw);
    }

    /* Unicode keysym */
    if (sym_raw & 0xff000000) == XKB_KEYSYM_UNICODE_OFFSET {
        let mut convert = ucs_convert_case(sym_raw & 0x00ffffff);
        convert.upper |= XKB_KEYSYM_UNICODE_OFFSET;
        convert.lower |= XKB_KEYSYM_UNICODE_OFFSET;
        return convert;
    }

    /* Legacy keysym */

    let mut lower = sym_raw;
    let mut upper = sym_raw;

    match sym_raw >> 8 {
        0 => {
            // Latin 1 (second part)
            if sym == Keysym::mu {
                upper = Keysym::Greek_MU.raw();
            } else if sym == Keysym::ydiaeresis {
                upper = Keysym::Ydiaeresis.raw();
            } else {
                let case = ucs_convert_case(sym_raw);
                lower = case.lower;
                upper = case.upper;
            }
        }
        1 => {
            /* Latin 2 */
            /* Assume the KeySym is a legal value (ignore discontinuities) */
            if sym == Keysym::Aogonek {
                lower = Keysym::aogonek.raw();
            } else if sym >= Keysym::Lstroke && sym <= Keysym::Sacute {
                lower += Keysym::lstroke.raw() - Keysym::Lstroke.raw();
            } else if sym >= Keysym::Scaron && sym <= Keysym::Zacute {
                lower += Keysym::scaron.raw() - Keysym::Scaron.raw();
            } else if sym >= Keysym::Zcaron && sym <= Keysym::Zabovedot {
                lower += Keysym::zcaron.raw() - Keysym::Zcaron.raw();
            } else if sym == Keysym::aogonek {
                upper = Keysym::Aogonek.raw();
            } else if sym >= Keysym::lstroke && sym <= Keysym::sacute {
                upper -= Keysym::lstroke.raw() - Keysym::Lstroke.raw();
            } else if sym >= Keysym::scaron && sym <= Keysym::zacute {
                upper -= Keysym::scaron.raw() - Keysym::Scaron.raw();
            } else if sym >= Keysym::zcaron && sym <= Keysym::zabovedot {
                upper -= Keysym::zcaron.raw() - Keysym::Zcaron.raw();
            } else if sym >= Keysym::Racute && sym <= Keysym::Tcedilla {
                lower += Keysym::racute.raw() - Keysym::Racute.raw();
            } else if sym >= Keysym::racute && sym <= Keysym::tcedilla {
                upper -= Keysym::racute.raw() - Keysym::Racute.raw();
            }
        }
        2 => {
            /* Latin 3 */
            /* Assume the KeySym is a legal value (ignore discontinuities) */
            if sym >= Keysym::Hstroke && sym <= Keysym::Hcircumflex {
                lower += Keysym::hstroke.raw() - Keysym::Hstroke.raw();
            } else if sym >= Keysym::Gbreve && sym <= Keysym::Jcircumflex {
                lower += Keysym::gbreve.raw() - Keysym::Gbreve.raw();
            } else if sym >= Keysym::hstroke && sym <= Keysym::hcircumflex {
                upper -= Keysym::hstroke.raw() - Keysym::Hstroke.raw();
            } else if sym >= Keysym::gbreve && sym <= Keysym::jcircumflex {
                upper -= Keysym::gbreve.raw() - Keysym::Gbreve.raw();
            } else if sym >= Keysym::Cabovedot && sym <= Keysym::Scircumflex {
                lower += Keysym::cabovedot.raw() - Keysym::Cabovedot.raw();
            } else if sym >= Keysym::cabovedot && sym <= Keysym::scircumflex {
                upper -= Keysym::cabovedot.raw() - Keysym::Cabovedot.raw();
            }
        }
        3 => {
            /* Latin 4 */
            /* Assume the KeySym is a legal value (ignore discontinuities) */
            if sym >= Keysym::Rcedilla && sym <= Keysym::Tslash {
                lower += Keysym::rcedilla.raw() - Keysym::Rcedilla.raw();
            } else if sym >= Keysym::rcedilla && sym <= Keysym::tslash {
                upper -= Keysym::rcedilla.raw() - Keysym::Rcedilla.raw();
            } else if sym == Keysym::ENG {
                lower = Keysym::eng.raw();
            } else if sym == Keysym::eng {
                upper = Keysym::ENG.raw();
            } else if sym >= Keysym::Amacron && sym <= Keysym::Umacron {
                lower += Keysym::amacron.raw() - Keysym::Amacron.raw();
            } else if sym >= Keysym::amacron && sym <= Keysym::umacron {
                upper -= Keysym::amacron.raw() - Keysym::Amacron.raw();
            }
        }
        6 => {
            /* Cyrillic */
            /* Assume the KeySym is a legal value (ignore discontinuities) */
            if sym >= Keysym::Serbian_DJE && sym <= Keysym::Cyrillic_DZHE {
                lower -= Keysym::Serbian_DJE.raw() - Keysym::Serbian_dje.raw();
            } else if sym >= Keysym::Serbian_dje && sym <= Keysym::Cyrillic_dzhe {
                upper += Keysym::Serbian_DJE.raw() - Keysym::Serbian_dje.raw();
            } else if sym >= Keysym::Cyrillic_YU && sym <= Keysym::Cyrillic_HARDSIGN {
                lower -= Keysym::Cyrillic_YU.raw() - Keysym::Cyrillic_yu.raw();
            } else if sym >= Keysym::Cyrillic_yu && sym <= Keysym::Cyrillic_hardsign {
                upper += Keysym::Cyrillic_YU.raw() - Keysym::Cyrillic_yu.raw();
            }
        }
        7 => {
            /* Greek */
            /* Assume the KeySym is a legal value (ignore discontinuities) */
            if sym >= Keysym::Greek_ALPHAaccent && sym <= Keysym::Greek_OMEGAaccent {
                lower += Keysym::Greek_alphaaccent.raw() - Keysym::Greek_ALPHAaccent.raw();
            } else if sym >= Keysym::Greek_alphaaccent
                && sym <= Keysym::Greek_omegaaccent
                && sym != Keysym::Greek_iotaaccentdieresis
                && sym != Keysym::Greek_upsilonaccentdieresis
            {
                upper -= Keysym::Greek_alphaaccent.raw() - Keysym::Greek_ALPHAaccent.raw();
            } else if sym >= Keysym::Greek_ALPHA && sym <= Keysym::Greek_OMEGA {
                lower += Keysym::Greek_alpha.raw() - Keysym::Greek_ALPHA.raw();
            } else if sym >= Keysym::Greek_alpha
                && sym <= Keysym::Greek_omega
                && sym != Keysym::Greek_finalsmallsigma
            {
                upper -= Keysym::Greek_alpha.raw() - Keysym::Greek_ALPHA.raw();
            }
        }
        0x13 => {
            /* Latin 9 */
            if sym == Keysym::OE {
                lower = Keysym::oe.raw();
            } else if sym == Keysym::oe {
                upper = Keysym::OE.raw();
            } else if sym == Keysym::Ydiaeresis {
                lower = Keysym::ydiaeresis.raw();
            }
        }
        _ => {}
    }

    ConvertCase { lower, upper }
}

pub fn keysym_is_lower(ks: &Keysym) -> bool {
    let convert = x_convert_case(ks);

    if convert.lower == convert.upper {
        return false;
    }

    ks.raw() == convert.lower
}

pub fn keysym_is_upper(ks: &Keysym) -> bool {
    let convert = x_convert_case(ks);

    if convert.lower == convert.upper {
        return false;
    }

    ks.raw() == convert.upper
}

pub fn keysym_to_lower(ks: &Keysym) -> Keysym {
    let convert = x_convert_case(ks);

    Keysym::from(convert.lower)
}

pub fn keysym_to_upper(ks: &Keysym) -> Keysym {
    let convert = x_convert_case(ks);

    Keysym::from(convert.upper)
}

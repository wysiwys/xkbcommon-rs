// changed: all hash map names entries in lowercase for easier lookup

use crate::atom::Atom;
use crate::context::Context;
use crate::keymap::{
    ActionControls, ActionType, MatchOperation, ModSet, MOD_REAL_MASK_ALL, XKB_MOD_NONE,
};
use crate::keysyms::*;
use crate::rust_xkbcommon::*;

pub(crate) fn lookup_key<'s, K, V, B>(
    tab: &'static phf::OrderedMap<B, V>,
    key: &'s str,
) -> Option<&'static V>
where
    K: Eq + Ord + phf::PhfHash + std::convert::From<&'s str>,
    B: phf_shared::PhfBorrow<K>,
{
    let key: K = key.into();
    tab.get(&key)
}

pub(crate) fn lookup_value<T, K, V>(
    tab: &phf::OrderedMap<K, T>,
    value: V,
    reverse: bool,
) -> Option<&'static str>
where
    T: PartialEq,
    K: Into<&'static str> + Copy,
    V: TryInto<T>,
{
    let value = match value.try_into() {
        Ok(value) => value,
        Err(_) => return None,
    };

    if reverse {
        for (entry_name, entry_value) in tab.entries().rev() {
            if *entry_value == value {
                return Some((*entry_name).into());
            }
        }
    } else {
        for (entry_name, entry_value) in tab.entries() {
            if *entry_value == value {
                return Some((*entry_name).into());
            }
        }
    }

    None
}

use unicase::UniCase;
pub(crate) static CTRL_MASK_NAMES: phf::OrderedMap<UniCase<&'static str>, ActionControls> = phf::phf_ordered_map! {
        UniCase::ascii("RepeatKeys") => ActionControls::Repeat,
        UniCase::ascii("Repeat") => ActionControls::Repeat,
        UniCase::ascii("AutoRepeat") => ActionControls::Repeat,
        UniCase::ascii("SlowKeys") => ActionControls::Slow,
        UniCase::ascii("BounceKeys") => ActionControls::Debounce,
        UniCase::ascii("StickyKeys") => ActionControls::Sticky,
        UniCase::ascii("MouseKeys") => ActionControls::Mousekeys,
        UniCase::ascii("MouseKeysAccel") => ActionControls::MousekeysAccel,
        UniCase::ascii("AccessXKeys") => ActionControls::Ax,
        UniCase::ascii("AccessXTimeout") => ActionControls::AxTimeout,
        UniCase::ascii("AccessXFeedback") => ActionControls::AxFeedback,
        UniCase::ascii("AudibleBell") => ActionControls::Bell,
        UniCase::ascii("IgnoreGroupLock") => ActionControls::IgnoreGroupLock,
        UniCase::ascii("all") => ActionControls::all(),
        UniCase::ascii("none") => ActionControls::empty(),
        UniCase::ascii("Overlay1") => ActionControls::empty(),
        UniCase::ascii("Overlay2") => ActionControls::empty()


};

pub(crate) static MOD_COMPONENT_MASK_NAMES: phf::OrderedMap<UniCase<&'static str>, StateComponent> = phf::phf_ordered_map! {

        UniCase::ascii("base") => StateComponent::MODS_DEPRESSED,
        UniCase::ascii("latched") => StateComponent::MODS_LATCHED,
        UniCase::ascii("locked") => StateComponent::MODS_LOCKED,
        UniCase::ascii("effective") => StateComponent::MODS_EFFECTIVE,
        UniCase::ascii("compat") => StateComponent::MODS_EFFECTIVE,
        UniCase::ascii("any") => StateComponent::MODS_EFFECTIVE,
        UniCase::ascii("none") => StateComponent::empty(),

};

pub(crate) static GROUP_COMPONENT_MASK_NAMES: phf::OrderedMap<
    UniCase<&'static str>,
    StateComponent,
> = phf::phf_ordered_map! {

        UniCase::ascii("base") => StateComponent::LAYOUT_DEPRESSED,
        UniCase::ascii("latched") => StateComponent::LAYOUT_LATCHED,
        UniCase::ascii("locked") => StateComponent::LAYOUT_LOCKED,
        UniCase::ascii("effective") => StateComponent::LAYOUT_EFFECTIVE,
        UniCase::ascii("any") => StateComponent::LAYOUT_EFFECTIVE,
        UniCase::ascii("none") => StateComponent::empty(),

};

pub(crate) static GROUP_MASK_NAMES: phf::OrderedMap<UniCase<&'static str>, u32> = phf::phf_ordered_map! {

        UniCase::ascii("Group1") => 0x01,
        UniCase::ascii("Group2") => 0x02,
        UniCase::ascii("Group3") => 0x04,
        UniCase::ascii("Group4") => 0x08,
        UniCase::ascii("Group5") => 0x10,
        UniCase::ascii("Group6") => 0x20,
        UniCase::ascii("Group7") => 0x40,
        UniCase::ascii("Group8") => 0x80,
        UniCase::ascii("none") => 0x00,
        UniCase::ascii("all") => 0xff,


};

pub(crate) static GROUP_NAMES: phf::OrderedMap<UniCase<&'static str>, u8> = phf::phf_ordered_map! {

        UniCase::ascii("Group1") => 1,
        UniCase::ascii("Group2") => 2,
        UniCase::ascii("Group3") => 3,
        UniCase::ascii("Group4") => 4,
        UniCase::ascii("Group5") => 5,
        UniCase::ascii("Group6") => 6,
        UniCase::ascii("Group7") => 7,
        UniCase::ascii("Group8") => 8,


};
pub(crate) static LEVEL_NAMES: phf::OrderedMap<UniCase<&'static str>, u32> = phf::phf_ordered_map! {

        UniCase::ascii("Level1") => 1,
        UniCase::ascii("Level2") => 2,
        UniCase::ascii("Level3") => 3,
        UniCase::ascii("Level4") => 4,
        UniCase::ascii("Level5") => 5,
        UniCase::ascii("Level6") => 6,
        UniCase::ascii("Level7") => 7,
        UniCase::ascii("Level8") => 8,
};
pub(crate) static BUTTON_NAMES: phf::OrderedMap<UniCase<&'static str>, u32> = phf::phf_ordered_map! {

        UniCase::ascii("Button1") => 1,
        UniCase::ascii("Button2") => 2,
        UniCase::ascii("Button3") => 3,
        UniCase::ascii("Button4") => 4,
        UniCase::ascii("Button5") => 5,
        UniCase::ascii("default") => 0,
};
pub(crate) static USE_MOD_MAP_VALUE_NAMES: phf::OrderedMap<UniCase<&'static str>, bool> = phf::phf_ordered_map! {

        UniCase::ascii("LevelOne") => true,
        UniCase::ascii("Level1") => true,
        UniCase::ascii("AnyLevel") => false,
        UniCase::ascii("any") => false,
};

pub(crate) static ACTION_TYPE_NAMES: phf::OrderedMap<UniCase<&'static str>, ActionType> = phf::phf_ordered_map! {

        UniCase::ascii("NoAction") => ActionType::None,
        UniCase::ascii("SetMods") => ActionType::ModSet,
        UniCase::ascii("LatchMods") => ActionType::ModLatch,
        UniCase::ascii("LockMods") => ActionType::ModLock,
        UniCase::ascii("SetGroup") => ActionType::GroupSet,
        UniCase::ascii("LatchGroup") => ActionType::GroupLatch,
        UniCase::ascii("LockGroup") => ActionType::GroupLock,
        UniCase::ascii("MovePtr") => ActionType::PtrMove,
        UniCase::ascii("MovePointer") => ActionType::PtrMove,
        UniCase::ascii("PtrBtn") => ActionType::PtrButton,
        UniCase::ascii("PointerButton") => ActionType::PtrButton,
        UniCase::ascii("LockPtrBtn") => ActionType::PtrLock,
        UniCase::ascii("LockPtrButton") => ActionType::PtrLock,
        UniCase::ascii("LockPointerButton") => ActionType::PtrLock,
        UniCase::ascii("LockPointerBtn") => ActionType::PtrLock,
        UniCase::ascii("SetPtrDflt") => ActionType::PtrDefault,
        UniCase::ascii("SetPointerDefault") => ActionType::PtrDefault,
        UniCase::ascii("Terminate") => ActionType::Terminate,
        UniCase::ascii("TerminateServer") => ActionType::Terminate,
        UniCase::ascii("SwitchScreen") => ActionType::SwitchVT,
        UniCase::ascii("SetControls") => ActionType::CtrlSet,
        UniCase::ascii("LockControls") => ActionType::CtrlLock,
        UniCase::ascii("Private") => ActionType::Private,

        // deprecated actions below - unused
        UniCase::ascii("RedirectKey") => ActionType::None,
        UniCase::ascii("Redirect") => ActionType::None,
        UniCase::ascii("ISOLock") => ActionType::None,
        UniCase::ascii("ActionMessage") => ActionType::None,
        UniCase::ascii("MessageAction") => ActionType::None,
        UniCase::ascii("Message") => ActionType::None,
        UniCase::ascii("DeviceBtn") => ActionType::None,
        UniCase::ascii("DevBtn" )=> ActionType::None,
        UniCase::ascii("DevButton") => ActionType::None,
        UniCase::ascii("LockDeviceBtn") => ActionType::None,
        UniCase::ascii("LockDevBtn") => ActionType::None,
        UniCase::ascii("LockDevButton") => ActionType::None,
        UniCase::ascii("LockDeviceButton") => ActionType::None,
        UniCase::ascii("DeviceValuator") => ActionType::None,
        UniCase::ascii("DevVal") => ActionType::None,
        UniCase::ascii("DeviceVal") => ActionType::None,
        UniCase::ascii("DevValuator") => ActionType::None,
};

pub(crate) static SYM_INTERPRET_MATCH_MASK_NAMES: phf::OrderedMap<
    UniCase<&'static str>,
    MatchOperation,
> = phf::phf_ordered_map! {

        UniCase::ascii("NoneOf") => MatchOperation::None,
        UniCase::ascii("AnyOfOrNone") => MatchOperation::AnyOrNone,
        UniCase::ascii("AnyOf") => MatchOperation::Any,
        UniCase::ascii("AllOf") => MatchOperation::All,
        UniCase::ascii("Exactly") => MatchOperation::Exactly,
};

impl Context {
    pub(crate) fn mod_index_text<'a>(&'a self, mods: &ModSet, ndx: ModIndex) -> &'a str {
        if ndx == XKB_MOD_INVALID {
            return "none";
        };

        if ndx == XKB_MOD_NONE {
            return "None";
        };

        let name = match mods.mods.get(ndx) {
            Some(m) => m.name,
            None => return "",
        };

        self.xkb_atom_text(name)
    }
}
impl ActionType {
    pub(crate) fn text(&self) -> String {
        let name = lookup_value(&ACTION_TYPE_NAMES, *self, false);

        name.unwrap_or("Private").into()
    }
}
impl Context {
    pub(crate) fn keysym_text(&self, sym: &Keysym) -> String {
        keysym_get_name(sym).unwrap_or("Invalid".into())
    }

    pub(crate) fn key_name_text(&self, name: Atom) -> String {
        format!("<{}>", self.xkb_atom_text(name))
    }

    pub(crate) fn si_match_text(&self, _type: &MatchOperation) -> String {
        lookup_value(&SYM_INTERPRET_MATCH_MASK_NAMES, _type.clone(), false)
            .unwrap_or("")
            .into()
    }
    pub(crate) fn mod_mask_text(&self, mods: &ModSet, mask: ModMask) -> String {
        const BUF_SIZE: usize = 1024;
        if mask == 0 {
            return "none".into();
        }

        if mask == MOD_REAL_MASK_ALL {
            return "all".into();
        }

        let text = mods
            .mods
            .iter()
            .enumerate()
            .filter(|(i, _mod)| (mask & (1 << i)) != 0)
            .map(|(_, _mod)| {
                let text = self.xkb_atom_text(_mod.name);

                text
            })
            .collect::<Vec<&str>>()
            .join("+");

        text[0..usize::min(BUF_SIZE, text.len())].into()
    }

    pub(crate) fn led_state_mask_text(&self, mask: StateComponent) -> String {
        const BUF_SIZE: usize = 1024;
        let mut string = String::with_capacity(BUF_SIZE);

        let mut pos: usize = 0;

        let mut mask: u16 = mask.bits();
        if mask == 0 {
            return "0".into();
        }

        for i in 0..16 {
            if (mask & (1 << i)) == 0 {
                continue;
            }

            mask &= !(1 << i);

            let s = format!(
                "{}{}",
                match pos {
                    0 => "",
                    _ => "+",
                },
                lookup_value(&MOD_COMPONENT_MASK_NAMES, 1 << i, false).unwrap_or("")
            );

            pos += s.len();

            string = string + &s;

            if pos >= BUF_SIZE {
                break;
            }
        }

        string[0..usize::min(BUF_SIZE, string.len())].into()
    }

    pub(crate) fn control_mask_text(&self, mask: ActionControls) -> String {
        const BUF_SIZE: usize = 1024;

        if mask.is_empty() {
            return "none".into();
        }

        if mask == ActionControls::all() {
            return "all".into();
        }

        let mut pos: usize = 0;
        let mut mask: u16 = mask.bits();
        let mut string = String::with_capacity(BUF_SIZE);

        for i in 0..16 {
            if (mask & (1 << i)) == 0 {
                continue;
            }

            mask &= !(1 << i);

            let s = format!(
                "{}{}",
                match pos {
                    0 => "",
                    _ => "+",
                },
                lookup_value(&CTRL_MASK_NAMES, 1 << i, false).unwrap_or("")
            );

            pos += s.len();

            string = string + &s;

            if pos >= BUF_SIZE {
                break;
            }
        }

        string[0..usize::min(BUF_SIZE, string.len())].into()
    }
}

// This file is based on text.c and text.h
/*
 * Copyright © 2009 Dan Nicholson
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
 */

/************************************************************
 * Copyright (c) 1994 by Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies and that both that copyright
 * notice and this permission notice appear in supporting
 * documentation, and that the name of Silicon Graphics not be
 * used in advertising or publicity pertaining to distribution
 * of the software without specific prior written permission.
 * Silicon Graphics makes no representation about the suitability
 * of this software for any purpose. It is provided "as is"
 * without any express or implied warranty.
 *
 * SILICON GRAPHICS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SILICON
 * GRAPHICS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
 * THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 ********************************************************/

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
    tab.get(&key.into())
}

// TODO: how efficient is this/does this need to be?
// TODO: is the reverse direction still needed?
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
    let value = value.try_into().ok()?;
    match reverse {
        true => tab
            .entries()
            .rev()
            .find(|(_, entry_value)| **entry_value == value),
        false => tab
            .entries()
            .find(|(_, entry_value)| **entry_value == value),
    }
    .map(|(name, _)| (*name).into())
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

        mods.mods
            .get(ndx)
            .map(|_mod| self.xkb_atom_text(_mod.name))
            .unwrap_or("")
    }
}
impl ActionType {
    pub(crate) fn text(&self) -> &'static str {
        lookup_value(&ACTION_TYPE_NAMES, *self, false).unwrap_or("Private")
    }
}
impl Context {
    pub(crate) fn keysym_text(&self, sym: &Keysym) -> String {
        keysym_get_name(sym).unwrap_or("Invalid".into())
    }

    pub(crate) fn key_name_text(&self, name: Atom) -> String {
        format!("<{}>", self.xkb_atom_text(name))
    }

    pub(crate) fn si_match_text(&self, _type: &MatchOperation) -> &str {
        lookup_value(&SYM_INTERPRET_MATCH_MASK_NAMES, _type.clone(), false).unwrap_or("")
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
            .map(|(_, _mod)| self.xkb_atom_text(_mod.name))
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

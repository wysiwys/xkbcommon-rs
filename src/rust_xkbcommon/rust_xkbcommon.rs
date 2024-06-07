//based on xkbcommon.h
/*
 * Copyright 1985, 1987, 1990, 1998  The Open Group
 * Copyright 2008  Dan Nicholson
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the names of the authors or their
 * institutions shall not be used in advertising or otherwise to promote the
 * sale, use or other dealings in this Software without prior written
 * authorization from the authors.
 */

/************************************************************
 * Copyright (c) 1993 by Silicon Graphics Computer Systems, Inc.
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

/*
 * Copyright © 2009-2012 Daniel Stone
 * Copyright © 2012 Intel Corporation
 * Copyright © 2012 Ran Benita
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

pub(crate) use crate::keycode::Keycode;

pub(crate) use xkeysym::Keysym;

/// A number used to represent a physical key on a keyboard.
///
/// A standard PC-compatible keyboard might have 102 keys.
/// An appropriate keymap would assign each of them a keycode,
/// by which the user should refer to the key throughout
/// the library.
///
/// Historically, the X11 protocol, and consequently the
/// XKB protocol, assigned only 8 bits for keycodes.
/// This limits the number of different keys that can be
/// used simultaneously in a single keymap to 256
/// (disregarding other limitations). This library does not
/// share this limit.
///
/// Corresponds to `xkb_keycode_t`
pub type RawKeycode = u32;

impl From<Keycode> for u32 {
    fn from(val: Keycode) -> Self {
        val.0
    }
}

impl From<u32> for Keycode {
    fn from(raw: u32) -> Self {
        Self(raw)
    }
}
impl Keycode {
    pub fn new(raw: u32) -> Self {
        Self(raw)
    }

    pub fn raw(&self) -> u32 {
        self.0
    }
}

/// Corresponds to `xkb_layout_index_t`
pub type LayoutIndex = usize;

/// Corresponds to `xkb_layout_mask_t`
pub type LayoutMask = u32;

/// Corresponds to `xkb_level_index_t`
pub type LevelIndex = usize;

/// Corresponds to `xkb_mod_index_t`
pub type ModIndex = usize;

/// Corresponds to `xkb_mod_mask_t`
pub type ModMask = u32;

/// Corresponds to `xkb_led_index_t`
pub type LedIndex = usize;

/// Corresponds to `xkb_led_mask_t`
pub type LedMask = u32;

pub(crate) const XKB_KEYCODE_INVALID: RawKeycode = 0xffffffff;
//pub(crate) const XKB_LAYOUT_INVALID: LayoutIndex = 0xffffffff;
//pub(crate) const XKB_LEVEL_INVALID: LevelIndex = 0xffffffff;
pub(crate) const XKB_MOD_INVALID: ModIndex = 0xffffffff;
//pub(crate) const XKB_LED_INVALID: LedIndex = 0xffffffff;

pub const XKB_KEYCODE_MAX: RawKeycode = 0xffffffff - 1;
pub const XKB_KEYSYM_MAX: u32 = 0x1fffffff;

impl Keycode {
    pub fn is_legal_ext(&self) -> bool {
        self.0 <= XKB_KEYCODE_MAX
    }

    pub fn is_legal_x11(&self) -> bool {
        self.0 >= 8 && self.0 <= 255
    }
}

#[derive(Clone, Debug)]
pub struct RuleNames
// xkb_rule_names
{
    /// The rules file to use. The rules file describes how to interpret the values of the model,
    /// layout, variant, and options fields.
    ///
    /// If `None` or the empty string `""`, a default value is used.
    /// If the `XKB_DEFAULT_RULES` environment variable is set, it is used as the default.
    /// Otherwise, the system default is used.
    ///
    pub rules: Option<String>,

    /// The keyboard model by which to interpret keycodes and LEDs.
    ///
    /// If `None` or the empty string `""`, a default value is used.
    /// If the `XKB_DEFAULT_MODEL` environment variable is set, it is used as the default.
    /// Otherwise the system default is used.
    ///
    ///
    pub model: Option<String>,

    /// A comma-separated list of layouts (languages) to include in the keymap.
    ///
    /// If `None` or the empty string `""`, a default value is used.
    /// If the `XKB_DEFAULT_LAYOUT` environment variable is set, it is used as the default.
    /// Otherwise, the system default is used.
    pub layout: Option<String>,

    /// A comma-separated list of variants, one per layout, which may modify or augment the
    /// respective layout in various ways.
    ///
    /// Generally, this should either be empty or have the same number of values as the number of
    /// layouts. You may use empty values as in `"intl,,neo"`.
    ///
    /// If `None` or the empty string `""`, and a default value is also used for the layout, a
    /// default value is used. Otherwise, no variant is used.
    ///
    /// If the `XKB_DEFAULT_VARIANT` environment variable is set, it is used as the default.
    /// Otherwise, the system default is used.
    pub variant: Option<String>,

    /// A comma-separated list of options, through which the user specifies non-layout related
    /// preferences, like which key combinations are used for switching layouts, or which key is
    /// the Compose key.
    ///
    /// If the `XKB_DEFAULT_OPTIONS` environment variable is set, it is used as the default.
    /// Otherwise, the system default is used.
    pub options: Option<String>,
}

impl RuleNames {
    pub fn new(rules: &str, model: &str, layout: &str, variant: &str, options: &str) -> Self {
        Self {
            rules: match rules {
                "" => None,
                s => Some(s.into()),
            },
            model: match model {
                "" => None,
                s => Some(s.into()),
            },
            layout: match layout {
                "" => None,
                s => Some(s.into()),
            },
            variant: match variant {
                "" => None,
                s => Some(s.into()),
            },
            options: match options {
                "" => None,
                s => Some(s.into()),
            },
        }
    }

    pub(crate) fn empty() -> Self {
        Self {
            rules: None,
            model: None,
            layout: None,
            variant: None,
            options: None,
        }
    }
}

bitflags::bitflags! {
    pub struct KeysymFlags: u8 {
        /// Do not apply any flags
        const NO_FLAGS = 0;
        /// Find keysym by case-insensitive search
        const CASE_INSENSITIVE = (1 << 0);
    }
}

impl TryFrom<u8> for KeysymFlags {
    type Error = &'static str;

    fn try_from(u: u8) -> Result<Self, Self::Error> {
        match u {
            0 => Ok(KeysymFlags::NO_FLAGS),
            1 => Ok(KeysymFlags::CASE_INSENSITIVE),
            _ => Err("no such flags"),
        }
    }
}

bitflags::bitflags! {
    /// Flags for context creation.
pub struct ContextFlags: u32 {
    /// Do not apply any context flags.
    const NO_FLAGS = 0;

    /// Create this context with an empty include path.
    const NO_DEFAULT_INCLUDES = (1 << 0);

    /// Don't take RMLVO names from the environment.
    const NO_ENVIRONMENT_NAMES = (1 << 1);
    /// Disable the use of secure_getenv for this context,
    /// so that privileged processes can use environment variables.
    /// Client uses at their own risk.
    /// TODO: Not implemented
    const NO_SECURE_GETENV = (1 << 2);
    }
}

impl From<u32> for ContextFlags {
    fn from(bits: u32) -> Self {
        Self::from_bits_truncate(bits)
    }
}

/*
pub enum LogLevel {
    CRITICAL = 10,
    ERROR = 20,
    WARNING = 30,
    INFO = 40,
    DEBUG = 50
}
*/

bitflags::bitflags! {
    #[derive(Clone, Debug)]
    /// Flags for keymap compilation.
    pub struct CompileFlags: u32 {
        const NO_FLAGS = 0;
    }
}

impl From<CompileFlags> for u32 {
    fn from(val: CompileFlags) -> Self {
        val.bits()
    }
}

impl TryFrom<u32> for CompileFlags {
    type Error = ();

    fn try_from(u: u32) -> Result<Self, Self::Error> {
        CompileFlags::from_bits(u).ok_or(())
    }
}

/// The possible keymap formats. Currently, only `TextV1` is supported.
#[repr(u32)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum KeymapFormat {
    /// The current/classic XKB format, as generated by `xkbcomp -xkb`.
    TextV1 = 1,
    OriginalFormat = 0xffff_ffff,
}

impl From<KeymapFormat> for u32 {
    fn from(val: KeymapFormat) -> Self {
        val as u32
    }
}

impl TryFrom<u32> for KeymapFormat {
    type Error = &'static str;

    fn try_from(u: u32) -> Result<Self, Self::Error> {
        if u == 1 {
            Ok(KeymapFormat::TextV1)
        } else if u == 0xffff_ffff {
            Ok(KeymapFormat::OriginalFormat)
        } else {
            Err("Invalid keymap format")
        }
    }
}

pub(crate) trait KeymapFormatType: Into<KeymapFormat> {}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct TextV1;
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct OriginalFormat;

impl From<TextV1> for KeymapFormat {
    fn from(_: TextV1) -> Self {
        KeymapFormat::TextV1
    }
}
impl KeymapFormatType for TextV1 {}
impl From<OriginalFormat> for KeymapFormat {
    fn from(_: OriginalFormat) -> Self {
        KeymapFormat::OriginalFormat
    }
}
impl KeymapFormatType for OriginalFormat {}

/// Specifies the direction of key (press/release)
#[derive(PartialEq, Clone, Copy, Debug)]
pub enum KeyDirection {
    /// The key was released.
    Up,
    /// The key was pressed.
    Down,
}

bitflags::bitflags! {
    #[derive(Copy,Clone, Eq, PartialEq, Debug)]
/// Modifier and layout types for state objects.
///
/// This enum is bitmaskable. E.g.
/// `StateComponent::MODS_DEPRESSED | StateComponent::MODS_LATCHED` is valid to exclude locked
/// modifiers.
///
/// In XKB, the `DEPRESSED` components are also known as 'base'.
    pub struct StateComponent: u16 {
        /// Depressed modifiers, i.e. a key is physically holding them.
        const MODS_DEPRESSED = (1 << 0);
        /// Latched modifiers, i.e. will be unset after the next non-modifier key press.
        const MODS_LATCHED = (1 << 1);
        /// Locked modifiers, i.e. will be unset after the key provoking the lock has been pressed
        /// again.
        const MODS_LOCKED = (1 << 2);
    /// Effective modifiers, i.e. currently active and affect key
    /// processing (derived from the other state components).
    /// Use this unless you explicitly care how the state came about.
        const MODS_EFFECTIVE = (1 << 3);
    /** Depressed layout, i.e. a key is physically holding it. */
        const LAYOUT_DEPRESSED = (1 << 4);
    /** Latched layout, i.e. will be unset after the next non-modifier key press. */
        const LAYOUT_LATCHED = (1 << 5);
    /** Locked layout, i.e. will be unset after the key provoking the lock
     *  has been pressed again. */
        const LAYOUT_LOCKED = (1 << 6);
    /** Effective layout, i.e. currently active and affects key processing
     *  (derived from the other state components).
     *  Use this unless you explicitly care how the state came about. */
        const LAYOUT_EFFECTIVE = (1 << 7);
    /** LEDs (derived from the other state components). */
        const LEDS = (1 << 8);
    }
}

impl From<StateComponent> for i64 {
    fn from(val: StateComponent) -> Self {
        val.bits() as i64
    }
}

impl TryFrom<i64> for StateComponent {
    type Error = &'static str;

    fn try_from(i: i64) -> Result<Self, Self::Error> {
        let u = match i.try_into() {
            Err(_) => return Err("Could not convert from i64"),
            Ok(u) => u,
        };
        match Self::from_bits(u) {
            None => Err("Could not convert to StateComponent"),
            Some(a) => Ok(a),
        }
    }
}

bitflags::bitflags! {
    #[derive(Copy,Clone)]
pub struct StateMatch: u32 {
    /// Returns true if any of the modifiers are active
    const ANY = (1 << 0);
    /// Returns true if all of the modifiers are active
    const ALL = (1 << 1);
    /// Makes matching non-exclusive, i.e. will not return false if a modifier not specified in the
    /// arguments is active.
    const NON_EXCLUSIVE = (1 << 16);
}}

pub enum ConsumedMode {
    /// This is the mode defined in the XKB specification and used by libX11.
    ///
    /// A modifier is consumed if and only if it *may affect* key translation.
    ///
    /// For example, if `Control+Alt+<Backspace>` produces some assigned keysym,
    /// then, when pressing just `<Backspace>`, `Control` and `Alt` are consumed, even though they
    /// are not active, since if they had been active they would have affected key translation.
    Xkb,

    /// This is the mode used by the GTK+ toolkit.
    ///
    ///
    Gtk,
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_keysym_0() {
        assert_eq!(Keysym::from(0), xkeysym::NO_SYMBOL);
    }
}

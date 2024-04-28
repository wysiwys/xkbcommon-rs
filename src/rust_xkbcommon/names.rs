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

// TODO: for both of these, implement deref as str?
pub struct ModName(pub &'static str);

impl AsRef<str> for ModName {
    fn as_ref(&self) -> &str {
        self.0
    }
}

impl AsRef<str> for LedName {
    fn as_ref(&self) -> &str {
        self.0
    }
}

pub struct LedName(pub &'static str);

impl ModName {
    pub const SHIFT: ModName = ModName("Shift");
    pub const CAPS: ModName = ModName("Lock");
    pub const CTRL: ModName = ModName("Control");
    pub const ALT: ModName = ModName("Mod1");
    pub const NUM: ModName = ModName("Mod2");
    pub const LOGO: ModName = ModName("Mod4");

    pub fn name(&self) -> &'static str {
        self.0
    }
}

impl LedName {
    pub const CAPS: LedName = LedName("Caps Lock");
    pub const NUM: LedName = LedName("Num Lock");
    pub const SCROLL: LedName = LedName("Scroll Lock");

    pub fn name(&self) -> &'static str {
        self.0
    }
}

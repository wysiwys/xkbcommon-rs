use xkeysym::Keysym;

/*
 * Copyright © 2012 Intel Corporation
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
 * Author: Rob Bradford <rob@linux.intel.com>
 */

pub fn keysym_to_utf8(keysym: &Keysym) -> Option<Vec<u8>> {
    let mut buf = [0; 4];

    let c = keysym.key_char()?;

    // TODO: is this correct?
    if c == '\0' {
        return None;
    }

    let len = c.encode_utf8(&mut buf).len();

    Some(buf[0..len].into())
}

pub fn keysym_to_utf32(keysym: &Keysym) -> Option<u32> {
    if let Some(c) = keysym.key_char() {
        return Some(c as u32);
        /*
        } else if (0x0100d800..=0x0100dfff).contains(&keysym.raw()) {
            return None;
        } else if (XKB_KEYSYM_UNICODE_OFFSET..=XKB_KEYSYM_UNICODE_MAX).contains(&keysym.raw()) {

            return Some(keysym.raw() - XKB_KEYSYM_UNICODE_OFFSET);
            */
    }

    None
}

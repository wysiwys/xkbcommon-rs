//based on part of scanner-utils.h
/*
 * Copyright © 2012 Ran Benita <ran234@gmail.com>
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

pub(crate) fn check_supported_char_encoding(s: &str) -> Result<&str, ()> {
    if s.len() < 2 {
        return Ok(s);
    }

    let bom = "\u{feff}";
    if let Some(s) = s.strip_prefix(bom) {
        return Ok(s);
    }

    // early detection of wrong file encoding, e.g. UTF-16 or UTF-32

    let mut c = s.chars();
    let first_char = c.next().unwrap();
    let second_char = c.next().unwrap();

    // TODO: is this necessary?
    if first_char == '\0' || second_char == '\0' {
        // TODO: handle case where first char not '\0'
        log::error!("Unexpected null character");
        return Err(());
    }

    // enforce the first character to be ASCII
    if !first_char.is_ascii() {
        log::error!("Unexpected non-ASCII character.");
        return Err(());
    }

    Ok(s)
}

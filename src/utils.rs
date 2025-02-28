/*
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
 */

use std::fs::*;

use nix::unistd::AccessFlags;

/// Placeholders for likely, unlikely
/// See implementation in `hashbrown`
pub(crate) use core::convert::identity as likely;
pub(crate) use core::convert::identity as unlikely;

/// If we can, check that the permission bits of the file permit the requested access.
#[cfg(target_os = "linux")]
pub(super) fn check_permissions(meta: &Metadata, requested_mode: AccessFlags) -> bool {
    use std::os::unix::fs::PermissionsExt;

    // Use the Unix-specifix extensions to `fs::Permissions`,
    // since the regular permissions only have a read-only bit set.

    let permissions = meta.permissions();
    let actual_mode = permissions
        .mode()
        .try_into()
        .expect("Mode requested is negative");

    // truncate: leave any unknown bits unset
    let actual_mode = AccessFlags::from_bits_truncate(actual_mode);

    actual_mode.intersects(requested_mode)
}

#[cfg(not(target_os = "linux"))]
pub(super) fn check_permissions(meta: &Metadata, requested_mode: AccessFlags) -> bool {
    todo!()
}

// `one_bit_set`: original license from utils.h:
/*
 * Copyright © 2012 Ran Benita <ran234@gmail.com>
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
pub(super) fn one_bit_set(x: u32) -> bool {
    x > 0 && (x & (x - 1)) == 0
}

// TODO: `open_file`

// based on vmod.c
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
use super::ast::*;

use crate::context::Context;
use crate::errors::*;
use crate::keymap::{Mod, ModSet, ModType, XKB_MAX_MODS};

impl ModSet {
    pub(super) fn handle_vmod_def(
        &mut self,
        ctx: &Context,
        stmt: VModDef,
        mut merge: MergeMode,
    ) -> Result<(), HandleVModError> {
        let mut mods = self.clone();

        let stmt_value_is_none = stmt.value.is_none();

        let mut mapping = match stmt.value {
            None => 0,
            Some(value) =>
            // This is a statement such as 'virtualModifiers NumLock = Mod1';
            // it sets the vmod-to-real-mod[s] mapping directly instead of going through
            // modifier_map or some such.
            {
                value
                    .resolve_mod_mask(ctx, ModType::REAL, self)
                    .ok_or_else(|| {
                        log::error!(
                            "{:?}: Declaration of {:?} ignored",
                            XkbMessageCode::NoId,
                            ctx.xkb_atom_text(stmt.name)
                        );
                        HandleVModError::CouldNotResolveModMask
                    })?
            }
        };

        if merge == MergeMode::Default {
            merge = stmt.merge;
        }

        for _mod in self.mods.iter_mut() {
            if _mod.name == stmt.name {
                if _mod.mod_type != ModType::VIRT {
                    let name = ctx.atom_text(_mod.name);
                    log::error!("{:?}: Can't add a virtual modifier named \"{:?}\"; there is already a non-virtual modifier with this name! Ignored",
                        XkbMessageCode::NoId,
                        name
                    );

                    return Err(HandleVModError::ExistingRealModHasName(
                        name.expect("Mod has no name").into(),
                    ));
                }
                if _mod.mapping == mapping || stmt_value_is_none {
                    /*
                     * Same definition or no new explicit mapping: do nothing.
                     * Note that we must test the statement value and not the mapping
                     * in order to allow resetting it: e.g. `VMod=none`.
                     */
                    return Ok(());
                }
                let vmod = 0;
                if (mods.explicit_vmods & vmod) > 0 {
                    // Handle conflicting mappings
                    assert_ne!(_mod.mapping, 0);
                    let clobber = merge != MergeMode::Augment;
                    let (_use, ignore) = if clobber {
                        (mapping, _mod.mapping)
                    } else {
                        (_mod.mapping, mapping)
                    };

                    log::warn!("{:?}: Virtual modifier {:?} defined multiple times; Using {:?}, ignoring {:?}",
                        XkbMessageCode::NoId,
                        ctx.xkb_atom_text(stmt.name),
                        ctx.mod_mask_text(&mods, _use),
                        ctx.mod_mask_text(&mods, ignore)
                    );

                    mapping = _use;
                }

                _mod.mapping = mapping;
                if mapping > 0 {
                    mods.explicit_vmods |= vmod;
                } else {
                    mods.explicit_vmods &= !vmod;
                }
                return Ok(());
            }
        }

        if self.mods.len() >= XKB_MAX_MODS {
            let err = XkbMessageCode::NoId;
            log::error!(
                "{:?}:: Too many modifiers defined (maximum {})",
                err,
                XKB_MAX_MODS
            );
            return Err(HandleVModError::TooManyModifiersDefined);
        }

        self.mods.push(Mod {
            name: stmt.name,
            mod_type: ModType::VIRT,
            mapping,
        });

        if mapping > 0 {
            mods.explicit_vmods |= 1 << mods.mods.len();
        }

        Ok(())
    }
}

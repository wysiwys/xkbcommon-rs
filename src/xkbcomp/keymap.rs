// based on xkbcomp/keymap.c
/*
 * Copyright © 2009 Dan Nicholson
 * Copyright © 2012 Intel Corporation
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
 *
 * Author: Dan Nicholson <dbn.lists@gmail.com>
 *         Daniel Stone <daniel@fooishbar.org>
 *         Ran Benita <ran234@gmail.com>
 *         wysyiwys
 */

use super::ast::*;

use crate::errors::*;
use crate::keymap::*;
use crate::rust_xkbcommon::*;

impl KeymapBuilder<TextV1> {
    // from keycodes.c
    fn compile_keycodes(
        &mut self,
        file: XkbFile,
        merge: MergeMode,
    ) -> Result<(), CompileKeycodesError> {
        super::keycodes::compile_keycodes(self, file, merge)
    }

    fn compile_keytypes(
        &mut self,
        file: XkbFile,
        merge: MergeMode,
    ) -> Result<(), CompileTypesError> {
        super::types::compile_keytypes(self, file, merge)
    }

    fn compile_compat_map(
        &mut self,
        file: XkbFile,
        merge: MergeMode,
    ) -> Result<(), CompileCompatError> {
        super::compat::compile_compat(self, file, merge)
    }

    fn compile_symbols(
        &mut self,
        file: XkbFile,
        merge: MergeMode,
    ) -> Result<(), CompileSymbolsError> {
        super::symbols::compile_symbols(self, file, merge)
    }

    fn compile(&mut self, file: XkbFile, merge: MergeMode) -> Result<(), KeymapCompileError> {
        match file.file_type() {
            XkbFileType::Keycodes => self.compile_keycodes(file, merge)?,
            XkbFileType::Types => self.compile_keytypes(file, merge)?,
            XkbFileType::Compat => self.compile_compat_map(file, merge)?,
            XkbFileType::Symbols => self.compile_symbols(file, merge)?,
            _ => {} // TODO: error
        };

        Ok(())
    }

    pub(crate) fn compile_keymap(
        &mut self,
        mut xkb_file: XkbFile,
        merge: MergeMode,
    ) -> Result<(), KeymapCompileError> {
        let mut files = std::collections::BTreeMap::new();
        for file in xkb_file.take_files() {
            let file_type = file.file_type();
            if file_type < XkbFileType::first_type() || file_type > XkbFileType::last_type() {
                if file_type == XkbFileType::Geometry {
                    log::warn!("Geometry sections are not supported; Ignoring");
                } else {
                    log::warn!("Cannot define this file type in a keymap file");
                }
                continue;
            }

            if files.contains_key(&file_type) {
                log::error!("More than one {:?} section in keymap file; All sections after the first ignored", 

                        file_type);
                continue;
            }
            files.insert(file_type, file);
        }
        for i in XkbFileType::iter_possible() {
            if !files.contains_key(&i) {
                log::error!("Required section {:?} missing from keymap", i);

                return Err(KeymapCompileError::RequiredSectionMissing(i));
            }
        }

        // Compile sections
        for file_type in XkbFileType::iter_possible() {
            log::debug!("Compiling {:?}", file_type);
            let file: XkbFile = files
                .remove(&file_type)
                .expect("map of files was not properly set up"); //TODO: preserve the file from above

            self.compile(file, merge)?;
        }

        Ok(())
    }
}

impl ModSet {
    fn compute_effective_mask(&self, mods: &mut Mods) {
        mods.mask = self.mod_mask_get_effective(mods.mods)
    }
}

impl ModAction {
    fn update_action_mods(&mut self, modmap: ModMask, keymap_mods: &ModSet) {
        if [
            ActionType::ModSet,
            ActionType::ModLatch,
            ActionType::ModLock,
        ]
        .contains(&self.action_type)
        {
            if self.flags.intersects(ActionFlags::ModsLookupModMap) {
                self.mods.mods = modmap;
            }

            keymap_mods.compute_effective_mask(&mut self.mods);
        }
    }
}

const DEFAULT_INTERPRET: SymInterpret = SymInterpret {
    sym: None,
    repeat: true,
    match_op: MatchOperation::AnyOrNone,
    mods: Mods { mods: 0, mask: 0 },
    virtual_mod: None,
    action: Action::None,
    level_one_only: false,
};

/// Find an interpretation which applies to this particular
/// level, either by finding an exact match for the symbol
/// and modifier combination, or a generic XKB_KEY_NoSymbol match.
fn find_interp_for_key(
    interps: &[SymInterpret],
    syms: &[Keysym],
    key: &Key,
    level: LevelIndex,
) -> Option<SymInterpret> {
    // TODO: this may not be the right value
    if syms.is_empty() {
        return None;
    }

    // There may be multiple matching interprets;
    // we should always return the most specific.
    // Here, we rely on `compat.rs` to set up the
    // sym_interprets array from the most specific
    // to the least specific, such that when we
    // find a match we return immediately.

    for interp in interps.iter() {
        if (syms.len() > 1 || interp.sym.as_ref() != syms.first()) && interp.sym.is_some() {
            continue;
        }

        let mods = match interp.level_one_only && level != 0 {
            true => 0,
            false => key.modmap,
        };

        let found = match &interp.match_op {
            MatchOperation::None => (interp.mods.mods & mods) == 0,
            MatchOperation::AnyOrNone => mods == 0 || (interp.mods.mods & mods) != 0,
            MatchOperation::Any => (interp.mods.mods & mods) != 0,
            MatchOperation::All => (interp.mods.mods & mods) == interp.mods.mods,
            MatchOperation::Exactly => interp.mods.mods == mods,
        };

        if found {
            return Some(interp.clone());
        }
    }

    Some(DEFAULT_INTERPRET)
}

impl Keymap {
    fn apply_interps_to_key(&mut self, kc: &RawKeycode) {
        let mut vmodmap = 0;

        if self.keys[kc]
            .explicit
            .intersects(ExplicitComponents::INTERP)
        {
            return;
        }

        // iterate vec of group
        for group in 0..self.keys[kc].groups.len() {
            // iterate vec of levels
            for level in 0..self.keys[kc].groups[group].levels.len() {
                let interp = find_interp_for_key(
                    &self.sym_interprets,
                    &self
                        .key_get_syms_by_level(Keycode::new(*kc), group, level)
                        .expect("Error getting syms for this level"),
                    &self.keys[kc],
                    level,
                );

                let interp = match interp {
                    Some(interp) => interp,
                    None => continue,
                };

                // Infer default key behaviors from the base level
                if group == 0
                    && level == 0
                    && !self.keys[kc]
                        .explicit
                        .intersects(ExplicitComponents::REPEAT)
                    && interp.repeat
                {
                    self.keys.get_mut(kc).expect("No key for keycode").repeats = true;
                }
                if (group == 0 && level == 0) || !interp.level_one_only {
                    if let Some(vmod) = interp.virtual_mod {
                        vmodmap |= 1 << vmod;
                    }
                }

                if interp.action.action_type() != ActionType::None {
                    let key = self.keys.get_mut(kc).unwrap();
                    key.groups[group].levels[level].action = interp.action;
                }
            }
        }

        if !self.keys[kc]
            .explicit
            .intersects(ExplicitComponents::VMODMAP)
        {
            let key = self.keys.get_mut(kc).unwrap();
            key.vmodmap = vmodmap;
        }
    }
}

impl Action {
    fn is_mod_action(&self) -> bool {
        use ActionType::*;
        match self.action_type() {
            ModSet | ModLatch | ModLock => true,
            _ => false,
        }
    }
    fn is_group_action(&self) -> bool {
        use ActionType::*;
        match self.action_type() {
            GroupSet | GroupLatch | GroupLock => true,
            _ => false,
        }
    }
}

impl Keymap {
    pub(super) fn update_derived_keymap_fields(&mut self) {
        let keycodes: Vec<RawKeycode> = self.keys.keys().copied().collect();

        // Find all the interprets for the key and bind them
        // to actions, which will also update the vmodmap.
        for idx in keycodes.iter() {
            self.apply_interps_to_key(idx);
        }

        // Update keymap->mods, the virtual->real mod mapping.
        for (_kc, key) in self.keys.iter() {
            for (i, _mod) in self.mods.mods.iter_mut().enumerate() {
                if (key.vmodmap & (1 << i)) != 0 {
                    _mod.mapping |= key.modmap;
                }
            }
        }

        // Now update the level masks for all the types
        // to reflect the vmods.
        for _type in self.types.iter_mut() {
            self.mods.compute_effective_mask(&mut _type.mods);

            for entry in _type.entries.iter_mut() {
                self.mods.compute_effective_mask(&mut entry.mods);
                self.mods.compute_effective_mask(&mut entry.preserve);
            }
        }

        // Update action modifiers
        for kc in keycodes.iter() {
            let key = self.keys.get_mut(kc).unwrap();
            let modmap = key.modmap;

            for group in key.groups.iter_mut() {
                for level in group.levels.iter_mut() {
                    if let Action::Mods(ref mut action) = level.action {
                        action.update_action_mods(modmap, &self.mods);
                    }
                }
            }
        }

        // Update vmod -> led maps
        for led in self.leds.iter_mut().flatten() {
            self.mods.compute_effective_mask(&mut led.mods);
        }

        // Find maximum number of groups out of all keys
        // in the keymap
        let max_key_groups = self.keys.values().map(|key| key.groups.len()).max();
        if let Some(val) = max_key_groups {
            self.num_groups = std::cmp::max(val, self.num_groups);
        }
    }
}

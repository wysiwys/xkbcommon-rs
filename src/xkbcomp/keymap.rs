use super::ast::*;

use crate::errors::*;
use crate::keymap::*;
use crate::rust_xkbcommon::*;


impl KeymapBuilder<TextV1> {
    // from keycodes.c
    fn compile_keycodes(&mut self, file: XkbFile, merge: MergeMode) 
        -> Result<(), CompileKeycodesError> {
        super::keycodes::compile_keycodes(self, file, merge)
    }

    fn compile_keytypes(&mut self, file: XkbFile, merge: MergeMode) 
        -> Result<(), CompileTypesError> {
        super::types::compile_keytypes(self, file, merge)
    }

    fn compile_compat_map(&mut self, file: XkbFile, merge: MergeMode) 
        -> Result<(), CompileCompatError> {
        super::compat::compile_compat(self, file, merge)
    }

    fn compile_symbols(&mut self, file: XkbFile, merge: MergeMode) 
        -> Result<(), CompileSymbolsError> {
        super::symbols::compile_symbols(self, file, merge)
    }

    fn compile(&mut self, file: XkbFile, merge: MergeMode) -> Result<(), KeymapCompileError> {
        match file.file_type() {
            XkbFileType::Keycodes => self.compile_keycodes(file, merge)?,
            XkbFileType::Types => self.compile_keytypes(file, merge)?,
            XkbFileType::Compat => self.compile_compat_map(file, merge)?,
            XkbFileType::Symbols => self.compile_symbols(file, merge)?,
            _ => {}, // TODO: error
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
            if !files.contains_key(&i.into()) {
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
    fn compute_effective_mask(&self, mods: Mods) -> ModMask {
        self.mod_mask_get_effective(mods.mods)
    }
}

impl ModAction {
    fn update_action_mods(&mut self, modmap: ModMask, mods: &ModSet) {
        if self.flags.intersects(ActionFlags::ModsLookupModMap) {
            self.mods.mods = modmap;
        }

        self.mods.mask = mods.compute_effective_mask(self.mods);
    }
}

const DEFAULT_INTERPRET: SymInterpret = SymInterpret {
    sym: None,
    repeat: true,
    match_op: MatchOperation::AnyOrNone,
    mods: Mods { mods: 0, mask: 0 },
    virtual_mod: None,
    action: None,
    level_one_only: false,
};

/// Find an interpretation which applies to this particular
/// level, either by finding an exact match for the symbol
/// and modifier combination, or a generic XKB_KEY_NoSymbol match.
fn find_interp_for_key(
    interps: &Vec<SymInterpret>,
    syms: &Vec<Keysym>,
    key: &Key,
    level: LevelIndex,
) -> Option<SymInterpret> {
    // TODO: this may not be the right value
    if syms.len() == 0 {
        return None;
    }

    // There may be multiple matching interprets;
    // we should always return the most specific.
    // Here, we rely on `compat.c` to set up the
    // sym_interprets array from the most specific
    // to the least specific, such that when we
    // find a match we return immediately.
    // TODO: test

    for interp in interps.iter() {
        let mods;

        if interp.sym.is_some() {
            if syms.len() > 1 {
                continue;
            } else if interp.sym.as_ref() != syms.get(0) {
                continue;
            }
        }

        if interp.level_one_only && level != 0 {
            mods = 0
        } else {
            mods = key.modmap;
        }

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
    fn apply_interps(&mut self, kc: &RawKeycode) {
        let mut vmodmap = 0;

        if self.keys[kc]
            .explicit
            .intersects(ExplicitComponents::INTERP)
        {
            return;
        }

        // iterate vec of group
        for g_idx in 0..self.keys[kc].groups.len() {
            // iterate vec of levels
            for l_idx in 0..self.keys[kc].groups[g_idx].levels.len() {
                let interp = find_interp_for_key(
                    &self.sym_interprets,
                    &self.key_get_syms_by_level(Keycode::new(*kc), g_idx, l_idx)
                    .map_err(|e| format!("Could not get syms by level: {:?}", e)).unwrap(),
                    &self.keys[kc],
                    l_idx,
                );

                let interp = match interp {
                    Some(interp) => interp,
                    None => continue,
                };

                if (g_idx == 0 && l_idx == 0) || !interp.level_one_only {
                    if let Some(vmod) = interp.virtual_mod {
                        vmodmap |= 1 << vmod;
                    }
                }
                // Infer default key behaviors from the base
                // level.

                if g_idx == 0 && l_idx == 0 {
                    if !self.keys[&kc]
                        .explicit
                        .intersects(ExplicitComponents::REPEAT)
                        && interp.repeat
                    {
                        let key = self.keys.get_mut(&kc).unwrap();
                        key.repeats = true;
                    }
                }

                if let Some(action) = interp.action {
                    let key = self.keys.get_mut(&kc).unwrap();
                    key.groups[g_idx].levels[l_idx].action = Some(action);
                }
            }
        }

        if !self.keys[&kc]
            .explicit
            .intersects(ExplicitComponents::VMODMAP)
        {
            let key = self.keys.get_mut(&kc).unwrap();
            key.vmodmap = vmodmap;
        }

    }
}


impl Keymap {
    pub(super) fn update_derived_keymap_fields(&mut self) {
        let mut keycodes: Vec<RawKeycode> = self.keys.keys().copied().collect();

        // TODO: this order may be wrong
        keycodes.sort();

        // Find all the interprets for the key and bind them
        // to actions, which will also update the vmodmap.
        for idx in keycodes.iter() {
            self.apply_interps(idx);
        }

        // Update keymap->mods, the virtual->real mod mapping.
        for (_, key) in self.keys.iter() {
            for (i, _mod) in self.mods.mods.iter_mut().enumerate() {
                if (key.vmodmap & (1 << i)) != 0 {
                    _mod.mapping |= key.modmap;
                }
            }
        }

        // Now update the level masks for all the types
        // to reflect the vmods.
        for _type in self.types.iter_mut() {
            let mods = &mut _type.mods;

            mods.mask = self.mods.compute_effective_mask(*mods);

            for entry in _type.entries.iter_mut() {
                entry.mods.mask = self.mods.compute_effective_mask(entry.mods);
                entry.preserve.mask = self.mods.compute_effective_mask(entry.preserve);
            }
        }

        // Update action modifiers
        for kc in keycodes.iter() {
            let key = self.keys.get_mut(kc).unwrap();
            let modmap = key.modmap;

            for group in key.groups.iter_mut() {
                for level in group.levels.iter_mut() {
                    if let Some(Action::Mods(ref mut action)) = level.action {
                        action.update_action_mods(modmap, &self.mods);
                    }
                }
            }
        }

        // Update vmod -> led maps
        for led in self.leds.iter_mut() {
            if let Some(led) = led {
                led.mods.mask = self.mods.compute_effective_mask(led.mods);
            }
        }

        // Find maximum number of groups out of all keys
        // in the keymap
        let max_key_groups = self.keys.iter().map(|(_, key)| key.groups.len()).max();
        if let Some(val) = max_key_groups {
            self.num_groups = std::cmp::max(val, self.num_groups);
        }

    }
}

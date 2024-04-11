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

        let mods = self.clone();

        if merge == MergeMode::Default {
            merge = stmt.merge;
        }

        let mut mapping;
        if let Some(value) = stmt.value {
            mapping = match value.resolve_mod_mask(ctx, ModType::REAL, self) {
                Some(mapping) => mapping,
                None => {
                    log::error!("{:?}: Declaration of {:?} ignored",
                        XkbMessageCode::NoId,
                        ctx.xkb_atom_text(stmt.name)
                    );
                    return Err(HandleVModError::CouldNotResolveModMask);
                }
            };
        } else {
            mapping = 0;
        }

        for _mod in self.mods.iter_mut() {
            if _mod.name == stmt.name {
                if _mod.mod_type != ModType::VIRT {

                    let name = ctx.xkb_atom_text(_mod.name);
                    log::error!("{:?}: Can't add a virtual modifier named \"{:?}\"; there is already a non-virtual modifier with this name! Ignored",
                        XkbMessageCode::NoId,
                        name
                    );

                    return Err(HandleVModError::ExistingRealModHasName(name.expect("Mod has no name").into()));
                    
                } else if _mod.mapping == mapping {
                    return Ok(());
                } else if _mod.mapping != 0 {
                    let (_use, ignore) = match merge {
                        MergeMode::Override => (mapping, _mod.mapping),
                        _ => (_mod.mapping, mapping)
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
            mapping,
            mod_type: ModType::VIRT,
        });

        Ok(())
    }
}


use super::ast::*;
use super::types::KeyTypesInfo;

use crate::context::Context;
use crate::errors::*;
use crate::keymap::{ModSet, ModType, Mod, XKB_MAX_MODS};
use crate::rust_xkbcommon::*;

impl ModSet {

    pub(super) fn handle_vmod_def(
        &mut self,
        ctx: &Context,
        stmt: VModDef,
        mut merge: MergeMode)
        -> Result<(),KeytypeErr> {


            if merge == MergeMode::Default { merge = stmt.merge; }

            let mapping;
            if let Some(value) = stmt.value {

                mapping = match value.resolve_mod_mask(
                    ctx, ModType::REAL,
                    self) {

                    Some(mapping) => todo!(),
                    None => 0 //TODO: check this in expr_resolve_mod_mask
                              //and ensure that no data was uninitialized
                };
            } else {
                mapping = 0;
            }

            for _mod in self.mods.iter_mut() {
                if _mod.name == stmt.name {

                    if !_mod.mod_type.intersects(ModType::VIRT) {
                        todo!();

                    }
                    else if _mod.mapping == mapping {

                        return Ok(());
                    }
                    else if _mod.mapping != 0 {

                        todo!()
                    }

                    _mod.mapping = mapping;
                    return Ok(());

                }
            }

            if self.mods.len() >= XKB_MAX_MODS {

                let err = XkbMessageCode::NoId;
                log::error!("{:?}:: Too many modifiers defined (maximum {})", err, XKB_MAX_MODS);
                return Err(todo!());
            }

            self.mods.push(Mod {
                name: stmt.name,
                mapping,
                mod_type: ModType::VIRT,

            });

            Ok(())

    }


}


use super::ast::*;
use super::include::process_include_file;
use crate::atom::Atom;
use crate::context::Context;
use crate::keymap::{Mods, ModType, ModSet, KeyType, KeyTypeEntry, KeymapBuilder};
use crate::rust_xkbcommon::*;

use crate::errors::*;


use std::collections::BTreeMap;

bitflags::bitflags! {
    #[derive(Copy,Clone)]
    pub(crate) struct TypeField: u8 {
        const Mask = 1 << 0;
        const Map = 1 << 1;
        const Preserve = 1 << 2;
        const LevelName = 1 << 3;

    }
}



struct KeyTypeInfo {

    defined: TypeField,
    merge: MergeMode,
    name: Atom,
    mods: ModMask,
    num_levels: LevelIndex,
    entries: Vec<KeyTypeEntry>,
    level_names: BTreeMap<usize,Atom>

}

pub(super) struct KeyTypesInfo {
    name: Option<String>,
    error_count: u32,
    include_depth: u32, //unsigned int
    types: Vec<KeyTypeInfo>,
    pub(super) mods: ModSet,
}

impl Context {

    fn map_entry_txt(
        &self,
        info: &KeyTypesInfo,
        entry: &KeyTypeEntry)
        -> String {


            self.mod_mask_text(
                &info.mods, entry.mods.mods)
                

    }

    fn type_txt<'a>(
        &'a self,
        _type: &KeyTypeInfo)
        -> Option<&'a str> {

            self.xkb_atom_text(_type.name)

    }

    fn type_mask_txt(
        &self,
        info: &KeyTypesInfo,
        _type: &KeyTypeInfo
    ) -> Option<String> {

        Some(self.mod_mask_text(
            &info.mods, _type.mods))

    }

    fn report_type_should_be_array(
        &self,
        _type: &KeyTypeInfo,
        field: &str)
    -> Result<(), XkbMessageCode> {

        self.report_should_be_array(
            "key type", field,
            &self.type_txt(_type)
                .unwrap_or(""))


    }

    fn report_type_bad_type(
        &self,
        code: XkbError,
        _type: &KeyTypeInfo,
        field: &str,
        wanted: &str)
    -> Result<(), XkbMessageCode> {

        self.report_bad_type(
            code.into(),
            "key type",
            field,
            &self.type_txt(_type).unwrap_or(""),
            wanted)


    }

    
}

impl KeyTypesInfo {
    
    fn new(
        include_depth: u32,
        mods: ModSet) -> Self {

        Self {
            mods,
            include_depth,
            error_count: 0,
            types: vec![],
            name: None
        }


    }

    
    fn find_matching_key_type<'s>(&'s mut self, name: Atom)
        -> Option<&'s mut KeyTypeInfo> {

            for old in self.types.iter_mut() {
                if old.name == name {
                    return Some(old);
                }
            }

            None

    }

    fn add_keytype(
        &mut self,
        ctx: &Context,
        new: KeyTypeInfo,
        same_file: bool
    ) -> Result<(), KeytypeErr> {

        let old = self.find_matching_key_type(new.name);

        if old.is_some() {

            if new.merge == MergeMode::Replace
                && new.merge == MergeMode::Override {
                
                    todo!();
                    let _ = std::mem::replace(old.unwrap(), new);

                    return Ok(());



            }

            if same_file {
                todo!(); }

            return Ok(());

        }
        self.types.push(new);
        return Ok(());

    }

    fn merge_included_key_types(
        &mut self, ctx: &Context,
        from: KeyTypesInfo, merge: MergeMode) {

        if from.error_count > 0 {

            self.error_count += from.error_count;
            return;
        }

        self.mods = from.mods;
        
        if self.name.is_none() { 
            self.name = from.name;
        }

        if self.types.is_empty() {
            self.types = from.types;
        }
        else {
            for mut _type in from.types {
                _type.merge = match merge {
                    MergeMode::Default => _type.merge,
                    _ => merge };
                if self.add_keytype(ctx, _type, false).is_err() {
                    self.error_count += 1;
                }
            }
        }



    }


    fn handle_include_keytypes(
        &mut self,
        ctx: &mut Context,
        include: IncludeStmt)
        -> Result<(), KeytypeErr> {

            if ctx.exceeds_include_max_depth(
                self.include_depth) {

                self.error_count += 10;
                return Err(todo!());
            }

            let mut included = KeyTypesInfo::new(0, self.mods.clone());

            for stmt in include.maps {

                let file = process_include_file(ctx, &stmt, XkbFileType::Types);

                if let Err(e) = file {
                    self.error_count += 10;
                    return Err(todo!());
                }

                let mut next_incl = KeyTypesInfo::new(
                    self.include_depth + 1, included.mods.clone());

                next_incl.handle_key_types_file(ctx, file.unwrap(), stmt.merge)?;

                included.merge_included_key_types(
                    ctx, next_incl, stmt.merge);


            }

            self.merge_included_key_types(ctx, included, include.merge);


            match self.error_count {
                0 => Ok(()),
                _ => Err(todo!())
            }

    }

    fn set_modifiers(
        &mut self,
        ctx: &Context,
        _type: &mut KeyTypeInfo,
        array_ndx: Option<ExprDef>,
        value: ExprDef)
        -> Result<(), XkbMessageCode> {

            if let Some(ndx) = array_ndx {
                log::info!(
                    "The modifiers field of a key type is not an array; Illegal array subscript ignored.");
            }

            let mods = value.resolve_mod_mask(
                ctx, ModType::BOTH, &self.mods);

            if mods.is_none() {
                let err = XkbError::UnsupportedModifierMask;
                log::error!(
                    "{:?}: Key type mask field must be a modifier mask; Key type definition ignored",
                    err);
                return Err(err.into());
            }
            let mods = mods.unwrap();


            if _type.defined.intersects(TypeField::Mask) {

                log::warn!(
                    "Multiple modifier mask definitions for key type {:?}; using {:?}, ignoring {:?}",
                    ctx.xkb_atom_text(_type.name),
                    ctx.type_mask_txt(self,_type),
                    ctx.mod_mask_text(&self.mods, mods));




            }


            _type.mods = mods;
            Ok(())
                    


    }

    fn find_matching_map_entry<'c>(
        &self,
        _type: &'c KeyTypeInfo,
        mods: &ModMask)
        -> Option<usize> {

            for (i, entry) in _type.entries.iter().enumerate() {
                if entry.mods.mods == *mods {
                    return Some(i);
                }
            }

            None


    }

    fn add_map_entry(
        &mut self,
        ctx: &Context,
        _type: &mut KeyTypeInfo,
        new: KeyTypeEntry,
        clobber: bool,
        report: bool
    ) -> Result<(), XkbMessageCode> {

        let type_txt =ctx.type_txt(_type);
        let old = self.find_matching_map_entry(
            _type, &new.mods.mods);
        if let Some(old_idx) = old {

            let old = _type.entries.get_mut(old_idx).unwrap();
            if report && (old.level != new.level) {

                log::warn!(
                    "{:?}: Multiple map entries for {:?} in {:?}; Using {:?}, ignoring {:?}", 
                    XkbWarning::ConflictingKeyTypeMapEntry,
                    ctx.map_entry_txt(self, &new),
                    type_txt,
                    match clobber {
                        true => new.level,
                        false => old.level } + 1,
                    match clobber {
                        true => old.level,
                        false => new.level} + 1);

            } else {

                log::warn!(
                    "{:?}: Multiple occurrences of map[{:?}] = {:?} in {:?}; Ignored",
                    XkbWarning::ConflictingKeyTypeMapEntry,
                    ctx.map_entry_txt(self,&new),
                    new.level + 1,
                    ctx.type_txt(_type));
                return Ok(());
            }

            if clobber {
                if new.level >= _type.num_levels {
                    _type.num_levels = new.level + 1;
                }
                old.level = new.level;
            }

            return Ok(());
        }

        if new.level >= _type.num_levels {
            _type.num_levels = new.level + 1;
        }
        _type.entries.push(new);

        Ok(())



    }

    fn set_map_entry(
        &mut self,
        ctx: &Context,
        _type: &mut KeyTypeInfo,
        array_ndx: Option<ExprDef>,
        value: ExprDef)
    -> Result<(), XkbMessageCode> {

        // new empty KeyTypeEntry
        let mut entry = KeyTypeEntry {
            level: 0,
            mods: Mods { mods: 0, mask: 0},
            preserve: Mods { mods: 0, mask: 0}};

        let  array_ndx = match array_ndx {
            Some(ndx) => ndx,
            None => return ctx.report_type_should_be_array(
                _type, "map entry") };


        let mods = array_ndx.resolve_mod_mask(
            ctx, ModType::BOTH, &self.mods);

        entry.mods.mods = match mods {
            Some(mods) => mods,
            None => return ctx.report_type_bad_type(
                    XkbError::UnsupportedModifierMask.into(),
                    _type, "map entry", "modifier mask") };


        if (entry.mods.mods & !_type.mods) > 0 {
            log::warn!(
                "{:?}: Map entry for modifiers not used by type {:?}; Using {:?} instead of {:?}",
                XkbWarning::UndeclaredModifiersInKeyType,
                ctx.type_txt(_type),
                ctx.mod_mask_text(&self.mods, entry.mods.mods & _type.mods),
                ctx.map_entry_txt(self, &entry));
            entry.mods.mods &= _type.mods;

        }

        let level = value.resolve_level(ctx);

        if level.is_none() {
            let err = XkbError::UnsupportedShiftLevel;
            log::error!(
                "{:?}: Level specifications in a key type must be integer; Ignoring malformed level specification",
                &err);

        return Err(err.into());
        }

        entry.level = level.unwrap();
        entry.preserve.mods = 0;

        return self.add_map_entry(
            ctx, _type, entry, true, true);


    }

    fn add_preserve(
        &mut self,
        ctx: &Context,
        _type: &mut KeyTypeInfo,
        mods: ModMask,
        preserve_mods: ModMask)
        -> Result<(), XkbMessageCode> {


            let type_txt = ctx.type_txt(_type);
            for entry in _type.entries.iter_mut() {

                if entry.mods.mods != mods {
                    continue;
                }

                // map exists without previous preserve
                else if entry.preserve.mods == 0 {
                    entry.preserve.mods = preserve_mods;
                    return Ok(());
                }
                
                // map exists with same preserve;
                // do nothing.
                else if entry.preserve.mods == preserve_mods {
                    log::warn!(
                        "{:?}: Identical definitions for preserve[{:?}] in {:?}; Ignored", XkbWarning::DuplicateEntry, 
                        ctx.mod_mask_text(&self.mods, mods),
                        ctx.type_txt(_type));
                    return Ok(());

                }

                log::warn!(
                    "{:?}: Multiple definitions for preserve[{:?}] in {:?}; Using {:?}, ignoring {:?}",
                    XkbWarning::ConflictingKeyTypePreserveEntries,

                    ctx.mod_mask_text(&self.mods, mods),
                    type_txt,
                    ctx.mod_mask_text(&self.mods, preserve_mods),
                    ctx.mod_mask_text(&self.mods, entry.preserve.mods));
                entry.preserve.mods = preserve_mods;
                return Ok(());

            }

            // Map does not exist, i.e. preserve[] came before map[].
            // Create a map with the specified mask mapping to Level1. The level may be overriden
            // later with an explicit map[] statement.

            // TODO: mark that these are uninitialized?
            let new = KeyTypeEntry {
                level: 0,
                mods: Mods {
                    mods,
                    mask: 0
                },
                preserve: Mods {
                    mods: preserve_mods,
                    mask: 0
                }
            };

            _type.entries.push(new);
                

            Ok(())

    }

    fn set_preserve(
        &mut self,
        ctx: &Context,
        _type: &mut KeyTypeInfo,
        array_ndx: Option<ExprDef>,
        value: ExprDef
    ) -> Result<(), XkbMessageCode> {

        if array_ndx.is_none() {

            return ctx.report_type_should_be_array(_type, "preserve entry");

        }

        let mods = (array_ndx.unwrap()).resolve_mod_mask(
            ctx, ModType::BOTH, &self.mods);

        if mods.is_none() {

            return ctx.report_type_bad_type(
                XkbError::UnsupportedModifierMask.into(),
                _type, "preserve entry", "modifier mask");
        }
        let mut mods = mods.unwrap();

        if (mods & !_type.mods) != 0 {
            let before = ctx.mod_mask_text(
                &self.mods, mods);
            mods = mods & _type.mods;
            let after = ctx.mod_mask_text(
                &self.mods, mods);

            log::warn!(
                "{:?}: Preserve entry for modifiers not used by the {:?} type; Index {:?} converted to {:?}.",
                XkbWarning::UndeclaredModifiersInKeyType,
                ctx.type_txt(_type), before, after);


        }

    let preserve_mods = value.resolve_mod_mask(
        ctx, ModType::BOTH, &self.mods);

    if preserve_mods.is_none() {

        log::error!(
            "{:?}: Preserve value in a key type is not a modifier mask; Ignoring preserve {:?} in type {:?}.",
            XkbError::UnsupportedModifierMask,
            ctx.mod_mask_text(&self.mods, mods),
            ctx.type_txt(_type)
        );
        return Err(XkbError::UnsupportedModifierMask.into());

    }
    let mut preserve_mods = preserve_mods.unwrap();

    if (preserve_mods & !mods) != 0 {

        let before = ctx.mod_mask_text(&self.mods, preserve_mods);
        preserve_mods = preserve_mods & mods;

        let after = ctx.mod_mask_text(&self.mods, preserve_mods);

        log::warn!(
                "{:?}: Illegal value for preserve[{}] in type{:?}; Converted {} to {}.", XkbWarning::IllegalKeyTypePreserveResult, ctx.mod_mask_text(&self.mods, mods), ctx.type_txt(_type), before, after);

    }



        return self.add_preserve(ctx, _type, mods, preserve_mods);

    }

    fn add_level_name(
        &mut self,
        ctx: &Context,
        _type: &mut KeyTypeInfo,
        level: LevelIndex,
        name: Atom,
        clobber: bool
    ) -> Result<(), XkbMessageCode> {

        // same level, same name
        if let Some(stored_name) = _type.level_names.get(&level) {
            if *stored_name == name {

               log::warn!(
                   "{:?}: Duplicate names for level {} of key type {:?}; Ignored",
                   XkbWarning::DuplicateEntry,
                   level + 1,
                   ctx.type_txt(_type));

                return Ok(());
            }
        }

        // same level, different name
        if let Some(stored_name) = _type.level_names.get(&level) {
            let old = ctx.xkb_atom_text(*stored_name);
            let new = ctx.xkb_atom_text(name);

            log::warn!(
                "{:?}: Multiple names for level {} of key type {:?}; Using {:?}, ignoring {:?}",
                XkbWarning::ConflictingKeyTypeLevelNames,
                level + 1,
                ctx.type_txt(_type),
                match clobber {
                    true => &new, false => &old},
                match clobber {
                    true => &old, false => &new});

            if !clobber { return Ok(()); }

        }

        _type.level_names.insert(level,name);

        Ok(())

    }

    fn set_level_name(
        &mut self,
        ctx: &Context,
        _type: &mut KeyTypeInfo,
        array_ndx: Option<ExprDef>,
        value: ExprDef)
        -> Result<(), XkbMessageCode> {
    
            if array_ndx.is_none() {
                return ctx.report_type_should_be_array(
                    _type, "level name");
            }

            let level = (array_ndx.unwrap()).resolve_level(ctx);

            if level.is_none() {

                return ctx.report_type_bad_type(
                    XkbError::UnsupportedModifierMask.into(),
                    _type, "level name", "integer");

            }
            let level = level.unwrap();

            let level_name = value.resolve_string(ctx); 

            if level_name.is_none() {
                log::error!(
                    "{:?}: Non-string name for level {:?} in key type {:?}; ignoring illegal level name definition\n", XkbError::WrongFieldType, level + 1, ctx.xkb_atom_text(_type.name));
                return Err(XkbError::WrongFieldType.into());
            }

 
            self.add_level_name(
                ctx, _type, level, level_name.unwrap(), true)
    }

    fn set_keytype_field(
        &mut self,
        ctx: &Context,
        _type: &mut KeyTypeInfo,
        field: String,
        array_ndx: Option<ExprDef>,
        value: ExprDef)
        -> Result<(),XkbMessageCode> {

            let type_field;

            let ok = match field.to_lowercase().as_str() {
                "modifiers"
                    => {

                        type_field = TypeField::Mask;
                        self.set_modifiers(
                        ctx,
                        _type,
                        array_ndx,
                        value)
                        
                    },
                "map"
                    => {
                        type_field = TypeField::Map;

                        self.set_map_entry(
                        ctx,
                        _type,
                        array_ndx,
                        value)

                    },
                "preserve"
                    => {
                        type_field = TypeField::Preserve;
                        self.set_preserve(
                        ctx,
                        _type,
                        array_ndx,
                        value)

                    },
                f if ["levelname","level_name"].contains(&f)
                    => {
                        type_field = TypeField::LevelName;
                        self.set_level_name(
                        ctx,
                        _type,
                        array_ndx,
                        value)

                    },
                _ => {
                    let err = XkbError::UnknownField;
                    log::error!("{:?}: Unknown field {:?} in key type {:?}; definition ignored",
                        err, field, ctx.type_txt(_type)); 
                    
                    type_field = TypeField::empty();
                    Err(err.into())
                }

            };

            //bitwise or
            _type.defined = _type.defined.union(type_field);


            ok

    }

    fn handle_keytype_body(
        &mut self,
        ctx: &Context,
        def: Vec<VarDef>,
        _type: &mut KeyTypeInfo)
        -> Result<(), XkbMessageCode> {

            let mut ok = Ok(());

                for def in def {

                    let lhs = match def.name {
                        None => None,
                        Some(name) => name.resolve_lhs(ctx)};

                    ok = match lhs.is_some() { 
                        true => Ok(()),
                        false => Err(todo!()) };
                        
                    if ok.is_err() { continue; }

                    else if let Some(ret) = lhs {

                        if let Some(elem) = ret.elem {
                            if elem.to_lowercase().as_str() == "type" {

                                log::error!("{:?}: Support for changing the default type has been removed; Statement ignored.",
                                    XkbError::InvalidSetDefaultStatement);
                                continue;

                            }
                        }
                        ok = self.set_keytype_field(
                            ctx,
                            _type,
                            ret.field,
                            ret.index,
                            def.value);

                        
                    
                    }
                }

                ok
        }


    fn handle_keytype_def(
        &mut self,
        ctx: &Context,
        def: KeyTypeDef,
        merge: MergeMode)
        -> Result<(),KeytypeErr> {

            let mut _type = KeyTypeInfo {
                defined: TypeField::empty(),
                merge: match merge {
                    MergeMode::Default => merge,
                    _ => def.merge},
                name: def.name,
                mods: 0,
                num_levels: 1,
                entries: vec![],
                level_names: BTreeMap::new(),

            };

            if self.handle_keytype_body(ctx, def.body, &mut _type).is_err()
            || self.add_keytype(ctx, _type, true).is_err() {

                self.error_count += 1;
                return Err(KeytypeErr::FailedHandleKeytypeDef);

            }

            Ok(())

    }


    fn handle_key_types_file(
        &mut self,
        ctx: &mut Context,
        file: XkbFile,
        merge: MergeMode)
        -> Result<(), KeytypeErr> {


            self.name = Some(file.name);

            for def in file.defs {

                let result = match def {
                    Decl::Include(stmt) 
                        => self.handle_include_keytypes(ctx, stmt),
                    Decl::KeyType(stmt)
                        => self.handle_keytype_def(ctx, stmt,merge),
                    Decl::Var(stmt)
                        => {
                            log::error!("{:?}: Support for changing the default type has been removed; Statement ignored",
                            XkbError::WrongStatementType);
                            
                            Ok(())

                        },
                    Decl::VMod(stmt)
                        => self.mods.handle_vmod_def(
                            ctx, stmt, merge),
                    stmt => {

                        log::error!("{:?}: Key type files may not include other declarations; Ignoring {}",
                            XkbError::WrongStatementType, todo!());

                        Err(todo!())

                    }
                };

                if !result.is_ok() {
                    self.error_count += 1;
                }

                if self.error_count > 10 {
                    let err = XkbError::InvalidSyntax;

                    log::error!("{:?}: Abandoning keytypes file {:?}",
                        err, &self.name);
                    return Err(KeytypeErr::InvalidSyntax);
                }


            }



            Ok(())
    }
}


impl KeymapBuilder<TextV1> {

    fn copy_keytypes(
        &mut self,
        info: KeyTypesInfo)
        -> Result<(), KeymapErr> {

            let types;
            // If no types were specified, a default unnamed one-level type
            // is used for all keys.
            if info.types.is_empty() {
                let _type = KeyType {
                    mods: Mods { mods: 0, mask:0 },
                    entries: vec![],
                    name: self.context.atom_intern("default".to_owned()),
                    num_levels: 1,
                    level_names: vec![],

                };

                types = vec![_type];
            } else {

                types = info.types.into_iter().map(|def| {

                    KeyType {
                        name: def.name,
                        mods: Mods {
                            mods: def.mods,
                            mask: 0
                        },
                        num_levels: def.num_levels,

                        // TODO: does it matter if there are gaps?
                        level_names:  def.level_names.values()
                            .map(|c| *c).collect(),
                        entries: def.entries,
                    }
                    
                }).collect();

            }
            self.types_section_name = info.name;
            // TODO: escape
            self.types = types;
            self.mods = info.mods;
            
            Ok(())

    }


}

pub(super) fn compile_keytypes(
    builder: &mut KeymapBuilder<TextV1>,
    file: XkbFile,
    merge: MergeMode)
-> Result<(), KeymapErr> {

    let mut info =  KeyTypesInfo::new(
        0,
        builder.mods.clone());

    info.handle_key_types_file(
        &mut builder.context, 
        file, 
        merge)?;
    
    builder.copy_keytypes(info)?;

    
    Ok(())

}
    

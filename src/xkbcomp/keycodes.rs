use super::ast::*;

use crate::atom::Atom;
use crate::context::Context;
use crate::errors::*;

use crate::keymap::XKB_MAX_LEDS;
use crate::keymap::{KeyAlias, KeyBuilder, KeymapBuilder, Led};

use crate::rust_xkbcommon::*;

struct AliasInfo {
    merge_mode: MergeMode,
    alias: Atom,
    real: Atom,
}

#[derive(Copy, Clone, Debug)]
struct LedNameInfo {
    merge_mode: MergeMode,
    name: Atom,
}

struct KeyNamesInfo {
    name: Option<String>,
    unrecoverable_error: Option<CompileKeycodesError>,
    errors: Vec<CompileKeycodesError>,
    include_depth: u32,
    min_key_code: RawKeycode,
    max_key_code: RawKeycode,
    key_names: std::collections::BTreeMap<RawKeycode, Atom>,

    led_names: [Option<LedNameInfo>; XKB_MAX_LEDS],
    num_led_names: usize, //unsigned into

    aliases: Vec<AliasInfo>, //darray
}

impl KeyNamesInfo {
    pub(crate) fn new(include_depth: u32) -> Self {
        if XKB_KEYCODE_INVALID < XKB_KEYCODE_MAX {
            // from original implementation
            panic!("Hey, you can't be changing stuff like that.");
        }
        Self {
            include_depth,
            min_key_code: XKB_KEYCODE_INVALID,

            // zeroed in original
            max_key_code: 0,
            unrecoverable_error: None,
            errors: vec![],
            num_led_names: 0,
            name: None,
            key_names: std::collections::BTreeMap::new(),
            led_names: [None; XKB_MAX_LEDS],
            aliases: vec![],
        }
    }

    fn find_led_by_name(&self, name: Atom) -> Option<usize> {
        self.led_names[0..self.num_led_names]
            .iter()
            .position(|ledi| match ledi {
                Some(ledi) => ledi.name == name,
                None => false,
            })
    }

    fn add_led_name(
        &mut self,
        ctx: &Context,
        merge: MergeMode,
        same_file: bool,
        new: LedNameInfo,
        new_idx: usize,
    ) -> Result<(), CompileKeycodesError> {
        let verbosity = ctx.get_log_verbosity();
        let report = (same_file && verbosity > 0) || verbosity > 9;
        let replace = merge == MergeMode::Replace || merge == MergeMode::Override;

        // If this name is found, update it
        if let Some(old_idx) = self.find_led_by_name(new.name) {
            if old_idx == new_idx {
                log::warn!(
                    "Multiple indicators named {:?}; Identical definitions ignored.",
                    ctx.xkb_atom_text(new.name)
                );

                return Ok(());
            }

            if report {
                let _use = if replace { new_idx + 1 } else { old_idx + 1 };
                let ignore = if replace { old_idx + 1 } else { new_idx + 1 };
                log::warn!(
                    "{:?}: Multiple indicators named {:?}; Using {}, ignoring {}",
                    XkbMessageCode::NoId,
                    ctx.xkb_atom_text(new.name),
                    _use,
                    ignore
                );
            }

            if replace {
                self.led_names[old_idx] = Some(new);
            }

            return Ok(());
        }

        if new_idx >= self.num_led_names {
            self.num_led_names = new_idx + 1;
        }

        if let Some(Some(old)) = self.led_names.get(new_idx) {
            // LED with the same index already exists

            if report {
                let (_use, ignore) = match replace {
                    true => (new.name, old.name),
                    false => (old.name, new.name),
                };
                log::warn!(
                    "Multiple names for indicator {:?}
                            Using {:?}, ignoring {:?}",
                    new_idx + 1,
                    ctx.xkb_atom_text(_use),
                    ctx.xkb_atom_text(ignore)
                );
            }
            if replace {
                self.led_names[new_idx] = Some(new);
            }

            return Ok(());
        }

        // None of the above cases apply
        // TODO: check XKB_MAX_LEDS
        self.led_names[new_idx] = Some(new);

        Ok(())
    }

    fn find_key_by_name(&self, name: Atom) -> Option<RawKeycode> {
        // TODO: ensure between min and max keycodes
        self.key_names
            .iter()
            .find(|(_, key_name)| *key_name == &name)
            .map(|(kc, _)| *kc)
    }

    fn add_key_name(
        &mut self,
        ctx: &Context,
        kc: RawKeycode,
        name: Atom,
        merge: MergeMode,
        same_file: bool,
        report: bool, //TODO: log levels
    ) -> Result<(), CompileKeycodesError> {
        let verbosity = ctx.get_log_verbosity();
        let report = report && (same_file && verbosity > 0) || verbosity > 7;

        // update min/max key names
        self.min_key_code = u32::min(self.min_key_code, kc);
        self.max_key_code = u32::max(self.max_key_code, kc);

        // there's already a key with this keycode
        if let Some(old_name) = self.key_names.get(&kc) {
            let lname = ctx.xkb_atom_text(*old_name);
            let kname = ctx.xkb_atom_text(name);

            if *old_name == name {
                if report {
                    log::warn!(
                        "Multiple identical key name definitions.
                        Later occurrences of \"{} = {}\" ignored.",
                        lname,
                        kc
                    );
                }

                return Ok(());
            } else if merge == MergeMode::Augment {
                if report {
                    log::warn!(
                        "Multiple names for keycode {};
                    Using {}, ignoring {}",
                        kc,
                        lname,
                        kname
                    );
                }

                return Ok(());
            } else {
                if report {
                    log::warn!(
                        "Multiple names for keycode {};
                    Using {}, ignoring {}",
                        kc,
                        kname,
                        lname
                    );
                }

                self.key_names.remove(&kc);
            }

            // There's already a key with this name
            if let Some(old_kc) = self.find_key_by_name(name) {
                if old_kc != XKB_KEYCODE_INVALID && old_kc != kc {
                    let kname = ctx.key_name_text(name);

                    if merge == MergeMode::Override {
                        self.key_names.remove(&old_kc);

                        if report {
                            log::warn!(
                                "Key name {} assigned to multiple keys;
                            Using {}, ignoring {}",
                                kname,
                                kc,
                                old_kc
                            );
                        }
                    } else {
                        if report {
                            log::warn!(
                                "Key name {} assigned to mulitple keys;
                            Using {}, ignoring {}",
                                kname,
                                old_kc,
                                kc
                            );
                        }

                        return Ok(());
                    }
                }
            }
        }

        self.key_names.insert(kc, name);
        Ok(())
    }

    fn merge_included_keycodes(&mut self, ctx: &Context, mut from: Self, merge: MergeMode) {
        if !from.errors.is_empty() {
            self.errors.append(&mut from.errors);
            return;
        }
        if let Some(e) = from.unrecoverable_error {
            self.unrecoverable_error = Some(e);
            return;
        }

        if self.name.is_none() {
            std::mem::swap(&mut self.name, &mut from.name);
        }

        if self.key_names.is_empty() {
            std::mem::swap(&mut self.key_names, &mut from.key_names);
            self.min_key_code = from.min_key_code;
            self.max_key_code = from.max_key_code;
        } else {
            // TODO: ensure kc between min and max
            from.key_names.iter().for_each(|(kc, name)| {
                if let Err(e) = self.add_key_name(ctx, *kc, *name, merge, true, false) {
                    self.errors.push(e);
                }
            });
        }

        // Merge key aliases
        if self.aliases.is_empty() {
            std::mem::swap(&mut self.aliases, &mut from.aliases);
        } else {
            for alias in from.aliases {
                let mut def = KeyAliasDef::create(alias.alias, alias.real);

                def.merge = match merge {
                    MergeMode::Default => alias.merge_mode,
                    _ => merge,
                };

                let merge = def.merge;
                if let Err(e) = self.handle_alias_def(ctx, def, merge) {
                    self.errors.push(e);
                }
            }
        }

        // Merge LED names
        if self.num_led_names == 0 {
            self.led_names = from.led_names;
            std::mem::swap(&mut self.num_led_names, &mut from.num_led_names);
        } else {
            for idx in 0..from.num_led_names {
                if let Some(Some(mut ledi)) = from.led_names.get(idx) {
                    ledi.merge_mode = match merge {
                        MergeMode::Default => ledi.merge_mode,
                        _ => merge,
                    };

                    if let Err(e) = self.add_led_name(ctx, ledi.merge_mode, false, ledi, idx) {
                        self.errors.push(e);
                    }
                }
            }
        }
    }

    fn handle_include_keycodes(
        &mut self,
        ctx: &mut Context,
        include: IncludeStmt,
    ) -> Result<(), CompileKeycodesError> {
        if ctx.exceeds_include_max_depth(self.include_depth) {
            let err = CompileKeycodesError::ExceedsIncludeMaxDepth(self.include_depth);
            self.unrecoverable_error = Some(err.clone());
            return Err(err);
        }

        let mut included: KeyNamesInfo = KeyNamesInfo::new(0 /*unused*/);
        included.name = Some(include.stmt);

        for stmt in include.maps.into_iter() {
            let file = ctx
                .process_include_file(&stmt, XkbFileType::Keycodes)
                .map_err(|e| {
                    self.unrecoverable_error = Some(e.clone().into());
                    e
                })?;

            let mut next_incl = KeyNamesInfo::new(self.include_depth + 1);

            next_incl.handle_keycodes_file(ctx, file, MergeMode::Override)?;
            included.merge_included_keycodes(ctx, next_incl, stmt.merge);
        }

        self.merge_included_keycodes(ctx, included, include.merge);

        if let Some(e) = self.unrecoverable_error.as_ref() {
            return Err(e.clone());
        } else if !self.errors.is_empty() {
            return Err(CompileKeycodesError::Multiple(self.errors.clone()));
        }

        Ok(())
    }

    fn handle_keycode_def(
        &mut self,
        ctx: &Context,
        stmt: KeycodeDef,
        mut merge: MergeMode,
    ) -> Result<(), CompileKeycodesError> {
        if stmt.merge != MergeMode::Default {
            if stmt.merge == MergeMode::Replace {
                merge = MergeMode::Override;
            } else {
                merge = stmt.merge;
            }
        }

        let value: u32 = match stmt.value.try_into() {
            Ok(v) if v < XKB_KEYCODE_MAX => v,
            _ => {
                log::error!(
                    "Illegal keycode; {:?} must be between 0..{}
                    Key ignored",
                    stmt.value,
                    XKB_KEYCODE_MAX
                );
                return Err(CompileKeycodesError::IllegalKeycode(stmt.value));
            }
        };

        self.add_key_name(ctx, value, stmt.name, merge, false, true)
    }

    fn handle_alias_def(
        &mut self,
        ctx: &Context,
        def: KeyAliasDef,
        merge: MergeMode,
    ) -> Result<(), CompileKeycodesError> {
        for old in self.aliases.iter_mut() {
            if old.alias == def.alias {
                if def.real == old.real {
                    log::warn!("{:?}: Alias of {} for {} declared more than once; first definition ignored.",
                            XkbWarning::ConflictingKeyName,
                            ctx.key_name_text(def.alias),
                            ctx.key_name_text(def.real))
                } else {
                    let _use = match merge {
                        MergeMode::Augment => old.real,
                        _ => def.real,
                    };

                    let ignore = match merge {
                        MergeMode::Augment => def.real,
                        _ => old.real,
                    };

                    log::warn!(
                        "{:?}: Multiple definitions for alias {}; Using {}, ignoring {}",
                        XkbWarning::ConflictingKeyName,
                        ctx.key_name_text(old.alias),
                        ctx.key_name_text(_use),
                        ctx.key_name_text(ignore)
                    );

                    old.real = _use;
                }
                old.merge_mode = merge;
                return Ok(());
            }
        }

        self.aliases.push(AliasInfo {
            merge_mode: merge,
            alias: def.alias,
            real: def.real,
        });
        Ok(())
    }

    fn handle_key_name_var(
        &mut self,
        ctx: &Context,
        stmt: VarDef,
    ) -> Result<(), CompileKeycodesError> {
        let lhs = stmt
            .name
            .and_then(|name| name.resolve_lhs(ctx))
            .ok_or(CompileKeycodesError::CouldNotResolveLhs)?;

        if let Some(ref elem) = lhs.elem {
            log::error!("{:?}: Cannot set global defaults for \"{}\" element; Assignment to \"{}.{}\" ignored",
                XkbError::GlobalDefaultsWrongScope,
                elem,
                elem,
                lhs.field);

            return Err(CompileKeycodesError::UnknownElement(elem.into()));
        }

        if !["minimum", "maximum"].contains(&lhs.field.as_str()) {
            log::error!(
                "{:?}: Unknown field encountered; assignment to field \"{}\" ignored",
                XkbMessageCode::NoId,
                lhs.field
            );

            return Err(CompileKeycodesError::UnknownField(lhs.field));
        }

        Ok(())
    }

    fn handle_led_name_def(
        &mut self,
        ctx: &Context,
        def: LedNameDef,
        merge: MergeMode,
    ) -> Result<(), CompileKeycodesError> {
        if def.ndx < 1 || def.ndx > XKB_MAX_LEDS {
            let err = CompileKeycodesError::IllegalIndicatorIndex {
                index: def.ndx,
                max: XKB_MAX_LEDS,
            };
            self.errors.push(err.clone());

            log::error!(
                "Illegal indicator index {:?} specified; must be between 1..{}",
                def.ndx,
                XKB_MAX_LEDS
            );

            return Err(err);
        }

        let name = def.name.resolve_string(ctx).ok_or_else(|| {
            ctx.report_bad_type(
                XkbError::WrongFieldType.into(),
                "indicator",
                "name",
                format!("{}", def.ndx).as_str(),
                "string",
            );

            let err = CompileKeycodesError::IndicatorBadType;
            self.errors.push(err.clone());
            err
        })?;

        let ledi = LedNameInfo {
            name,
            merge_mode: merge,
        };

        self.add_led_name(ctx, merge, true, ledi, def.ndx - 1)
    }

    fn handle_keycodes_file(
        &mut self,
        ctx: &mut Context,
        file: XkbFile,
        merge: MergeMode,
    ) -> Result<(), CompileKeycodesError> {
        self.name = Some(file.name);
        // TODO: iterate through the `ParseCommon`s in the XbkFile,
        // i.e. the defs in the list.
        for def in file.defs {
            let result = match def {
                Decl::Include(stmt)
                    => self.handle_include_keycodes(ctx,stmt),
                Decl::KeyName(stmt) //KeycodeDef
                    => self.handle_keycode_def(ctx, stmt, merge),
                Decl::KeyAlias(stmt)
                    => self.handle_alias_def(ctx, stmt,merge),
                Decl::Var(stmt)
                    => self.handle_key_name_var(ctx, stmt),
                Decl::LedName(stmt)
                    => self.handle_led_name_def(ctx, stmt, merge),
                stmt => {

                    log::error!("{:?}: Keycode files may define key and indicator names only; Ignoring {}",
                        XkbMessageCode::NoId,
                        stmt.stmt_type());

                    Err(CompileKeycodesError::WrongDeclType)
                }

            };

            if let Err(e) = result {
                self.errors.push(e);
            }

            if self.errors.len() > 10 || self.unrecoverable_error.is_some() {
                log::error!("Abandoning keycode file {:?}", self.name);
            }
        }

        Ok(())
    }
}

impl KeymapBuilder<TextV1> {
    fn copy_key_names_to_keymap(&mut self, info: &KeyNamesInfo) {
        let mut min_key_code = info.min_key_code;
        let mut max_key_code = info.max_key_code;

        if min_key_code == XKB_KEYCODE_INVALID {
            min_key_code = 8;
            max_key_code = 255;
        }

        self.min_key_code = Some(min_key_code);
        self.max_key_code = Some(max_key_code);

        for kc in min_key_code..max_key_code + 1 {
            if let Some(name) = info.key_names.get(&kc) {
                // TODO: reduce the number of
                // intermediate stages here?
                let key_builder = KeyBuilder::new(kc.into(), *name);
                self.keys.insert(kc, key_builder);
            }
        }
    }
    fn copy_key_aliases_to_keymap(&mut self, info: &KeyNamesInfo) {
        // sanity check
        let aliases = info.aliases.iter().filter_map(|alias| {
                // check that ->real is a key
                if !self.keys.values()
                    .any(|c| c.name == alias.real) {
                        log::warn!("{:?}:Attempted to alias {} to non-existent key {:?}; ignored", 
                            XkbWarning::UndefinedKeycode,
                            self.context.key_name_text(alias.alias),
                            self.context.key_name_text(alias.real)
                            );
                        return None;
                }

                //check that ->alias is not a key
                if self.keys.values()
                    .any(|c| c.name == alias.alias) {
                        log::warn!("{:?}:Attempted to create alias {} with the name of a real key {:?}; ignored", 
                            XkbWarning::IllegalKeycodeAlias,
                            self.context.key_name_text(alias.alias),
                            self.context.key_name_text(alias.real)
                            );
                        return None;
                }

                Some(KeyAlias { real: alias.real, alias: alias.alias })
            })
            .collect::<Vec<KeyAlias>>();

        //self.num_key_aliases = aliases.len();
        self.key_aliases = Some(aliases);
    }

    fn copy_led_names_to_keymap(&mut self, info: &KeyNamesInfo) {
        for idx in 0..info.num_led_names {
            if let Some(Some(ledi)) = info.led_names.get(idx) {
                self.leds[idx] = Some(Led::new(ledi.name));
            }
        }
    }

    fn copy_keynames_info(&mut self, info: KeyNamesInfo) {
        self.copy_key_names_to_keymap(&info);
        self.copy_key_aliases_to_keymap(&info);
        self.copy_led_names_to_keymap(&info);

        // TODO: does the original strdup_safe
        // provide any benefit in Rust
        // over just copying the name?
        let escaped_name = info.name;
        self.keycodes_section_name = escaped_name;
    }
}

pub(crate) fn compile_keycodes(
    builder: &mut KeymapBuilder<TextV1>,
    file: XkbFile,
    merge: MergeMode,
) -> Result<(), CompileKeycodesError> {
    let mut info = KeyNamesInfo::new(0);

    info.handle_keycodes_file(&mut builder.context, file, merge)?;

    if let Some(e) = info.unrecoverable_error {
        return Err(e);
    } else if !info.errors.is_empty() {
        return Err(CompileKeycodesError::Multiple(info.errors));
    }
    builder.copy_keynames_info(info);

    Ok(())
}

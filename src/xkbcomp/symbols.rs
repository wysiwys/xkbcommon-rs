// based on symbols.c
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
 * Author: Daniel Stone <daniel@fooishbar.org>
 *         Ran Benita <ran234@gmail.com>
 *         wysiwys
 */

use super::action::*;
use super::ast::*;

use crate::atom::Atom;
use crate::context::Context;
use crate::errors::*;
use crate::keymap::*;
use crate::keysyms::*;
use crate::rust_xkbcommon::*;

use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq)]
enum KeyRepeat {
    Undefined = 0,
    Yes = 1,
    No = 2,
}

impl KeyRepeat {
    fn lookup(s: &str) -> Option<Self> {
        let k = match s {
            "true" => KeyRepeat::Yes,
            "yes" => KeyRepeat::Yes,
            "on" => KeyRepeat::Yes,
            "false" => KeyRepeat::No,
            "no" => KeyRepeat::No,
            "off" => KeyRepeat::No,
            "default" => KeyRepeat::Undefined,
            _ => return None,
        };

        Some(k)
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug)]
    struct GroupField: u8 {
        const SYMS = 1 << 0;
        const ACTS = 1 << 1;
        const TYPE = 1 << 2;
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy)]
    struct KeyField: u8 {
        const REPEAT = 1 << 0;
        const DEFAULT_TYPE = 1 << 1;
        const GROUPINFO = 1 << 2;
        const VMODMAP = 1 << 3;
    }
}

#[derive(Clone, Debug)]
struct GroupInfo {
    defined: GroupField,
    levels: Vec<Level>,
    type_name: Option<Atom>,
}

impl GroupInfo {
    fn new() -> Self {
        Self {
            defined: GroupField::empty(),
            levels: Vec::new(),
            type_name: None,
        }
    }
}

#[derive(Clone)]
struct KeyInfo {
    defined: KeyField,
    merge: MergeMode,
    name: Atom,
    groups: BTreeMap<LayoutIndex, GroupInfo>,
    repeat: KeyRepeat,
    vmodmap: ModMask,
    default_type: Option<Atom>, // TODO: correct type?

    out_of_range_group_action: RangeExceedType,
    out_of_range_group_number: LayoutIndex,
}

impl KeyInfo {
    fn new(ctx: &mut Context) -> Self {
        Self {
            merge: MergeMode::Override,
            name: ctx.atom_intern("*"),
            out_of_range_group_action: RangeExceedType::Wrap,

            // memset 0 in original
            groups: BTreeMap::new(),
            repeat: KeyRepeat::Undefined,
            vmodmap: 0,
            default_type: None,
            defined: KeyField::empty(),
            out_of_range_group_number: 0,
        }
    }
}

#[derive(PartialEq, Debug)]
enum ModMapEntryKey {
    Name(Atom),
    Keysym(Keysym),
}

impl ModMapEntryKey {
    fn key_name_text(&self, ctx: &Context) -> String {
        use ModMapEntryKey::*;

        if let Name(atom) = self {
            ctx.key_name_text(*atom)
        } else {
            "".into()
        }
    }

    fn keysym_text(&self, ctx: &Context) -> String {
        use ModMapEntryKey::*;

        if let Keysym(sym) = self {
            ctx.keysym_text(sym)
        } else {
            "".into()
        }
    }
}

#[derive(Debug)]
struct ModMapEntry {
    merge: MergeMode,
    have_symbol: bool,
    modifier: Option<ModIndex>, // None means "don't add a modifier to the modmap"
    u: ModMapEntryKey,
}

struct SymbolsInfo {
    name: Option<String>, // e.g. pc+us+inet(evdev)
    errors: Vec<CompileSymbolsError>,
    unrecoverable_error: Option<CompileSymbolsError>,
    include_depth: u32,
    _merge: MergeMode, // TODO: should this be unused?
    explicit_group: Option<LayoutIndex>,
    keys: Vec<KeyInfo>,
    default_key: KeyInfo,
    actions: ActionsInfo,
    group_names: BTreeMap<LayoutIndex, Atom>,
    modmaps: Vec<ModMapEntry>,
    mods: ModSet,
}

impl SymbolsInfo {
    fn new(ctx: &mut Context, include_depth: u32, actions: ActionsInfo, mods: ModSet) -> Self {
        Self {
            _merge: MergeMode::Override,
            default_key: KeyInfo::new(ctx),
            actions,
            mods,
            explicit_group: None,

            errors: Vec::new(),
            unrecoverable_error: None,

            include_depth,
            // memset 0 in original
            group_names: BTreeMap::new(),
            name: None,
            keys: Vec::new(),
            modmaps: Vec::new(),
        }
    }
}

impl KeyInfo {
    fn info_text(&self, ctx: &Context) -> String {
        ctx.key_name_text(self.name)
    }
}

impl GroupInfo {
    fn merge(
        &mut self,
        ctx: &Context,
        from: &mut GroupInfo,
        clobber: bool,
        report: bool,
        group: LayoutIndex,
        key_name: Atom,
    ) -> Result<(), CompileSymbolsError> {
        // Find the type of the merged group
        if self.type_name != from.type_name {
            if from.type_name.is_none() {
                // Empty for consistency with other comparisons
            } else if self.type_name.is_none() {
                self.type_name = from.type_name;
            } else {
                let to_use = if clobber {
                    from.type_name
                } else {
                    self.type_name
                };
                let to_ignore = if clobber {
                    self.type_name
                } else {
                    from.type_name
                };

                if report {
                    log::warn!("{:?}: Multiple definitions for group {:?} type of key {:?}; Using {:?}, ignoring {:?}",
                        XkbWarning::ConflictingKeyTypeMergingGroups,
                        group + 1, ctx.key_name_text(key_name),
                        ctx.xkb_atom_text(to_use.unwrap_or(0)),
                        ctx.xkb_atom_text(to_ignore.unwrap_or(0)));
                }
                self.type_name = to_use;
            }
        }
        self.defined.insert(from.defined & GroupField::TYPE);

        // Now look at the levels

        if from.levels.is_empty() {
            return Ok(());
        }

        if self.levels.is_empty() {
            from.type_name = self.type_name;
            *self = from.clone();
            *from = GroupInfo::new();
            return Ok(());
        }

        // Merge the actions and syms
        // If `from` has extra levels, get them as well
        for (i, from_level) in from.levels.iter_mut().enumerate() {
            if self.levels.get_mut(i).is_none() {
                self.levels.push(from_level.clone());
            } else if let Some(into_level) = self.levels.get_mut(i) {
                if from_level.action.action_type() == ActionType::None {
                    // Empty for consistency with other comparisons
                } else if into_level.action.action_type() == ActionType::None {
                    into_level.action = from_level.action.clone();
                } else {
                    let (_use, ignore) = match clobber {
                        true => (&from_level.action, &into_level.action),
                        false => (&into_level.action, &from_level.action),
                    };

                    if report {
                        log::warn!("{:?}: Multiple actions for level {}/group {} on key {}; Using {:?}, ignoring {:?}",
                            XkbWarning::ConflictingKeyAction,
                            i + 1, group + 1,
                            ctx.key_name_text(key_name),
                            _use.action_type(),
                            ignore.action_type());
                    }

                    into_level.action = _use.clone();
                }

                if from_level.num_syms() == 0 {
                    // empty for consistency with other comparisons
                } else if into_level.num_syms() == 0 {
                    into_level.syms.clone_from(&from_level.syms);
                } else if from_level.syms != into_level.syms {
                    if report {
                        log::warn!("{:?}: Multiple symbols for level {}/group {} on key {}; Using {}, ignoring {}",
                            XkbWarning::ConflictingKeySymbol,
                            i + 1, group + 1, ctx.key_name_text(key_name),
                            match clobber { true => "from", false => "to" },
                            match clobber { true => "to", false => "from" }
                        );
                    }

                    if clobber {
                        *into_level = Level {
                            action: Action::None,
                            syms: vec![],
                        };
                        into_level.syms.clone_from(&from_level.syms);
                        from_level.syms = vec![];
                    }
                }
            }
        }

        self.defined |= from.defined.intersection(GroupField::ACTS);
        self.defined |= from.defined.intersection(GroupField::SYMS);

        Ok(())
    }
}

fn use_new_key_field(
    field: KeyField,
    old: KeyField,
    new: KeyField,
    clobber: bool,
    collide: &mut KeyField,
) -> bool {
    if !old.intersects(field) {
        return new.intersects(field);
    }

    if new.intersects(field) {
        collide.insert(field);

        if clobber {
            return true;
        }
    }

    false
}

#[derive(Debug, PartialEq)]
enum LookupType {
    Symbols,
    Actions,
}

impl KeyInfo {
    fn merge(
        &mut self,
        ctx: &Context,
        from: &mut KeyInfo,
        same_file: bool,
    ) -> Result<(), CompileSymbolsError> {
        let mut collide = KeyField::empty();
        let verbosity = ctx.get_log_verbosity();
        let clobber = from.merge != MergeMode::Augment;
        let report = (same_file && verbosity > 0) || verbosity > 9;

        if from.merge == MergeMode::Replace {
            *self = from.clone();
            return Ok(());
        }

        //If `from` has extra groups, just move them to `into`
        for (i, g_from) in from.groups.iter_mut() {
            if let Some(g_into) = self.groups.get_mut(i) {
                // merge group
                g_into.merge(ctx, g_from, clobber, report, *i, self.name)?;
            } else {
                self.groups.insert(*i, g_from.clone());
            }
        }

        if use_new_key_field(
            KeyField::VMODMAP,
            self.defined,
            from.defined,
            clobber,
            &mut collide,
        ) {
            self.vmodmap = from.vmodmap;
            self.defined.insert(KeyField::VMODMAP);
        }

        if use_new_key_field(
            KeyField::REPEAT,
            self.defined,
            from.defined,
            clobber,
            &mut collide,
        ) {
            self.repeat = from.repeat;
            self.defined.insert(KeyField::REPEAT);
        }
        if use_new_key_field(
            KeyField::DEFAULT_TYPE,
            self.defined,
            from.defined,
            clobber,
            &mut collide,
        ) {
            self.default_type = from.default_type;
            self.defined.insert(KeyField::DEFAULT_TYPE);
        }
        if use_new_key_field(
            KeyField::GROUPINFO,
            self.defined,
            from.defined,
            clobber,
            &mut collide,
        ) {
            self.out_of_range_group_action = from.out_of_range_group_action.clone();
            self.out_of_range_group_number = from.out_of_range_group_number;
            self.defined.insert(KeyField::GROUPINFO);
        }

        if report && !collide.is_empty() {
            let err = XkbWarning::ConflictingKeyFields;
            log::warn!(
                "{:?}: Symbol map for key {} redefined;
                 Using {} definition for conflicting fields",
                err,
                ctx.key_name_text(self.name),
                if clobber { "first" } else { "last" }
            );
        }

        Ok(())
    }
}
impl SymbolsInfo {
    fn add_key_symbols(
        &mut self,
        builder: &KeymapBuilder<TextV1>,
        mut keyi: KeyInfo,
        same_file: bool,
    ) -> Result<(), CompileSymbolsError> {
        // Don't keep aliases in the keys array;
        // this guarantees that searching for keys to merge
        // with by straight comparison (see the following loop)
        // is enough, and we won't get multiple `KeyInfo`s
        // for the same key because of aliases.

        // If got the name of real key,
        // set keyi's name to that for the search
        if let Some(real_name) = builder.resolve_key_alias(keyi.name) {
            keyi.name = real_name;
        }

        for iter in &mut self.keys {
            if iter.name == keyi.name {
                return iter.merge(&builder.context, &mut keyi, same_file);
            }
        }

        self.keys.push(keyi);

        Ok(())
    }

    fn add_mod_map_entry(
        &mut self,
        ctx: &Context,
        new: ModMapEntry,
    ) -> Result<(), CompileSymbolsError> {
        let clobber = new.merge != MergeMode::Augment;

        for old in self.modmaps.iter_mut() {
            if new.have_symbol != old.have_symbol || new.u != old.u
            // Just compare the enum variants
            {
                continue;
            }

            if new.modifier == old.modifier {
                return Ok(());
            }

            let _use = if clobber { new.modifier } else { old.modifier };
            let ignore = if clobber { old.modifier } else { new.modifier };

            if new.have_symbol {
                log::warn!("{:?}: Symbol \"{}\" added to modifier mask for multiple modifiers; Using {}, ignoring {}", XkbWarning::ConflictingModmap,
                    new.u.keysym_text(ctx),
                    ctx.mod_index_text(&self.mods, _use.unwrap_or(0)),
                    ctx.mod_index_text(&self.mods, ignore.unwrap_or(0)));
            } else {
                log::warn!("{:?}: Key \"{}\" added to modifier map for multiple modifiers; Using {}, ignoring {}",
                    XkbWarning::ConflictingModmap,
                    new.u.key_name_text(ctx),
                    ctx.mod_index_text(&self.mods,
                        _use.unwrap_or(0) ),
                    ctx.mod_index_text(&self.mods,
                        ignore.unwrap_or(0)) );
            }

            old.modifier = _use;
            return Ok(());
        }

        self.modmaps.push(new);

        Ok(())
    }

    fn merge_included_symbols(
        &mut self,
        builder: &KeymapBuilder<TextV1>,
        mut from: SymbolsInfo,
        merge: MergeMode,
    ) {
        // TODO: does order matter?
        if let Some(err) = from.unrecoverable_error {
            self.unrecoverable_error = Some(err);
            return;
        }
        if !from.errors.is_empty() {
            self.errors.append(&mut from.errors);
            return;
        }

        self.mods = from.mods;

        if self.name.is_none() {
            self.name = from.name;
        }

        for (i, from_group) in from.group_names.iter() {
            if let Some(group) = self.group_names.get_mut(i) {
                if merge == MergeMode::Augment {
                    continue;
                }

                *group = *from_group;
            } else {
                // if `from` has more, get them as well
                self.group_names.insert(*i, *from_group);
            }
        }

        if self.keys.is_empty() {
            self.keys = from.keys;
        } else {
            for mut keyi in from.keys {
                keyi.merge = match merge {
                    MergeMode::Default => keyi.merge,
                    _ => merge,
                };

                if let Err(e) = self.add_key_symbols(builder, keyi, false) {
                    self.errors.push(e);
                }
            }
        }

        if self.modmaps.is_empty() {
            self.modmaps = from.modmaps;
        } else {
            for mut mm in from.modmaps {
                mm.merge = match merge {
                    MergeMode::Default => mm.merge,
                    _ => merge,
                };

                if let Err(e) = self.add_mod_map_entry(&builder.context, mm) {
                    self.errors.push(e);
                }
            }
        }
    }

    fn handle_include_symbols(
        &mut self,
        builder: &mut KeymapBuilder<TextV1>,
        include: IncludeStmt,
    ) -> Result<(), CompileSymbolsError> {
        let first_merge = match include.maps.first() {
            Some(stmt) => stmt.merge,
            None => include.merge,
        };

        let mut included = SymbolsInfo::new(
            &mut builder.context,
            0, //unused
            self.actions.clone(),
            self.mods.clone(),
        );

        for stmt in include.maps.iter() {
            // TODO: make sure the error message contains the file and/or depth
            //  See `process_include_file`
            let file = builder
                .context
                .process_include_file(stmt, XkbFileType::Symbols)
                .map_err(|e| {
                    self.unrecoverable_error = Some(e.clone().into());
                    e
                })?;

            let mut next_incl = SymbolsInfo::new(
                &mut builder.context,
                self.include_depth + 1,
                self.actions.clone(),
                included.mods.clone(),
            );

            if let Some(modifier) = stmt.modifier.as_ref() {
                let group: LayoutIndex = modifier.parse().unwrap();
                let explicit_group = group - 1;
                next_incl.explicit_group = Some(explicit_group);
                if explicit_group >= XKB_MAX_GROUPS.into() {
                    log::error!("{:?}: Cannot set explicit group to {} - must be between 1..{}; Ignoring group number", 
                            XkbError::UnsupportedGroupIndex,
                            explicit_group + 1,
                            XKB_MAX_GROUPS);

                    next_incl.explicit_group = self.explicit_group;
                }
            } else {
                next_incl.explicit_group = self.explicit_group;
            }

            next_incl.handle_symbols_file(builder, file, MergeMode::Override);

            included.merge_included_symbols(builder, next_incl, stmt.merge);
        }

        // moved from above
        included.name = Some(include.stmt);

        self.merge_included_symbols(builder, included, first_merge);

        if let Some(err) = self.unrecoverable_error.as_ref() {
            Err(err.clone())
        } else if !self.errors.is_empty() {
            Err(CompileSymbolsError::MultipleErrors(self.errors.clone()))
        } else {
            Ok(())
        }
    }
}

impl KeyInfo {
    fn get_group_index(
        &mut self,
        ctx: &Context,
        array_ndx: Option<ExprDef>,
        what: LookupType,
    ) -> Result<LayoutIndex, CompileSymbolsError> {
        use LookupType::*;

        if array_ndx.is_none() {
            let field = match what {
                Symbols => GroupField::SYMS,
                Actions => GroupField::ACTS,
            };

            for (i, groupi) in self.groups.iter() {
                if !groupi.defined.intersects(field) {
                    return Ok(*i);
                }
            }

            if self.groups.len() >= XKB_MAX_GROUPS.into() {
                log::error!("{:?}: Too many groups of {:?} for key {} (max {}); Ignoring {:?} defined for extra groups",
                XkbError::UnsupportedGroupIndex,
                what, self.info_text(ctx),
                XKB_MAX_GROUPS,
                what);

                return Err(CompileSymbolsError::TooManyGroups);
            }

            // make a new group at the end
            // and return its index
            let next_idx = self.groups.keys().max().map(|n| n + 1).unwrap_or(0);

            let groupi = GroupInfo::new();
            self.groups.insert(next_idx, groupi);

            Ok(next_idx)
        } else {
            let group = array_ndx
                .unwrap()
                .resolve_group(ctx)
                .map(|g| g - 1)
                .ok_or_else(|| {
                    log::error!(
                        "{:?}: Illegal group index for {:?} of key {}
                    Definition with non-integer array index ignored",
                        XkbError::UnsupportedGroupIndex,
                        what,
                        self.info_text(ctx)
                    );

                    CompileSymbolsError::IllegalGroupIndex
                })?;

            self.groups.entry(group).or_insert_with(GroupInfo::new);
            Ok(group)
        }
    }
}
impl KeyInfo {
    fn add_symbols_to_key(
        &mut self,
        ctx: &Context,
        array_ndx: Option<ExprDef>,
        value: ExprDef,
    ) -> Result<(), CompileSymbolsError> {
        let ndx = self.get_group_index(ctx, array_ndx, LookupType::Symbols)?;

        // get reference to the group
        // this group may have just been created,
        // and its levels may therefore be empty
        let groupi = self
            .groups
            .get_mut(&ndx)
            .expect("Group should have been created");

        let value = match value {
            ExprDef::KeysymList(value) => value,
            _ => {
                let err = XkbError::WrongFieldType;
                log::error!(
                    "{:?}: Expected a list of symbols, found {:?}; Ignoring symbols for group {} of {}", err, value.op_type(),
                    ndx + 1, self.info_text(ctx));

                return Err(CompileSymbolsError::WrongOpType {
                    expected: ExprOpType::KeysymList,
                    found: value.op_type(),
                });
            }
        };

        if groupi.defined.intersects(GroupField::SYMS) {
            let err = XkbError::ConflictingKeySymbolsEntry;

            log::error!(
                "{:?}: Symbols for key {}, group {} already defined; Ignoring duplicate definition",
                err,
                self.info_text(ctx),
                ndx + 1
            );

            return Err(CompileSymbolsError::DuplicateSymbolsDef {
                key: self.info_text(ctx),
                group: ndx + 1,
            });
        }

        groupi.defined.insert(GroupField::SYMS);

        // TODO: check order here
        for syms_list in value.syms_lists.into_iter() {
            let leveli = Level {
                action: Action::None,
                syms: syms_list,
            };

            groupi.levels.push(leveli);
        }

        Ok(())
    }

    fn add_actions_to_key(
        &mut self,
        ctx: &Context,
        mods: &ModSet,
        actions_info: &mut ActionsInfo,
        array_ndx: Option<ExprDef>,
        value: ExprDef,
    ) -> Result<(), CompileSymbolsError> {
        let info_text = self.info_text(ctx);

        let ndx = self.get_group_index(ctx, array_ndx, LookupType::Actions)?;

        let groupi = self.groups.get_mut(&ndx).expect("Group not found");

        let expr = match value {
            ExprDef::Actions(expr) if expr.op == ExprOpType::ActionList => expr,
            expr => {
                log::warn!("{:?}: Bad expression type ({:?}) for action list value; Ignoring actions for group {} of {}",
                    XkbMessageCode::NoId,
                    expr.op_type(),
                    ndx,
                    self.info_text(ctx));

                return Err(CompileSymbolsError::WrongOpType {
                    expected: ExprOpType::ActionList,
                    found: expr.op_type(),
                });
            }
        };

        if groupi.defined.intersects(GroupField::ACTS) {
            log::warn!(
                "{:?}: Actions for key {}, group {} already defined",
                XkbMessageCode::NoId,
                self.info_text(ctx),
                ndx
            );

            return Err(CompileSymbolsError::DuplicateActionsDef {
                key: self.info_text(ctx),
                group: ndx,
            });
        }

        let mut n_acts = 0;
        let mut actions = expr.actions.into_iter();
        let mut action: Option<ExprDef> = actions.next();
        while action.is_some() {
            n_acts += 1;

            for _ in groupi.levels.len()..n_acts {
                groupi.levels.push(Level {
                    action: Action::None,
                    syms: vec![],
                });
            }

            groupi.defined |= GroupField::ACTS;

            for i in 0..n_acts {
                let current_action = match action {
                    Some(a) => a,
                    None => break,
                };

                let to_act = &mut groupi.levels.get_mut(i).unwrap().action;
                let val = actions_info.handle_action_def(ctx, mods, current_action);
                // TODO: add to errors list?
                if let Ok(action) = val {
                    *to_act = action;
                } else {
                    log::error!("{:?}: Illegal action definition for {}; Action for group {}/level {} ignored",
                            XkbError::InvalidValue,
                            info_text,
                            ndx + 1,
                            i + 1);
                }
                action = actions.next();
            }
        }

        Ok(())
    }

    fn set_symbols_field(
        &mut self,
        ctx: &Context,
        mods: &ModSet,
        actions_info: &mut ActionsInfo,
        field: String,
        array_ndx: Option<ExprDef>,
        value: ExprDef,
    ) -> Result<(), CompileSymbolsError> {
        let field = field.to_lowercase();
        let field_str = field.as_str();

        if field_str == "type" {
            let type_name: Atom = value.resolve_string(ctx)
            .ok_or_else(|| {
                    log::error!("{:?}: The type field of a key symbol map must be a string; Ignoring illegal type definition",
                        XkbError::WrongFieldType);

                    CompileSymbolsError::CouldNotResolveString
            })?;

            if array_ndx.is_none() {
                self.default_type = Some(type_name);
                self.defined.insert(KeyField::DEFAULT_TYPE);
            } else if let Some(mut ndx) = array_ndx.unwrap().resolve_group(ctx) {
                ndx -= 1;

                // insert or update
                if let Some(ref mut groupi) = self.groups.get_mut(&ndx) {
                    groupi.type_name = Some(type_name);
                    groupi.defined.insert(GroupField::TYPE);
                } else {
                    self.groups.insert(
                        ndx,
                        GroupInfo {
                            type_name: Some(type_name),
                            defined: GroupField::TYPE,
                            levels: Vec::new(),
                        },
                    );
                }
            } else {
                log::error!("{:?}: Illegal group index for type of key {:?}; Definition with non-integer array index ignored",
                    XkbError::UnsupportedGroupIndex,
                    self.info_text(ctx)
                );

                return Err(CompileSymbolsError::UnsupportedGroupIndex);
            }
        } else if field_str == "symbols" {
            return self.add_symbols_to_key(ctx, array_ndx, value);
        } else if field_str == "actions" {
            return self.add_actions_to_key(ctx, mods, actions_info, array_ndx, value);
        } else if ["vmods", "virtualmods", "virtualmodifiers"].contains(&field_str) {
            let op = value.op_type();
            self.vmodmap = value.resolve_mod_mask(ctx, ModType::VIRT, mods)
                .ok_or_else(|| {
                    log::error!("{:?}: Expected a virtual modifier mask, found {:?}; Ignoring virtual modifiers definition for key {:?}",
                        XkbError::UnsupportedModifierMask,
                        op,
                        self.info_text(ctx));

                    CompileSymbolsError::ExpectedVModMask
                })?;

            self.defined.insert(KeyField::VMODMAP);
        } else if ["locking", "lock", "locks"].contains(&field_str) {
            log::warn!(
                "{:?}: Key behaviors not supported; Ignoring locking specification for key {:?}",
                XkbWarning::UnsupportedSymbolsField,
                self.info_text(ctx)
            );
        } else if ["radiogroup", "permanentradiogroup", "allownone"].contains(&field_str) {
            log::warn!(
                "{:?}: Radio groups not supported; Ignoring radio group specification for key {:?}",
                XkbWarning::UnsupportedSymbolsField,
                self.info_text(ctx)
            );
        } else if field_str.starts_with("overlay") || field_str.starts_with("permanentoverlay") {
            log::warn!(
                "{:?}: Overlays not supported; Ignoring overlay specification for key {:?}",
                XkbWarning::UnsupportedSymbolsField,
                self.info_text(ctx)
            );
        } else if ["repeating", "repeats", "repeat"].contains(&field_str) {
            // TODO: check case
            let val = value.resolve_enum(ctx, KeyRepeat::lookup).ok_or_else(|| {
                log::error!(
                    "{:?}: Illegal repeat setting for {:?}; Non-boolean repeat setting ignored",
                    XkbError::InvalidValue,
                    self.info_text(ctx)
                );
                CompileSymbolsError::IllegalRepeatSetting
            })?;

            self.repeat = val;
            self.defined.insert(KeyField::REPEAT);
        } else if ["groupswrap", "wrapgroups"].contains(&field_str) {
            let set = value.resolve_boolean(ctx).ok_or_else(|| {
                log::error!(
                    "{:?}: Illegal groupsWrap setting for {}; Non-boolean value ignored",
                    XkbError::InvalidValue,
                    self.info_text(ctx)
                );

                CompileSymbolsError::IllegalGroupsWrap
            })?;

            self.out_of_range_group_action = match set {
                true => RangeExceedType::Wrap,
                false => RangeExceedType::Saturate,
            };

            self.defined.insert(KeyField::GROUPINFO);
        } else if ["groupsclamp", "clampgroups"].contains(&field_str) {
            let set = value.resolve_boolean(ctx).ok_or_else(|| {
                log::error!(
                    "{:?}: Illegal groupsClamp setting for {}; Non-boolean value ignored",
                    XkbError::InvalidValue,
                    self.info_text(ctx)
                );

                CompileSymbolsError::IllegalGroupsClamp
            })?;

            self.out_of_range_group_action = match set {
                true => RangeExceedType::Saturate,
                false => RangeExceedType::Wrap,
            };

            self.defined.insert(KeyField::GROUPINFO);
        } else if ["groupsredirect", "redirectgroups"].contains(&field_str) {
            let grp = value.resolve_group(ctx)
                .ok_or_else(|| {

                    log::error!("{:?}: Illegal group index for redirect of key {}; Definition with non-integer group ignored",
                        XkbError::UnsupportedGroupIndex,
                        self.info_text(ctx)
                    );

                    CompileSymbolsError::IllegalGroupIndexForRedirect

                })?;

            self.out_of_range_group_action = RangeExceedType::Redirect;
            self.out_of_range_group_number = grp - 1;
            self.defined.insert(KeyField::GROUPINFO);
        } else {
            log::error!(
                "{:?}: Unknown field {} in a symbol interpretation; Definition ignored",
                XkbError::UnknownField,
                field
            );
            return Err(CompileSymbolsError::UnknownFieldInSymInterp(field));
        }

        Ok(())
    }
}

impl SymbolsInfo {
    fn set_group_name(
        &mut self,
        ctx: &Context,
        array_ndx: Option<ExprDef>,
        value: ExprDef,
    ) -> Result<(), CompileSymbolsError> {
        let group = array_ndx
            .ok_or_else(|| {
            log::warn!("{:?}: You must specify an index when specifying a group name; Group name definition without array subscript ignored", XkbWarning::MissingSymbolsGroupNameIndex);
            CompileSymbolsError::IndexUnspecified
            })?
        .resolve_group(ctx)
        .ok_or_else(|| {
                log::error!("{:?}: Illegal index in group name definition; Definition with non-integer array index ignored",XkbError::UnsupportedGroupIndex);
                CompileSymbolsError::IllegalIndexInGroupNameDef
            })?;

        let name = value.resolve_string(ctx).ok_or_else(|| {
            log::error!(
                "{:?}: Group name must be a string; Illegal name for group {} ignored",
                XkbError::WrongFieldType,
                group
            );
            CompileSymbolsError::IllegalGroupName
        })?;

        let group_to_use;

        if self.explicit_group.is_none() {
            group_to_use = group - 1;
        } else if group - 1 == 0 {
            group_to_use = self.explicit_group.unwrap();
        } else {
            let warn = XkbWarning::NonBaseGroupName;
            log::warn!("{:?}: An explicit group was specified for the '{:?}' map, but it provides a name for a group other than Group1 ({}); Ignoring group name '{:?}'", 
                    warn, &self.name, group,
                    ctx.xkb_atom_text(name));
            return Err(CompileSymbolsError::NonBaseGroupName);
        }

        self.group_names.insert(group_to_use, name);

        Ok(())
    }

    fn handle_global_var(
        &mut self,
        ctx: &Context,
        stmt: VarDef,
    ) -> Result<(), CompileSymbolsError> {
        let ret: Result<(), CompileSymbolsError>;

        let lhs = stmt
            .name
            .and_then(|name| name.resolve_lhs(ctx))
            .ok_or(CompileSymbolsError::CouldNotResolveLhs)?;

        let elem = lhs.elem.map(|e| e.to_lowercase());
        let field = lhs.field.to_lowercase();
        let field_str = field.as_str();

        if elem == Some("key".into()) {
            ret = self.default_key.set_symbols_field(
                ctx,
                &self.mods,
                &mut self.actions,
                lhs.field,
                lhs.index,
                stmt.value,
            );
        } else if elem.is_none() && ["name", "groupname"].contains(&field_str) {
            ret = self.set_group_name(ctx, lhs.index, stmt.value);
        } else if elem.is_none() && ["groupswrap", "wrapgroups"].contains(&field_str) {
            log::error!(
                "{:?}: Global \"groupswrap\" not supported; Ignored",
                XkbWarning::UnsupportedSymbolsField
            );
            ret = Ok(());
        } else if elem.is_none() && ["groupsclamp", "clampgroups"].contains(&field_str) {
            log::error!(
                "{:?}: Global \"groupsclamp\" not supported; Ignored",
                XkbWarning::UnsupportedSymbolsField
            );
            ret = Ok(());
        } else if elem.is_none() && ["groupsredirect", "redirectgroups"].contains(&field_str) {
            log::error!(
                "{:?}: Global \"groupsredirect\" not supported; Ignored",
                XkbWarning::UnsupportedSymbolsField
            );
            ret = Ok(());
        } else if elem.is_none() && field_str == "allownone" {
            log::error!(
                "{:?}: Radio groups not supported; Ignoring \"allownone\" specification",
                XkbWarning::UnsupportedSymbolsField
            );
            ret = Ok(());
        } else {
            ret = self
                .actions
                .set_action_field(ctx, &self.mods, elem, field_str, lhs.index, stmt.value)
                .map_err(|error| CompileSymbolsError::SetActionFieldFailed { error });
        }

        ret
    }

    fn handle_symbols_body(
        &mut self,
        ctx: &Context,
        defs: Vec<VarDef>,
        keyi: &mut KeyInfo,
    ) -> Result<(), CompileSymbolsError> {
        let mut ok = Ok(());

        for def in defs {
            let field;
            let array_ndx;
            let elem;

            if let Some(name) = def.name {
                if let Some(lhs) = name.resolve_lhs(ctx) {
                    field = lhs.field;
                    array_ndx = lhs.index;
                    elem = lhs.elem;

                    ok = Ok(())
                } else {
                    ok = Err(CompileSymbolsError::CouldNotResolveLhs);
                    // nothing else is done with this value, so just continue
                    continue;
                }

                if let Some(elem) = elem {
                    log::error!("{:?}: Cannot set global defaults for \"{}\" element within a key statement: move statements to the global file scope. Assignment to \"{}.{}\" ignored.",
                        XkbError::GlobalDefaultsWrongScope,
                        elem, elem, field);

                    ok = Err(CompileSymbolsError::GlobalDefaultsWrongScope);
                    continue;
                }

            // case where no name
            } else {
                // TODO: ensure we don't need to check for None
                // TODO: potentially make `field` an enum
                if def.value.op_type() == ExprOpType::KeysymList {
                    field = "symbols".into();
                } else {
                    field = "actions".into();
                }
                array_ndx = None;
            }

            if ok.is_ok() {
                ok = keyi.set_symbols_field(
                    ctx,
                    &self.mods,
                    &mut self.actions,
                    field,
                    array_ndx,
                    def.value,
                );
            }
        }

        ok
    }

    fn set_explicit_group(
        &self,
        ctx: &Context,
        keyi: &mut KeyInfo,
    ) -> Result<(), CompileSymbolsError> {
        let mut warn: bool = false;

        let explicit_group = match self.explicit_group {
            Some(group) => group,
            None => return Ok(()),
        };

        // clear all groups
        let k = keyi.groups.keys().cloned().collect::<Vec<_>>();
        for i in k {
            if i == 0 {
                continue;
            }

            if !keyi.groups[&i].defined.is_empty() {
                warn = true;
                keyi.groups.remove(&i);
            }
        }

        if warn {
            log::warn!("{:?}: For the map {} an explicit group specified, but key {} has more than one group defined; All groups except first one will be ignored",
                    XkbWarning::MultipleGroupsAtOnce,
                    self.name
                        .as_deref()
                        .unwrap_or(""),
                    keyi.info_text(ctx));
        }

        // TODO: is this right?
        if explicit_group > 0 {
            if let Some(group0_prev) = keyi.groups.remove(&0) {
                keyi.groups.insert(explicit_group, group0_prev);
            }

            //keyi.groups.insert(0, GroupInfo::new());
        }

        Ok(())
    }

    fn handle_symbols_def(
        &mut self,
        builder: &KeymapBuilder<TextV1>,
        stmt: SymbolsDef,
    ) -> Result<(), CompileSymbolsError> {
        let mut keyi = self.default_key.clone();
        keyi.merge = stmt.merge;
        keyi.name = stmt.key_name;

        // copy defaults
        for (i, groupi) in keyi.groups.iter_mut() {
            if let Some(default) = self.default_key.groups.get(i) {
                *groupi = default.clone();
            }
        }

        if let Err(e) = self.handle_symbols_body(&builder.context, stmt.symbols, &mut keyi) {
            self.errors.push(e.clone());
            return Err(e);
        };

        if let Err(e) = self.set_explicit_group(&builder.context, &mut keyi) {
            self.errors.push(e.clone());
            return Err(e);
        };

        if let Err(e) = self.add_key_symbols(builder, keyi, true) {
            self.errors.push(e.clone());
            return Err(e);
        };

        Ok(())
    }

    fn handle_mod_map_def(
        &mut self,
        ctx: &Context,
        def: ModMapDef,
    ) -> Result<(), CompileSymbolsError> {
        let modifier_name = ctx.atom_text(def.modifier).map(|n| n.to_lowercase());

        let ndx = match modifier_name {
            Some(n) if n.as_str() == "none" => None,
            _ => self.mods.mod_name_to_index(def.modifier, ModType::REAL)
                .ok_or_else(|| {
                    log::error!("{:?}: Illegal modifier map definition; Ignoring map for non-modifier \"{:?}\"",
                        XkbError::InvalidRealModifier,
                        ctx.xkb_atom_text(def.modifier)
                    );
                    CompileSymbolsError::InvalidRealModifier
                })?.into()
            };

        let mut ok = Ok(());

        for key in def.keys {
            let have_symbol;
            let u;

            match key {
                ExprDef::KeyName(e)
                    if key.op_type() == ExprOpType::Value
                        && key.value_type() == ExprValueType::Keyname =>
                {
                    have_symbol = false;
                    u = ModMapEntryKey::Name(e.key_name);
                }
                expr => {
                    if let Some(sym) = expr.resolve_keysym(ctx) {
                        have_symbol = true;
                        u = ModMapEntryKey::Keysym(sym);
                    } else {
                        log::error!("{:?}: Modmap entries may contain only key names or keysyms; Illegal definition for {} modifier ignored",
                        XkbError::InvalidModmapEntry,
                        ndx.map(|ndx| ctx.mod_index_text(&self.mods, ndx)).unwrap_or(""));

                        continue;
                    }
                }
            }

            let tmp = ModMapEntry {
                modifier: ndx,
                have_symbol,
                u,
                merge: def.merge,
            };

            ok = self.add_mod_map_entry(ctx, tmp);
        }

        ok
    }

    fn handle_symbols_file(
        &mut self,
        builder: &mut KeymapBuilder<TextV1>,
        file: XkbFile,
        merge: MergeMode,
    ) {
        let mut ok;

        self.name = Some(file.name);

        for stmt in file.defs {
            ok = match stmt {
                Decl::Include(s) => self.handle_include_symbols(builder, s),
                Decl::Symbols(s) => self.handle_symbols_def(builder, s),
                Decl::Var(s) => self.handle_global_var(&builder.context, s),
                Decl::VMod(s) => self
                    .mods
                    .handle_vmod_def(&builder.context, s, merge)
                    .map_err(|e| e.into()),
                Decl::ModMap(s) => self.handle_mod_map_def(&builder.context, s),
                _ => {
                    log::error!(
                        "{:?}: Symbols file may not include other types; Ignoring {}",
                        XkbError::WrongStatementType,
                        stmt.stmt_type()
                    );
                    Err(CompileSymbolsError::WrongStatementType(stmt.stmt_type()))
                }
            };

            // TODO: in include, the error is pushed twice
            if let Err(e) = ok {
                self.errors.push(e);
            }

            if self.unrecoverable_error.is_some() || self.errors.len() > 10 {
                log::error!(
                    "{:?}: Abandoning symbols file \"{}\"",
                    XkbError::InvalidSyntax,
                    self.name.as_ref().unwrap()
                );
                break;
            }
        }
    }
}

impl KeymapBuilder<TextV1> {
    /// Given a keysym @sym, return a key which generates it,
    /// or None. This is used for example in a modifier
    /// map definition, such as: modifier_map Lock { Caps_Lock };
    /// where we want to add the Lock modifier to the modmap of the key
    /// which matches the keysym Caps_Lock.
    /// Since there can be many keys which generate the keysym,
    /// the key is chosen first by lowest group in which the keysym
    /// appears, then by lowest level, then by lowest key code.
    fn find_key_for_symbol_mut(&mut self, target_sym: Keysym) -> Option<&mut KeyBuilder> {
        self.get_matching_keycode(target_sym)
            .and_then(|kc| self.keys.get_mut(&kc.raw()))
    }

    // helper function for find_key_for_symbol_mut()
    fn get_matching_keycode(&self, target_sym: Keysym) -> Option<Keycode> {
        let mut current_group = 0;

        // group loop (run once for each group)
        loop {
            let mut current_level = 0;
            let mut no_groups_found_for_this_index = true;
            // level loop (run once for each level)
            loop {
                let mut no_levels_found_for_this_index = true;
                // NOTE: keycodes are in order (B-Tree map)
                for (kc, key) in self.keys.iter() {
                    if current_group >= key.num_groups() {
                        continue;
                    }
                    // found a valid group
                    no_groups_found_for_this_index = false;

                    let key_num_levels = key.num_levels(current_group, self).unwrap_or(0);
                    if current_level >= key_num_levels {
                        continue;
                    }

                    // found a valid level for this group
                    no_levels_found_for_this_index = false;

                    let syms = key
                        .groups
                        .data()
                        .and_then(|d| d.get(current_group))
                        .and_then(|g| g.levels.get(current_level))
                        .map(|l| &l.syms);

                    match syms {
                        Some(syms) if syms.contains(&Some(target_sym)) => {
                            return Some(Keycode(*kc));
                        }
                        _ => continue,
                    }
                }
                current_level += 1;
                if no_levels_found_for_this_index {
                    // return to group loop
                    break;
                }
            }
            current_group += 1;
            if no_groups_found_for_this_index {
                break;
            }
        }

        None
    }

    fn find_type_for_group(&mut self, keyi: &KeyInfo, group: LayoutIndex) -> (bool, usize) {
        let groupi = keyi.groups.get(&group).expect("No such group");

        let mut explicit_type = true;

        let type_name = groupi.type_name.or_else(|| {
            keyi.default_type.or_else(|| {
                let type_name = groupi.find_automatic_type(&mut self.context);
                if type_name.is_some() {
                    explicit_type = false;
                }

                type_name
            })
        });

        let type_name = match type_name {
            Some(n) => n,
            None => {
                log::warn!("{:?}: Couldn't find an automatic type for key '{}' group '{} with {} levels; Using the default type", 
                    XkbWarning::CannotInferKeyType,
                    self.context.key_name_text(keyi.name),
                        group + 1,
                    groupi.levels.len());

                // Index 0 is guaranteed to contain something,
                // usually ONE_LEVEL or at least some default
                // one-level type.
                return (explicit_type, 0);
            }
        };

        let i = match self.types.iter().position(|t| t.name == type_name) {
            Some(i) => i,
            _ => {
                log::warn!("{:?}: The type \"{:?}\" for key '{}' group {} was not previously defined; Using the default type",
                    XkbWarning::UndefinedKeyType,
                    self.context.xkb_atom_text(type_name),
                    self.context.key_name_text(keyi.name),
                    group + 1);

                // Index 0 is guaranteed to contain something,
                // usually ONE_LEVEL or at least some default
                // one-level type.
                return (explicit_type, 0);
            }
        };

        (explicit_type, i)
    }
}

impl GroupInfo {
    fn get_first_sym_at_level(&self, level: LevelIndex) -> Option<Keysym> {
        let level = self.levels.get(level)?;

        level.syms.first().copied()?
    }

    fn find_automatic_type(&self, ctx: &mut Context) -> Option<Atom> {
        let width = self.levels.len();

        if width <= 1 {
            return Some(ctx.atom_intern("ONE_LEVEL"));
        }

        let sym0 = self.get_first_sym_at_level(0).unwrap_or(xkeysym::NO_SYMBOL);
        let sym1 = self.get_first_sym_at_level(1).unwrap_or(xkeysym::NO_SYMBOL);

        if width == 2 {
            if keysym_is_lower(&sym0) && keysym_is_upper(&sym1) {
                return Some(ctx.atom_intern("ALPHABETIC"));
            }
            if sym0.is_keypad_key() || sym1.is_keypad_key() {
                return Some(ctx.atom_intern("KEYPAD"));
            }

            return Some(ctx.atom_intern("TWO_LEVEL"));
        }

        if width <= 4 {
            if keysym_is_lower(&sym0) && keysym_is_upper(&sym1) {
                let sym2 = self.get_first_sym_at_level(2).unwrap_or(xkeysym::NO_SYMBOL);
                let sym3 = match width == 4 {
                    true => self.get_first_sym_at_level(3).unwrap_or(xkeysym::NO_SYMBOL),
                    false => xkeysym::NO_SYMBOL,
                };
                if keysym_is_lower(&sym2) && keysym_is_upper(&sym3) {
                    return Some(ctx.atom_intern("FOUR_LEVEL_ALPHABETIC"));
                }

                return Some(ctx.atom_intern("FOUR_LEVEL_SEMIALPHABETIC"));
            }

            if sym0.is_keypad_key() || sym1.is_keypad_key() {
                return Some(ctx.atom_intern("FOUR_LEVEL_KEYPAD"));
            }

            return Some(ctx.atom_intern("FOUR_LEVEL"));
        }

        None
    }
}

impl KeyInfo {
    fn copy_symbols_def_to_keymap(
        mut self,
        builder: &mut KeymapBuilder<TextV1>,
    ) -> Result<(), CompileSymbolsError> {
        let info_text = self.info_text(&builder.context);

        // Find the range of groups needed
        let num_groups = self
            .groups
            .values()
            .filter(|g| !g.defined.is_empty())
            .count();

        if num_groups == 0 {
            return Err(CompileSymbolsError::NoGroupsCreated);
        }

        // If there are empty groups between non-empty ones,
        // fill them with data from the first group.
        // We can make a wrong assumption here,
        // but leaving gaps is worse.

        let group0 = self.groups.get(&0).unwrap().clone();

        for i in 0..self.groups.keys().max().copied().unwrap_or(0) {
            if let Some(groupi) = self.groups.get_mut(&i) {
                if i >= 1 && groupi.defined.is_empty() {
                    *groupi = group0.clone();
                } else {
                    continue;
                }
            } else {
                // Insert new entry into b tree
                self.groups.insert(i, group0.clone());
            }
        }

        // Find and assign the groups' types in the keymap.
        let mut groups = Vec::new();

        let group_indices: Vec<usize> = self.groups.keys().copied().collect();

        // check for intersection
        for i in group_indices.iter() {
            let (explicit_type, type_index) = builder.find_type_for_group(&self, *i);

            // Always have as many levels as the type specifies
            let num_levels = builder.types[type_index].num_levels;
            let type_name = builder.types[type_index].name;
            if num_levels < self.groups[i].levels.len() {
                log::warn!(
                    "{:?}: Type {:?} has {} levels, but {} has {} levels; Ignoring extra symbols",
                    XkbWarning::ExtraSymbolsIgnored,
                    builder.context.xkb_atom_text(type_name),
                    num_levels,
                    self.info_text(&builder.context),
                    self.groups[i].levels.len()
                );

                // ClearLevelInfo
                self.groups.get_mut(i).unwrap().levels.truncate(num_levels);
            } else {
                // Resize in the other direction
                // This is the equivalent of `darray_resize0(groupi->levels, type->num_levels)`,
                // which resizes and fills with zeroes
                // Essentially, we make blank levels to fill the rest of self.groups[&i]'s levels
                for _ in self.groups[i].levels.len()..num_levels {
                    let levels = &mut self.groups.get_mut(i).unwrap().levels;

                    levels.push(Level {
                        action: Action::None,
                        syms: vec![],
                    });
                }
            }

            // append a group to the new groups list
            groups.push(Group {
                explicit_type,
                key_type: type_index,
                // copy levels
                levels: self.groups[i].levels.clone(),
            });
        }
        // Moved from above for borrowing reasons
        let text = builder.context.atom_text(self.name).map(|x| x.to_owned());

        let key = builder
            .get_key_by_name_mut(self.name, false)
            .ok_or_else(|| {
                let err = XkbWarning::UndefinedKeycode;

                log::warn!(
                    "{:?}: Key {} not found in keycodes; Symbols ignored",
                    err,
                    info_text
                );

                CompileSymbolsError::KeyNotFoundInKeycodes(text.clone())
            })?;

        key.groups.initialize_with(groups);
        key.out_of_range_group_number = Some(self.out_of_range_group_number);
        key.out_of_range_group_action = Some(self.out_of_range_group_action);

        if self.defined.intersects(KeyField::VMODMAP) {
            key.vmodmap = self.vmodmap;
            key.explicit.insert(ExplicitComponents::VMODMAP)
        };

        if self.repeat != KeyRepeat::Undefined {
            key.repeats = self.repeat == KeyRepeat::Yes;
            key.explicit.insert(ExplicitComponents::REPEAT)
        };

        if self
            .groups
            .values()
            .any(|g| g.defined.intersects(GroupField::ACTS))
        {
            key.explicit.insert(ExplicitComponents::INTERP)
        };

        Ok(())
    }
}

impl ModMapEntry {
    fn copy_mod_map_def_to_keymap(
        self,
        builder: &mut KeymapBuilder<TextV1>,
        info_mods: &ModSet,
    ) -> Result<(), CompileSymbolsError> {
        use ModMapEntryKey::*;
        // have_symbol indicates that is Atom
        let key_to_edit = match self.u {
            Name(name) => {
                match builder.get_key_by_name_mut(name, true) {
                    Some(key) => key,
                    None => {
                        let name = builder.context.key_name_text(name);
                        log::warn!("{:?}: Key {} not found in keycodes; Modifier map entry for {} not updated",
                            XkbWarning::UndefinedKeycode,
                            name,
                            builder.context.mod_index_text(info_mods, self.modifier.unwrap_or(0)));
                        return Err(CompileSymbolsError::NoSuchKeyForName(name));
                    }
                }
            }
            // this indicates that is Keysym
            Keysym(s) => match builder.find_key_for_symbol_mut(s) {
                Some(key) => key,
                None => {
                    log::warn!("{:?}: Key \"{}\" not found in symbol map; Modifier map entry for {} not updated",
                            XkbWarning::UnresolvedKeymapSymbol,
                            self.u.keysym_text(&builder.context),
                            builder.context.mod_index_text(info_mods, self.modifier.unwrap_or(0)));
                    return Err(CompileSymbolsError::NoSuchKeyForSym(s));
                }
            },
        };

        if let Some(m) = self.modifier {
            if m != XKB_MOD_NONE {
                key_to_edit.modmap |= 1 << m;
            }
        }

        Ok(())
    }
}

impl SymbolsInfo {
    fn copy_symbols_to_keymap(
        mut self,
        builder: &mut KeymapBuilder<TextV1>,
    ) -> Result<(), CompileSymbolsError> {
        builder.symbols_section_name = self.name;

        // TODO: escape map name

        builder.mods = self.mods.clone();

        builder.group_names = self.group_names.values().copied().collect();
        self.keys.into_iter().for_each(|keyi| {
            if let Err(e) = keyi.copy_symbols_def_to_keymap(builder) {
                self.errors.push(e);
            }
        });

        if builder.context.get_log_verbosity() > 3 {
            builder
                .keys
                .iter()
                //.filter(|(_, k)| k.name != XKB_ATOM_NONE)
                .filter(|(_, k)| k.num_groups() > 0)
                .for_each(|(_, key)| {
                    log::info!(
                        "{:?}: No symbols defined for {}",
                        XkbMessageCode::NoId,
                        builder.context.key_name_text(key.name)
                    );
                });
        }

        for mm in self.modmaps {
            if let Err(e) = mm.copy_mod_map_def_to_keymap(builder, &self.mods) {
                self.errors.push(e);
            }
        }

        // XXX: If we don't ignore errorCount,
        // things break.
        Ok(())
    }
}

pub(super) fn compile_symbols(
    builder: &mut KeymapBuilder<TextV1>,
    file: XkbFile,
    merge: MergeMode,
) -> Result<(), CompileSymbolsError> {
    let ctx = &mut builder.context;

    let actions = ActionsInfo::new();
    let mut info = SymbolsInfo::new(ctx, 0, actions, builder.mods.clone());
    info.default_key.merge = merge;

    info.handle_symbols_file(builder, file, merge);

    if let Some(err) = info.unrecoverable_error {
        return Err(err); //e.g. error from handling include
    } else if !info.errors.is_empty() {
        return Err(CompileSymbolsError::MultipleErrors(info.errors));
    }

    info.copy_symbols_to_keymap(builder)
}

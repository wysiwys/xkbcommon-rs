// Includes port of code in `state.c`,
// and also some comments from include/xkbcommon.h
//
/*
 * Copyright 1985, 1987, 1990, 1998  The Open Group
 * Copyright 2008  Dan Nicholson
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the names of the authors or their
 * institutions shall not be used in advertising or otherwise to promote the
 * sale, use or other dealings in this Software without prior written
 * authorization from the authors.
 */
/************************************************************
 * Copyright (c) 1993 by Silicon Graphics Computer Systems, Inc.
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
 */
use crate::keymap::*;

use crate::keysyms::*;
use crate::rust_xkbcommon::*;

use std::borrow::Borrow;

pub(crate) mod errors {
    use super::*;
    use thiserror::Error;

    #[derive(Debug, Error, PartialEq)]
    pub enum ModIndexIsConsumedError {
        #[error("The provided mod index does not exist: {0}")]
        NoSuchModIndex(usize),

        #[error("The keycode {0:?} does not correspond to a key in the State")]
        NoSuchKeyAtKeycode(Keycode),
    }

    #[derive(Debug, Error)]
    pub enum LedIsActiveError {
        #[error("The provided LED index does not exist: {0}")]
        NoSuchLedIndex(usize),

        #[error("The provided LED name does not exist: {0}")]
        NoSuchLedName(String),
    }

    #[derive(Debug, Error)]
    pub enum LayoutIsActiveError {
        #[error("The provided layout index does not exist: {0}")]
        NoSuchLayoutIndex(usize),

        #[error("The provided layout name does not exist: {0}")]
        NoSuchLayoutName(String),
    }

    #[derive(Debug, Error)]
    pub enum ModIsActiveError {
        #[error("The provided mod index does not exist: {0}")]
        NoSuchModIndex(usize),

        #[error("The provided mod name does not exist: {0}")]
        NoSuchModName(String),
    }

    #[derive(PartialEq, Debug, Clone)]
    pub(super) enum InternalStateError {
        NoSuchFilter,
        CannotCreateFilterFromActionType(ActionType),
        WrongActionType,
        CannotInitializeNullFilter,
        CannotApplyNullFilter,
        WrongFilterData,
        NoSuchKey,
    }
}
use errors::*;

impl Filter {
    fn new(action: Action, key: RawKeycode) -> Result<Self, InternalStateError> {
        use ActionType::*;

        let filter = match action {
            Action::Mods(ref mod_action) if mod_action.action_type == ModSet => Filter {
                refcnt: 0,
                action,
                key,
                _priv: FilterData::None,
                func: FilterFunc::ModSet,
            },
            Action::Mods(ref mod_action) if mod_action.action_type == ModLatch => Filter {
                refcnt: 0,
                action,
                key,
                _priv: FilterData::None,
                func: FilterFunc::ModLatch,
            },
            Action::Mods(ref mod_action) if mod_action.action_type == ModLock => Filter {
                refcnt: 0,
                action,
                key,
                _priv: FilterData::None,
                func: FilterFunc::ModLock,
            },
            Action::Group(ref group_action) if group_action.action_type == GroupSet => Filter {
                refcnt: 0,
                action,
                key,
                _priv: FilterData::None,
                func: FilterFunc::GroupSet,
            },
            Action::Group(ref group_action) if group_action.action_type == GroupLock => Filter {
                refcnt: 0,
                action,
                key,
                _priv: FilterData::None,
                func: FilterFunc::GroupLock,
            },
            a => {
                return Err(InternalStateError::CannotCreateFilterFromActionType(
                    a.action_type(),
                ))
            }
        };

        Ok(filter)
    }
}

impl State {
    fn apply_filter(
        &mut self,
        kc: RawKeycode,
        filter_idx: usize,
        direction: KeyDirection,
    ) -> Result<FilterResult, InternalStateError> {
        // TODO: the filter search happens a bit too often

        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;
        use FilterFunc::*;
        match filter.func {
            ModSet => self.filter_mod_set_func(filter_idx, kc, direction),
            ModLatch => self.filter_mod_latch_func(filter_idx, kc, direction),

            ModLock => self.filter_mod_lock_func(filter_idx, kc, direction),

            GroupSet => self.filter_group_set_func(filter_idx, kc, direction),

            GroupLock => self.filter_group_lock_func(filter_idx, kc, direction),
            None => Err(InternalStateError::CannotApplyNullFilter),
        }
    }
}

#[derive(Clone, Debug)]
struct Filter {
    action: Action, //TODO: it's possible this should be a reference
    func: FilterFunc,
    key: RawKeycode,
    refcnt: usize,
    _priv: FilterData,
}

impl Filter {
    fn mod_action(&mut self) -> Result<&mut ModAction, InternalStateError> {
        match self.action {
            Action::Mods(ref mut action) => Ok(action),
            _ => Err(InternalStateError::WrongActionType),
        }
    }

    fn group_action(&mut self) -> Result<&mut GroupAction, InternalStateError> {
        match self.action {
            Action::Group(ref mut action) => Ok(action),
            _ => Err(InternalStateError::WrongActionType),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum FilterFunc {
    ModSet,
    ModLatch,
    ModLock,
    GroupSet,
    GroupLock,
    None,
}

impl TryFrom<ActionType> for FilterFunc {
    type Error = InternalStateError;

    fn try_from(t: ActionType) -> Result<Self, Self::Error> {
        use ActionType::*;
        let func = match t {
            ModSet => FilterFunc::ModSet,
            ModLatch => FilterFunc::ModLatch,
            ModLock => FilterFunc::ModLock,
            GroupSet => FilterFunc::GroupSet,
            GroupLock => FilterFunc::GroupLock,
            t => return Err(InternalStateError::CannotCreateFilterFromActionType(t)),
        };

        Ok(func)
    }
}
#[derive(Clone, Debug)]
enum FilterData {
    None,
    Latch(LatchState),
    Mods(ModMask),
    Group(u32),
}

#[derive(Clone, Default)]
struct StateComponents {
    // these may be negative
    base_group: i32,
    latched_group: i32,
    locked_group: i32,
    group: LayoutIndex,
    base_mods: ModMask,
    latched_mods: ModMask,
    locked_mods: ModMask,
    mods: ModMask,
    leds: LedMask,
}

#[derive(Clone)]
pub struct State {
    // Before updating the state, we keep a copy of just this struct.
    // This allows us to report which components of the state have been changed.
    components: StateComponents,

    // At each event, we accumulate all the needed modifications to the
    // base modifiers, and apply them at the end.
    // These keep track of the state.
    set_mods: ModMask,
    clear_mods: ModMask,

    // We mustn't clear a base modifier if there's another depressed key
    // which affects it, e.g. given this sequence
    // < Left Shift down, Right Shift down, Left Shift up >
    // the modifier should still be set. This keeps the count.
    mod_key_count: [i16; XKB_MAX_MODS],

    // Could be GroupAction, ModAction,...
    filters: Filters,

    // Keymap contains the context (for now)
    keymap: Keymap,
}

#[derive(Clone)]
struct Filters {
    filters: Vec<Filter>,
}

impl KeyType {
    fn get_entry_for_mods(&self, mods: &ModMask) -> Option<&KeyTypeEntry> {
        let entry = self
            .entries
            .iter()
            .find(|entry| entry.is_active() && entry.mods.mask == *mods);

        entry
    }
}

impl State {
    fn get_entry_for_key_state<'e>(
        &'e self,
        key: &Key,
        group: LayoutIndex,
    ) -> Option<&'e KeyTypeEntry> {
        let type_index = key.groups.get(group)?.key_type;
        let _type = self.keymap.types.get(type_index)?;
        let active_mods = self.components.mods & _type.mods.mask;

        _type.get_entry_for_mods(&active_mods)
    }

    /// Returns the level to use for the given key and state
    ///
    /// Returns `None` if invalid.
    pub fn key_get_level(&self, kc: RawKeycode, layout: LayoutIndex) -> Option<LevelIndex> {
        let key = self.keymap.xkb_key(kc)?;

        if layout >= key.groups.len() {
            return None;
        }

        // If we don't find an explicit match, the default is 0.
        let level = match self.get_entry_for_key_state(key, layout) {
            Some(entry) => entry.level,
            None => 0,
        };

        Some(level)
    }
}

pub(super) fn wrap_group_into_range(
    group: i32,
    num_groups: LayoutIndex,
    out_of_range_group_action: &RangeExceedType,
    out_of_range_group_number: &LayoutIndex,
) -> Option<LayoutIndex> {
    if num_groups == 0 {
        return None;
    }

    if let Ok(layout_idx) = group.try_into() {
        if layout_idx < num_groups {
            return Some(layout_idx);
        }
    }

    use RangeExceedType::*;

    match out_of_range_group_action {
        Redirect => match out_of_range_group_number {
            n if *n >= num_groups => None,
            n => Some(*n),
        },

        Saturate => match group {
            g if g < 0 => None,
            _ => Some(num_groups - 1),
        },
        Wrap => {
            let ngroups: i32 = num_groups.try_into().ok()?;
            let wrapped_idx = match group {
                // Wrap or default
                // TODO: reevaluate these operations
                // In original:
                // "C99 says a negative dividend in a modulo operation
                // always gives a negative result."
                g if g < 0 => ngroups + (g % ngroups),
                g => g % ngroups,
            };

            wrapped_idx.try_into().ok()
        }
    }
}

impl State {
    /// Returns the layout to use for the given
    /// key and state, taking wrapping/clamping/
    /// etc. into account
    ///
    /// Returns `None` if invalid.
    pub fn key_get_layout(&self, kc: RawKeycode) -> Option<LayoutIndex> {
        let key = self.keymap.xkb_key(kc)?;

        wrap_group_into_range(
            self.components.group.try_into().unwrap(),
            key.groups.len(),
            &key.out_of_range_group_action,
            &key.out_of_range_group_number,
        )
    }

    fn key_get_action(&self, kc: RawKeycode) -> Option<&Action> {
        // Changed to use kc for borrow reasons
        // TODO: avoid these repeat `get`s

        let layout = self.key_get_layout(kc)?;

        let level = self.key_get_level(kc, layout)?;
        let key = self.keymap.xkb_key(kc)?;

        let action = &key.groups.get(layout)?.levels.get(level)?.action;

        // TODO: return None if ActionType::None?
        Some(action)
    }
}

impl Filters {
    /// corresponds to `xkb_filter_new`
    fn add_or_modify_filter(
        &mut self,
        action: Action,
        key: RawKeycode,
    ) -> Result<Option<usize>, InternalStateError> {
        let prev_filter = self
            .filters
            .iter_mut()
            .enumerate()
            .find(|(_, f)| f.func == FilterFunc::None);

        let (idx, filter_to_modify) = match prev_filter {
            Some((idx, f)) => {
                f.key = key;
                f.action = action;
                f.func = match FilterFunc::try_from(f.action.action_type()) {
                    Ok(func) => func,
                    Err(_) => return Ok(None),
                };

                (idx, f)
            }
            None => {
                // not found, so add filter
                // func is derived automatically
                // if invalid, return None.
                let new_filter = match Filter::new(action, key).ok() {
                    Some(filter) => filter,
                    None => return Ok(None),
                };

                self.filters.push(new_filter);

                let idx = self.filters.len() - 1;
                (idx, self.filters.last_mut().unwrap())
            }
        };

        filter_to_modify.refcnt = 1;

        Ok(Some(idx))
    }
}

impl State {
    /// Matches the filter to the `new` function
    fn initialize_with_filter(&mut self, filter_idx: usize) -> Result<(), InternalStateError> {
        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        use FilterFunc::*;
        match filter.func {
            ModSet => self.filter_mod_set_new(filter_idx),
            ModLatch => self.filter_mod_latch_new(filter_idx),
            ModLock => self.filter_mod_lock_new(filter_idx),

            GroupSet => self.filter_group_set_new(filter_idx),
            GroupLock => self.filter_group_lock_new(filter_idx),
            None => Err(InternalStateError::CannotInitializeNullFilter),
        }
    }
}

#[derive(PartialEq)]
enum FilterResult {
    // The event is consumed by the filters.
    // An event is always processed by all filters,
    // but any filter can prevent it from being processed further
    // by consuming it.
    Consume,

    // The event may continue to be processed as far as
    // this filter is concerned
    Continue,
}

impl State {
    fn filter_group_set_new(&mut self, filter_idx: usize) -> Result<(), InternalStateError> {
        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        let base_group = self.components.base_group.try_into().unwrap();

        filter._priv = FilterData::Group(base_group);

        if filter
            .group_action()?
            .flags
            .intersects(ActionFlags::AbsoluteSwitch)
        {
            self.components.base_group = filter.group_action()?.group.unwrap_or(0);
        } else {
            self.components.base_group += filter.group_action()?.group.unwrap_or(0);
        }

        Ok(())
    }

    fn filter_group_set_func(
        &mut self,
        filter_idx: usize,
        kc: RawKeycode,
        direction: KeyDirection,
    ) -> Result<FilterResult, InternalStateError> {
        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        // check that key exists
        self.keymap
            .xkb_key(kc)
            .ok_or(InternalStateError::NoSuchKey)?;

        if kc != filter.key {
            filter.group_action()?.flags &= !ActionFlags::LockClear;
            return Ok(FilterResult::Continue);
        }

        if direction == KeyDirection::Down {
            filter.refcnt += 1;
            return Ok(FilterResult::Consume);
        } else {
            filter.refcnt -= 1; // TODO: check bounds
            if filter.refcnt > 0 {
                return Ok(FilterResult::Consume);
            }
        }

        self.components.base_group = match &filter._priv {
            FilterData::Group(u) => (*u).try_into().unwrap(),
            FilterData::None => 0,
            _ => return Err(InternalStateError::WrongFilterData),
        };

        if filter
            .group_action()?
            .flags
            .intersects(ActionFlags::LockClear)
        {
            self.components.locked_group = 0;
        }

        filter.func = FilterFunc::None;
        Ok(FilterResult::Continue)
    }

    fn filter_group_lock_new(&mut self, filter_idx: usize) -> Result<(), InternalStateError> {
        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        let group_action = filter.group_action()?;

        if group_action.flags.intersects(ActionFlags::AbsoluteSwitch) {
            self.components.locked_group = group_action.group.unwrap_or(0);
        } else {
            self.components.locked_group += group_action.group.unwrap_or(0);
        }

        Ok(())
    }

    fn filter_group_lock_func(
        &mut self,
        filter_idx: usize,
        kc: RawKeycode,
        direction: KeyDirection,
    ) -> Result<FilterResult, InternalStateError> {
        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        self.keymap
            .xkb_key(kc)
            .ok_or(InternalStateError::NoSuchKey)?;

        use FilterResult::*;
        if kc != filter.key {
            return Ok(Continue);
        }

        if direction == KeyDirection::Down {
            filter.refcnt += 1;
            return Ok(Consume);
        }
        filter.refcnt -= 1;
        if filter.refcnt > 0 {
            return Ok(Consume);
        }

        filter.func = FilterFunc::None;

        Ok(Continue)
    }

    fn filter_mod_set_new(&mut self, filter_idx: usize) -> Result<(), InternalStateError> {
        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        self.set_mods = filter.mod_action()?.mods.mask;

        Ok(())
    }
}

impl State {
    fn filter_mod_set_func(
        &mut self,
        filter_idx: usize,
        kc: RawKeycode,
        direction: KeyDirection,
    ) -> Result<FilterResult, InternalStateError> {
        use FilterResult::*;

        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        // determine if key exists
        self.keymap
            .xkb_key(kc)
            .ok_or(InternalStateError::NoSuchKey)?;

        if kc != filter.key {
            filter.mod_action()?.flags &= !ActionFlags::LockClear;

            return Ok(Continue);
        }
        if direction == KeyDirection::Down {
            filter.refcnt += 1;
            return Ok(Consume);
        } else {
            // If something else refers to it, consume
            filter.refcnt -= 1;
            if filter.refcnt > 0 {
                return Ok(Consume);
            }
        }

        let mod_action = filter.mod_action()?;

        self.clear_mods = mod_action.mods.mask;
        if mod_action.flags.intersects(ActionFlags::LockClear) {
            self.components.locked_mods &= !mod_action.mods.mask;
        }

        filter.func = FilterFunc::None;
        Ok(Continue)
    }
}

impl State {
    fn filter_mod_lock_new(&mut self, filter_idx: usize) -> Result<(), InternalStateError> {
        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        filter._priv =
            FilterData::Mods(self.components.locked_mods & filter.mod_action()?.mods.mask);

        let mod_action = filter.mod_action()?;

        self.set_mods |= mod_action.mods.mask;

        if !mod_action.flags.intersects(ActionFlags::LockNoLock) {
            self.components.locked_mods |= mod_action.mods.mask;
        }

        Ok(())
    }

    fn filter_mod_lock_func(
        &mut self,
        filter_idx: usize,
        kc: RawKeycode,
        direction: KeyDirection,
    ) -> Result<FilterResult, InternalStateError> {
        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        self.keymap
            .xkb_key(kc)
            .ok_or(InternalStateError::NoSuchKey)?;

        use FilterResult::*;

        if kc != filter.key {
            return Ok(Continue);
        }

        if direction == KeyDirection::Down {
            filter.refcnt += 1;
            return Ok(Consume);
        }

        filter.refcnt -= 1;
        if filter.refcnt > 0 {
            return Ok(Consume);
        }

        self.clear_mods |= filter.mod_action()?.mods.mask;

        if !filter
            .mod_action()?
            .flags
            .intersects(ActionFlags::LockNoUnlock)
        {
            let mods = match filter._priv {
                FilterData::Mods(mods) => mods,
                FilterData::None => 0,
                _ => return Err(InternalStateError::WrongFilterData),
            };
            self.components.locked_mods &= !mods;
        }

        filter.func = FilterFunc::None;

        Ok(Continue)
    }
}

#[derive(Clone, PartialEq, Debug, Default)]
enum LatchState {
    #[default]
    NoLatch,
    KeyDown,
    Pending,
}

impl Action {
    fn breaks_latch(&self) -> bool {
        use ActionType::*;

        [
            None, PtrButton, PtrLock, CtrlSet, CtrlLock, SwitchVT, Terminate,
        ]
        .contains(&self.action_type())
    }
}

impl State {
    fn filter_mod_latch_new(&mut self, filter_idx: usize) -> Result<(), InternalStateError> {
        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        filter._priv = FilterData::Latch(LatchState::KeyDown);
        self.set_mods = filter.mod_action()?.mods.mask;

        Ok(())
    }

    fn filter_mod_latch_func(
        &mut self,
        filter_idx: usize,
        kc: RawKeycode,
        direction: KeyDirection,
    ) -> Result<FilterResult, InternalStateError> {
        let action = self.key_get_action(kc).cloned();
        let filter = self
            .filters
            .filters
            .get_mut(filter_idx)
            .ok_or(InternalStateError::NoSuchFilter)?;

        self.keymap
            .xkb_key(kc)
            .ok_or(InternalStateError::NoSuchKey)?;

        use FilterResult::*;
        let mut latch: LatchState = match &filter._priv {
            FilterData::Latch(latch) => latch.clone(),
            _ => return Err(InternalStateError::WrongFilterData),
        };

        if direction == KeyDirection::Down && latch == LatchState::Pending {
            // If this is a new keypress and we're awaiting our
            // single latched keypress, then either break the latch
            // if any random key is pressed, or promote it to a lock
            // or plain base set if it's the same modifier.
            match action {
                Some(Action::Mods(mod_action))
                    if mod_action.action_type == ActionType::ModLatch
                        && mod_action.flags == filter.mod_action()?.flags
                        && mod_action.mods.mask == filter.mod_action()?.mods.mask =>
                {
                    // TODO: should this be a reference?
                    filter.action = Action::Mods(mod_action.clone());

                    if mod_action.flags.intersects(ActionFlags::LatchToLock) {
                        filter.mod_action()?.action_type = ActionType::ModLock;
                        filter.func = FilterFunc::ModLock;

                        self.components.locked_mods |= mod_action.mods.mask;
                    } else {
                        filter.mod_action()?.action_type = ActionType::ModSet;
                        filter.func = FilterFunc::ModSet;
                        self.set_mods = mod_action.mods.mask;
                    }
                    filter.key = kc;
                    self.components.latched_mods &= !mod_action.mods.mask;
                    // "XXX beep beep!"

                    return Ok(Consume);
                }
                Some(a) if a.breaks_latch() => {
                    // TODO: enum variant is not guaranteed...

                    // "XXX: this may be totally broken,
                    // we might need to break the latch in the
                    // next run after this press?"

                    self.components.latched_mods &= !filter.mod_action()?.mods.mask;

                    filter.func = FilterFunc::None;
                    return Ok(Continue);
                }
                _ => {} // do nothing
            }
        } else if direction == KeyDirection::Up && kc == filter.key {
            // Our key got released. If we've set it to clear locks,
            // and we currently have the same modifiers locked, then
            // release them and don't actually latch. Else we've
            // actually hit the latching stage, so set PENDING and move
            // out modifier from base to latched.

            if latch == LatchState::NoLatch
                || (filter
                    .mod_action()?
                    .flags
                    .intersects(ActionFlags::LockClear)
                    && (self.components.locked_mods & filter.mod_action()?.mods.mask)
                        == filter.mod_action()?.mods.mask)
            {
                // "XXX: we might be a bit overenthusiastic about
                // clearing mods other filters have set here?"

                if latch == LatchState::Pending {
                    self.components.latched_mods &= !filter.mod_action()?.mods.mask;
                } else {
                    self.clear_mods = filter.mod_action()?.mods.mask;
                }

                self.components.locked_mods &= !filter.mod_action()?.mods.mask;

                filter.func = FilterFunc::None;
            } else {
                latch = LatchState::Pending;
                self.clear_mods = filter.mod_action()?.mods.mask;
                self.components.latched_mods |= filter.mod_action()?.mods.mask;
                // "XXX beep beep!"
            }
        } else if direction == KeyDirection::Down && latch == LatchState::KeyDown {
            // Someone's pressed another key while we've still
            // got the latching key held down, so keep the base
            // modifier state active (from xkb_filter_mod_latch_new
            // ), but don't trip the latch, just clear it as soon
            // as the modifier gets released.

            latch = LatchState::NoLatch;
        }

        filter._priv = FilterData::Latch(latch);

        Ok(Continue)
    }

    /// Applies any relevant filters to the key, first from the list of
    /// filters that are currently active, then if no filter has claimed
    /// the key, possibly apply a new filter from the key action.

    fn filter_apply_all(
        &mut self,
        kc: RawKeycode,
        direction: KeyDirection,
    ) -> Result<(), InternalStateError> {
        let mut consumed = false;

        // First run through all the currently active filters
        // and see if any of them have consumed this event.

        for idx in 0..self.filters.filters.len() {
            // TODO: since these are not overwritten, doesn't the filters array build up and become very long?
            if let FilterFunc::None = self.filters.filters[idx].func {
                continue;
            }

            if self.apply_filter(kc, idx, direction)? == FilterResult::Consume {
                consumed = true;
            }
        }

        if consumed || direction == KeyDirection::Up {
            return Ok(());
        }

        let action = match self.key_get_action(kc) {
            Some(action) => action.clone(),
            None => return Ok(()),
        };

        // It's possible for the keymap to set action.type explicitly,
        // like so:
        //      ```
        //      interpret XF86_Next_VMode {
        //          action = Private(type=0x86, data="+VMode");
        //      };
        //      ```
        //We don't handle those

        if action.action_type() >= ActionType::Private {
            return Ok(());
        }

        if self.keymap.xkb_key(kc).is_none() {
            return Ok(());
        }

        let filter_idx = match self.filters.add_or_modify_filter(action, kc)? {
            Some(idx) => idx,
            None => return Ok(()),
        }; // skip invalid action

        self.initialize_with_filter(filter_idx)?;

        Ok(())
    }

    /// Create a new keyboard state machine
    /// from a provided keymap.
    ///
    ///Corresponds to `xkb_state_new`
    pub fn new(keymap: Keymap) -> Self {
        // calloc'ed in the original
        // TODO: keep keymap as reference?
        Self {
            keymap,
            components: Default::default(),
            filters: Filters { filters: vec![] },
            set_mods: 0,
            clear_mods: 0,
            mod_key_count: [0; XKB_MAX_MODS],
        }
    }

    /// Get the keymap which a keyboard state object is using
    ///
    /// Returns the keymap which was passed to [State::new()] when creating this state object.
    pub fn get_keymap(&self) -> &Keymap {
        &self.keymap
    }

    // Update the LED state to match the rest of the State
    fn led_update_all(&mut self) {
        self.components.leds = 0;

        for (idx, led) in self.keymap.leds.iter().enumerate() {
            if let Some(led) = led {
                let mut mod_mask = 0;
                let mut group_mask = 0;

                if !led.which_mods.is_empty() && led.mods.mask != 0 {
                    if led.which_mods.intersects(StateComponent::MODS_EFFECTIVE) {
                        mod_mask |= self.components.mods;
                    }
                    if led.which_mods.intersects(StateComponent::MODS_DEPRESSED) {
                        mod_mask |= self.components.base_mods;
                    }
                    if led.which_mods.intersects(StateComponent::MODS_LATCHED) {
                        mod_mask |= self.components.latched_mods;
                    }
                    if led.which_mods.intersects(StateComponent::MODS_LOCKED) {
                        mod_mask |= self.components.locked_mods;
                    }

                    if (led.mods.mask & mod_mask) != 0 {
                        self.components.leds |= 1u32 << idx;
                        continue;
                    }
                }

                // TODO: should led.groups be None?
                if led.which_groups.is_empty() && led.groups != 0 {
                    if led
                        .which_groups
                        .intersects(StateComponent::LAYOUT_EFFECTIVE)
                    {
                        group_mask |= 1u32 << self.components.group;
                    }
                    if led
                        .which_groups
                        .intersects(StateComponent::LAYOUT_DEPRESSED)
                    {
                        group_mask |= 1u32 << self.components.base_group;
                    }
                    if led.which_groups.intersects(StateComponent::LAYOUT_LATCHED) {
                        group_mask |= 1u32 << self.components.latched_group;
                    }
                    if led.which_groups.intersects(StateComponent::LAYOUT_LOCKED) {
                        group_mask |= 1u32 << self.components.locked_group;
                    }

                    if (led.groups & group_mask) > 0 {
                        self.components.leds |= 1u32 << idx;
                        continue;
                    }
                }

                if led.ctrls.intersects(self.keymap.enabled_ctrls) {
                    self.components.leds |= 1u32 << idx;
                    continue;
                }
            }
        }
    }

    /// Calculates the derived state (effective mods/group and LEDs)
    /// from an up-to-date State.
    fn update_derived(&mut self) {
        // Update state.components.mods

        self.components.mods =
            self.components.base_mods | self.components.latched_mods | self.components.locked_mods;

        // TODO: use groups_wrap to control instead of always RANGE_WRAP.
        let wrapped = wrap_group_into_range(
            self.components.locked_group,
            self.keymap.num_groups,
            &RangeExceedType::Wrap,
            &0,
        )
        .unwrap_or(0);

        self.components.locked_group = wrapped.try_into().unwrap_or(0);

        let wrapped = wrap_group_into_range(
            self.components.base_group
                + self.components.latched_group
                + self.components.locked_group,
            self.keymap.num_groups,
            &RangeExceedType::Wrap,
            &0,
        )
        .unwrap_or(0);

        self.components.group = wrapped;

        self.led_update_all();
    }
}

impl StateComponents {
    fn get_changes(&self, other: &Self) -> StateComponent {
        let mut mask = StateComponent::empty();

        if self.group != other.group {
            mask |= StateComponent::LAYOUT_EFFECTIVE;
        }
        if self.base_group != other.base_group {
            mask |= StateComponent::LAYOUT_DEPRESSED;
        }
        if self.latched_group != other.latched_group {
            mask |= StateComponent::LAYOUT_LATCHED;
        }
        if self.locked_group != other.locked_group {
            mask |= StateComponent::LAYOUT_LOCKED;
        }
        if self.mods != other.mods {
            mask |= StateComponent::MODS_EFFECTIVE;
        }
        if self.base_mods != other.base_mods {
            mask |= StateComponent::MODS_DEPRESSED;
        }
        if self.latched_mods != other.latched_mods {
            mask |= StateComponent::MODS_LATCHED;
        }
        if self.locked_mods != other.locked_mods {
            mask |= StateComponent::MODS_LOCKED;
        }
        if self.leds != other.leds {
            mask |= StateComponent::LEDS;
        }

        mask
    }
}

impl State {
    /// Update the keyboard state to reflect a given key being pressed or released.
    ///
    /// This entry point is intended for *server* applications and should not be used by *client*
    /// applications. Clients should use [State::update_mask()] instead.
    ///
    /// This function is often used in conjunction with the function [State::key_get_syms()] (or
    /// [State::key_get_one_sym()]), for example, when handling a key event. In this case, you
    /// should prefer to get the keysyms *before* updating the key, such that the keysyms reported
    /// for the key event are not affected by the event itself. This is the conventional behavior.
    ///
    /// Returns a mask of state components that have changed as a result of the update. If nothing
    /// in the state has changed, returns 0.
    #[cfg(feature = "server")]
    pub fn update_key(
        &mut self,
        kc: impl Into<RawKeycode>,
        direction: KeyDirection,
    ) -> StateComponent {
        let kc = kc.into();
        if self.keymap.xkb_key(kc).is_none() {
            return StateComponent::empty();
        }

        let prev_components = self.components.clone();

        // reset the mods for this turn
        self.set_mods = 0;
        self.clear_mods = 0;

        self.filter_apply_all(kc, direction)
            .expect("Could not apply filters");

        for bit_idx in 0..XKB_MAX_MODS {
            if self.set_mods == 0 {
                break;
            }

            let bit = 1 << bit_idx;

            if (self.set_mods & bit) != 0 {
                self.mod_key_count[bit_idx] += 1;
                self.components.base_mods |= bit;
                self.set_mods &= !bit;
            }
        }

        for bit_idx in 0..XKB_MAX_MODS {
            if self.clear_mods == 0 {
                break;
            }

            let bit = 1 << bit_idx;

            if (self.clear_mods & bit) != 0 {
                self.mod_key_count[bit_idx] -= 1;

                if self.mod_key_count[bit_idx] <= 0 {
                    self.components.base_mods &= !bit;
                    self.mod_key_count[bit_idx] = 0;
                }

                self.clear_mods &= !bit;
            }
        }
        self.update_derived();

        self.components.get_changes(&prev_components)
    }
    /// Updates the state from a set of explicit masks.
    ///
    /// This entry point is intended for *client* applications. *Server* applications should use
    /// [State::update_key()] instead.
    ///
    /// All parameters must always be passed, or the resulting state may be incoherent.
    ///
    /// The serialization is lossy and will not survive round trips; it must only be used to feed
    /// client state objects, and must not be used to update the server state.

    #[cfg(feature = "client")]
    pub fn update_mask(
        &mut self,
        base_mods: ModMask,
        latched_mods: ModMask,
        locked_mods: ModMask,
        base_group: LayoutIndex,
        latched_group: LayoutIndex,
        locked_group: LayoutIndex,
    ) -> StateComponent {
        let prev_components = self.components.clone();

        // Only include modifiers which exist in the keymap
        let mask: ModMask = (1 << self.keymap.num_mods()) - 1;

        self.components.base_mods = base_mods & mask;
        self.components.latched_mods = latched_mods & mask;
        self.components.locked_mods = locked_mods & mask;

        // Make sure the mods are fully resolved -
        // since we get arbitrary input, they might nt be.
        //
        // TODO: documentation

        // We OR here because mod_mask_get_effective() drops vmods.
        self.components.base_mods |= self
            .keymap
            .mods
            .mod_mask_get_effective(self.components.base_mods);
        self.components.latched_mods |= self
            .keymap
            .mods
            .mod_mask_get_effective(self.components.latched_mods);
        self.components.locked_mods |= self
            .keymap
            .mods
            .mod_mask_get_effective(self.components.locked_mods);

        // TODO: is this right?
        // TODO: return error if value out of bounds
        self.components.base_group = base_group.try_into().unwrap();

        self.components.latched_group = latched_group.try_into().unwrap();

        self.components.locked_group = locked_group.try_into().unwrap();
        self.update_derived();

        self.components.get_changes(&prev_components)
    }

    /// Get the keysyms obtained from pressing a particular key in a given keyboard state.
    ///
    /// Get the keysyms for a key according to the current active layout, modifiers and shift level
    /// for the key, as determined by a keyboard state.
    ///
    /// As an extension to XKB, this function can return more than one keysym. If you do not want
    /// to handle this case, you can use [State::key_get_one_sym()] for a simpler interface.
    ///
    /// This function does not perform any keysym transformations.
    pub fn key_get_syms(&self, kc: Keycode) -> Vec<Keysym> {
        let kc_raw = kc.raw();

        let layout = match self.key_get_layout(kc_raw) {
            Some(layout) => layout,
            None => return vec![],
        };

        let level = match self.key_get_level(kc_raw, layout) {
            Some(layout) => layout,
            None => return vec![],
        };

        match self.keymap.key_get_syms_by_level(kc, layout, level) {
            Ok(syms) => syms,
            _ => vec![],
        }
    }

    fn should_do_caps_transformation(&self, kc: RawKeycode) -> bool {
        let caps = match self.keymap.mod_get_index(ModName::CAPS.name()) {
            Some(caps) => caps,
            None => return false,
        };

        let is_active = matches!(
            self.mod_index_is_active(caps, StateComponent::MODS_EFFECTIVE),
            Ok(true)
        );

        is_active && self.mod_index_is_consumed(Keycode::from(kc), caps) == Ok(false)
    }

    fn should_do_ctrl_transformation(&self, kc: RawKeycode) -> bool {
        let ctrl = match self.keymap.mod_get_index(ModName::CTRL.name()) {
            Some(ctrl) => ctrl,
            None => return false,
        };

        let is_active = matches!(
            self.mod_index_is_active(ctrl, StateComponent::MODS_EFFECTIVE),
            Ok(true)
        );

        is_active && self.mod_index_is_consumed(Keycode::from(kc), ctrl) == Ok(false)
    }
}

fn xkb_to_control(ch: u8) -> char {
    let mut c = ch;
    let ch = char::from(c);

    let c_177 = u8::from_str_radix("177", 8).unwrap();
    let c_000 = u8::from_str_radix("000", 8).unwrap();
    let c_033 = u8::from_str_radix("033", 8).unwrap();

    if (ch >= '@' && c < c_177) || ch == ' ' {
        c &= 0x1F;
    } else if ch == '2' {
        c = c_000;
    } else if ('3'..='7').contains(&ch) {
        c -= u8::try_from('3').unwrap() - c_033;
    } else if ch == '8' {
        c = c_177;
    } else if ch == '/' {
        c = u8::try_from('_').unwrap() & 0x1F;
    }

    c.into()
}

impl State {
    /// Get the single keysym obtained from pressing a particular key in a given keyboard state.
    ///
    /// This function is similar to [State::key_get_syms()], but intended for users who cannot or
    /// do not want to handle the cases where multiple keysyms are returned (in which case this
    /// function is preferred).
    ///
    /// Returns the keysym. If the key does not have exactly one keysym, returns `None`.
    ///
    /// This function performs capitalization.
    pub fn key_get_one_sym(&self, kc: Keycode) -> Option<Keysym> {
        let syms = match self.key_get_syms(kc) {
            syms if syms.len() == 1 => syms,
            _ => return None,
        };

        let sym = syms[0];

        if self.should_do_caps_transformation(kc.raw()) {
            return Some(keysym_to_upper(&sym));
        }

        Some(sym)
    }

    // The caps and ctrl transformations require some special handling,
    // so we cannot simply use `State::get_one_sym()` for them.
    // In particular, if Control is set, we must try very hard to find
    // some layout in which the keysym is ASCUU and thus can be (maybe)
    // converted to a control character. libX11 allows to disable this
    // behavior with the XkbLC_ControlFallback (see XkbSetXlibControls(3)),
    // but it is enabled by default

    fn get_one_sym_for_string(&self, kc: RawKeycode) -> Option<Keysym> {
        let layout = self.key_get_layout(kc)?;

        let num_layouts = match self.keymap.num_layouts_for_key(Keycode::from(kc)) {
            Some(n) if n != 0 => n,
            _ => return None,
        };

        let level = self.key_get_level(kc, layout)?;

        let syms = match self
            .keymap
            .key_get_syms_by_level(Keycode::new(kc), layout, level)
        {
            Ok(syms) if syms.len() == 1 => syms,
            _ => return None,
        };

        let mut sym = syms[0];

        if self.should_do_ctrl_transformation(kc) && sym > 127.into() {
            for i in 0..num_layouts {
                let level = match self.key_get_level(kc, i) {
                    Some(level) => level,
                    None => continue,
                };

                if let Ok(syms) = self
                    .keymap
                    .key_get_syms_by_level(Keycode::new(kc), i, level)
                {
                    if syms.len() == 1 && syms[0] <= 127.into() {
                        sym = syms[0];
                        break;
                    }
                }
            }
        }

        if self.should_do_caps_transformation(kc) {
            sym = keysym_to_upper(&sym);
        }

        Some(sym)
    }

    /// Get the Unicode/UTF-8 string obtained from pressing a particular key in a given keyboard
    /// state.
    ///
    /// This function performs Capitalization and Control keysym transformations.
    pub fn key_get_utf8(&self, kc: Keycode) -> Option<Vec<u8>> {
        let syms;

        if let Some(sym) = self.get_one_sym_for_string(kc.raw()) {
            syms = vec![sym];
        } else {
            syms = self.key_get_syms(kc);
        }

        // make sure not to truncate in the middle of a UTF-8 sequence.
        let utf8: Vec<_> = syms
            .iter()
            .map(crate::keysyms_utf::keysym_to_utf8)
            .collect::<Option<_>>()?;

        let utf8: Vec<_> = utf8.into_iter().flatten().collect();

        // TODO: check is valid utf 8
        if !crate::utf8::is_valid_utf8(&utf8) {
            return None;
        }

        if utf8.len() == 1 && utf8[0] <= 127 && self.should_do_ctrl_transformation(kc.raw()) {
            let mut buf = Vec::with_capacity(4);
            let c = xkb_to_control(utf8[0]);
            c.encode_utf8(&mut buf);

            return Some(buf);
        }

        Some(utf8)
    }

    /// Get the Unicode/UTF-32 codepoint obtained from pressing a particular key in a given
    /// keyboard state.
    ///
    /// Returns the UTF-32 representation for the key, if it consists of only a single codepoint.
    /// Otherwise, returns 0.
    ///
    /// This function performs Capitalization and Control keysym transformations.
    ///
    pub fn key_get_utf32(&self, kc: Keycode) -> Option<u32> {
        let sym = self.get_one_sym_for_string(kc.raw())?;
        let mut cp: u32 = crate::keysyms_utf::keysym_to_utf32(&sym)?;

        if self.should_do_ctrl_transformation(kc.raw()) {
            if let Ok(c) = u8::try_from(cp) {
                cp = xkb_to_control(c) as u32;
            }
        }

        Some(cp)
    }

    /// The counterpart to [State::update_mask()] for modifiers, to be used on the server side of
    /// serialization.
    ///
    /// This entry point is intended for *server* applications. *Client* applications should use
    /// [State::mod_index_is_active()] or [State::mod_name_is_active()].
    /// # Arguments
    /// * `components`: A mask of the modifier state components to serialize.
    ///
    /// State components other than `StateComponent::MODS_*` are ignored.
    /// If `MODS_EFFECTIVE` is included, all other state components are ignored.
    /// # Output
    /// Returns a [ModMask] representing the given components of the modifier state.
    ///
    #[cfg(feature = "server")]
    pub fn serialize_mods(&self, _type: StateComponent) -> ModMask {
        self._serialize_mods(_type)
    }

    fn _serialize_mods(&self, _type: StateComponent) -> ModMask {
        let mut ret = 0;
        if _type.intersects(StateComponent::MODS_EFFECTIVE) {
            return self.components.mods;
        }
        if _type.intersects(StateComponent::MODS_DEPRESSED) {
            ret |= self.components.base_mods;
        }
        if _type.intersects(StateComponent::MODS_LATCHED) {
            ret |= self.components.latched_mods;
        }
        if _type.intersects(StateComponent::MODS_LOCKED) {
            ret |= self.components.locked_mods;
        }

        ret
    }

    /// The counterpart to [State::update_mask] for layouts, to be used on the server side of
    /// serialization.
    ///
    /// This entry point is intended for *server* applications. *Client* applications should use
    /// [State::layout_index_is_active()] and [State::layout_name_is_active()].
    /// # Arguments
    /// * `components`: A mask of the layout state components to serialize.
    ///
    /// State components other than `StateComponent::LAYOUT_*`
    #[cfg(feature = "server")]
    pub fn serialize_layout(&self, _type: StateComponent) -> LayoutIndex {
        let mut ret = 0;

        if _type.intersects(StateComponent::LAYOUT_EFFECTIVE) {
            return self.components.group;
        }
        if _type.intersects(StateComponent::LAYOUT_DEPRESSED) {
            ret += self.components.base_group;
        }
        if _type.intersects(StateComponent::LAYOUT_LATCHED) {
            ret += self.components.latched_group;
        }
        if _type.intersects(StateComponent::LAYOUT_LOCKED) {
            ret += self.components.locked_group;
        }

        ret.try_into().unwrap()
    }
}

impl ModSet {
    // Gets a modifier mask and returns the resolved effective mask; this is needed because some
    // modifiers can also map to other modifiers, e.g. the "NumLock" modifier usually also sets the
    // "Mod2" modifier.
    // drops vmods
    pub(crate) fn mod_mask_get_effective(&self, mods: ModMask) -> ModMask {
        // The effective mask is only real mods for now
        let mut mask = mods & MOD_REAL_MASK_ALL;

        for (i, _mod) in self.mods.iter().enumerate() {
            if (mods & (1u32 << i)) != 0 {
                mask |= _mod.mapping;
            }
        }

        mask
    }
}

impl State {
    /// Test whether a modifier is active in a given keyboard state by index.
    ///
    /// Returns Ok(true) if the given modifier is active with the specified type(s),
    /// Ok(false) if not,
    /// and Err(_) if the modifier is invalid.
    pub fn mod_index_is_active(
        &self,
        idx: ModIndex,
        _type: StateComponent,
    ) -> Result<bool, ModIsActiveError> {
        if idx >= self.keymap.num_mods() {
            return Err(ModIsActiveError::NoSuchModIndex(idx));
        }

        let serialized_mods = self._serialize_mods(_type);

        Ok((serialized_mods & (1u32 << idx)) != 0)
    }

    /// Helper function for `State::mod_indices_are_active`
    /// and `State::mod_names_are_active`.
    fn match_mod_masks(&self, _type: StateComponent, _match: StateMatch, wanted: ModMask) -> bool {
        let active = self._serialize_mods(_type);

        if !_match.intersects(StateMatch::NON_EXCLUSIVE) && (active & !wanted > 0) {
            return false;
        }

        if _match.intersects(StateMatch::ANY) {
            return (active & wanted) > 0;
        }

        (active & wanted) == wanted
    }

    /// Test whether a set of modifiers are active in a given keyboard state by index.
    pub fn mod_indices_are_active(
        &self,
        _type: StateComponent,
        _match: StateMatch,
        indices: &[ModIndex],
    ) -> Result<bool, ModIsActiveError> {
        let mut wanted: ModMask = 0;
        let num_mods = self.keymap.num_mods();

        for idx in indices.into_iter() {
            if *idx >= num_mods {
                return Err(ModIsActiveError::NoSuchModIndex(*idx));
            }
            wanted |= 1 << idx;
        }

        Ok(self.match_mod_masks(_type, _match, wanted))
    }

    /// Test whether a modifier is active in a given keyboard state by name.
    ///
    /// Returns `Ok(true)` if the given modifier is active with
    /// the specified type(s), `Ok(false)` if not, or `Err(...)`
    /// if the modifier is invalid.
    pub fn mod_name_is_active(
        &self,
        name: impl Borrow<str>,
        _type: StateComponent,
    ) -> Result<bool, ModIsActiveError> {
        let name: &str = name.borrow();
        let idx = match self.keymap.mod_get_index(name) {
            Some(idx) => idx,
            // TODO: rename to InvalidModifier
            None => return Err(ModIsActiveError::NoSuchModName(name.to_string())),
        };

        self.mod_index_is_active(idx, _type)
    }

    /// Test whether a set of modifiers are active in a given keyboard state by name
    ///
    /// Returns `Ok(true)` if the given modifiers are active with
    /// the specified type(s), `Ok(false)` if not, or `Err(...)`
    /// if the modifiers are invalid.
    pub fn mod_names_are_active(
        &self,
        _type: StateComponent,
        _match: StateMatch,
        args: &[impl Borrow<str>],
    ) -> Result<bool, ModIsActiveError> {
        let mut wanted: ModMask = 0;

        for arg in args {
            let idx = self
                .keymap
                .mod_get_index(arg.borrow())
                .ok_or(ModIsActiveError::NoSuchModName(arg.borrow().to_string()))?;
            wanted |= 1 << idx;
        }

        Ok(self.match_mod_masks(_type, _match, wanted))
    }

    /// Test whether a layout is active in a given keyboard state by index.
    ///
    /// Returns `Ok(true)` if the given group is active with
    /// the specified type(s), `Ok(false)` if not, or `Err(...)`
    /// if the group is invalid.
    pub fn layout_index_is_active(
        &self,
        idx: LayoutIndex,
        _type: StateComponent,
    ) -> Result<bool, LayoutIsActiveError> {
        if idx >= self.keymap.num_groups {
            return Err(LayoutIsActiveError::NoSuchLayoutIndex(idx));
        }

        if _type.intersects(StateComponent::LAYOUT_EFFECTIVE) && self.components.group == idx {
            return Ok(true);
        }

        let idx: i32 = idx
            .try_into()
            .map_err(|_| LayoutIsActiveError::NoSuchLayoutIndex(idx))?;

        if _type.intersects(StateComponent::LAYOUT_DEPRESSED) && self.components.base_group == idx {
            return Ok(true);
        }
        if _type.intersects(StateComponent::LAYOUT_LATCHED) && self.components.latched_group == idx
        {
            return Ok(true);
        }
        if _type.intersects(StateComponent::LAYOUT_LOCKED) && self.components.locked_group == idx {
            return Ok(true);
        }

        Ok(false)
    }

    /// Test whether a layout is active in a given keyboard state by name.
    ///
    /// Returns `Ok(true)` if the given modifier is active with
    /// the specified type(s), `Ok(false)` if not, or `Err(...)`
    /// if the modifier is invalid.
    ///
    /// If multiple layouts in the keymap have this name, the one with the lowest index is tested.
    pub fn layout_name_is_active(
        &self,
        name: &str,
        _type: StateComponent,
    ) -> Result<bool, LayoutIsActiveError> {
        let idx = match self.keymap.layout_get_index(name) {
            None => return Err(LayoutIsActiveError::NoSuchLayoutName(name.into())),
            Some(idx) => idx,
        };

        self.layout_index_is_active(idx, _type)
    }

    /// Test whether a LED is active in a given keyboard state by index.
    pub fn led_index_is_active(&self, idx: LedIndex) -> Result<bool, LedIsActiveError> {
        if idx >= self.keymap.leds.len() {
            return Err(LedIsActiveError::NoSuchLedIndex(idx));
        }

        if let Some(Some(_)) = self.keymap.leds.get(idx) {

            // do nothing
        } else {
            return Err(LedIsActiveError::NoSuchLedIndex(idx));
        }

        Ok((self.components.leds & (1 << idx)) != 0)
    }

    /// Test whether a LED is active in a given keyboard state by name.
    pub fn led_name_is_active(&self, name: impl Borrow<str>) -> Result<bool, LedIsActiveError> {
        let idx = match self.keymap.led_get_index(name.borrow()) {
            Some(idx) => idx,
            None => return Err(LedIsActiveError::NoSuchLedName(name.borrow().into())),
        };

        self.led_index_is_active(idx)
    }

    fn key_get_consumed(&self, key: &Key, mode: ConsumedMode) -> ModMask {
        let mut preserve: ModMask = 0;
        let mut consumed: ModMask = 0;

        let group = match self.key_get_layout(key.keycode.raw()) {
            Some(g) => g,
            None => return 0,
        };

        let _type = self
            .keymap
            .types
            .get(key.groups[group].key_type)
            .expect("Could not retrieve type for key");

        let matching_entry = self.get_entry_for_key_state(key, group);
        if let Some(entry) = matching_entry {
            preserve = entry.preserve.mask;
        }

        match mode {
            ConsumedMode::Xkb => consumed = _type.mods.mask,
            ConsumedMode::Gtk => {
                let no_mods_entry = _type.get_entry_for_mods(&0);
                let no_mods_leveli = match no_mods_entry {
                    Some(e) => e.level,
                    None => 0,
                };
                let no_mods_level = &key.groups[group].levels[no_mods_leveli];

                for entry in _type.entries.iter() {
                    if !entry.is_active() {
                        continue;
                    }

                    let level = &key.groups[group].levels[entry.level];
                    if level.same_syms(no_mods_level) {
                        continue;
                    }

                    use crate::utils::one_bit_set;
                    if Some(entry) == matching_entry || one_bit_set(entry.mods.mask) {
                        consumed |= entry.mods.mask & !entry.preserve.mask;
                    }
                }
            }
        };

        consumed & !preserve
    }

    /// Test whether a modifier is consumed by keyboard state translation for a key.
    pub fn mod_index_is_consumed2(
        &self,
        kc: RawKeycode,
        idx: ModIndex,
        mode: ConsumedMode,
    ) -> Result<bool, ModIndexIsConsumedError> {
        if idx >= self.keymap.num_mods() {
            return Err(ModIndexIsConsumedError::NoSuchModIndex(idx));
        }
        let key = self
            .keymap
            .xkb_key(kc)
            .ok_or_else(|| ModIndexIsConsumedError::NoSuchKeyAtKeycode(kc.into()))?;

        let mask = (1 << idx) & self.key_get_consumed(key, mode);

        Ok(mask > 0)
    }

    /// Same as [State::mod_index_is_consumed2()] with mode [ConsumedMode::Xkb]
    pub fn mod_index_is_consumed(
        &self,
        kc: Keycode,
        idx: ModIndex,
    ) -> Result<bool, ModIndexIsConsumedError> {
        self.mod_index_is_consumed2(kc.raw(), idx, ConsumedMode::Xkb)
    }

    /// Remove consumed modifiers from a modifier mask for a key.
    ///
    #[deprecated = "Use State::get_consumed_mods2() instead"]
    pub fn mod_mask_remove_consumed(&self, kc: Keycode, mask: ModMask) -> ModMask {
        let key = match self.keymap.xkb_key(kc.raw()) {
            Some(key) => key,
            None => return 0,
        };

        mask & !self.key_get_consumed(key, ConsumedMode::Xkb)
    }

    /// Get the mask of modifiers consumed by translating a given key.
    ///
    /// # Arguments
    /// * `key`: The keycode of the key
    /// * `mode`: The consumed modifiers mode to use
    ///
    /// # Output
    /// Returns a mask of the consumed modifiers.
    pub fn key_get_consumed_mods2(&self, kc: Keycode, mode: ConsumedMode) -> ModMask {
        // default case for unrecognized consumed modifiers mode
        let key = match self.keymap.xkb_key(kc.raw()) {
            Some(key) => key,
            None => return 0,
        };

        self.key_get_consumed(key, mode)
    }

    /// Same as [State::key_get_consumed_mods2()] with mode [ConsumedMode::Xkb]
    pub fn key_get_consumed_mods(&self, kc: Keycode) -> ModMask {
        self.key_get_consumed_mods2(kc, ConsumedMode::Xkb)
    }
}

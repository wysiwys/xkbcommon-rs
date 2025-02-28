// based on action.c and action.h
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

use super::ast::*;

use crate::context::Context;
use crate::errors::*;
use crate::keymap::*;
use crate::text::*;

use crate::rust_xkbcommon::*;

use std::collections::BTreeMap;

#[derive(Clone)]
pub(super) struct ActionsInfo {
    // TODO: replace with array of length _ACTION_TYPE_NUM_ENTRIES
    actions: BTreeMap<ActionType, Action>,
}

const CONST_TRUE: ExprDef = ExprDef::Boolean(ExprBoolean::new_true());
const CONST_FALSE: ExprDef = ExprDef::Boolean(ExprBoolean::new_false());

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ActionField {
    ClearLocks,
    LatchToLock,
    GenKeyEvent,
    Report,
    Default,
    Affect,
    Increment,
    Modifiers,
    Group,
    X,
    Y,
    Accel,
    Button,
    Value,
    Controls,
    Type,
    Count,
    Screen,
    Same,
    Data,
    Device,
    Keycode,
    ModsToClear,
}

impl ActionsInfo {
    pub(super) fn new() -> Self {
        use ActionType::*;

        let mut actions = BTreeMap::new();
        actions.insert(
            PtrDefault,
            Action::Dflt(DefaultAction {
                flags: ActionFlags::empty(),
                value: Some(1),
                action_type: PtrDefault,
            }),
        );
        actions.insert(
            PtrMove,
            Action::Ptr(PointerAction {
                action_type: PtrMove,

                flags: ActionFlags::Accel,
                x: Option::None,
                y: Option::None,
            }),
        );
        actions.insert(
            SwitchVT,
            Action::Screen(SwitchScreenAction {
                action_type: SwitchVT,
                flags: ActionFlags::SameScreen,
                screen: Option::None,
            }),
        );

        Self { actions }
    }
}

// changed to all lowercase
const FIELD_STRINGS: phf::Map<&'static str, ActionField> = phf::phf_map! {
    "clearlocks" => ActionField::ClearLocks,
    "latchtolock" => ActionField::LatchToLock,
    "genkeyevent" => ActionField::GenKeyEvent,
    "generatekeyevent" => ActionField::GenKeyEvent,
    "report" => ActionField::Report,
    "default" => ActionField::Default,
    "affect" => ActionField::Affect,
    "increment" => ActionField::Increment,
    "modifiers" => ActionField::Modifiers,
    "mods" => ActionField::Modifiers,
    "group" => ActionField::Group,
    "x" => ActionField::X,
    "y" => ActionField::Y,
    "accel" => ActionField::Accel,
    "accelerate" => ActionField::Accel,
    "repeat" => ActionField::Accel,
    "button" => ActionField::Button,
    "value" => ActionField::Value,
    "controls" => ActionField::Controls,
    "ctrls" => ActionField::Controls,
    "type" => ActionField::Type,
    "count" => ActionField::Count,
    "screen" => ActionField::Screen,
    "same" => ActionField::Same,
    "sameserver" => ActionField::Same,
    "data" => ActionField::Data,
    "device" => ActionField::Device,
    "dev" => ActionField::Device,
    "key" => ActionField::Keycode,
    "keycode" => ActionField::Keycode,
    "kc" => ActionField::Keycode,
    "clearmods" => ActionField::ModsToClear,
    "clearmodifiers" => ActionField::ModsToClear,

};

impl Context {
    fn report_mismatch(
        &self,
        code: XkbMessageCode,
        action: &ActionType,
        field: &ActionField,
        _type: &str,
    ) -> ReportedError {
        log::error!(
            "{:?}: Value of {:?} field must be of type {};
                Action {:?} definition ignored.",
            code,
            field,
            _type,
            action
        );

        ReportedError::ActionFieldMismatch {
            field: field.clone(),
            _type: _type.into(),
        }
    }
    fn report_illegal(&self, action: &ActionType, field: &ActionField) -> ReportedError {
        let err = XkbMessageCode::NoId;
        log::error!(
            "{:?}: Field {:?} is not defined for an action of type {:?};
                Action definition ignored.",
            err,
            field,
            action,
        );

        ReportedError::ActionFieldNotDefinedForType {
            field: field.clone(),
            _type: *action,
        }
    }
    fn report_action_not_array(&self, action: &ActionType, field: &ActionField) -> ReportedError {
        log::error!(
            "{:?}: The {:?} field in the {:?} action is not an array;
                Action definition ignored.",
            XkbError::WrongFieldType,
            field,
            action,
        );

        ReportedError::ActionNotArray {
            field: field.clone(),
            _type: *action,
        }
    }
}

fn check_boolean_flag(
    ctx: &Context,
    action: &ActionType,
    field: &ActionField,
    flag: ActionFlags,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
    flags_inout: &mut ActionFlags,
) -> Result<(), HandleActionError> {
    if array_ndx.is_some() {
        return Err(ctx.report_action_not_array(action, field).into());
    }

    let set = value.resolve_boolean(ctx).ok_or_else(|| {
        ctx.report_mismatch(XkbError::WrongFieldType.into(), action, field, "boolean")
    })?;

    if set {
        *flags_inout |= flag;
    } else {
        *flags_inout &= !flag;
    }

    Ok(())
}

fn check_modifier_field(
    ctx: &Context,
    mods: &ModSet,
    action: &ActionType,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
    flags_inout: &mut ActionFlags,
) -> Result<ModMask, HandleActionError> {
    if array_ndx.is_some() {
        return Err(ctx
            .report_action_not_array(action, &ActionField::Modifiers)
            .into());
    }

    if value.op_type() == ExprOpType::Ident {
        if let ExprDef::Ident(ref ident) = value {
            if let Some(val_str) = ctx.atom_text(ident.ident) {
                let val_str = val_str.to_lowercase();

                if ["usemodmapmods", "modmapmods"].contains(&val_str.as_str()) {
                    *flags_inout |= ActionFlags::ModsLookupModMap;
                    return Ok(0);
                }
            }
        }
    }

    let mods = value
        .resolve_mod_mask(ctx, ModType::BOTH, mods)
        .ok_or_else(|| {
            ctx.report_mismatch(
                XkbError::WrongFieldType.into(),
                action,
                &ActionField::Modifiers,
                "modifier mask",
            )
        })?;

    *flags_inout &= !ActionFlags::ModsLookupModMap;

    Ok(mods)
}

fn lock_which(s: &str) -> Option<ActionFlags> {
    match s {
        "both" => Some(ActionFlags::empty()),
        "lock" => Some(ActionFlags::LockNoUnlock),
        "neither" => Some(ActionFlags::LockNoLock | ActionFlags::LockNoUnlock),
        "unlock" => Some(ActionFlags::LockNoLock),
        _ => None,
    }
}

fn check_affect_field(
    ctx: &Context,
    action: &ActionType,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
    flags_inout: &mut ActionFlags,
) -> Result<(), HandleActionError> {
    if array_ndx.is_some() {
        return Err(ctx
            .report_action_not_array(action, &ActionField::Affect)
            .into());
    }

    let flags = value.resolve_enum(ctx, lock_which).ok_or_else(|| {
        ctx.report_mismatch(
            XkbError::WrongFieldType.into(),
            action,
            &ActionField::Affect,
            "lock, unlock, both, neither",
        )
    })?;

    *flags_inout &= !(ActionFlags::LockNoLock | ActionFlags::LockNoUnlock);

    *flags_inout |= flags;

    Ok(())
}

fn handle_set_latch_lock_mods(
    ctx: &Context,
    mods: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
) -> Result<(), HandleActionError> {
    if let Action::Mods(action) = action {
        if *field == ActionField::Modifiers {
            return check_modifier_field(
                ctx,
                mods,
                action_type,
                array_ndx,
                value,
                &mut action.flags,
            )
            .map(|mods| {
                action.mods.mods = mods;
            });
        }
        if [ActionType::ModSet, ActionType::ModLatch].contains(action_type)
            && *field == ActionField::ClearLocks
        {
            return check_boolean_flag(
                ctx,
                action_type,
                field,
                ActionFlags::LockClear,
                array_ndx,
                value,
                &mut action.flags,
            );
        }
        if *action_type == ActionType::ModLatch && *field == ActionField::LatchToLock {
            return check_boolean_flag(
                ctx,
                action_type,
                field,
                ActionFlags::LatchToLock,
                array_ndx,
                value,
                &mut action.flags,
            );
        }
        if *action_type == ActionType::ModLock && *field == ActionField::Affect {
            return check_affect_field(ctx, action_type, array_ndx, value, &mut action.flags);
        }
    }
    Err(ctx.report_illegal(action_type, field).into())
}

fn check_group_field(
    ctx: &Context,
    action: &ActionType,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
    flags_inout: &mut ActionFlags,
) -> Result<i32, HandleActionError> {
    let mut flags = flags_inout.clone();

    if array_ndx.is_some() {
        return Err(ctx
            .report_action_not_array(action, &ActionField::Group)
            .into());
    }

    let op = value.op_type();

    use ExprOpType::*;
    let spec = match value {
        ExprDef::Unary(unary) if [Negate, UnaryPlus].contains(&unary.op) => {
            flags &= !ActionFlags::AbsoluteSwitch;
            *unary.child
        }
        _ => {
            flags |= ActionFlags::AbsoluteSwitch;
            value
        }
    };

    let idx = spec.resolve_group(ctx).ok_or_else(|| {
        ctx.report_mismatch(
            XkbError::UnsupportedGroupIndex.into(),
            action,
            &ActionField::Group,
            "integer (range 1..8)",
        )
    })?;

    let idx = i32::try_from(idx).unwrap();
    let group_rtrn: i32 = match op {
        ExprOpType::Negate => -idx,
        ExprOpType::UnaryPlus => idx,
        _ => idx - 1,
    };

    *flags_inout = flags;
    Ok(group_rtrn)
}

fn handle_set_latch_lock_group(
    ctx: &Context,
    _: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
) -> Result<(), HandleActionError> {
    if let Action::Group(act) = action {
        if *field == ActionField::Group {
            return check_group_field(ctx, action_type, array_ndx, value, &mut act.flags).map(
                |group| {
                    // If group returned, set it as act.group
                    act.group = Some(group);
                },
            );
        } else if [ActionType::GroupSet, ActionType::GroupLatch].contains(action_type)
            && *field == ActionField::ClearLocks
        {
            return check_boolean_flag(
                ctx,
                action_type,
                field,
                ActionFlags::LockClear,
                array_ndx,
                value,
                &mut act.flags,
            );
        } else if ActionType::GroupLatch == *action_type && *field == ActionField::LatchToLock {
            return check_boolean_flag(
                ctx,
                action_type,
                field,
                ActionFlags::LatchToLock,
                array_ndx,
                value,
                &mut act.flags,
            );
        }
    }

    Err(ctx.report_illegal(action_type, field).into())
}

fn handle_move_ptr(
    ctx: &Context,
    _: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
) -> Result<(), HandleActionError> {
    if let Action::Ptr(act) = action {
        if [ActionField::X, ActionField::Y].contains(field) {
            let op = value.op_type();
            let absolute = op != ExprOpType::Negate && op != ExprOpType::UnaryPlus;

            if array_ndx.is_some() {
                return Err(ctx.report_action_not_array(action_type, field).into());
            }
            let val = value.resolve_integer(ctx).ok_or_else(|| {
                ctx.report_mismatch(
                    XkbError::WrongFieldType.into(),
                    action_type,
                    field,
                    "integer",
                )
            })?;
            if let Ok(val) = i16::try_from(val) {
                if *field == ActionField::X {
                    if absolute {
                        act.flags |= ActionFlags::AbsoluteX;
                    }
                    act.x = Some(val);
                } else {
                    if absolute {
                        act.flags |= ActionFlags::AbsoluteY;
                    }
                    act.y = Some(val);
                }

                return Ok(());
            }
        } else if *field == ActionField::Accel {
            return check_boolean_flag(
                ctx,
                action_type,
                field,
                ActionFlags::Accel,
                array_ndx,
                value,
                &mut act.flags,
            );
        }
    }
    Err(ctx.report_illegal(action_type, field).into())
}

fn handle_ptr_btn(
    ctx: &Context,
    _: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
) -> Result<(), HandleActionError> {
    if let Action::Btn(act) = action {
        if *field == ActionField::Button {
            if array_ndx.is_some() {
                return Err(ctx.report_action_not_array(action_type, field).into());
            }

            let btn = value.resolve_button(ctx).ok_or_else(|| {
                ctx.report_mismatch(
                    XkbError::WrongFieldType.into(),
                    action_type,
                    field,
                    "integer (range 1..5)",
                )
            })?;

            if !(0..=5).contains(&btn) {
                let err = XkbMessageCode::NoId;

                log::error!(
                    "{:?}: Button must specify default or be in the range 1..5;
                        Illegal button value {:?} ignored",
                    err,
                    btn
                );

                return Err(HandleActionError::InvalidButtonCode(btn));
            }

            act.button = Some(btn.try_into().unwrap());
            return Ok(());
        } else if *field == ActionField::Affect && *action_type == ActionType::PtrLock {
            return check_affect_field(ctx, action_type, array_ndx, value, &mut act.flags);
        } else if *field == ActionField::Count {
            if array_ndx.is_some() {
                return Err(ctx.report_action_not_array(action_type, field).into());
            }
            let val = value.resolve_integer(ctx).ok_or_else(|| {
                ctx.report_mismatch(
                    XkbError::WrongFieldType.into(),
                    action_type,
                    field,
                    "integer",
                )
            })?;

            act.count = val.try_into().map_err(|err| {
                log::error!(
                    "{:?}: The count field must have a value in the range 0.255;
                        Illegal count {:?} ignored",
                    err,
                    val
                );

                HandleActionError::InvalidCountField(val)
            })?;

            return Ok(());
        }
    }

    Err(ctx.report_illegal(action_type, field).into())
}

fn ptr_dflts(s: &str) -> Option<bool> {
    match s.to_lowercase().as_str() {
        "dfltbtn" => Some(true),
        "defaultbutton" => Some(true),
        "button" => Some(true),
        _ => None,
    }
}

fn handle_set_ptr_dflt(
    ctx: &Context,
    _: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
) -> Result<(), HandleActionError> {
    if let Action::Dflt(act) = action {
        if *field == ActionField::Affect {
            if array_ndx.is_some() {
                return Err(ctx.report_action_not_array(action_type, field).into());
            }

            return match value.resolve_enum(ctx, ptr_dflts) {
                Some(true) => Ok(()),
                _ => Err(ctx
                    .report_mismatch(
                        XkbError::WrongFieldType.into(),
                        action_type,
                        field,
                        "pointer component",
                    )
                    .into()),
            };
        } else if *field == ActionField::Button || *field == ActionField::Value {
            if array_ndx.is_some() {
                return Err(ctx.report_action_not_array(action_type, field).into());
            }

            let op = value.op_type();
            use ExprOpType::*;
            let button = match value {
                ExprDef::Unary(unary) if [Negate, UnaryPlus].contains(&op) => {
                    act.flags &= !ActionFlags::AbsoluteSwitch;
                    *unary.child
                }
                value => {
                    act.flags |= ActionFlags::AbsoluteSwitch;
                    value
                }
            };

            let btn: i64 = button.resolve_button(ctx).ok_or_else(|| {
                ctx.report_mismatch(
                    XkbError::WrongFieldType.into(),
                    action_type,
                    field,
                    "integer (range 1..5)",
                )
            })?;

            if !(1..=5).contains(&btn) {
                let err = XkbMessageCode::NoId;

                log::error!(
                    "{:?}: New default button value must be in the range 1..5;
                        Illegal default button value {:?} ignored",
                    err,
                    btn
                );

                return Err(HandleActionError::InvalidButtonCode(btn));
            }

            if btn == 0 {
                let err = XkbMessageCode::NoId;

                log::error!(
                    "{:?}: Cannot set default pointer button to \"default\";
                        Illegal default button setting ignored",
                    err
                );

                return Err(HandleActionError::DefaultPtrBtnCannotBeZero);
            }

            let btn = match op {
                Negate => -btn,
                _ => btn,
            } as i8;

            act.value = Some(btn);

            return Ok(());
        }
    }

    Err(ctx.report_illegal(action_type, field).into())
}

fn handle_switch_screen(
    ctx: &Context,
    _: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
) -> Result<(), HandleActionError> {
    if let Action::Screen(act) = action {
        if *field == ActionField::Screen {
            if array_ndx.is_some() {
                return Err(ctx.report_action_not_array(action_type, field).into());
            }

            let op = value.op_type();
            use ExprOpType::*;
            let scrn = match value {
                ExprDef::Unary(unary) if [Negate, UnaryPlus].contains(&op) => {
                    act.flags &= !ActionFlags::AbsoluteSwitch;
                    *unary.child
                }
                value => {
                    act.flags |= ActionFlags::AbsoluteSwitch;
                    value
                }
            };

            let val = scrn.resolve_integer(ctx).ok_or_else(|| {
                ctx.report_mismatch(
                    XkbError::WrongFieldType.into(),
                    action_type,
                    field,
                    "integer (0..255)",
                )
            })?;

            // TODO: i8 or i16??
            // Report this as a bug if necessary
            let val: i8 = val.try_into().ok().filter(|i| *i >= 1).ok_or_else(|| {
                log::error!(
                    "{:?}: Screen index must be in the range 1..255;
                        Illegal screen value {:?} ignored.",
                    XkbMessageCode::NoId,
                    val
                );

                HandleActionError::IllegalScreenIndex(val)
            })?;

            act.screen = match op {
                Negate => Some(-val),
                _ => Some(val),
            };

            return Ok(());
        } else if *field == ActionField::Same {
            return check_boolean_flag(
                ctx,
                action_type,
                field,
                ActionFlags::SameScreen,
                array_ndx,
                value,
                &mut act.flags,
            );
        }
    }

    Err(ctx.report_illegal(action_type, field).into())
}

fn handle_set_lock_controls(
    ctx: &Context,
    _: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
) -> Result<(), HandleActionError> {
    if let Action::Ctrls(act) = action {
        if *field == ActionField::Controls {
            if array_ndx.is_some() {
                return Err(ctx.report_action_not_array(action_type, field).into());
            }

            let mask = value
                .resolve_mask(ctx, |ident, _, ctx| {
                    let s = ctx.atom_text(ident)?;
                    lookup_key(&CTRL_MASK_NAMES, s).copied()
                })
                .ok_or_else(|| {
                    ctx.report_mismatch(
                        XkbError::WrongFieldType.into(),
                        action_type,
                        field,
                        "controls mask",
                    )
                })?;

            act.ctrls = mask;

            return Ok(());
        } else if *field == ActionField::Affect {
            return check_affect_field(ctx, action_type, array_ndx, value, &mut act.flags);
        }
    }

    Err(ctx.report_illegal(action_type, field).into())
}

fn handle_private(
    ctx: &Context,
    _: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
) -> Result<(), HandleActionError> {
    if let Action::Private(act) = action {
        if *field == ActionField::Type {
            if array_ndx.is_some() {
                return Err(ctx.report_action_not_array(action_type, field).into());
            }

            let _type = value.resolve_integer(ctx).ok_or_else(|| {
                ctx.report_mismatch(
                    XkbError::WrongFieldType.into(),
                    &ActionType::Private,
                    field,
                    "integer",
                )
            })?;

            let _type: u8 = _type.try_into().map_err(|_| {
                let err = XkbMessageCode::NoId;
                log::error!(
                    "{:?}: Private action type must be in the range 
                            0..255; Illegal type {:?} ignored",
                    err,
                    _type
                );

                HandleActionError::PrivateActionTypeNotU8(_type)
            })?;
            let _type: ActionType = _type.into();
            if _type < ActionType::Private {
                log::info!(
                    "{:?}: Private actions of type % are not supported; Ignored",
                    XkbMessageCode::NoId
                );
                act.action_type = ActionType::None;
            } else {
                act.action_type = _type;
            }

            return Ok(());
        } else if *field == ActionField::Data {
            if array_ndx.is_none() {
                let val = value.resolve_string(ctx).ok_or_else(|| {
                    ctx.report_mismatch(
                        XkbError::WrongFieldType.into(),
                        &action.action_type(),
                        field,
                        "string",
                    )
                })?;

                let s = ctx.xkb_atom_text(val);

                let data_size = std::mem::size_of::<ActionData>();
                if s.is_empty() || s.len() > data_size {
                    log::warn!(
                        "{:?}: A private action has {} data bytes; 
                            Illegal data ignored",
                        XkbMessageCode::NoId,
                        data_size
                    );

                    return Err(HandleActionError::PrivateActionInvalidSize(data_size));
                }

                return Ok(());
            } else {
                let ndx = array_ndx
                    .and_then(|i| i.resolve_integer(ctx))
                    .ok_or_else(|| {
                        log::error!(
                            "{:?}: Array subscript must be integer;
                            Illegal subscript ignored",
                            XkbMessageCode::NoId
                        );
                        HandleActionError::ArraySubscriptMustBeInt
                    })?;

                let ndx: usize = ndx
                    .try_into()
                    .ok()
                    .filter(|i| *i < ACTION_DATA_LEN)
                    .ok_or_else(|| {
                        log::error!(
                            "{:?}: The data for a private action has {} entries;
                            attempted to use data[{}] ignored",
                            XkbMessageCode::NoId,
                            ACTION_DATA_LEN,
                            ndx
                        );
                        HandleActionError::PrivateActionExceedMaxIndex {
                            max: ACTION_DATA_LEN,
                            index: ndx,
                        }
                    })?;

                let datum = value.resolve_integer(ctx).ok_or_else(|| {
                    ctx.report_mismatch(
                        XkbError::WrongFieldType.into(),
                        &act.action_type,
                        field,
                        "integer",
                    )
                })?;

                let datum: u8 = datum.try_into().map_err(|_| {
                    log::error!(
                        "{:?}: All data for a private action must 0..255;
                            Illegal datum {} ignored",
                        XkbMessageCode::NoId,
                        datum
                    );
                    HandleActionError::PrivateActionIllegalDatum(datum)
                })?;

                act.data[ndx] = Some(datum);
                return Ok(());
            }
        }
    }

    Err(ctx.report_illegal(&ActionType::None, field).into())
}

impl Action {
    fn handle(
        &mut self,
        ctx: &Context,
        mods: &ModSet,
        action_type: &ActionType,
        field: &ActionField,
        array_ndx: Option<ExprDef>,
        value: ExprDef,
    ) -> Result<(), HandleActionError> {
        use ActionType::*;
        match action_type {
            None => Ok(()),
            e if [ModSet, ModLatch, ModLock].contains(e) => {
                handle_set_latch_lock_mods(ctx, mods, self, action_type, field, array_ndx, value)
            }
            e if [GroupSet, GroupLatch, GroupLock].contains(e) => {
                handle_set_latch_lock_group(ctx, mods, self, action_type, field, array_ndx, value)
            }
            PtrMove => handle_move_ptr(ctx, mods, self, action_type, field, array_ndx, value),
            e if [PtrButton, PtrLock].contains(e) => {
                handle_ptr_btn(ctx, mods, self, action_type, field, array_ndx, value)
            }
            PtrDefault => {
                handle_set_ptr_dflt(ctx, mods, self, action_type, field, array_ndx, value)
            }
            Terminate => Ok(()),
            SwitchVT => handle_switch_screen(ctx, mods, self, action_type, field, array_ndx, value),
            e if [CtrlSet, CtrlLock].contains(e) => {
                handle_set_lock_controls(ctx, mods, self, action_type, field, array_ndx, value)
            }
            Private => handle_private(ctx, mods, self, action_type, field, array_ndx, value),
            _ => panic!("Should have already handled all types"),
        }
    }
}
impl ActionsInfo {
    pub(super) fn handle_action_def(
        &mut self,
        ctx: &Context,
        mods: &ModSet,
        def: ExprDef,
    ) -> Result<Action, HandleActionError> {
        let def = match def {
            ExprDef::Action(def) if def.op == ExprOpType::ActionDecl => def,
            _ => {
                log::error!(
                    "{:?}: Expected an action definition, found {:?}",
                    XkbError::WrongFieldType,
                    def.op_type()
                );

                return Err(HandleActionError::NotAnActionDefinition(def.op_type()));
            }
        };
        let name = ctx.xkb_atom_text(def.name);

        let handler_type = match lookup_key(&ACTION_TYPE_NAMES, name) {
            Some(h) if *h != ActionType::None => h,
            Some(_) => return Ok(Action::None), //TODO: is this correct?
            None => {
                let err = XkbMessageCode::NoId;
                log::error!("{:?}: Unknown action {}", err, name);
                return Err(HandleActionError::UnknownAction(name.into()));
            }
        };

        // Get the default values for this action type
        // Create if does not exist yet
        let mut action: Action = match self.actions.get(handler_type) {
            Some(action) => action.clone(), //already exists
            None => {
                let new_action = Action::empty_from(*handler_type);
                self.actions.insert(*handler_type, new_action);
                self.actions[handler_type].clone()
            }
        };

        // Now change the action properties as specified
        // for this particular instance, e.g. "modifers"
        // and "clearLocks" in:
        //  SetMods(modifiers=Alt,clearLocks);

        for arg in def.args {
            let field;
            let value;
            let op = arg.op_type();

            use ExprOpType::*;
            match arg {
                ExprDef::Binary(binary) if binary.op == Assign => {
                    field = *binary.left;
                    value = *binary.right;
                }
                ExprDef::Unary(unary) if [Not, Invert].contains(&op) => {
                    field = *unary.child;
                    value = CONST_FALSE;
                }
                arg => {
                    field = arg;
                    value = CONST_TRUE;
                }
            }

            let lhs = field
                .resolve_lhs(ctx)
                .ok_or(HandleActionError::CouldNotResolveLhs)?;

            if let Some(elem) = lhs.elem {
                let err = XkbError::GlobalDefaultsWrongScope;
                log::error!(
                    "{:?}: Cannot change defaults
                        in an action definition; Ignoring attempts
                        to change \"{}.{}\"",
                    err,
                    elem,
                    lhs.field
                );
                return Err(HandleActionError::CannotChangeDefaults);
            }

            let field_ndx = FIELD_STRINGS
                .get(&lhs.field.to_lowercase())
                .ok_or_else(|| {
                    let err = XkbMessageCode::NoId;
                    log::error!("{:?}: Unknown field name {}", err, lhs.field);
                    HandleActionError::UnknownFieldName(lhs.field)
                })?;

            action.handle(ctx, mods, handler_type, field_ndx, lhs.index, value)?;
        }

        Ok(action)
    }

    pub(super) fn set_action_field(
        &mut self,
        ctx: &Context,
        mods: &ModSet,
        elem: Option<String>,
        field: &str,
        array_ndx: Option<ExprDef>,
        value: ExprDef,
    ) -> Result<(), HandleActionError> {
        let elem = elem.ok_or(HandleActionError::ActionTypeMissing)?;

        let action_type =
            lookup_key(&ACTION_TYPE_NAMES, &elem).ok_or(HandleActionError::UnknownAction(elem))?;

        let action_field = FIELD_STRINGS.get(&field.to_lowercase()).ok_or_else(|| {
            log::error!(
                "{:?}: {:?} is not a legal field name",
                XkbMessageCode::NoId,
                field
            );
            HandleActionError::IllegalFieldName(field.into())
        })?;

        let action = match self.actions.get_mut(action_type) {
            Some(a) => a,
            None => {
                // create a new one
                self.actions
                    .insert(*action_type, Action::empty_from(*action_type));
                self.actions.get_mut(action_type).unwrap()
            }
        };

        action.handle(ctx, mods, action_type, action_field, array_ndx, value)
    }
}

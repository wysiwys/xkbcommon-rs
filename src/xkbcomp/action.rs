use super::ast::*;

use crate::context::Context;
use crate::errors::*;
use crate::keymap::*;
use crate::text::*;

use crate::rust_xkbcommon::*;

use std::collections::HashMap;

#[derive(Clone)]
pub(super) struct ActionsInfo {
    actions: HashMap<ActionType, Action>,
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

        let mut actions = HashMap::new();
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

        ReportedError::ActionFieldMismatch{
            field: field.clone(),
            _type: _type.into()
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

        return ReportedError::ActionFieldNotDefinedForType{
            field: field.clone(), _type: action.clone()};
    }
    fn report_action_not_array(&self, action: &ActionType, field: &ActionField) -> ReportedError {
        let code = XkbMessageCode::NoId;
        log::error!(
            "{:?}: The {:?} field in the {:?} action is not an array;
                Action definition ignored.",
            code,
            field,
            action,
        );

        ReportedError::ActionNotArray{
            field: field.clone(), _type: action.clone()
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

    let set = value.resolve_boolean(ctx)
        .ok_or_else(||
            ctx.report_mismatch(XkbError::WrongFieldType.into(), action, field, "boolean")
            )?;

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
        return Err(ctx.report_action_not_array(action, &ActionField::Modifiers).into());
    }

    if value.op_type() == ExprOpType::Ident {
        if let ExprDef::Ident(ref ident) = value {
            if let Some(val_str) = ctx.xkb_atom_text(ident.ident) {
                let val_str = val_str.to_lowercase();

                if val_str.as_str() == "usemodmapmods" || val_str.as_str() == "modmapmods" {
                    *flags_inout |= ActionFlags::ModsLookupModMap;
                    return Ok(0);
                }
            }
        }
    }

    let mods = value
        .resolve_mod_mask(ctx, ModType::BOTH, mods)
        .ok_or_else(|| ctx.report_mismatch(
            XkbError::WrongFieldType.into(),
            action,
            &ActionField::Modifiers,
            "modifier mask",
        ))?;

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
        return Err(ctx.report_action_not_array(action, &ActionField::Affect).into());
    }

    let flags = value.resolve_enum(ctx, lock_which)
        .ok_or_else(|| {
            ctx.report_mismatch(
                XkbError::WrongFieldType.into(),
                action,
                &ActionField::Affect,
                "lock,unlock,both,neither",
            )

            }
        )?;

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
            return match check_modifier_field(
                ctx,
                mods,
                action_type,
                array_ndx,
                value,
                &mut action.flags,
            ) {
                Err(e) => Err(e.into()),
                Ok(mods) => {
                    action.mods.mods = mods;
                    return Ok(());
                }
            };
        } else if [ActionType::ModSet, ActionType::ModLatch].contains(&action_type)
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
            )
            .map_err(|e| e.into());
        } else if *action_type == ActionType::ModLatch && *field == ActionField::LatchToLock {
            return check_boolean_flag(
                ctx,
                action_type,
                field,
                ActionFlags::LatchToLock,
                array_ndx,
                value,
                &mut action.flags,
            )
            .map_err(|e| e.into());
        } else if *action_type == ActionType::ModLock && *field == ActionField::Affect {
            return check_affect_field(ctx, action_type, array_ndx, value, &mut action.flags)
                .map_err(|e| e.into());
        }
    }
    return Err(ctx.report_illegal(action_type, field).into());
}

fn check_group_field(
    ctx: &Context,
    action: &ActionType,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
    flags_inout: &mut ActionFlags,
) -> Result<i32, HandleActionError> {
    let spec;
    let mut flags = flags_inout.clone();

    if array_ndx.is_some() {
        return Err(ctx.report_action_not_array(action, &ActionField::Group).into());
    }

    let op = value.op_type();

    use ExprOpType::*;
    match value {
        ExprDef::Unary(unary) if [Negate, UnaryPlus].contains(&unary.op) => {
            flags &= !ActionFlags::AbsoluteSwitch;
            spec = *unary.child;
        }
        _ => {
            flags |= ActionFlags::AbsoluteSwitch;
            spec = value;
        }
    }

    let idx = spec.resolve_group(ctx)
        .ok_or_else(||
            ctx.report_mismatch(
                XkbError::UnsupportedGroupIndex.into(),
                action,
                &ActionField::Group,
                "integer (range 1..8)",
            )
        )?;

   
    let group_rtrn: i32 = match op {
        ExprOpType::Negate => -1 * i32::try_from(idx).unwrap(),
        ExprOpType::UnaryPlus => idx.try_into().unwrap(),
        _ => i32::try_from(idx).unwrap() - 1,
    };

    *flags_inout = flags;
    return Ok(group_rtrn);
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
            return match check_group_field(ctx, action_type, array_ndx, value, &mut act.flags) {
                Err(e) => Err(e.into()),
                Ok(group) => {
                    act.group = Some(group);
                    Ok(())
                }
            };
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
            )
            .map_err(|e| e.into());
        } else if ActionType::GroupLatch == *action_type && *field == ActionField::LatchToLock {
            return check_boolean_flag(
                ctx,
                action_type,
                field,
                ActionFlags::LatchToLock,
                array_ndx,
                value,
                &mut act.flags,
            )
            .map_err(|e| e.into());
        }
    }

    return Err(ctx.report_illegal(action_type, field).into());
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
            let val = value.resolve_integer(ctx)
                .ok_or_else(||
                    ctx
                    .report_mismatch(
                        XkbError::WrongFieldType.into(),
                        action_type,
                        field,
                        "integer",
                    )
                )?;
            if let Ok(val) =i16::try_from(val) {
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
            )
            .map_err(|e| e.into());
        }
    }
    return Err(ctx.report_illegal(action_type, field).into());
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

            let btn = value.resolve_button(ctx)
                .ok_or_else(|| 
                    ctx
                        .report_mismatch(
                            XkbError::WrongFieldType.into(),
                            action_type,
                            field,
                            "integer (range 1..5)",
                        )
                )?;

            // TODO: is this check correct?
            if btn < 0 || btn > 5 {
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
            return check_affect_field(ctx, action_type, array_ndx, value, &mut act.flags)
                .map_err(|e| e.into());
        } else if *field == ActionField::Count {
            if array_ndx.is_some() {
                return Err(ctx.report_action_not_array(action_type, field).into());
            }
            let val = value.resolve_integer(ctx)
                .ok_or_else(|| 
                        ctx
                        .report_mismatch(
                            XkbError::WrongFieldType.into(),
                            action_type,
                            field,
                            "integer",
                        )
                )?;

            act.count = val.try_into()
                .map_err(|err| {

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

    return Err(ctx.report_illegal(action_type, field).into());
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

            return match value
                .resolve_enum(ctx, ptr_dflts) {
                    Some(true) => Ok(()),
                    _ => Err(ctx
                    .report_mismatch(
                        XkbError::WrongFieldType.into(),
                        action_type,
                        field,
                        "pointer component").into())
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

            let btn = button.resolve_button(ctx)
                .ok_or_else(|| 
                    ctx
                    .report_mismatch(
                        XkbError::WrongFieldType.into(),
                        action_type,
                        field,
                        "integer (range 1..5)",
                    )
                )?;


            if btn < 1 || btn > 5 {
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

            act.value = match op {
                Negate => Some(-1 * i8::try_from(btn).unwrap()),
                _ => Some(btn.try_into().unwrap()),
            };
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

            let val = scrn.resolve_integer(ctx)
                .ok_or_else(||

                    ctx
                    .report_mismatch(
                        XkbError::WrongFieldType.into(),
                        action_type,
                        field,
                        "integer (0..255)",
                    )
                )?;

            // TODO: i8 or i16??
            // Report this as a bug if necessary
            let val: i8 = match val.try_into() {
                Ok(val) if val >= 1 => val,
                _ =>  {
                let err = XkbMessageCode::NoId;
                log::error!(
                    "{:?}: Screen index must be in the range 1..255;
                        Illegal screen value {:?} ignored.",
                    err,
                    val
                );

                return Err(HandleActionError::IllegalScreenIndex(val));
                }
            };


            act.screen = match op {
                Negate => Some(-1 * val),
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
            )
            .map_err(|e| e.into());
        }
    }

    return Err(ctx.report_illegal(action_type, field).into());
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

            let mask = value.resolve_mask(ctx, |ident, _, ctx| {
                let s = ctx.xkb_atom_text(ident)?;
                lookup_key(&CTRL_MASK_NAMES, s).copied()
            });

            act.ctrls = match mask {
                None => {
                    return Err(ctx
                        .report_mismatch(
                            XkbError::WrongFieldType.into(),
                            action_type,
                            field,
                            "controls mask",
                        )
                        .into())
                }
                Some(mask) => mask,
            };

            return Ok(());
        } else if *field == ActionField::Affect {
            return check_affect_field(ctx, action_type, array_ndx, value, &mut act.flags)
                .map_err(|e| e.into());
        }
    }

    return Err(ctx.report_illegal(action_type, field).into());
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

            let _type = match value.resolve_integer(ctx) {
                Some(v) => v,
                None => {
                    return Err(ctx
                        .report_mismatch(
                            XkbError::WrongFieldType.into(),
                            &ActionType::Private,
                            field,
                            "integer",
                        )
                        .into())
                }
            };

            let _type: u8 = _type.try_into()
                .map_err(|_| {

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
                let val = match value.resolve_string(ctx) {
                    Some(val) => val,
                    None => {
                        return Err(ctx
                            .report_mismatch(
                                XkbError::WrongFieldType.into(),
                                &action.action_type(),
                                field,
                                "string",
                            )
                            .into())
                    }
                };

                let s = ctx.xkb_atom_text(val)
                    .unwrap_or_else(|| "");

                let data_size = std::mem::size_of::<ActionData>();
                if s.len() < 1 || s.len() > data_size {
                    let err = XkbMessageCode::NoId;
                    log::warn!(
                        "{:?}: A private action has {} data bytes; 
                            Illegal data ignored",
                        err,
                        data_size
                    );

                    return Err(HandleActionError::PrivateActionInvalidSize(data_size));
                }

                return Ok(());
            } else {
                let ndx = array_ndx
                    .map(|i| i.resolve_integer(ctx))
                    .flatten()
                    .ok_or_else(|| {
                        let err = XkbMessageCode::NoId;
                        log::error!(
                            "{:?}: Array subscript must be integer;
                            Illegal subscript ignored",
                            err
                        );
                        HandleActionError::ArraySubscriptMustBeInt
                    })?;

                let ndx: usize = match ndx.try_into() {
                    Ok(ndx) if ndx < ACTION_DATA_LEN => ndx,
                    _ => {
                        let err = XkbMessageCode::NoId;
                        log::error!(
                            "{:?}: The data for a private action has {} entries;
                            attempted to use data[{}] ignored",
                            err,
                            ACTION_DATA_LEN,
                            ndx
                        );
                        return Err(HandleActionError::PrivateActionExceedMaxIndex{
                            max: ACTION_DATA_LEN,
                            index: ndx});
                    }
                };

                let datum = match value.resolve_integer(ctx) {
                    Some(datum) => datum,
                    None => {
                        return Err(ctx
                            .report_mismatch(
                                XkbError::WrongFieldType.into(),
                                &act.action_type,
                                field,
                                "integer",
                            )
                            .into())
                    }
                };

                let datum: u8 = match datum.try_into() {
                    Ok(datum) => datum,
                    Err(_) => {
                        let err = XkbMessageCode::NoId;
                        log::error!(
                            "{:?}: All data for a private action must 0..255;
                            Illegal datum {} ignored",
                            err,
                            datum
                        );
                        return Err(HandleActionError::PrivateActionIllegalDatum(datum));
                    }
                };

                act.data[ndx] = Some(datum);
                return Ok(());
            }
        }
    }

    return Err(ctx.report_illegal(&ActionType::None, field).into());
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
            e if [ModSet, ModLatch, ModLock].contains(&e) => {
                handle_set_latch_lock_mods(ctx, mods, self, action_type, field, array_ndx, value)
            }
            e if [GroupSet, GroupLatch, GroupLock].contains(&e) => {
                handle_set_latch_lock_group(ctx, mods, self, action_type, field, array_ndx, value)
            }
            PtrMove => handle_move_ptr(ctx, mods, self, action_type, field, array_ndx, value),
            e if [PtrButton, PtrLock].contains(&e) => {
                handle_ptr_btn(ctx, mods, self, action_type, field, array_ndx, value)
            }
            PtrDefault => {
                handle_set_ptr_dflt(ctx, mods, self, action_type, field, array_ndx, value)
            }
            Terminate => Ok(()),
            SwitchVT => handle_switch_screen(ctx, mods, self, action_type, field, array_ndx, value),
            e if [CtrlSet, CtrlLock].contains(&e) => {
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
    ) -> Result<Option<Action>, HandleActionError> {
        if def.op_type() == ExprOpType::ActionDecl {
            if let ExprDef::Action(def) = def {
                let name = match ctx.xkb_atom_text(def.name) {
                    Some(s) => s,
                    None => "".into(),
                };

                let handler_type = match lookup_key(&ACTION_TYPE_NAMES, name) {
                    Some(h) if *h != ActionType::None => h,
                    Some(_) => return Ok(None), //TODO: is this correct?
                    None => {
                        let err = XkbMessageCode::NoId;
                        log::error!("{:?}: Unknown action {}", err, name);
                        return Err(HandleActionError::UnknownAction(name.into()));
                    }
                };

                // Get the default values for this action type

                let mut action: Action = match self.actions.get(handler_type) {
                    Some(action) => action.clone(), //already exists
                    None => {
                        let new_action = Action::empty_from(*handler_type);
                        self.actions.insert(*handler_type, new_action);
                        self.actions.get(handler_type).unwrap().clone()
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
                    match *arg {
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

                    let lhs = field.resolve_lhs(ctx)
                        .ok_or_else(|| HandleActionError::CouldNotResolveLhs)?;

                    if let Some(elem) = lhs.elem {
                        let err = XkbMessageCode::NoId;
                        log::error!(
                            "{:?}: Cannot change defaults
                        in an action definition; Ignoring attempts
                        to change {}.{}",
                            err,
                            elem,
                            lhs.field
                        );
                        return Err(HandleActionError::CannotChangeDefaults);
                    }

                    let field_ndx = FIELD_STRINGS.get(&lhs.field.to_lowercase())
                    .ok_or_else(|| {
                            let err = XkbMessageCode::NoId;
                            log::error!("{:?}: Unknown field name {}", err, lhs.field);
                            HandleActionError::UnknownFieldName(lhs.field)
                        })?;

                    action.handle(ctx, mods, handler_type, field_ndx, lhs.index, value)?;
                }

                return Ok(Some(action));
            }
        }

        let err = XkbMessageCode::NoId;
        log::error!(
            "{:?}: Expected an action definition, found {:?}",
            err,
            def.op_type()
        );

        Err(HandleActionError::NotAnActionDefinition(def.op_type()))
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
        // TODO: replace errors?

        let elem = elem.ok_or_else(|| HandleActionError::ActionTypeMissing)?;

        let action_type = lookup_key(&ACTION_TYPE_NAMES, &elem)
            .ok_or_else(|| HandleActionError::UnknownAction(elem))?;

        let action_field = FIELD_STRINGS.get(&field.to_lowercase())
            .ok_or_else(|| {
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

        return action.handle(ctx, mods, action_type, action_field, array_ndx, value);
    }
}

use super::ast::*;

use crate::keymap::*;
use crate::context::Context;
use crate::errors::*;
use crate::text::*;

use crate::rust_xkbcommon::*;

use std::collections::HashMap;


#[derive(Clone)]
pub(super) struct ActionsInfo {
    actions: HashMap<ActionType, Action>
}


const CONST_TRUE: ExprDef 
    = ExprDef::Boolean(ExprBoolean::new_true());
const CONST_FALSE: ExprDef 
    = ExprDef::Boolean(ExprBoolean::new_false());

#[derive(Debug, PartialEq)]
enum ActionField {
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

        let none = None;
        use ActionType::*;

        let mut actions = HashMap::new();
        actions.insert(
            PtrDefault, 
            Action::Dflt(DefaultAction{
                flags: ActionFlags::empty(), value: Some(1),
            action_type: PtrDefault }));
        actions.insert(
            PtrMove,
            Action::Ptr(PointerAction{
                action_type: PtrMove,
                
                flags: ActionFlags::Accel,
                x: Option::None, y: Option::None}));
        actions.insert(
            SwitchVT,
            Action::Screen(SwitchScreenAction{
                action_type: SwitchVT,
                flags: ActionFlags::SameScreen,
                screen: Option::None}));



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
        _type: &str)
        -> XkbMessageCode {

   
            log::error!(
                "{:?}: Value of {:?} field must be of type {};
                Action {:?} definition ignored.",
                code, field,
                _type,
                action);

            return code;

    }
    fn report_illegal(
        &self, 
        action: &ActionType,
        field: &ActionField,
        )
        -> XkbMessageCode {

  
            let err = XkbMessageCode::NoId;
            log::error!(
                "{:?}: Field {:?} is not defined for an action of type {:?};
                Action definition ignored.",
                err, field, action,
                );

            return err;

    }
    fn report_action_not_array(
        &self, 
        action: &ActionType,
        field: &ActionField,
        )
        -> XkbMessageCode {

   
            let code = XkbMessageCode::NoId;
            log::error!(
                "{:?}: The {:?} field in the {:?} action is not an array;
                Action definition ignored.",
                code, field,
                action,
                );

            return code;

    }

}

fn check_boolean_flag(
    ctx: &Context,
    action: &ActionType,
    field: &ActionField,
    flag: ActionFlags,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
    flags_inout: &mut ActionFlags)
    -> Result<(),XkbMessageCode> {

        if array_ndx.is_some() {
            return Err(ctx.report_action_not_array(
                action, field));
        }

        let set = value.resolve_boolean(ctx);

        // could not resolve boolean
        if set.is_none() {
            return Err(ctx.report_mismatch(
                XkbError::WrongFieldType.into(),
                action, field, "boolean"));
        }

        if set.unwrap() {
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
) -> Result<ModMask, XkbMessageCode> {


    if array_ndx.is_some() {

        return Err(ctx.report_action_not_array(action, &ActionField::Modifiers));

    }


    if value.op_type() == ExprOpType::Ident {

        if let ExprDef::Ident(ref ident) = value {
            if let Some(val_str) = ctx.xkb_atom_text(ident.ident) {

                let val_str = val_str.to_lowercase();

                if val_str.as_str() == "usemodmapmods"
                    || val_str.as_str() == "modmapmods" {

                        *flags_inout |= ActionFlags::ModsLookupModMap;
                        return Ok(0);

                }

            }
        }
    }

    let mods = value.resolve_mod_mask(ctx, ModType::BOTH, mods);

    if mods.is_none() {

        return Err(ctx.report_mismatch(XkbError::WrongFieldType.into(),
        action, &ActionField::Modifiers, "modifier mask"));

    }



    *flags_inout &= !ActionFlags::ModsLookupModMap;

    Ok(mods.unwrap())

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
    flags_inout: &mut ActionFlags)
    -> Result<(), XkbMessageCode> {

        if array_ndx.is_some() {
            return Err(ctx.report_action_not_array(
                    action, &ActionField::Affect));
        }

        if let Some(flags) = value.resolve_enum(ctx, lock_which) {

            *flags_inout &= !(
                ActionFlags::LockNoLock | ActionFlags::LockNoUnlock);

            *flags_inout |= flags;

            return Ok(());

        } else {

            return Err(ctx.report_mismatch(
                XkbError::WrongFieldType.into(), action, &ActionField::Affect,
                "lock,unlock,both,neither"));
        } 

}

fn handle_set_latch_lock_mods(
    ctx: &Context,
    mods: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
) -> Result<(), KeymapErr> {

    if let Action::Mods(action) = action { 

    if *field == ActionField::Modifiers {
        return match check_modifier_field(
            ctx, mods, action_type, array_ndx, value, &mut action.flags) {
            Err(e) => Err(e.into()),
            Ok(mods) => {
                action.mods.mods = mods;
                return Ok(());
            }


        };


    } else if [ActionType::ModSet,ActionType::ModLatch].contains(&action_type) && *field == ActionField::ClearLocks {

        return check_boolean_flag(
            ctx, action_type, field,
            ActionFlags::LockClear,
            array_ndx, value, &mut action.flags).map_err(|e| e.into());
    }
    
    else if *action_type == ActionType::ModLatch && *field == ActionField::LatchToLock {
        return check_boolean_flag(
            ctx, action_type, field,
            ActionFlags::LatchToLock,
            array_ndx, value, &mut action.flags).map_err(|e| e.into());

    }

    else if *action_type == ActionType::ModLock && *field == ActionField::Affect {

        return check_affect_field(
            ctx, action_type, 
            array_ndx, value, &mut action.flags).map_err(|e| e.into());
    }

    }
    return Err(ctx.report_illegal(action_type, field).into());


}

fn check_group_field(
    ctx: &Context,
    action: &ActionType,
    array_ndx: Option<ExprDef>,
    value: ExprDef,
    flags_inout: &mut ActionFlags)
    -> Result<i32, XkbMessageCode> {

        let spec;
        let mut flags = flags_inout.clone();

        if array_ndx.is_some() {
            return Err(ctx.report_action_not_array(
                    action, &ActionField::Group));
        }

        let op = value.op_type();

        use ExprOpType::*;
        match value {
            ExprDef::Unary(unary) if [Negate, UnaryPlus].contains(&unary.op) => {

            flags &= !ActionFlags::AbsoluteSwitch;
            spec = *unary.child;
                
            },
            _ => {
            flags |= ActionFlags::AbsoluteSwitch;
            spec = value;
        }}

        let idx = spec.resolve_group(ctx);

        if let Some(idx) = idx {

            let group_rtrn: i32 = match op {
                ExprOpType::Negate => -1 * i32::try_from(idx).unwrap(),
                ExprOpType::UnaryPlus => idx.try_into().unwrap(),
                _ => i32::try_from(idx).unwrap() - 1
            };
            
            *flags_inout = flags;
            return Ok(group_rtrn);


        } else {

            return Err(ctx.report_mismatch(
                XkbError::UnsupportedGroupIndex.into(),
                action, &ActionField::Group, 
                "integer (range 1..8)"));

        }
            

}


fn handle_set_latch_lock_group(
    ctx: &Context,
    mods: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef)
    -> Result<(), KeymapErr> {

        if let Action::Group(act) = action {

            if *field == ActionField::Group {

                return match check_group_field(
                    ctx, action_type, array_ndx,
                    value, &mut act.flags) {
                    Err(e) => Err(e.into()),
                    Ok(group) => {
                        act.group = Some(group);
                        Ok(())
                    }};
            }

            else if [ActionType::GroupSet, ActionType::GroupLatch]
                .contains(action_type) && *field == ActionField::ClearLocks {

                    return check_boolean_flag(
                        ctx, action_type, field, 
                        ActionFlags::LockClear,
                        array_ndx, value, &mut act.flags).map_err(|e| e.into());
            }

            
            else if ActionType::GroupLatch == *action_type
                && *field == ActionField::LatchToLock {

                    return check_boolean_flag(
                        ctx, action_type, field, 
                        ActionFlags::LatchToLock,
                        array_ndx, value, &mut act.flags).map_err(|e| e.into());
            }

        }

        return Err(ctx.report_illegal(
            action_type, field).into());

}

fn handle_move_ptr(
    ctx: &Context,
    mods: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef)
    -> Result<(), KeymapErr> {

        if let Action::Ptr(act) = action {
            if [ActionField::X, ActionField::Y].contains(field) {


                let op = value.op_type();
                let absolute = op != ExprOpType::Negate
                    && op != ExprOpType::UnaryPlus;

                if array_ndx.is_some() {
                    return Err(ctx.report_action_not_array(
                            action_type, field).into());
                }
                let val = value.resolve_integer(ctx);
                if val.is_none() {
                    return Err(ctx.report_mismatch(
                            XkbError::WrongFieldType.into(),
                            action_type, field, "integer").into());
                }
                if let Some(Ok(val)) = val.map(|x| i16::try_from(x) ) {

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

            }
            else if *field == ActionField::Accel {

                return check_boolean_flag(ctx, action_type,
                    field, ActionFlags::Accel, array_ndx,
                    value, &mut act.flags).map_err(|e| e.into());
            }
            
        }
        return Err(ctx.report_illegal(action_type, field).into());

}

fn handle_ptr_btn(
    ctx: &Context,
    mods: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef)
    -> Result<(), KeymapErr> {

        if let Action::Btn(act) = action {

            if *field == ActionField::Button {

                if array_ndx.is_some() {
                    return Err(ctx.report_action_not_array(
                            action_type, field).into());
                }

                let btn = value.resolve_button(ctx);
                if btn.is_none() {
                    return Err(ctx.report_mismatch(
                        XkbError::WrongFieldType.into(),
                        action_type, field, "integer (range 1..5)").into());
                }
                let btn = btn.unwrap();

                if btn < 0 || btn > 5 {

                    let err = XkbMessageCode::NoId;

                    log::error!(
                        "{:?}: Button must specify default or be in the range 1..5;
                        Illegal button value {:?} ignored",
                        err, btn);

                    return Err(err.into());

                }

                act.button = Some(btn.try_into().unwrap());
                return Ok(());
            }

            else if *field == ActionField::Affect && *action_type == ActionType::PtrLock {

                return check_affect_field(ctx, action_type, array_ndx,
                    value, &mut act.flags).map_err(|e| e.into());

            }

            else if *field == ActionField::Count {

                if array_ndx.is_some() {
                    return Err(ctx.report_action_not_array(
                            action_type, field).into());
                }
                let val = value.resolve_integer(ctx);
                if val.is_none() {
                    return Err(ctx.report_mismatch(
                        XkbError::WrongFieldType.into(),
                        action_type, field, "integer").into());
                }
                let val = val.unwrap();
                
                if val < 0 || val > 255 {

                    let err = XkbMessageCode::NoId;

                    log::error!(
                        "{:?}: The count field must have a value in the range 0.255;
                        Illegal count {:?} ignored",
                        err, val);

                    return Err(err.into());

                }

                act.count = val.try_into().unwrap();
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
        _ => None
    }

}

fn handle_set_ptr_dflt(
    ctx: &Context,
    mods: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef)
    -> Result<(), KeymapErr> {

        if let Action::Dflt(act) = action {

            if *field == ActionField::Affect {

                if array_ndx.is_some() {
                    return Err(ctx.report_action_not_array(
                            action_type, field).into());
                }

                let ptr_dflts = value.resolve_enum(
                    ctx, ptr_dflts);

                if ptr_dflts.is_none(){
                    return Err(ctx.report_mismatch(
                            XkbError::WrongFieldType.into(),
                            action_type, field, 
                            "pointer component").into());
                }

                return Ok(());


            }
            else if *field == ActionField::Button
                || *field == ActionField::Value {

                if array_ndx.is_some() {
                    return Err(ctx.report_action_not_array(
                            action_type, field).into());
                }

                let op = value.op_type();
                use ExprOpType::*;
                let button = match value {

                    ExprDef::Unary(unary)
                    if [Negate, UnaryPlus]
                        .contains(&op) => {

                        act.flags &= !ActionFlags::AbsoluteSwitch;
                        *unary.child
                    }, value => {
                        act.flags |= ActionFlags::AbsoluteSwitch;
                        value
                    }
                };

                
                let btn = button.resolve_button(ctx);
                if btn.is_none() {
                    return Err(ctx.report_mismatch(
                        XkbError::WrongFieldType.into(),
                        action_type, field, "integer (range 1..5)").into());
                }
                let btn = btn.unwrap();

                if btn < 0 || btn > 5 {

                    let err = XkbMessageCode::NoId;

                    log::error!(
                        "{:?}: New default button value must be in the range 1..5;
                        Illegal default button value {:?} ignored",
                        err, btn);

                    return Err(err.into());

                }

                if btn == 0 {

                    let err = XkbMessageCode::NoId;

                    log::error!(
                        "{:?}: Cannot set default pointer button to \"default\";
                        Illegal default button setting ignored",
                        err);

                    return Err(err.into());

                }

                act.value = match op {
                    Negate => Some(-1*i8::try_from(btn).unwrap()),
                    _ => Some(btn.try_into().unwrap())
                };
                return Ok(());

            }
        }

        Err(ctx.report_illegal(action_type, field).into())

}

fn handle_switch_screen(
    ctx: &Context,
    mods: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef)
    -> Result<(), KeymapErr> {

        if let Action::Screen(act) = action {

            if *field == ActionField::Screen {

                if array_ndx.is_some() {
                    return Err(ctx.report_action_not_array(
                            action_type, field).into());
                }

                let op = value.op_type();
                use ExprOpType::*;
                let scrn = match value {
                    ExprDef::Unary(unary)
                if [Negate, UnaryPlus].contains(&op) => {
                    act.flags &= !ActionFlags::AbsoluteSwitch;
                         *unary.child
                    },
                    value => {
                        act.flags |= ActionFlags::AbsoluteSwitch;
                        value
                    }
                };

                let val = scrn.resolve_integer(ctx);

                if val.is_none() {
                    return Err(ctx.report_mismatch(
                            XkbError::WrongFieldType.into(),
                            action_type, field, "integer (0..255)").into());
                }

                let val = val.unwrap();
                if val < 0 || val > 255 {
                    let err = XkbMessageCode::NoId;
                    log::error!(
                        "{:?}: Screen index must be in the range 1..255;
                        Illegal screen value {:?} ignored.",
                        err, val);

                    return Err(err.into());
                }

                act.screen = match op {
                    Negate => Some(-1 * i8::try_from(val).unwrap()),
                    _ => Some(val.try_into().unwrap()) };

                return Ok(());
                

            }

            else if *field == ActionField::Same {

                return check_boolean_flag(ctx, action_type,
                    field, ActionFlags::SameScreen, array_ndx, value,
                    &mut act.flags).map_err(|e| e.into());

            }


        }


        return Err(ctx.report_illegal(action_type, field).into());


}


fn handle_set_lock_controls(
    ctx: &Context,
    mods: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef)
    -> Result<(), KeymapErr> {

        if let Action::Ctrls(act) = action {

            if *field == ActionField::Controls {

                if array_ndx.is_some() {
                    return Err(ctx.report_action_not_array(
                            action_type, field).into());
                }

                let mask = value.resolve_mask(ctx,
                    |ident, _, ctx| {

                        let s = ctx.xkb_atom_text(ident)?;
                        lookup_key(&CTRL_MASK_NAMES, s).copied() 
                }
                    );

                act.ctrls = match mask {
                    None => return Err(ctx.report_mismatch(
                            XkbError::WrongFieldType.into(),
                            action_type, field, "controls mask").into()),
                    Some(mask) => mask
                };

                return Ok(());



            }
            else if *field == ActionField::Affect {
                return check_affect_field(ctx, action_type, array_ndx,
                    value, &mut act.flags).map_err(|e| e.into());
            }



        }



        return Err(ctx.report_illegal(action_type, field).into());
}


fn handle_private(
    ctx: &Context,
    mods: &ModSet,
    action: &mut Action,
    action_type: &ActionType,
    field: &ActionField,
    array_ndx: Option<ExprDef>,
    value: ExprDef)
    -> Result<(), KeymapErr> {

        if let Action::Private(act) = action {

            if *field == ActionField::Type {
                    
                    if array_ndx.is_some() {
                        return Err(ctx.report_action_not_array(
                                action_type, field).into());
                    }

                    let _type = match value.resolve_integer(ctx) {
                        Some(v) => v,
                        None => return Err(ctx.report_mismatch(
                            XkbError::WrongFieldType.into(),
                            &ActionType::Private, field, "integer").into())
                    };

                    if _type < 0 || _type > 255 {
                        let err = XkbMessageCode::NoId;
                        log::error!("{:?}: Private action type must be in the range 
                        0..255; Illegal type {:?} ignored", err, _type);

                        return Err(err.into());
                    }

                    let _type: u8 = _type.try_into().unwrap();
                    let _type: ActionType = _type.try_into().unwrap();
                    if _type < ActionType::Private {
                        log::info!("{:?}: Private actions of type % are not supported; Ignored", XkbMessageCode::NoId);
                        act.action_type = ActionType::None;
                    } else {

                        act.action_type = _type;
                    }

                    return Ok(());
            }
            else if *field == ActionField::Data {

                if array_ndx.is_none() {

                    let val = match value.resolve_string(ctx) {
                        Some(val) => val,
                        None => return Err(ctx.report_mismatch(
                            XkbError::WrongFieldType.into(),
                            &action.action_type(), field, "string").into())
                    };

                    let s = ctx.xkb_atom_text(val).unwrap();

                    let data_size = std::mem::size_of::<ActionData>();
                    if s.len() < 1 
                        || s.len() > data_size {

                            let err = XkbMessageCode::NoId;
                            log::warn!("{:?}: A private action has {} data bytes; 
                            Illegal data ignored",err, data_size);

                            return Err(err.into());

                    }

                    // TODO: check null termination?

                    return Ok(());


                } else {

                    let ndx = match array_ndx {
                        Some(ndx) => ndx.resolve_integer(ctx),
                        None => None };

                    let ndx = match ndx {
                        Some(ndx) => ndx,
                        None => {
                            let err = XkbMessageCode::NoId;
                            log::error!("{:?}: Array subscript must be integer;
                            Illegal subscript ignored", err);
                            return Err(err.into()); }};


                    let data_size = ACTION_DATA_LEN;
                    if ndx < 0 || ndx >= data_size.try_into().unwrap() {

                        // TODO: check this comparison
                        let err = XkbMessageCode::NoId;
                        log::error!("{:?}: The data for a private action has {} entries;
                        attempted to use data[{}] ignored", err, data_size, ndx );
                        return Err(err.into());

                    }
                    let ndx: usize = ndx.try_into().unwrap();

                    let datum = match value.resolve_integer(ctx) {
                        Some(datum) => datum,
                        None => return Err(ctx.report_mismatch(XkbError::WrongFieldType.into(),
                            &act.action_type, field, "integer").into()) };

                    let datum: u8 = match datum.try_into() {
                        Ok(datum) => datum,
                        Err(_) => {
                            let err = XkbMessageCode::NoId;
                            log::error!("{:?}: All data for a private action must 0..255;
                            Illegal datum {} ignored", err, datum);
                            return Err(err.into());
                        }};

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
        value: ExprDef)
        -> Result<(), KeymapErr>
    {

        use ActionType::*;
        match action_type {
            None => Ok(()),
            e if [ModSet, ModLatch,ModLock].contains(&e) 
                => handle_set_latch_lock_mods(
                ctx, mods, self, action_type, field, array_ndx,
                value),
            e if [GroupSet, GroupLatch,GroupLock].contains(&e) 
                => handle_set_latch_lock_group(
                ctx, mods, self, action_type, field, array_ndx,
                value),
            PtrMove
                => handle_move_ptr(
                ctx, mods, self, action_type, field, array_ndx,
                value),
            e if [PtrButton,PtrLock].contains(&e) 
                => handle_ptr_btn(
                ctx, mods, self, action_type, field, array_ndx,
                value),
            PtrDefault
                => handle_set_ptr_dflt(
                ctx, mods, self, action_type, field, array_ndx,
                value),
            Terminate => Ok(()),
            SwitchVT
                => handle_switch_screen(
                ctx, mods, self, action_type, field, array_ndx,
                value),
            e if [CtrlSet,CtrlLock].contains(&e) 
                => handle_set_lock_controls(
                ctx, mods, self, action_type, field, array_ndx,
                value),
            Private
                => handle_private(
                ctx, mods, self, action_type, field, array_ndx,
                value),
            _ => panic!("Should have already handled all types")
        }



    }



}
impl ActionsInfo {

    pub(super) fn handle_action_def(
        &mut self,
        ctx: &Context,
        mods: &ModSet,
        def: ExprDef,
        ) -> Result<Option<Action>, KeymapErr> {

        if def.op_type() == ExprOpType::ActionDecl {

            if let ExprDef::Action(def) = def {

                let name = match ctx.xkb_atom_text(def.name) {
                    Some(s) => s,
                    None => todo!()
                };

                let handler_type = match lookup_key(&ACTION_TYPE_NAMES, name) {
                    Some(h) if *h != ActionType::None => h,
                    Some(none_action) => return Ok(None), //TODO: is this correct?
                    None => {

                        let err = XkbMessageCode::NoId;
                        log::error!("{:?}: Unknown action {}",
                            err, name);
                        return Err(ActionErr::UnknownAction.into());
                    },
                  
                };

                // Get the default values for this action type
       

                let mut action: Action
                    = match self.actions.get(handler_type) {
                        Some(action) => action.clone(), //already exists
                        None => {
                            let new_action = Action::empty_from(*handler_type);
                            self.actions.insert(*handler_type, new_action);
                            self.actions.get(handler_type).unwrap().clone()
                        }};

                // Now change the action properties as specified
                // for this particular instance, e.g. "modifers"
                // and "clearLocks" in:
                //  SetMods(modifiers=Alt,clearLocks);

                for arg in def.args {

                    let field; let value;
                    let op = arg.op_type();

                    use ExprOpType::*;
                    match *arg {
                        ExprDef::Binary(binary) if binary.op == Assign => {

                            field = *binary.left;
                            value = *binary.right;
                        },
                        ExprDef::Unary(unary)
                            if [Not, Invert].contains(&op) => {
                            field =*unary.child;
                            value = CONST_FALSE;
                        
                        },
                        arg => {
                            field = arg;
                            value = CONST_TRUE;
                        }
                    }

                    let lhs = match field.resolve_lhs(ctx) {
                        Some(lhs) => lhs,
                        None => {
                            return Err(XkbMessageCode::NoId.into());
                        }
                    };


                    if let Some(elem) = lhs.elem {
                        let err = XkbMessageCode::NoId;
                        log::error!("{:?}: Cannot change defaults
                        in an action definition; Ignoring attempts
                        to change {}.{}",err, elem, lhs.field);
                        return Err(err.into());
                    }


                    let field_ndx = match FIELD_STRINGS.get(
                        &lhs.field.to_lowercase()) {
                        Some(f) => f,
                        None => {
                            let err = XkbMessageCode::NoId;
                            log::error!("{:?}: Unknown field name {}",
                                err, lhs.field);
                            return Err(err.into());

                        }
                    };

                    action.handle(
                        ctx, mods, handler_type, 
                        field_ndx, lhs.index, value)?;

                }

                
                return Ok(Some(action));

            }



        }

        
        
        let err = XkbMessageCode::NoId;
        log::error!(
            "{:?}: Expected an action definition, found {:?}",
            err, def.op_type());

        return Err(err.into());


    }

    pub(super) fn set_action_field(
        &mut self,
        ctx: &Context, mods: &ModSet,
        elem: Option<String>, field: &str,
        array_ndx: Option<ExprDef>,
        value: ExprDef)
        -> Result<(), KeymapErr> {

            // TODO: replace errors? 

            let elem = match elem {
                Some(e) => e,
                None => return Err(XkbMessageCode::NoId.into()) };

            let action_type = match lookup_key(&ACTION_TYPE_NAMES, &elem) {
                None => return Err(XkbMessageCode::NoId.into()),
                Some(action) => action };

            let action_field = match FIELD_STRINGS.get(&field.to_lowercase()) {
                None => {
                    log::error!("{:?}: {:?} is not a legal field name", XkbMessageCode::NoId, field);
                    return Err(ActionErr::IllegalFieldName.into());},
                Some(f) => f };



            let action = match self.actions.get_mut(action_type) {
                Some(a) => a,
                None => {
                    // create a new one
                    self.actions.insert(
                        *action_type, 
                        Action::empty_from(*action_type));
                    self.actions.get_mut(action_type)
                        .unwrap()
                }};

            return action.handle(ctx, mods, action_type, 
                action_field, array_ndx, value);


    }
}

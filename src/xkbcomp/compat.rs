use super::ast::*;
use super::action::*;
use super::include::*;

use crate::context::Context;
use crate::errors::*;
use crate::keymap::*;

use crate::text::*;
use crate::rust_xkbcommon::*;



bitflags::bitflags! {

    #[derive(Clone, Eq, PartialEq, Debug)]
    struct SiField: u8 {

        const VIRTUAL_MOD = 1 << 0;
        const ACTION = 1 << 1;
        const AUTO_REPEAT= 1 << 2;
        const LEVEL_ONE_ONLY = 1 << 3;


    }

}

#[derive(PartialEq, Eq, Clone)]
struct SymInterpInfo {
    defined: SiField,
    merge: MergeMode,
    interp: SymInterpret,
}

bitflags::bitflags! {

    #[derive(Clone)]
    struct LedField: u8 {

        const MODS = 1 << 0;
        const GROUPS = 1 << 1;
        const CTRLS = 1 << 2;


    }

}


#[derive(Clone)]
struct LedInfo {
    defined: LedField,
    merge: MergeMode,
    led: Led
}

impl Default for LedInfo {

    fn default() -> Self {

        Self {

            defined: LedField::empty(),
            merge: MergeMode::Default,
            led: Default::default()
        }

    }

}

struct CompatInfo {
    name: Option<String>,
    error_count: i64,
    include_depth: u32,
    default_interp: SymInterpInfo,
    interps: Vec<SymInterpInfo>,
    default_led: LedInfo,
    leds: Vec<LedInfo>, //max: XKB_MAX_LEDS
    actions: ActionsInfo,
    mods: ModSet,

}


impl SymInterpInfo {

    fn si_text(&self, 
        ctx: &Context,
        mods: &ModSet,
        default: bool) -> String {

        if default {
            return "default".to_owned();
        }

        // todo: 128-char limit
        let string = format!(
            "{}+{}({})",
            todo!(),
            todo!(),
            ctx.mod_mask_text(
                mods, 
                todo!())

        );

        return string;


    }

    fn report_si_not_array(
        &self, ctx: &Context,
        field: &str, mods: &ModSet, default: bool) -> XkbMessageCode {

        // TODO: return error?

        return ctx.report_not_array("symbol interpretation", field, 
            &self.si_text(ctx, mods, default));
    }

}
impl CompatInfo {

    fn report_si_bad_type(
        &self, ctx: &Context,
        field: &str, wanted: &str)
    -> Result<(), XkbMessageCode> {

        // TODO: return error?

        return ctx.report_bad_type( XkbError::WrongFieldType.into(), 
            "symbol interpretation", &field, 
            &self.default_interp.si_text(ctx, &self.mods, true), wanted);
    }

    fn report_led_bad_type(
        &self, ctx: &Context, ledi: &LedInfo, field: &str, wanted: &str)
    -> Result<(), XkbMessageCode> 
    {
        let led_name_text = match ledi.led.name {
            None => None,
            Some(name) => ctx.xkb_atom_text(name) };

        return ctx.report_bad_type( XkbError::WrongFieldType.into(), 
            "indicator map", &field, 
            &led_name_text.expect("LED name not inserted"), wanted);

    }
    fn report_led_not_array(
        &self, ctx: &Context, ledi: &LedInfo, field: &str)

    -> Result<(), XkbMessageCode> 
    {
        let led_name_text = match ledi.led.name {
            None => None,
            Some(name) => ctx.xkb_atom_text(name) };

        ctx.report_not_array( 
            "indicator map", &field, 
            &led_name_text.unwrap_or_else(|| ""));

        return todo!();

    }

    fn new(
        include_depth: u32,
        actions: ActionsInfo,
        mods: ModSet) -> Self {

        Self {
            include_depth,
            actions,
            mods,
            default_interp: SymInterpInfo {
                merge: MergeMode::Override,
                interp: Default::default(),
                defined: SiField::empty() },
            default_led: Default::default(),
            error_count: 0,
            leds: vec![],
            name: None,
            interps: vec![],



        }

    }

    fn find_matching_interp(
        &mut self, new: &SymInterpInfo)
        -> Option<&mut SymInterpInfo> {


            for interp in self.interps.iter_mut() {
            
                    if interp.interp.sym == new.interp.sym
                        && interp.interp.mods == new.interp.mods
                        && interp.interp.match_op == new.interp.match_op
                    {

                            return Some(interp);

                    }
                        
                }



            None

    }
    

}

fn use_new_interp_field(
    field: SiField,
    old: &SymInterpInfo,
    new: &SymInterpInfo,
    collision: &mut SiField
    ) -> bool {

    if !old.defined.intersects(field.clone()) { return true; }

    if new.defined.intersects(field.clone()) { 
        
        //collision
        *collision |= field;

        if new.merge != MergeMode::Augment {
            return true;
        }
    }

    return false;

}

impl CompatInfo {

    fn add_interp(
        &mut self,
        ctx: &Context,
        new: SymInterpInfo,
        same_file: bool)
        -> Result<(), XkbMessageCode>

    {

        if let Some(mut old) = self.find_matching_interp(&new) {

            let mut collision = SiField::empty();

            let verbosity: i32 = ctx.get_log_verbosity();
            let report = (same_file && (verbosity > 0)) || verbosity > 9;


            if new.merge == MergeMode::Replace {

                todo!(); //reporting
                *old = new;

                return Ok(());

            }

            if use_new_interp_field(SiField::VIRTUAL_MOD,
                old, &new, &mut collision) {

                old.interp.virtual_mod = new.interp.virtual_mod;
                old.defined |= SiField::VIRTUAL_MOD;

            }
            if use_new_interp_field(SiField::ACTION,
                old, &new, &mut collision) {

                old.interp.action = new.interp.action.clone();
                old.defined |= SiField::ACTION;

            }
            if use_new_interp_field(SiField::AUTO_REPEAT,
                old, &new, &mut collision) {

                old.interp.repeat = new.interp.repeat;
                old.defined |= SiField::AUTO_REPEAT;

            }
            if use_new_interp_field(SiField::LEVEL_ONE_ONLY,
                old, &new, &mut collision) {

                old.interp.level_one_only = new.interp.level_one_only;
                old.defined |= SiField::LEVEL_ONE_ONLY;

            }

            if report && !collision.is_empty() {

                log::warn!(
                    "{:?}: Multiple interpretations of {:?};
                    Using {} definition for duplicate fields.",
                    XkbMessageCode::NoId, new.si_text(ctx, &self.mods,false),
                    match new.merge {
                        MergeMode::Augment => "first",
                        _ => "last" });


            }

            return Ok(());


        } else {

            // default case
            self.interps.push(new);
        }

        Ok(())

    }

    fn resolve_state_and_predicate(
        &self, ctx: &Context, mut expr: Option<ExprDef>) 
        -> Result<(MatchOperation, Option<ModMask>), 
        XkbMessageCode> {

            // TODO: rewrite this function,
            // This function is only used in `handle_interp_def`,
            // and the data it modifies is not used if the function fails.
            // Maybe return an empty ModMask instead of a None

            if expr.is_none() {

                return Ok((MatchOperation::AnyOrNone,
                    Some(MOD_REAL_MASK_ALL)));

            }
            let mut expr = expr.unwrap();


            let mut pred = MatchOperation::Exactly;
            if let ExprDef::Action(mut action) = expr {

                let pred_txt = ctx.xkb_atom_text(action.name)
                    .unwrap_or_else(|| "");
                let key  = lookup_key(&SYM_INTERPRET_MATCH_MASK_NAMES,pred_txt);

                if key.is_none() 
                    || action.args.len() == 0
                    || action.args.len() > 1 { //TODO: is this correct?

                    let err = XkbMessageCode::NoId;
                    log::error!("{:?}: Illegal modifier predicate {:?}; Ignored", err, pred_txt);
                    
                    return Err(err);
                } else if let Some(txt) = key { 
                    pred = txt.clone(); }

                // Take the first arg from the args,
                // as this is what appears to actually happen in 
                // the lookup.
                // TODO: will need to determine if this is intended behavior.
           
                expr = *action.args.remove(0);




            
            }

            else if let ExprDef::Ident(ref ident) = expr {

                if let Some(pred_txt) = ctx.xkb_atom_text(ident.ident) {
                    if pred_txt.to_lowercase().as_str() == "any" {
                        return Ok((MatchOperation::Any,
                            Some(MOD_REAL_MASK_ALL)));
                    }
                }

                  

            }

            

        let mod_mask = expr.resolve_mod_mask(ctx, ModType::REAL, &self.mods);

        return Ok((pred, mod_mask));

        }

}
fn use_new_led_field(
    field: LedField,
    old: &LedInfo,
    new: &LedInfo,
    collision: &mut LedField
    ) -> bool {

    if !old.defined.intersects(field.clone()) { return true; }

    if new.defined.intersects(field.clone()) { 
        
        //collision
        *collision |= field;

        if new.merge != MergeMode::Augment { 
            return true;
        }
    }

    return false;

}

impl CompatInfo {

    fn add_led_map(&mut self, ctx: &Context, new: LedInfo, same_file: bool)
    -> Result<(), XkbMessageCode> {


        let mut collision;
        let verbosity: i32 = ctx.get_log_verbosity();
        let report = (same_file && verbosity > 0) || verbosity > 9;

        for old in self.leds.iter_mut() {

            if  old.led.name != new.led.name {
                continue; }

            if old.led.mods.mods == new.led.mods.mods
                && old.led.groups == new.led.groups
                    && old.led.ctrls == new.led.ctrls
                        && old.led.which_mods == new.led.which_mods
                        && old.led.which_groups == new.led.which_groups
            {
                old.defined |= new.defined;
                return Ok(());
            }

            if new.merge == MergeMode::Replace {
                if report {
                    todo!() }
                *old = new;
                return Ok(());

            }

            collision = LedField::empty();

            if use_new_led_field(LedField::MODS, old, &new,&mut collision) {
                old.led.which_mods = new.led.which_mods;
                old.led.mods = new.led.mods;
                old.defined |= LedField::MODS;
            }
            if use_new_led_field(LedField::GROUPS, old, &new,&mut collision) {
                old.led.which_groups = new.led.which_groups;
                old.led.groups = new.led.groups;
                old.defined |= LedField::GROUPS;
            }
            if use_new_led_field(LedField::CTRLS, old, &new,&mut collision) {
                old.led.ctrls = new.led.ctrls;
                old.defined |= LedField::CTRLS;
            }

            if report && !collision.is_empty() {

                log::warn!(
                    "{:?}: Map for indicator {:?} redefined;
                    Using {} definition for duplicate fields.",
                    XkbMessageCode::NoId, 
                    match old.led.name {
                        None => None,
                        Some(name) => ctx.xkb_atom_text(name)},
                    match new.merge {
                        MergeMode::Augment => "first",
                        _ => "last" });


            }

            return Ok(());

        }

        if self.leds.len() >= XKB_MAX_LEDS {
            todo!();
            return Err(XkbMessageCode::NoId);
        }

        self.leds.push(new);


        Ok(())
    }


    fn merge_included_compat_maps(
        &mut self, ctx: &Context, from: Self, merge: MergeMode)
    {


        if from.error_count > 0 {
            self.error_count += from.error_count;
            return;
        }

        // added here in case this was updated by `from`
        self.actions = from.actions;

        self.mods = from.mods;
        
        if self.name.is_none() {
            self.name = from.name;
        }

        if self.interps.is_empty() {
            self.interps = from.interps;
        }
        else {
            for mut interp in from.interps {
                interp.merge = match merge {
                    MergeMode::Default => interp.merge,
                    _ => merge };

                if self.add_interp(ctx, interp,false).is_err() {
                    self.error_count += 1;
                }
            }
        }

        if self.leds.len() == 0 {
            self.leds = from.leds;
        } else {
            for mut ledi in from.leds {
                ledi.merge = match merge {
                    MergeMode::Default => ledi.merge,
                    _ => merge };
                if self.add_led_map(ctx, ledi, false).is_err() {
                    self.error_count += 1;
                }

            }
        }
    }

    fn handle_include_compat_map(
        &mut self, ctx: &mut Context, include: IncludeStmt)
    -> Result<(),KeymapErr> {

        // TODO: don't clone so much

        let merge = include.merge;

        let include: Vec<IncludeStmtPart> = include.maps;

        if ctx.exceeds_include_max_depth(self.include_depth) {
            self.error_count += 10;
            return Err(todo!());
        }

        // TODO: use a mutable pointer to self.actions instead of cloning
        let mut included = Self::new(0, self.actions.clone(), self.mods.clone());
        for stmt in include {

            let file = process_include_file(ctx, &stmt, XkbFileType::Compat);

            let file = match file {
                Ok(file) => file,
                Err(e) => {
                    self.error_count += 10;
                    return Err(e);
                }
            };

            let mut next_incl = Self::new(
                self.include_depth + 1, 
                included.actions.clone(), included.mods.clone());
            next_incl.default_interp = self.default_interp.clone();
            next_incl.default_interp.merge = stmt.merge;
            next_incl.default_led.merge = stmt.merge;

            next_incl.handle_compat_map_file(ctx, file, MergeMode::Override);
            
            included.merge_included_compat_maps(ctx, next_incl,stmt.merge);

        }


        
        self.merge_included_compat_maps(
            ctx, included, merge);

        match self.error_count {
            0 => Ok(()),
            _ => Err(XkbMessageCode::NoId.into())
        }


    }

    fn set_interp_field(
        &mut self, ctx: &Context,
        non_default: Option<&mut SymInterpInfo>,
        field: &str,
        array_ndx: Option<ExprDef>, value: ExprDef)
        -> Result<(), KeymapErr> {

            // Determine whether to set interp field
            // on own default, or to use a passed_in value
            let (default, si) = match non_default {
                Some(si) => (false,si), 
                None => (true, &mut self.default_interp) };


            let field_lowercase = field.to_lowercase();

            if &field_lowercase == "action" {
                if array_ndx.is_some() {
                    return Err(si.report_si_not_array(
                        ctx, field, &self.mods, default).into()); }

                si.interp.action = self.actions.handle_action_def(
                    ctx, &self.mods, value)?;


                si.defined |= SiField::ACTION;
            }
            else if ["virtualmodifier","virtualmod"]
                .contains(&field_lowercase.as_str()) {
                if array_ndx.is_some() {
                    return Err(si.report_si_not_array(
                        ctx, field, &self.mods, default).into()); }

                let ndx = value.resolve_mod(ctx, ModType::VIRT, &self.mods);

                let ndx = match ndx {
                    None => {
                        return Err(si.report_si_not_array(ctx, field, &self.mods, default).into())
                    },
                    Some(ndx) => ndx };

                si.interp.virtual_mod = Some(ndx.try_into().unwrap());
                si.defined |= SiField::VIRTUAL_MOD;


            }
            else if &field_lowercase == "repeat" {
                if array_ndx.is_some() {
                    return Err(si.report_si_not_array(
                        ctx, field, &self.mods, default).into()); }

                let set = match value.resolve_boolean(ctx) {
                    Some(set) => set,
                    None => return self.report_si_bad_type(
                        ctx, field, "boolean").map_err(|e| e.into()) };

                si.interp.repeat = set;
                si.defined |= SiField::AUTO_REPEAT;

            }
            else if &field_lowercase == "locking" { 

                log::debug!("{:?}: The \"locking\" field in symbol interpretation is unsupported;
                Ignored", XkbMessageCode::NoId);

            }

            else if ["usemodmap", "usemodmapmods"]
                .contains(&field_lowercase.as_str()) {
                if array_ndx.is_some() {
                    return Err(si.report_si_not_array(
                        ctx, field, &self.mods, default).into()); }

                let val = match value.resolve_enum(ctx, 
                    |s| lookup_key(&USE_MOD_MAP_VALUE_NAMES, s)) {
                    Some(val) => val,

                    None => return self.report_si_bad_type(ctx, field, "level specification")
                                    .map_err(|e| e.into()),
                };

                si.interp.level_one_only = *val;
                si.defined |= SiField::LEVEL_ONE_ONLY;

                
            }

            else {
                ctx.report_bad_field(
            "symbol interpretation", field, &si.si_text(ctx, &self.mods, default));
            return Err(XkbMessageCode::NoId.into());}


            Ok(())



    }


    fn set_led_map_field(
        &mut self, ctx: &Context, ledi: &mut LedInfo, field: &str,
        array_ndx: Option<ExprDef>, value: ExprDef)
        -> Result<(), KeymapErr> {


            let field_lowercase = field.to_lowercase();
        
            if ["modifiers", "mods"].contains(&field_lowercase.as_str()) {
                if array_ndx.is_some() {
                    return self.report_led_not_array(
                        ctx, &ledi, field).map_err(|e| e.into()); }

                ledi.led.mods.mods = 
                    match value.resolve_mod_mask(
                    ctx, ModType::BOTH, &self.mods,) 
                {
                    Some(mods) => mods,
                    None => return self.report_led_bad_type(
                        ctx, &ledi, field, "modifier mask")
                        .map_err(|e| e.into()),

                };

                ledi.defined |= LedField::MODS;

                return Ok(());

            }
            else if field_lowercase.as_str() == "groups" {
                if array_ndx.is_some() {
                    return self.report_led_not_array(
                        ctx, &ledi, field).map_err(|e| e.into()); }

                let mask = value.resolve_mask(ctx, |ident, _, ctx| {

                        let s = ctx.xkb_atom_text(ident)?;
                        lookup_key(&GROUP_MASK_NAMES, s).copied()


                    });
                ledi.led.groups = match mask {
                    Some(mask) => mask,
                    None => return self.report_led_bad_type(
                        ctx, ledi, field, "group mask").map_err(|e| e.into()),
                };

                ledi.defined |= LedField::GROUPS;

                return Ok(());

            }
            else if ["controls","ctrls"].contains(&field_lowercase.as_str()) {
                if array_ndx.is_some() {
                    return self.report_led_not_array(
                        ctx, &ledi, field).map_err(|e| e.into()); }
            
            let mask = value.resolve_mask(ctx, |ident, _,ctx| {

                        let s = ctx.xkb_atom_text(ident)?;
                        lookup_key(&CTRL_MASK_NAMES,s).copied()

                    });
                ledi.led.ctrls = match mask {
                    Some(mask) => mask,
                    None => return self.report_led_bad_type(
                        ctx, ledi, field, "controls mask").map_err(|e| e.into()),
                };

                ledi.defined |= LedField::CTRLS;

                return Ok(());


            }

            else if field_lowercase.as_str() == "allowexplicit" {

                log::debug!(
                    "{:?}: The \"allowExplicit\" field in indicator statements is unsupported; Ignored", XkbMessageCode::NoId);

                return Ok(());

            }
            else if ["whichmodstate","whichmodifierstate"].contains(&field_lowercase.as_str()) {
                if array_ndx.is_some() {
                    return self.report_led_not_array(
                        ctx, &ledi, field).map_err(|e| e.into()); }


            let mask = value.resolve_mask(ctx, |ident, _, ctx| {

                        let s = ctx.xkb_atom_text(ident)?;
                        lookup_key(&MOD_COMPONENT_MASK_NAMES, s)
                            .copied()    

                    });
                ledi.led.which_mods = match mask {
                    Some(mask) => mask,
                    None => return self.report_led_bad_type(
                        ctx, ledi, field, "mask of modifier state components").map_err(|e| e.into()),
                };


                return Ok(());
            }
            else if field_lowercase.as_str() == "whichgroupstate" {
                if array_ndx.is_some() {
                    return self.report_led_not_array(
                        ctx, &ledi, field).map_err(|e| e.into()); }
                
                let mask = value.resolve_mask(ctx, |ident, _, ctx| {

                        let s = ctx.xkb_atom_text(ident)?;
                        GROUP_COMPONENT_MASK_NAMES.get(&s).copied()    

                    });
                ledi.led.which_groups = match mask {
                    Some(mask) => mask,
                    None => return self.report_led_bad_type(
                        ctx, ledi, field, "mask of group state components").map_err(|e| e.into()),
                };

                return Ok(());


            }
            else if ["driveskbd","driveskeyboard", "leddriveskbd",
"leddriveskeyboard", "indicatordriveskbd", "indicatordriveskeyboard"].contains(&field_lowercase.as_str()) {

                log::debug!("The \"{}\" field in indicator statements is unsupported; Ignored", field);

                return Ok(());


            }

else if field_lowercase.as_str() == "index" {
                // Users should see this, as it might cause unexpected behavior
                log::error!("{:?}: The \"index\" field in indicator statements is unsupported; Ignored", XkbMessageCode::NoId);
                return Ok(());
            }

            else {

                log::error!("{:?}: Unknown field {} in map for {:?} indicator; definition ignored", XkbMessageCode::NoId, field,
                    ctx.xkb_atom_text(ledi.led.name.unwrap_or_else(|| 0)));

                return Err(XkbMessageCode::NoId.into());


            }



    }

    fn handle_global_var(&mut self, ctx: &Context, stmt: VarDef) 
        -> Result<(), KeymapErr>  {

        let lhs = match stmt.name {
            None => return Err(todo!()),

            Some(name) => name.resolve_lhs(ctx) };

        let lhs = match lhs {
            Some(lhs) => lhs,
            None => return Err(todo!()) };

        if let Some(ref elem) = lhs.elem {

            let elem = elem.to_lowercase();

            if elem.as_str() == "interpret" {

                return self.set_interp_field(
                    ctx, None,
                    &lhs.field,lhs.index, stmt.value);
            }
            else if elem.as_str() == "indicator" {

                return self.set_led_map_field(
                    ctx, todo!(), &lhs.field, lhs.index, stmt.value);

            }

        }

        self.actions.set_action_field(
            ctx, &self.mods, lhs.elem,
            &lhs.field, lhs.index, stmt.value)

    }


    fn handle_interp_body(
        &mut self, ctx: &Context, defs: Vec<VarDef>, si: &mut SymInterpInfo)
        -> Result<(), KeymapErr> {

            // TODO: in the original, there is a value `ok`
            // which could be changed from 'true' to 'false' to 'true'
            // in the course of the loop.
            // This is kept the same

            let mut ok = Ok(());
            for def in defs {

                if def.name.is_none() {

                    // equivalent to resolve_lhs().is_none()
                    ok = Err(todo!());

                }
               
                else if let Some(name) = def.name {
                    
                    if name.op_type() == ExprOpType::FieldRef {
                        let err = XkbMessageCode::NoId;
                        log::error!("{:?}: Cannot set a global default
                        value from within an interpret statement;
                        Move statements to the global file scope",err);

                        ok = Err(err.into());
                        continue;



                    }
                    let lhs =  match name.resolve_lhs(ctx) {
                        Some(lhs) => lhs,
                        None => {
                            ok = Err(todo!());
                            continue;
                        }
                    };
             
                    ok = self.set_interp_field(
                        ctx, Some(si), &lhs.field, lhs.index, def.value);


                }


            }

            ok

    }

    fn handle_interp_def(
        &mut self, ctx: &Context, def: InterpDef, merge: MergeMode)
        -> Result<(), KeymapErr> {

            let (pred, mods) 
                = match self.resolve_state_and_predicate(
                ctx, def._match) {
                Err(e) => {

                    let err = XkbMessageCode::NoId;

                    log::error!("{:?}: Couldn't determine matching modifiers; Symbol interpretation ignored.", err);
                    return Err(err.into());

                },
                Ok(q) => q };

            let mods = match mods {
                Some(mods) => mods, None => 0 };

            let mut si = self.default_interp.clone();
            si.merge = match def.merge {
                MergeMode::Default => merge,
                _ => def.merge };
            si.interp.sym = def.sym;
            si.interp.match_op = pred;
            si.interp.mods.mods = mods; // TODO: is this correct? 

            if let Err(e) = self.handle_interp_body(ctx, def.def, &mut si) {
                self.error_count += 1;
                return Err(e);
            }

            if let Err(e) = self.add_interp(ctx, si,true) {
                self.error_count += 1;
                return Err(e.into());
            }
            
            Ok(())

    }

    fn handle_led_map_def(
        &mut self, ctx: &Context, def: LedMapDef, mut merge: MergeMode)
        -> Result<(), KeymapErr> {

            // TODO: in the original, there is a value `ok`
            // which could be changed from 'true' to 'false' to 'true'
            // in the course of the loop.
            // In the Rust version, this has been kept the same


            if def.merge != MergeMode::Default {
                merge = def.merge;
            }

            let mut ledi = self.default_led.clone();
            ledi.merge = merge;
            ledi.led.name = Some(def.name);

            let mut ok: Result<(), KeymapErr> = Ok(());

            for var in def.body {

                let lhs = match var.name {
                    None => None,
                    Some(name) => name.resolve_lhs(ctx)};

                let lhs = match lhs {
                    Some(lhs) => lhs,
                    None => {
                    
                        ok = Err(todo!());
                        continue; }};

                if let Some(elem) = lhs.elem {

                    let err = XkbError::GlobalDefaultsWrongScope;
                    log::error!("{:?}: Cannot set defaults for {:?} element in indicator map;
                    Assignment to {}.{} ignored", err, elem, elem, lhs.field);
                    ok = Err(err.into());

                } else {
                    ok = self.set_led_map_field(ctx, &mut ledi, &lhs.field,
                        lhs.index, var.value).map_err(|e| e.into());
                }


            }

            if ok.is_ok() {
                return self.add_led_map(ctx, ledi, true).map_err(|e| e.into());
            }


            ok

    }

    fn handle_compat_map_file(
        &mut self, ctx: &mut Context, file: XkbFile, merge: MergeMode) 
    {


        let merge = match merge {
            MergeMode::Default => MergeMode::Augment,
            _ => merge };

        self.name = Some(file.name);

        for stmt in file.defs {

            let error: bool = match stmt {

                Decl::Include(include) 
                    => self.handle_include_compat_map(ctx, include)
                        .is_err(),
                Decl::Interp(interp)
                    => self.handle_interp_def(ctx, interp, merge)
                        .is_err(),
                Decl::GroupCompat(_)
                    => {
                        log::debug!(
                            "{:?}: The \"group\" statement in compat is unsupported; Ignored", XkbMessageCode::NoId);
                        false
                    },
                Decl::LedMap(map)
                    => self.handle_led_map_def(ctx, map, merge)
                        .is_err(),
                Decl::Var(var)
                    => self.handle_global_var(ctx, var)
                        .is_err(),
                Decl::VMod(vmod)
                    => self.mods.handle_vmod_def(ctx, vmod, merge)
                        .is_err(),
                _ => {
                    log::error!(
                        "{:?}: Compat files may not include other types;
                        Ignoring {:?}", XkbMessageCode::NoId,
                        stmt.stmt_type());
                    true
                }
            };

            if error { 
                self.error_count += 1; 
            }

            if self.error_count > 10 {
                log::error!(
                    "{:?}: Abandoning compatibility map {:?}",
                    XkbMessageCode::NoId, &self.name);
                break;
            }

        }

        
    }



    fn copy_compat_to_keymap(self, builder: &mut KeymapBuilder<TextV1>) {


        // TODO: escape map name


        let sym_interprets = match self.interps.len() {

            0 => vec![],
            _ => copy_interps(self.interps)
        };


        builder.compat_section_name = self.name;
        builder.mods = self.mods;

        builder.sym_interprets 
            = Some(sym_interprets);

        copy_led_map_defs_to_keymap(self.leds, builder);

    }


}

fn copy_led_map_defs_to_keymap(leds: Vec<LedInfo>, builder: &mut KeymapBuilder<TextV1>) {

    // TODO: rewrite this function

    for ledi in leds {

        // Find the LED with the given name, if it was already declared
        // in keycodes.
        let mut available_position = builder.leds.iter().position(|led| { 
            if let Some(led) = led {led.name == ledi.led.name }
            else { false }});


        // Not previously declared; create it with next free index
        if available_position.is_none() {
            log::debug!("{:?}: Indicator name \"{}\" was not declared in the keycodes section; Adding new indicator", XkbMessageCode::NoId, 
            builder.context
                .xkb_atom_text(ledi.led.name.unwrap_or_else(|| 0)).unwrap_or_else(|| ""));

            // get next free index
            let next_free_pos = builder.leds.iter_mut().position(|l| l.is_none());

            if next_free_pos.is_none() {
                // No place to put it; ignore.
                if builder.leds.len() >= XKB_MAX_LEDS {
                    log::error!("{:?}: Too many indicators (maximum is {}); Indicator name \"{}\" ignored", XkbMessageCode::NoId, XKB_MAX_LEDS,
                    builder.context
                        .xkb_atom_text(ledi.led.name.unwrap_or_else(|| 0)).unwrap_or_else(|| ""));
                    
                    continue;
                }

                // push to end of array is not currently necessary
                continue;
            } else if let Some(idx) = next_free_pos {

                available_position = Some(idx);
            }


        }  


        if let Some(idx) = available_position {
        
            let led = builder.leds.get_mut(idx).unwrap();

            let mut copied_led = ledi.led;
            if copied_led.groups != 0 && copied_led.which_groups.is_empty() {
                copied_led.which_groups = StateComponent::LAYOUT_EFFECTIVE; }
            if copied_led.mods.mods != 0 && copied_led.which_mods.is_empty() {
                copied_led.which_mods = StateComponent::MODS_EFFECTIVE; }
            *led = Some(copied_led);

                 
        }

        

    }

}


fn copy_interps(
    mut interps: Vec<SymInterpInfo>, 
)
    -> Vec<SymInterpret>
    {

        // Most specific to least specific

        let options = [
            (true, MatchOperation::Exactly),
            (true, MatchOperation::All),
            (true, MatchOperation::None),
            (true, MatchOperation::Any),
            (true, MatchOperation::AnyOrNone),
            (false, MatchOperation::Exactly),
            (false, MatchOperation::All),
            (false, MatchOperation::None),
            (false, MatchOperation::Any),
            (false, MatchOperation::AnyOrNone),
        ];

        interps.sort_by(move |a,b| {

            // sort by order in the `options` list

            let a_option_idx: Option<usize> 
                = options.iter().position(|q| 
                    q.0 == a.interp.sym.is_some() 
                    && q.1 == a.interp.match_op);

            let b_option_idx: Option<usize> 
                = options.iter().position(|q| 
                    q.0 == b.interp.sym.is_some() 
                    && q.1 == b.interp.match_op);


            a_option_idx.cmp(&b_option_idx)


        
        });

        interps.into_iter().map(|si| si.interp)
            .collect()
}


pub(super) fn compile_compat(
    builder: &mut KeymapBuilder<TextV1>,
    file: XkbFile,
    merge: MergeMode
) -> Result<(), KeymapErr> {

    let actions = ActionsInfo::new();

    let mut info = CompatInfo::new(
        0, actions, builder.mods.clone());
    info.default_interp.merge = merge;
    info.default_led.merge = merge;

    info.handle_compat_map_file(&mut builder.context, file, merge);
    if info.error_count != 0 {
        return Err(KeymapErr::CouldNotCompileCompat);
    }
    
        
    info.copy_compat_to_keymap(builder);


    
    return Ok(());



}

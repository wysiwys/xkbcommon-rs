use crate::context::*;
use crate::keymap::*;
use crate::rust_xkbcommon::*;

use std::fmt::Write;

struct KeymapWriter {
    buf: String,
}

impl KeymapWriter {
    fn write_vmods(
        &mut self,
        ctx: &Context,
        keymap: &Keymap,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut num_vmods = 0;

        for _mod in &keymap.mods.mods {
            if _mod.mod_type != ModType::VIRT {
                continue;
            }

            if num_vmods == 0 {
                write!(&mut self.buf, "\tvirtual_modifiers ")?;
            } else {
                write!(&mut self.buf, ",")?;
            }

            write!(
                &mut self.buf,
                "{}",
                ctx.xkb_atom_text(_mod.name).unwrap_or_else(|| "".into())
            )?;

            num_vmods += 1;
        }

        if num_vmods > 0 {
            write!(&mut self.buf, ";\n\n")?;
        }

        Ok(())
    }

    fn write_keycodes(&mut self, keymap: &Keymap) -> Result<(), Box<dyn std::error::Error>> {
        let ctx = &keymap.context;
        if let Some(name) = keymap.keycodes_section_name.as_ref() {
            write!(&mut self.buf, "xkb_keycodes \"{}\" {{\n", name)?;
        } else {
            write!(&mut self.buf, "xkb_keycodes {{\n")?;
        }

        // xkbcomp and X11 really want to see keymaps with a minimum
        // of 8, and a maximum of at least 255, else XWayland really
        // starts hating life. If this is a problem and people
        // really need strictly bounded keymaps, we should probably
        // control this with a flag.

        write!(
            &mut self.buf,
            "\tminimum = {};\n",
            u32::min(keymap.min_key_code, 8)
        )?;
        write!(
            &mut self.buf,
            "\tmaximum = {};\n",
            u32::max(keymap.min_key_code, 255)
        )?;

        for (kc, key) in keymap.keys.iter() {
            write!(
                &mut self.buf,
                "\t{:20} = {};\n",
                ctx.key_name_text(key.name),
                kc
            )?;
        }

        for (idx, led) in keymap.leds.iter().enumerate() {
            if let Some(led) = led.as_ref() {
                if let Some(name) = led.name.as_ref() {
                    write!(
                        &mut self.buf,
                        "\tindicator {} = \"{}\";\n",
                        idx + 1,
                        ctx.xkb_atom_text(*name).unwrap_or_else(|| "".into())
                    )?;
                }
            }
        }

        for alias in keymap.key_aliases.iter() {
            write!(
                &mut self.buf,
                "\talias {:14} = {};\n",
                ctx.key_name_text(alias.alias),
                ctx.key_name_text(alias.real)
            )?;
        }

        write!(&mut self.buf, "}};\n\n")?;

        Ok(())
    }
    fn write_types(&mut self, keymap: &Keymap) -> Result<(), Box<dyn std::error::Error>> {
        let ctx = &keymap.context;

        if let Some(name) = keymap.types_section_name.as_ref() {
            write!(&mut self.buf, "xkb_types \"{}\" {{\n", name)?;
        } else {
            write!(&mut self.buf, "xkb_types {{\n")?;
        }

        self.write_vmods(&ctx, keymap)?;

        for _type in keymap.types.iter() {
            write!(
                &mut self.buf,
                "\ttype \"{}\" {{\n",
                ctx.xkb_atom_text(_type.name).unwrap_or_else(|| "")
            )?;

            write!(
                &mut self.buf,
                "\t\tmodifiers= {};\n",
                ctx.mod_mask_text(&keymap.mods, _type.mods.mods)
            )?;

            for entry in _type.entries.iter() {
                if entry.level == 0 && entry.preserve.mods == 0 {
                    continue;
                }

                let s = ctx.mod_mask_text(&keymap.mods, entry.mods.mods);

                write!(&mut self.buf, "\t\tmap[{}]= {};\n", s, entry.level + 1)?;

                if entry.preserve.mods != 0 {
                    write!(
                        &mut self.buf,
                        "\t\tpreserve[{}]= {};\n",
                        s,
                        ctx.mod_mask_text(&keymap.mods, entry.preserve.mods)
                    )?;
                }
            }

            for (n, level_name) in _type.level_names.iter().enumerate() {
                // TODO: reconsider level_names vec

                write!(
                    &mut self.buf,
                    "\t\tlevel_name[{}]= \"{}\";\n",
                    n + 1,
                    ctx.xkb_atom_text(*level_name).unwrap_or_else(|| "")
                )?;
            }

            write!(&mut self.buf, "\t}};\n")?;
        }
        write!(&mut self.buf, "}};\n\n")?;

        Ok(())
    }
    fn write_led_map(
        &mut self,
        ctx: &Context,
        led: &Led,
        keymap_mods: &ModSet,
    ) -> Result<(), Box<dyn std::error::Error>> {
        write!(
            &mut self.buf,
            "\tindicator \"{}\" {{\n",
            match led.name.as_ref() {
                Some(name) => ctx.xkb_atom_text(*name),
                None => None,
            }
            .unwrap_or_else(|| "".into())
        )?;

        if !led.which_groups.is_empty() {
            if led.which_groups != StateComponent::LAYOUT_EFFECTIVE {
                write!(
                    &mut self.buf,
                    "\t\twhichGroupState= {};\n",
                    ctx.led_state_mask_text(led.which_groups)
                )?;
            }
            write!(&mut self.buf, "\t\tgroups= {:#04x};\n", led.groups)?;
        }

        if !led.which_mods.is_empty() {
            if led.which_mods != StateComponent::MODS_EFFECTIVE {
                write!(
                    &mut self.buf,
                    "\t\twhichModState= {};\n",
                    ctx.led_state_mask_text(led.which_mods)
                )?;
            }
            write!(
                &mut self.buf,
                "\t\tmodifiers= {};\n",
                ctx.mod_mask_text(keymap_mods, led.mods.mods)
            )?;
        }

        if !led.ctrls.is_empty() {
            write!(
                &mut self.buf,
                "\t\tcontrols= {};\n",
                ctx.control_mask_text(led.ctrls)
            )?;
        }
        write!(&mut self.buf, "\t}};\n")?;

        Ok(())
    }
}

impl ActionFlags {
    fn affect_lock_text(&self, show_both: bool) -> String {
        match self
            .clone()
            .intersection(ActionFlags::LockNoLock | ActionFlags::LockNoUnlock)
        {
            e if e == ActionFlags::empty() => match show_both {
                true => ",affect=both",
                false => "",
            },
            ActionFlags::LockNoUnlock => ",affect=lock",
            ActionFlags::LockNoLock => ",affect=unlock",
            e if e == ActionFlags::LockNoLock.intersection(ActionFlags::LockNoUnlock) => {
                ",affect=neither"
            }
            _ => "",
        }
        .into()
    }
}

impl KeymapWriter {
    fn write_action(
        &mut self,
        ctx: &Context,
        builder: &Keymap,
        action: Option<&Action>,
        prefix: &str,
        suffix: &str,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let action = match action {
            Some(action) => action,
            Option::None => &Action::None,
        };

        let action_type = action.action_type();
        let _type = action_type.text();

        use ActionType::*;
        match action {
            Action::Mods(m) => {
                let args = match m.flags.intersects(ActionFlags::ModsLookupModMap) {
                    true => "modMapMods".into(),
                    false => ctx.mod_mask_text(&builder.mods, m.mods.mods),
                };

                write!(
                    &mut self.buf,
                    "{}{}(modifiers={}{}{}{}){}",
                    prefix,
                    _type,
                    args,
                    if action_type != ActionType::ModLock
                        && m.flags.intersects(ActionFlags::LockClear)
                    {
                        ",clearLocks"
                    } else {
                        ""
                    },
                    if action_type != ActionType::ModLock
                        && m.flags.intersects(ActionFlags::LatchToLock)
                    {
                        ",latchToLock"
                    } else {
                        ""
                    },
                    if action_type == ActionType::ModLock {
                        m.flags.affect_lock_text(false)
                    } else {
                        "".into()
                    },
                    suffix
                )?;
            }
            Action::Group(g) => {
                let group = g.group.unwrap_or_else(|| 0);

                write!(
                    &mut self.buf,
                    "{}{}(group={}{}{}{}){}",
                    prefix,
                    _type,
                    if !g.flags.intersects(ActionFlags::AbsoluteSwitch) && group > 0 {
                        "+"
                    } else {
                        ""
                    },
                    if g.flags.intersects(ActionFlags::AbsoluteSwitch) {
                        group + 1
                    } else {
                        group
                    },
                    if action_type != ActionType::GroupLock
                        && g.flags.intersects(ActionFlags::LockClear)
                    {
                        ",clearLocks"
                    } else {
                        ""
                    },
                    if action_type != ActionType::GroupLock
                        && g.flags.intersects(ActionFlags::LatchToLock)
                    {
                        ",latchToLock"
                    } else {
                        ""
                    },
                    suffix
                )?;
            }
            Action::Terminate => write!(&mut self.buf, "{}{}(){}", prefix, _type, suffix)?,
            Action::Ptr(p) if p.action_type == PtrMove => {
                // TODO: check these defaults
                let x = p.x.unwrap_or_else(|| -1);
                let y = p.y.unwrap_or_else(|| -1);

                write!(
                    &mut self.buf,
                    "{}{}(x={}{},y={}{}{}){}",
                    prefix,
                    _type,
                    if !p.flags.intersects(ActionFlags::AbsoluteX) && x >= 0 {
                        "+"
                    } else {
                        ""
                    },
                    x,
                    if !p.flags.intersects(ActionFlags::AbsoluteY) && y >= 0 {
                        "+"
                    } else {
                        ""
                    },
                    y,
                    if p.flags.intersects(ActionFlags::Accel) {
                        ""
                    } else {
                        ",!accel"
                    },
                    suffix
                )?;
            }
            Action::Btn(b) if [PtrLock, PtrButton].contains(&b.action_type) => {
                let args = match b.action_type {
                    PtrLock => Some(b.flags.affect_lock_text(true)),
                    _ => Option::None,
                };
                write!(&mut self.buf, "{}{}(button=", prefix, _type)?;

                if let Some(button) = b.button {
                    if button > 0 && button <= 5 {
                        write!(&mut self.buf, "{}", button)?;
                    } else {
                        write!(&mut self.buf, "default")?;
                    }
                }
                if b.count != 0 {
                    write!(&mut self.buf, ",count={}", b.count)?;
                }
                if let Some(args) = args {
                    write!(&mut self.buf, "{}", args)?;
                }

                write!(&mut self.buf, "){}", suffix)?;
            }
            Action::Dflt(d) if d.action_type == PtrDefault => {
                write!(&mut self.buf, "{}{}(", prefix, _type)?;

                // TODO: check this default
                let value = d.value.unwrap_or_else(|| -1);

                write!(
                    &mut self.buf,
                    "affect=button,button={}{}",
                    if !d.flags.intersects(ActionFlags::AbsoluteSwitch) && value >= 0 {
                        "+"
                    } else {
                        ""
                    },
                    value
                )?;

                write!(&mut self.buf, "){}", suffix)?;
            }
            Action::Screen(s) if s.action_type == ActionType::SwitchVT => {
                // TODO: check default
                let screen = s.screen.unwrap_or_else(|| -1);

                write!(
                    &mut self.buf,
                    "{}{}(screen={}{},{}same){}",
                    prefix,
                    _type,
                    if !s.flags.intersects(ActionFlags::AbsoluteSwitch) && screen >= 0 {
                        "+"
                    } else {
                        ""
                    },
                    screen,
                    if s.flags.intersects(ActionFlags::SameScreen) {
                        ""
                    } else {
                        "!"
                    },
                    suffix
                )?;
            }
            Action::Ctrls(c) if [CtrlSet, CtrlLock].contains(&c.action_type) => {
                write!(
                    &mut self.buf,
                    "{}{}(controls={}{}){}",
                    prefix,
                    _type,
                    ctx.control_mask_text(c.ctrls),
                    if c.action_type == CtrlLock {
                        c.flags.affect_lock_text(false)
                    } else {
                        "".into()
                    },
                    suffix
                )?;
            }
            Action::None => write!(&mut self.buf, "{}NoAction(){}", prefix, suffix)?,
            Action::Private(p) => {
                let data: Vec<u8> = p.data.iter().map(|d| d.unwrap_or_else(|| 0)).collect();
                let action_num: u8 = action_type.into();
                write!(&mut self.buf,
                    "{}{}(type={:#04x},data[0]={:#04x},data[1]={:#04x},data[2]={:#04x},data[3]={:#04x},data[4]={:#04x},data[5]={:#04x},data[6]={:#04x}){}",
                    prefix, _type, action_num, data[0],
                    data[1], data[2], data[3],
                    data[4], data[5], data[6],
                    suffix)?;
            }
            _ => {}
        }

        Ok(())
    }
    fn write_compat(&mut self, keymap: &Keymap) -> Result<(), Box<dyn std::error::Error>> {
        let ctx = &keymap.context;

        if let Some(name) = keymap.compat_section_name.as_ref() {
            write!(&mut self.buf, "xkb_compatibility \"{}\" {{\n", name)?;
        } else {
            write!(&mut self.buf, "xkb_compatibility {{\n")?;
        }

        self.write_vmods(&ctx, keymap)?;

        write!(&mut self.buf, "\tinterpret.useModMapMods= AnyLevel;\n")?;
        write!(&mut self.buf, "\tinterpret.repeat= False;\n")?;

        for si in keymap.sym_interprets.iter() {
            write!(
                &mut self.buf,
                "\tinterpret {}+{}({}) {{\n",
                match si.sym {
                    Some(sym) => ctx.keysym_text(&sym),
                    None => "Any".into(),
                },
                ctx.si_match_text(&si.match_op),
                ctx.mod_mask_text(&keymap.mods, si.mods.mods)
            )?;

            if let Some(vmod) = si.virtual_mod {
                write!(
                    &mut self.buf,
                    "\t\tvirtualModifier= {};\n",
                    ctx.mod_index_text(&keymap.mods, vmod)
                )?;
            }

            if si.level_one_only {
                write!(&mut self.buf, "\t\tuseModMapMods=level1;\n")?;
            }

            if si.repeat {
                write!(&mut self.buf, "\t\trepeat= True;\n")?;
            }

            // TODO: is `si` guaranteed to have an action?
            self.write_action(&ctx, keymap, si.action.as_ref(), "\t\taction= ", ";\n")?;
            write!(&mut self.buf, "\t}};\n")?;
        }

        for led in keymap.leds.iter() {
            if let Some(led) = led {
                if !led.which_groups.is_empty()
                    || led.groups != 0
                    || !led.which_mods.is_empty()
                    || led.mods.mods != 0
                    || !led.ctrls.is_empty()
                {
                    self.write_led_map(&ctx, led, &keymap.mods)?;
                }
            }
        }

        write!(&mut self.buf, "}};\n\n")?;

        Ok(())
    }

    fn write_keysyms(
        &mut self,
        ctx: &Context,
        keymap: &Keymap,
        key: &Key,
        group: LayoutIndex,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let num_levels = key.num_levels(group, keymap)?;

        for level in 0..num_levels {
            if level != 0 {
                write!(&mut self.buf, ", ")?;
            }

            let syms = keymap.key_get_syms_by_level(key.keycode, group, level)?;

            let num_syms = syms.len();

            match num_syms {
                0 => write!(&mut self.buf, "{:>15}", "NoSymbol")?,
                1 => write!(&mut self.buf, "{:>15}", ctx.keysym_text(&syms[0]))?,
                _ => {
                    write!(&mut self.buf, "{{ ")?;
                    for (s, sym) in syms.iter().enumerate() {
                        if s != 0 {
                            write!(&mut self.buf, ", ")?;
                        }

                        write!(&mut self.buf, "{}", ctx.keysym_text(sym))?;
                    }
                    write!(&mut self.buf, " }}")?;
                }
            };
        }
        Ok(())
    }

    fn write_key(
        &mut self,
        ctx: &Context,
        keymap: &Keymap,
        key: &Key,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut simple = true;
        let mut explicit_types = false;
        let mut multi_type = false;

        // TODO: key.name is not optional
        write!(&mut self.buf, "\tkey {:20} {{", ctx.key_name_text(key.name))?;

        for (idx, group) in key.groups.iter().enumerate() {
            if group.explicit_type {
                explicit_types = true;
            }

            if idx != 0 && group.key_type != key.groups[0].key_type {
                multi_type = true;
            }
        }

        if explicit_types {
            simple = false;

            if multi_type {
                for (idx, group) in key.groups.iter().enumerate() {
                    if !group.explicit_type {
                        continue;
                    }

                    let _type = keymap.types.get(group.key_type).unwrap();
                    write!(
                        &mut self.buf,
                        "\n\t\ttype[Group{}]= \"{}\",",
                        idx + 1,
                        ctx.xkb_atom_text(_type.name).unwrap_or_else(|| "".into())
                    )?;
                }
            } else {
                // can only exist if groups.len() > 0
                let _type = key.groups[0].key_type;
                let _type = keymap.types.get(_type).unwrap();
                write!(
                    &mut self.buf,
                    "\n\t\ttype= \"{}\",",
                    ctx.xkb_atom_text(_type.name).unwrap_or_else(|| "".into())
                )?;
            }
        }

        if key.explicit.intersects(ExplicitComponents::REPEAT) {
            if key.repeats {
                write!(&mut self.buf, "\n\t\trepeat= Yes,")?;
            } else {
                write!(&mut self.buf, "\n\t\trepeat= No,")?;
            }
            simple = false;
        }

        if key.vmodmap != 0 && key.explicit.intersects(ExplicitComponents::VMODMAP) {
            write!(
                &mut self.buf,
                "\n\t\tvirtualMods= {},",
                ctx.mod_mask_text(&keymap.mods, key.vmodmap)
            )?;
        }

        match key.out_of_range_group_action {
            RangeExceedType::Saturate => write!(&mut self.buf, "\n\t\tgroupsClamp,")?,
            RangeExceedType::Redirect => write!(
                &mut self.buf,
                "\n\t\tgroupsRedirect= Group{},",
                key.out_of_range_group_number + 1
            )?,
            _ => {}
        }

        let show_actions = key.explicit.clone() & ExplicitComponents::INTERP;

        if key.groups.len() > 1 || !show_actions.is_empty() {
            simple = false;
        }

        if simple {
            write!(&mut self.buf, "\t[ ")?;

            self.write_keysyms(ctx, keymap, key, 0)?;

            write!(&mut self.buf, " ] }};\n")?;
        } else {
            for (idx, group) in key.groups.iter().enumerate() {
                if idx != 0 {
                    write!(&mut self.buf, ",")?;
                }

                write!(&mut self.buf, "\n\t\tsymbols[Group{}]= [ ", idx + 1)?;

                self.write_keysyms(ctx, keymap, key, idx)?;

                write!(&mut self.buf, " ]")?;

                if !show_actions.is_empty() {
                    write!(&mut self.buf, ",\n\t\tactions[Group{}]= [ ", idx + 1)?;
                    let num_levels = match key.num_levels(idx, keymap) {
                        Ok(n) => n,
                        Err(e) => return Err(e.into()),
                    };
                    for level in 0..num_levels {
                        if level != 0 {
                            write!(&mut self.buf, ", ")?;
                        }

                        let action = group.levels[level].action.as_ref();
                        self.write_action(ctx, keymap, action, "", "")?;
                    }

                    write!(&mut self.buf, " ]")?;
                }
            }
            write!(&mut self.buf, "\n\t}};\n")?;
        }

        Ok(())
    }
    fn write_symbols(&mut self, keymap: &Keymap) -> Result<(), Box<dyn std::error::Error>> {
        let ctx = &keymap.context;

        if let Some(name) = keymap.symbols_section_name.as_ref() {
            write!(&mut self.buf, "xkb_symbols \"{}\" {{\n", name)?;
        } else {
            write!(&mut self.buf, "xkb_symbols {{\n")?;
        }

        for (idx, group) in keymap.group_names.iter().enumerate() {
            write!(
                &mut self.buf,
                "\tname[Group{}]=\"{}\";\n",
                idx + 1,
                ctx.xkb_atom_text(*group).unwrap_or_else(|| "".into())
            )?;
        }
        if keymap.num_groups > 0 {
            write!(&mut self.buf, "\n")?;
        }

        for (_kc, key) in keymap.keys.iter() {
            if key.groups.len() > 0 {
                // TODO: find a way to do this without cloning
                self.write_key(&ctx, keymap, &key.clone())?;
            }
        }

        for (mod_idx, _mod) in keymap.mods.mods.iter().enumerate() {
            let mut had_any = false;

            // TODO: is this order right?
            for (_, key) in keymap.keys.iter() {
                if (key.modmap & (1 << mod_idx)) != 0 {
                    if !had_any {
                        write!(
                            &mut self.buf,
                            "\tmodifier_map {} {{ ",
                            ctx.xkb_atom_text(_mod.name).unwrap_or_else(|| "".into())
                        )?;
                    }

                    write!(
                        &mut self.buf,
                        "{}{}",
                        if had_any { ", " } else { "" },
                        ctx.key_name_text(key.name)
                    )?;

                    had_any = true;
                }
            }
            if had_any {
                write!(&mut self.buf, " }};\n")?;
            }
        }

        write!(&mut self.buf, "}};\n\n")?;
        Ok(())
    }
}

impl Keymap {
    fn write_keymap(&self) -> Result<String, Box<dyn std::error::Error>> {
        let mut writer = KeymapWriter { buf: String::new() };

        write!(&mut writer.buf, "xkb_keymap {{\n")?;
        writer.write_keycodes(self)?;
        writer.write_types(self)?;
        writer.write_compat(self)?;
        writer.write_symbols(self)?;
        write!(&mut writer.buf, "}};\n")?;

        Ok(writer.buf)
    }

    pub(crate) fn text_v1_keymap_get_as_string(
        &self,
    ) -> Result<String, Box<dyn std::error::Error>> {
        self.write_keymap()
    }
}

// based on keymap-dump.c
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

            write!(&mut self.buf, "{}", ctx.xkb_atom_text(_mod.name))?;

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
            writeln!(&mut self.buf, "xkb_keycodes \"{}\" {{", name)?;
        } else {
            writeln!(&mut self.buf, "xkb_keycodes {{")?;
        }

        // xkbcomp and X11 really want to see keymaps with a minimum
        // of 8, and a maximum of at least 255, else XWayland really
        // starts hating life. If this is a problem and people
        // really need strictly bounded keymaps, we should probably
        // control this with a flag.

        writeln!(
            &mut self.buf,
            "\tminimum = {};",
            u32::min(keymap.min_key_code, 8)
        )?;
        writeln!(
            &mut self.buf,
            "\tmaximum = {};",
            u32::max(keymap.min_key_code, 255)
        )?;

        for (kc, key) in keymap.keys.iter() {
            writeln!(
                &mut self.buf,
                "\t{:20} = {};",
                ctx.key_name_text(key.name),
                kc
            )?;
        }

        for (idx, led) in keymap.leds.iter().enumerate() {
            if let Some(led) = led.as_ref() {
                if let Some(name) = led.name.as_ref() {
                    writeln!(
                        &mut self.buf,
                        "\tindicator {} = \"{}\";",
                        idx + 1,
                        ctx.xkb_atom_text(*name)
                    )?;
                }
            }
        }

        for alias in keymap.key_aliases.iter() {
            writeln!(
                &mut self.buf,
                "\talias {:14} = {};",
                ctx.key_name_text(alias.alias),
                ctx.key_name_text(alias.real)
            )?;
        }

        writeln!(&mut self.buf, "}};\n")?;

        Ok(())
    }
    fn write_types(&mut self, keymap: &Keymap) -> Result<(), Box<dyn std::error::Error>> {
        let ctx = &keymap.context;

        if let Some(name) = keymap.types_section_name.as_ref() {
            writeln!(&mut self.buf, "xkb_types \"{}\" {{", name)?;
        } else {
            writeln!(&mut self.buf, "xkb_types {{")?;
        }

        self.write_vmods(ctx, keymap)?;

        for _type in keymap.types.iter() {
            writeln!(
                &mut self.buf,
                "\ttype \"{}\" {{",
                ctx.xkb_atom_text(_type.name)
            )?;

            writeln!(
                &mut self.buf,
                "\t\tmodifiers= {};",
                ctx.mod_mask_text(&keymap.mods, _type.mods.mods)
            )?;

            for entry in _type.entries.iter() {
                if entry.level == 0 && entry.preserve.mods == 0 {
                    continue;
                }

                let s = ctx.mod_mask_text(&keymap.mods, entry.mods.mods);

                writeln!(&mut self.buf, "\t\tmap[{}]= {};", s, entry.level + 1)?;

                if entry.preserve.mods != 0 {
                    writeln!(
                        &mut self.buf,
                        "\t\tpreserve[{}]= {};",
                        s,
                        ctx.mod_mask_text(&keymap.mods, entry.preserve.mods)
                    )?;
                }
            }

            for (n, level_name) in _type.level_names.iter() {
                writeln!(
                    &mut self.buf,
                    "\t\tlevel_name[{}]= \"{}\";",
                    n + 1,
                    ctx.xkb_atom_text(*level_name)
                )?;
            }

            writeln!(&mut self.buf, "\t}};")?;
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
        writeln!(
            &mut self.buf,
            "\tindicator \"{}\" {{",
            led.name.map(|n| ctx.xkb_atom_text(n)).unwrap_or("")
        )?;

        if !led.which_groups.is_empty() {
            if led.which_groups != StateComponent::LAYOUT_EFFECTIVE {
                writeln!(
                    &mut self.buf,
                    "\t\twhichGroupState= {};",
                    ctx.led_state_mask_text(led.which_groups)
                )?;
            }
            writeln!(&mut self.buf, "\t\tgroups= {:#04x};", led.groups)?;
        }

        if !led.which_mods.is_empty() {
            if led.which_mods != StateComponent::MODS_EFFECTIVE {
                writeln!(
                    &mut self.buf,
                    "\t\twhichModState= {};",
                    ctx.led_state_mask_text(led.which_mods)
                )?;
            }
            writeln!(
                &mut self.buf,
                "\t\tmodifiers= {};",
                ctx.mod_mask_text(keymap_mods, led.mods.mods)
            )?;
        }

        if !led.ctrls.is_empty() {
            writeln!(
                &mut self.buf,
                "\t\tcontrols= {};",
                ctx.control_mask_text(led.ctrls)
            )?;
        }
        writeln!(&mut self.buf, "\t}};")?;

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
        action: &Action,
        prefix: &str,
        suffix: &str,
    ) -> Result<(), Box<dyn std::error::Error>> {
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
                let group = g.group.unwrap_or(0);

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
                let x = p.x.unwrap_or(-1);
                let y = p.y.unwrap_or(-1);

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
                let value = d.value.unwrap_or(-1);

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
                let screen = s.screen.unwrap_or(-1);

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
            writeln!(&mut self.buf, "xkb_compatibility \"{}\" {{", name)?;
        } else {
            writeln!(&mut self.buf, "xkb_compatibility {{")?;
        }

        self.write_vmods(ctx, keymap)?;

        writeln!(&mut self.buf, "\tinterpret.useModMapMods= AnyLevel;")?;
        writeln!(&mut self.buf, "\tinterpret.repeat= False;")?;

        for si in keymap.sym_interprets.iter() {
            writeln!(
                &mut self.buf,
                "\tinterpret {}+{}({}) {{",
                match si.sym {
                    Some(sym) => ctx.keysym_text(&sym),
                    None => "Any".into(),
                },
                ctx.si_match_text(&si.match_op),
                ctx.mod_mask_text(&keymap.mods, si.mods.mods)
            )?;

            if let Some(vmod) = si.virtual_mod {
                writeln!(
                    &mut self.buf,
                    "\t\tvirtualModifier= {};",
                    ctx.mod_index_text(&keymap.mods, vmod)
                )?;
            }

            if si.level_one_only {
                writeln!(&mut self.buf, "\t\tuseModMapMods=level1;")?;
            }

            if si.repeat {
                writeln!(&mut self.buf, "\t\trepeat= True;")?;
            }

            // TODO: is `si` guaranteed to have an action?
            self.write_action(ctx, keymap, &si.action, "\t\taction= ", ";\n")?;
            writeln!(&mut self.buf, "\t}};")?;
        }

        // `flatten`: use Some(..) variants only
        for led in keymap.leds.iter().flatten() {
            if !led.which_groups.is_empty()
                || led.groups != 0
                || !led.which_mods.is_empty()
                || led.mods.mods != 0
                || !led.ctrls.is_empty()
            {
                self.write_led_map(ctx, led, &keymap.mods)?;
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
                        ctx.xkb_atom_text(_type.name)
                    )?;
                }
            } else {
                // can only exist if groups.len() > 0
                let _type = key.groups[0].key_type;
                let _type = keymap.types.get(_type).unwrap();
                write!(
                    &mut self.buf,
                    "\n\t\ttype= \"{}\",",
                    ctx.xkb_atom_text(_type.name)
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

            writeln!(&mut self.buf, " ] }};")?;
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

                        let action = &group.levels[level].action;
                        self.write_action(ctx, keymap, action, "", "")?;
                    }

                    write!(&mut self.buf, " ]")?;
                }
            }
            writeln!(&mut self.buf, "\n\t}};")?;
        }

        Ok(())
    }
    fn write_symbols(&mut self, keymap: &Keymap) -> Result<(), Box<dyn std::error::Error>> {
        let ctx = &keymap.context;

        if let Some(name) = keymap.symbols_section_name.as_ref() {
            writeln!(&mut self.buf, "xkb_symbols \"{}\" {{", name)?;
        } else {
            writeln!(&mut self.buf, "xkb_symbols {{")?;
        }

        for (idx, group) in keymap.group_names.iter().enumerate() {
            writeln!(
                &mut self.buf,
                "\tname[Group{}]=\"{}\";",
                idx + 1,
                ctx.xkb_atom_text(*group)
            )?;
        }
        if keymap.num_groups > 0 {
            writeln!(&mut self.buf)?;
        }

        for (_kc, key) in keymap.keys.iter() {
            if !key.groups.is_empty() {
                // TODO: find a way to do this without cloning
                self.write_key(ctx, keymap, &key.clone())?;
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
                            ctx.xkb_atom_text(_mod.name)
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
                writeln!(&mut self.buf, " }};")?;
            }
        }

        write!(&mut self.buf, "}};\n\n")?;
        Ok(())
    }
}

impl Keymap {
    fn write_keymap(&self) -> Result<String, Box<dyn std::error::Error>> {
        let mut writer = KeymapWriter { buf: String::new() };

        writeln!(&mut writer.buf, "xkb_keymap {{")?;
        writer.write_keycodes(self)?;
        writer.write_types(self)?;
        writer.write_compat(self)?;
        writer.write_symbols(self)?;
        writeln!(&mut writer.buf, "}};")?;

        Ok(writer.buf)
    }

    pub(crate) fn text_v1_keymap_get_as_string(
        &self,
    ) -> Result<String, Box<dyn std::error::Error>> {
        self.write_keymap()
    }
}

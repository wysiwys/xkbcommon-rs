// based on expr.h and expr.c

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
 */

use crate::atom::Atom;
use crate::context::Context;

use crate::xkbcomp::ast::*;

use crate::keymap::{ModSet, ModType, MOD_REAL_MASK_ALL, XKB_MAX_GROUPS};
use crate::keysyms::*;
use crate::rust_xkbcommon::*;
use crate::text::*;

use crate::keysyms_generated_phf::*;

use crate::errors::*;

// See keycodes.rs
pub(super) struct LhsReturn {
    pub(super) elem: Option<String>,
    pub(super) field: String,
    pub(super) index: Option<ExprDef>,
}

impl ExprDef {
    pub(super) fn resolve_lhs(self, ctx: &Context) -> Option<LhsReturn> {
        match self {
            ExprDef::Ident(ident) => Some(LhsReturn {
                elem: None,
                field: ctx.atom_text(ident.ident)?.into(),
                index: None,
            }),
            ExprDef::FieldRef(fr) => {
                //return None if elem or field is None

                let elem = ctx.atom_text(fr.element)?.into();

                let field = ctx.atom_text(fr.field)?.into();

                Some(LhsReturn {
                    elem: Some(elem),
                    field,
                    index: None,
                })
            }
            ExprDef::ArrayRef(ar) => {
                // return None if:
                //  - ar.elem is Some && elem is None
                //  - field is None

                let elem = match ar.element {
                    Some(elem) => Some(ctx.atom_text(elem)?).map(|s| s.into()),
                    None => None,
                };

                let field = ctx.atom_text(ar.field)?.into();

                Some(LhsReturn {
                    elem,
                    field,
                    index: Some(*ar.entry),
                })
            }
            other => {
                log::error!("Unexpected operator in ResolveLhs: {:?}", other.op_type());
                None
            }
        }
    }
}

impl ExprDef {
    pub(crate) fn resolve_boolean(self, ctx: &Context) -> Option<bool> {
        use ExprOpType::*;
        match self {
            expr if expr.op_type() == Value => {
                if expr.value_type() != ExprValueType::Boolean {
                    log::error!(
                        "{:?}:Found constant of type {:?} where boolean was expected",
                        XkbError::WrongFieldType,
                        expr.value_type()
                    );
                    return None;
                }

                if let ExprDef::Boolean(boolean_expr) = expr {
                    return Some(boolean_expr.set);
                }
            }
            ExprDef::Ident(ident) if ident.op == Ident => {
                if let Some(ident) = ctx.atom_text(ident.ident) {
                    let ident_lower = ident.to_lowercase();
                    let ident = ident_lower.as_str();

                    if ident == "true" || ident == "yes" || ident == "on" {
                        return Some(true);
                    } else if ident == "false" || ident == "no" || ident == "off" {
                        return Some(false);
                    }
                }
            }
            ExprDef::FieldRef(field_ref) if field_ref.op == FieldRef => {
                log::error!(
                    "{:?}: Default \"{}.{}\" of type boolean is unknown",
                    XkbError::InvalidExpressionType,
                    ctx.xkb_atom_text(field_ref.element),
                    ctx.xkb_atom_text(field_ref.field)
                );

                return None;
            }
            ExprDef::Unary(unary) if unary.op == Invert => {
                return (*unary.child).resolve_boolean(ctx);
            }
            ExprDef::Unary(unary) if unary.op == Not => {
                return (*unary.child).resolve_boolean(ctx);
            }
            _ => log::error!(
                "{:?}: {:?} of boolean values not permitted",
                XkbError::InvalidOperation,
                self.op_type()
            ),
        }

        None
    }

    /*
    pub(crate) fn resolve_keycode(self, ctx: &Context) -> Option<RawKeycode> {
        use ExprOpType::*;
        match self {
            ExprDef::Integer(int) if int.op == Value => {
                // TODO: is is assured that this will be non-negative?
                return u32::try_from(int.ival).ok();
            }
            // Non-Integer value
            expr if expr.op_type() == Value => {
                log::error!(
                    "{:?}: Found constant of type {:?} where int was expected",
                    XkbError::WrongFieldType,
                    expr.value_type()
                );
                return None;
            }
            ExprDef::Binary(binary) if [Add, Subtract, Multiply, Divide].contains(&binary.op) => {
                let left_rtrn = binary.left.resolve_keycode(ctx)?;
                let right_rtrn = binary.right.resolve_keycode(ctx)?;

                // TODO: bounds checking
                match binary.op {
                    Add => return Some(left_rtrn + right_rtrn),
                    Subtract => return Some(left_rtrn - right_rtrn),
                    Multiply => return Some(left_rtrn * right_rtrn),
                    Divide => {
                        if right_rtrn == 0 {
                            log::error!(
                                "{:?}: Cannot divide by zero: {} / {}",
                                XkbError::InvalidOperation,
                                left_rtrn,
                                right_rtrn
                            );
                            return None;
                        }

                        return Some(left_rtrn / right_rtrn);
                    }
                    _ => return None,
                }
            }
            ExprDef::Unary(unary) if unary.op == Negate => {
                let left = unary.child.resolve_keycode(ctx)?;

                return Some(!left);
            }
            ExprDef::Unary(unary) if unary.op == UnaryPlus => {
                return unary.child.resolve_keycode(ctx);
            }
            expr => {
                log::error!(
                    "{:?}: Unknown operator {:?} in ResolveKeyCode",
                    XkbError::InvalidSyntax,
                    expr.op_type()
                );

                return None;
            }
        }
    }
    */

    pub(crate) fn resolve_integer_lookup<T, F: FnOnce(usize, &Context) -> Option<T>>(
        self,
        lookup_fn: F,
        ctx: &Context,
    ) -> Option<i64>
    where
        T: Into<i64>,
        F: Copy,
    {
        use ExprOpType::*;
        match self {
            expr if expr.op_type() == Value => match expr {
                ExprDef::Integer(e) if e.value_type == ExprValueType::Int => Some(e.ival),
                expr => {
                    log::error!(
                        "{:?}: Found constant of type {:?} where an int was expected",
                        XkbError::WrongFieldType,
                        expr.value_type()
                    );
                    None
                }
            },
            ExprDef::Ident(ident) => {
                let resolved = lookup_fn(ident.ident, ctx).map(|x| x.into());

                if resolved.is_none() {
                    log::error!(
                        "{:?}: Identifier \"{}\" of type int is unknown",
                        XkbError::InvalidIdentifier,
                        ctx.xkb_atom_text(ident.ident)
                    );
                }
                resolved
            }
            ExprDef::FieldRef(fr) => {
                log::error!(
                    "{:?}: Default \"{}.{}\" of type int is unknown",
                    XkbError::InvalidExpressionType,
                    ctx.xkb_atom_text(fr.element),
                    ctx.xkb_atom_text(fr.field)
                );

                None
            }
            ExprDef::Binary(e) if [Add, Subtract, Multiply, Divide].contains(&e.op) => {
                let l = (*e.left).resolve_integer_lookup(lookup_fn, ctx)?;
                let r = (*e.right).resolve_integer_lookup(lookup_fn, ctx)?;

                match e.op {
                    Add => Some(l + r),
                    Subtract => Some(l - r),
                    Multiply => Some(l * r),
                    Divide => {
                        if r == 0 {
                            log::error!(
                                "{:?}: Cannot divide by zero: {} / {}",
                                XkbError::InvalidOperation,
                                l,
                                r
                            );
                            return None;
                        }
                        Some(l / r)
                    }
                    _ => {
                        log::error!(
                            "{:?}: {:?} of integers not permitted",
                            XkbError::InvalidOperation,
                            e.op
                        );

                        None
                    }
                }
            }

            expr if expr.op_type() == Assign => {
                log::error!(
                    "{:?}: Assignment operator not implemented yet",
                    XkbError::InvalidOperation
                );

                None
            }
            expr if expr.op_type() == Not => {
                log::error!(
                    "{:?}: The ! operator cannot be applied to an integer",
                    XkbError::InvalidOperation
                );

                None
            }
            ExprDef::Unary(unary) if [Invert, Negate].contains(&unary.op) => {
                let l = (*unary.child).resolve_integer_lookup(lookup_fn, ctx)?;

                match unary.op {
                    Negate => Some(-l),
                    _ => Some(!l), //bitwise not
                }
            }
            ExprDef::Unary(unary) if unary.op == UnaryPlus => {
                (*unary.child).resolve_integer_lookup(lookup_fn, ctx)
            }
            expr => {
                log::error!(
                    "{:?}: Unknown operator {:?} in ResolveInteger",
                    XkbError::UnknownOperator,
                    expr.op_type()
                );

                None
            }
        }
    }

    pub(crate) fn resolve_integer(self, ctx: &Context) -> Option<i64> {
        // TODO: why None?
        self.resolve_integer_lookup::<u32, _>(|_, _| None, ctx)
    }

    pub(crate) fn resolve_group(self, ctx: &Context) -> Option<LayoutIndex> {
        let result = self.resolve_integer_lookup::<u8, _>(
            |ident, ctx| {
                let s = ctx.atom_text(ident)?;
                lookup_key(&GROUP_NAMES, s).cloned()
            },
            ctx,
        )?;

        if result <= 0 || result > XKB_MAX_GROUPS.into() {
            log::error!(
                "{:?}: Group index {} is out of range (1..{})",
                XkbError::UnsupportedGroupIndex,
                result,
                XKB_MAX_GROUPS
            );

            return None;
        }

        Some(result.try_into().unwrap())
    }

    pub(crate) fn resolve_level(self, ctx: &Context) -> Option<LevelIndex> {
        let result = self.resolve_integer_lookup(
            |ident, ctx| {
                let s = ctx.atom_text(ident)?;
                lookup_key(&LEVEL_NAMES, s).copied()
            },
            ctx,
        )?;

        if result < 1 {
            log::error!(
                "{:?}: Shift level {} is out of range",
                XkbError::UnsupportedShiftLevel,
                result
            );
            return None;
        }

        // level is zero-indexed from now on
        //
        let index = result - 1;

        Some(index.try_into().unwrap())
    }

    pub(crate) fn resolve_button(self, ctx: &Context) -> Option<i64> {
        self.resolve_integer_lookup(
            |ident, ctx| {
                let s = ctx.atom_text(ident)?;
                lookup_key(&BUTTON_NAMES, s).copied()
            },
            ctx,
        )
    }

    pub(crate) fn resolve_string(self, ctx: &Context) -> Option<Atom> {
        use ExprOpType::*;
        match self {
            ExprDef::String(string) if string.op == Value => {
                return Some(string.str);
            }
            // Other, non-String value
            expr if expr.op_type() == Value => {
                log::error!(
                    "{:?}: Found constant of type {:?}, expected a string",
                    XkbError::WrongFieldType,
                    expr.value_type()
                );
            }
            ExprDef::Ident(ident) => {
                log::error!(
                    "{:?}: Identifier {:?} of type string not found.",
                    XkbError::InvalidIdentifier,
                    ctx.xkb_atom_text(ident.ident)
                );
            }
            ExprDef::FieldRef(fr) => {
                let element = ctx.xkb_atom_text(fr.element);
                let field = ctx.xkb_atom_text(fr.field);

                log::error!(
                    "{:?}: Default {}.{} of type string not found",
                    XkbError::InvalidExpressionType,
                    element,
                    field
                );
            }
            _ => {
                log::error!(
                    "{:?}: {:?} of strings not permitted",
                    XkbError::InvalidSyntax,
                    self.op_type()
                );
            }
        }

        None
    }

    pub(crate) fn resolve_enum<T, F>(self, ctx: &Context, f: F) -> Option<T>
    where
        F: Copy + FnOnce(&str) -> Option<T>,
        T: std::fmt::Debug,
    {
        if self.op_type() != ExprOpType::Ident {
            log::error!(
                "{:?}: Found a {:?} where an enumerated value was expected.",
                XkbError::WrongFieldType,
                self.op_type()
            );
            return None;
        }

        if let ExprDef::Ident(ident) = self {
            let text = ctx.atom_text(ident.ident)?;
            // looking up the enum variant
            if let Some(value) = f(text) {
                return Some(value);
            } else {
                log::error!(
                    "{:?}: Illegal identifier {}",
                    XkbError::InvalidIdentifier,
                    ident.ident
                );

                return None;
            }
        }

        None
    }

    fn resolve_mask_lookup<I, T, F>(self, ctx: &Context, lookup: F) -> Option<T>
    where
        F: Copy + FnOnce(I, ExprValueType, &Context) -> Option<T>,
        I: std::convert::TryFrom<usize> + std::convert::TryInto<usize>,
        <I as TryFrom<usize>>::Error: std::fmt::Debug,
        T: std::ops::Not<Output = T>
            + std::ops::BitAnd<Output = T>
            + std::ops::BitOr<Output = T>
            + std::convert::TryFrom<i64>
            + std::convert::TryInto<i64>,
        <T as TryFrom<i64>>::Error: std::fmt::Debug,
        <T as TryInto<i64>>::Error: std::fmt::Debug,
    {
        // This function iterates through the different types of ExprOp, and applies the
        // supplied lookup function accordingly.
        //
        use ExprOpType::*;

        match self {
            ExprDef::Integer(i) if i.op == Value => {
                // TODO: ensure non-negative?
                Some(i.ival.try_into().unwrap())
            }
            expr if expr.op_type() == Value => {
                log::error!(
                    "{:?}: Found constant of type {:?} where a mask was expected",
                    XkbError::WrongFieldType,
                    expr.value_type()
                );
                None
            }
            ExprDef::Ident(ident) => {
                let value = lookup(ident.ident.try_into().unwrap(), ident.value_type, ctx);
                if value.is_none() {
                    let err = XkbError::InvalidIdentifier;
                    log::error!(
                        "{:?}: Identifier {:?} of type int is unknown",
                        err,
                        ctx.xkb_atom_text(ident.ident)
                    );
                    return None;
                }

                value
            }
            ExprDef::FieldRef(fr) => {
                log::error!(
                    "{:?}: Default \"{:?}.{:?}\" of type int is unknown",
                    XkbError::InvalidExpressionType,
                    ctx.xkb_atom_text(fr.element),
                    ctx.xkb_atom_text(fr.field)
                );

                None
            }
            ExprDef::ArrayRef(_) => {
                log::error!(
                    "{:?}: Unexpected array reference in mask expression; Expression ignored",
                    XkbError::WrongFieldType
                );
                None
            }
            ExprDef::Action(_) => {
                log::error!(
                    "{:?}: Unexpected function use in mask expression; Expression ignored",
                    XkbError::WrongFieldType
                );
                None
            }
            ExprDef::Binary(binary) if [Add, Subtract, Multiply, Divide].contains(&binary.op) => {
                let l = (*binary.left).resolve_mask_lookup(ctx, lookup)?;
                let r = (*binary.right).resolve_mask_lookup(ctx, lookup)?;

                match binary.op {
                    Add => Some(l | r),
                    Subtract => Some(l & !r),
                    op => {
                        log::error!(
                            "{:?}: Cannot {} masks; Illegal operation ignored",
                            XkbError::InvalidOperation,
                            match op {
                                Multiply => "multiply",
                                _ => "divide",
                            }
                        );

                        None
                    }
                }
            }
            expr if expr.op_type() == Assign => {
                log::error!(
                    "{:?}: Assignment operator not implemented yet",
                    XkbError::InvalidOperation
                );

                None
            }
            ExprDef::Unary(unary) if unary.op == Invert => {
                let v: i64 = unary.child.resolve_integer_lookup::<i64, _>(
                    |ident, ctx| {
                        lookup(ident.try_into().ok()?, unary.value_type, ctx)
                            .map(|v| v.try_into().unwrap())
                    },
                    ctx,
                )?;

                let v: T = v.try_into().unwrap();
                Some(!v)
            }
            ExprDef::Unary(unary) if [UnaryPlus, Negate, Not].contains(&unary.op) => {
                let v: Option<i64> = unary.child.resolve_integer_lookup::<i64, _>(
                    |ident, ctx| {
                        lookup(ident.try_into().ok()?, unary.value_type, ctx)
                            .map(|v| v.try_into().unwrap())
                    },
                    ctx,
                );

                match v {
                    Some(v) => v,
                    None => {
                        // TODO: what about UnaryPlus?
                        log::error!(
                            "{:?}: The {:?} operator cannot be used with a mask",
                            XkbError::InvalidOperation,
                            match unary.op {
                                Negate => "-",
                                _ => "!",
                            }
                        );

                        return None;
                    }
                };

                // TODO: in original, this always fails.
                // See ln. 675
                None
            }
            expr => {
                log::error!(
                    "{:?}: Unknown operator {:?} in ResolveMask",
                    XkbError::UnknownOperator,
                    expr.op_type()
                );

                None
            }
        }
    }

    pub(crate) fn resolve_mask<I, T, F>(self, ctx: &Context, f: F) -> Option<T>
    where
        F: Copy + FnOnce(I, ExprValueType, &Context) -> Option<T>,
        I: std::convert::TryFrom<usize> + std::convert::TryInto<usize>,
        <I as TryFrom<usize>>::Error: std::fmt::Debug,
        T: std::ops::Not<Output = T>
            + std::ops::BitAnd<Output = T>
            + std::ops::BitOr<Output = T>
            + std::convert::TryFrom<i64>
            + std::convert::TryInto<i64>,
        <T as TryFrom<i64>>::Error: std::fmt::Debug,
        <T as TryInto<i64>>::Error: std::fmt::Debug,
    {
        self.resolve_mask_lookup(ctx, f)
    }

    pub(crate) fn resolve_mod_mask(
        self,
        ctx: &Context,
        mod_type: ModType,
        mods: &ModSet,
    ) -> Option<ModMask> {
        self.resolve_mask_lookup(ctx, |field, _value_type, ctx| {
            let s = ctx.atom_text(field)?.to_lowercase();

            match s.as_str() {
                "all" => Some(MOD_REAL_MASK_ALL),
                "none" => Some(0),
                _ => {
                    let ndx = mods.mod_name_to_index(field, mod_type)?;
                    Some(1u32 << ndx)
                }
            }
        })
    }

    pub(crate) fn resolve_keysym(self, ctx: &Context) -> Option<Keysym> {
        let op = self.op_type();

        if op == ExprOpType::Ident {
            if let ExprDef::Ident(ref e) = self {
                if let Some(s) = ctx.atom_text(e.ident) {
                    if let Some(sym) = NAME_TO_KEYSYM.get(s) {
                        if *sym != xkeysym::NO_SYMBOL {
                            return Some(*sym);
                        }
                    }
                }
            }
        }

        let val = match self.resolve_integer(ctx) {
            Some(val) => val,
            None => return None,
        };

        if val < XKB_KEYSYM_MIN.into() {
            log::warn!(
                "{:?}: unrecognized keysym \"-0x{}\" ({})",
                XkbWarning::UnrecognizedKeysym,
                -val,
                val
            );
            return None;
        }

        // special case for digits 0..9
        if val < 10 {
            let raw: u32 = Keysym::_0.raw() + u32::try_from(val).unwrap();
            return Some(Keysym::from(raw));
        }

        if val <= XKB_KEYSYM_MAX.into() {
            log::warn!(
                "{:?}: numeric keysym \"0x{}\" ({})",
                XkbWarning::NumericKeysym,
                val,
                val
            );
            return Some(Keysym::from(u32::try_from(val).unwrap()));
        }

        log::warn!(
            "{:?}: unrecognized keysym \"-0x{}\" ({})",
            XkbWarning::UnrecognizedKeysym,
            -val,
            val
        );

        None
    }

    pub(crate) fn resolve_mod(
        self,
        ctx: &Context,
        mod_type: ModType,
        mods: &ModSet,
    ) -> Option<ModIndex> {
        let ident = match self {
            ExprDef::Ident(ident) if ident.op == ExprOpType::Ident => ident,
            expr => {
                let err = XkbError::WrongFieldType;
                log::error!(
                    "{:?}: Cannot resolve virtual modifier:
                found {:?} where a virtual modifier name was expected.",
                    err,
                    expr.op_type()
                );
                return None;
            }
        };

        let name = ident.ident;
        let ndx = mods.mod_name_to_index(name, mod_type);
        if ndx.is_none() {
            let err = XkbError::UndeclaredVirtualModifier;
            log::error!(
                "{:?}: Cannot resolve virtual modifier:
                {:?} was not previously declared.",
                err,
                ctx.xkb_atom_text(name)
            );
            return None;
        }

        ndx
    }
}

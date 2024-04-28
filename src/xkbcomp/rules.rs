// based on rules.c
/************************************************************
 * Copyright (c) 1996 by Silicon Graphics Computer Systems, Inc.
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
 */
use super::ast::XkbFileType;

use super::xkbcomp::ComponentNames;

use crate::context::Context;
use crate::errors::*;
use crate::keymap::XKB_MAX_GROUPS;
use crate::rust_xkbcommon::*;

use std::collections::BTreeMap;
use std::path::PathBuf;

use strum::{EnumCount, IntoEnumIterator};

use logos::{Lexer, Logos};

const MAX_INCLUDE_DEPTH: usize = 5;

#[derive(Logos, Debug, PartialEq)]
#[logos(error = &'static str)]
pub(crate) enum RulesToken<'input> {
    #[token("!", priority = 3)]
    Bang,

    #[token("=", priority = 3)]
    Equals,

    #[token("*", priority = 3)]
    Star,

    #[regex("[\n\r]", |_| RulesToken::EndOfLine, priority=3)]
    EndOfLine,

    #[regex(r"[ \t]+", |_| logos::Skip)]
    Whitespace,

    #[regex(r"\\[\n\r]", |_| logos::Skip, priority=5)]
    LineContinue,

    #[regex("//[^\n]*[\n\r]?", |_| logos::Skip, priority=4)]
    Comment,

    // is_graph and != \\
    #[regex(r"\$[\x21-\x5B\x5D-\x7E]+", 
        |lex| &lex.slice()[1..], priority=3)]
    GroupName(&'input str),

    #[token("include", priority = 2)]
    Include,

    // is_graph and != \\
    #[regex(r"[\x21-\x5B\x5D-\x7E]+", 
        |lex| lex.slice(), priority=1)]
    Identifier(&'input str),
}

#[derive(Default, Clone, Debug, PartialEq)]
enum IncludeTokenLexingError {
    #[default]
    Unrecognized,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = IncludeTokenLexingError)]
enum IncludeToken<'input> {
    #[token("%%", priority = 3)]
    DoublePercent,

    #[token("%H", priority = 3)]
    Home,

    #[token("%S", priority = 3)]
    S,

    #[token("%E", priority = 3)]
    E,
    #[regex(r"%[.]", |lex| lex.slice(), priority=2)]
    UnknownFormat(&'input str),

    #[regex("[^%]+", |lex| lex.slice(), priority=1)]
    OtherText(&'input str),
}

#[derive(Clone, Copy, Debug, PartialEq, strum_macros::EnumCount, strum_macros::EnumIter)]
enum RulesMlvo {
    Model = 0,
    Layout = 1,
    Variant = 2,
    Option = 3,
}

impl RulesMlvo {
    fn sval(&self) -> &'static str {
        use RulesMlvo::*;
        match self {
            Model => "model",
            Layout => "layout",
            Variant => "variant",
            Option => "option",
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    strum_macros::EnumCount,
    strum_macros::EnumIter,
)]
enum RulesKccgst {
    Keycodes = 0,
    Types = 1,
    Compat = 2,
    Symbols = 3,
    Geometry = 4,
}

impl RulesKccgst {
    fn sval(&self) -> &'static str {
        use RulesKccgst::*;
        match self {
            Keycodes => "keycodes",
            Types => "types",
            Compat => "compat",
            Symbols => "symbols",
            Geometry => "geometry",
        }
    }
}

#[derive(Clone, Debug)]
struct MatchedSval<'input> {
    sval: &'input str, // TODO: in original, was pointer to a slice in the input
    matched: bool,
}

// A broken-down version of xkb_rule_names (without the rules, obviously)
struct MatcherRuleNames<'input> {
    model: MatchedSval<'input>,
    layouts: Vec<MatchedSval<'input>>,
    variants: Vec<MatchedSval<'input>>,
    options: Vec<MatchedSval<'input>>,
}

struct Group {
    name: String,
    elements: Vec<String>,
}

struct Mapping {
    mlvo_at_pos: [Option<RulesMlvo>; RulesMlvo::COUNT],
    num_mlvo: usize,
    defined_mlvo_mask: u32,
    layout_index: Option<LayoutIndex>,
    variant_index: Option<LayoutIndex>,
    defined_kccgst_mask: u32,
    kccgst_at_pos: [Option<RulesKccgst>; RulesKccgst::COUNT],
    num_kccgst: usize, // TODO: remove
    skip: bool,
}
impl Default for Mapping {
    fn default() -> Self {
        Mapping {
            mlvo_at_pos: [None; RulesMlvo::COUNT],
            kccgst_at_pos: [None; RulesKccgst::COUNT],
            layout_index: None,
            variant_index: None,
            num_mlvo: 0,
            num_kccgst: 0,
            defined_kccgst_mask: 0,
            defined_mlvo_mask: 0,
            skip: false,
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum MlvoMatchType {
    Normal = 0,
    Wildcard,
    Group,
}

struct Rule {
    mlvo_value_at_pos: Vec<String>,        //RulesMlvo::COUNT],
    match_type_at_pos: Vec<MlvoMatchType>, //RulesMlvo::COUNT],
    kccgst_value_at_pos: Vec<String>,      //RulesKccgst::COUNT],
    skip: bool,
}

impl Default for Rule {
    fn default() -> Self {
        Self {
            mlvo_value_at_pos: Vec::with_capacity(RulesMlvo::COUNT),
            match_type_at_pos: Vec::with_capacity(RulesMlvo::COUNT),
            kccgst_value_at_pos: Vec::with_capacity(RulesKccgst::COUNT),

            skip: false,
        }
    }
}

// TODO: use ranges instead of slices where possible
struct Matcher<'c> {
    ctx: &'c Context,

    rmlvo: MatcherRuleNames<'c>,
    groups: Vec<Group>,
    kccgst: BTreeMap<RulesKccgst, String>,

    // current mapping
    mapping: Mapping,

    // current rule
    rule: Rule,
}

impl<'c> Matcher<'c> {
    fn new(ctx: &'c Context, rmlvo: &'c RuleNames) -> Self {
        // TODO: don't clone the string content,
        // if possible

        // TODO: is this correct?
        let model = MatchedSval {
            sval: rmlvo.model.as_deref().unwrap_or(""),
            matched: false,
        };
        let rmlvo = MatcherRuleNames {
            layouts: split_comma_separated_mlvo(rmlvo.layout.as_deref()),
            variants: split_comma_separated_mlvo(rmlvo.variant.as_deref()),
            options: split_comma_separated_mlvo(rmlvo.options.as_deref()),
            model,
        };
        Self {
            ctx,
            rmlvo,
            groups: vec![],
            kccgst: BTreeMap::new(),

            mapping: Mapping::default(),
            rule: Rule::default(),
        }
    }

    fn group_start_new(&mut self, name: &str) {
        let group = Group {
            name: name.to_owned(),
            elements: vec![],
        };

        self.groups.push(group);
    }

    fn group_add_element(&mut self, element: &str) -> Result<(), MatcherError> {
        self.groups
            .iter_mut()
            .last()
            .map(|g| g.elements.push(element.to_owned()))
            .ok_or(MatcherError::NoGroupAvailable)
    }

    fn include(&mut self, include_depth: usize, inc: &str) -> Result<(), MatcherError> {
        // parse the include value
        // This needs a separate lexer

        if include_depth >= MAX_INCLUDE_DEPTH {
            log::error!(
                "maximum include depth {} exceeded; maybe there is an include loop?",
                MAX_INCLUDE_DEPTH
            );
            return Ok(());
        }

        // TODO: max size for scanner buf,
        // and checks in the loop
        let mut buf = String::new();

        for token in IncludeToken::lexer(inc) {
            use IncludeToken::*;
            match token.expect("Lexer failed on token") {
                DoublePercent => buf += "%",
                Home => {
                    let home = self
                        .ctx
                        .getenv("HOME")
                        .ok_or(MatcherError::IncludeHButNoHOME)?;

                    buf += home.as_str();
                }
                S => {
                    let default_root = self.ctx.include_path_get_system_path();

                    // TODO: limit size of scanner buf
                    buf += default_root.as_str();
                    buf += "/rules";
                }
                E => {
                    let default_root: String = self.ctx.include_path_get_extra_path();

                    buf += default_root.as_str();
                    buf += "/rules";
                }
                UnknownFormat(f) => {
                    return Err(MatcherError::UnknownFormatInIncludeStmt(f.to_owned()));
                }
                OtherText(s) => buf += s,
            }
        }

        let file = std::fs::File::open(&buf).map_err(|e| {
            let error_msg = format!(
                "{:?}: Failed to open included XKB rules \"{}\"",
                XkbMessageCode::NoId,
                buf
            );
            log::error!("{}", error_msg);

            MatcherError::FailedToOpenXKBRules {
                rules: buf.clone(),
                error: e,
            }
        })?;

        // Read the rules file
        if let Err(e) = self.read_rules_file(include_depth + 1, file, buf.clone().into()) {
            log::error!(
                "{:?}: No components returned from included XKB rules \"{}\"",
                XkbMessageCode::NoId,
                buf
            );
            Err(e)
        } else {
            Ok(())
        }
    }
    fn mapping_start_new(&mut self) {
        self.mapping = Mapping::default();
    }

    fn mapping_set_mlvo(&mut self, ident: &str) {
        // TODO: make this and its equivalent for kccgst more concise
        let pos = RulesMlvo::iter().enumerate().find(|(_, mlvo)| {
            let sval = mlvo.sval();
            ident.len() >= sval.len() //TODO: equals?
                        && ident[..sval.len()] == *sval
        });

        // Not found
        let (mlvo_pos, mlvo) = match pos {
            None => {
                log::error!(
                    "invalid mapping: {} is not a valid value here; ignoring rule set",
                    ident
                );
                self.mapping.skip = true;
                return;
            }
            Some(p) => p,
        };

        let mlvo_sval = mlvo.sval();

        if (self.mapping.defined_mlvo_mask & (1 << mlvo_pos)) != 0 {
            log::error!(
                "invalid mapping: {}.{} appears twice on the same line; ignoring rule set",
                mlvo_sval.len(),
                mlvo_sval
            );
            self.mapping.skip = true;
            return;
        }

        // If there are leftovers still, it must be an index.

        if mlvo_sval.len() < ident.len() {
            let (idx, consumed) = match extract_layout_index(&ident[mlvo_sval.len()..]) {
                Some((idx, consumed)) => (Some(idx), Some(consumed)),
                None => (None, None),
            };

            if let Some(consumed) = consumed {
                if (ident.len() - mlvo_sval.len()) != consumed {
                    log::error!("invalid mapping: \"{}.{}\" may only be followed by a valid group index; ignoring rule set",
                        mlvo_sval.len(),
                        mlvo_sval);
                    self.mapping.skip = true;
                    return;
                }
            }

            if mlvo == RulesMlvo::Layout {
                self.mapping.layout_index = idx;
            } else if mlvo == RulesMlvo::Variant {
                self.mapping.variant_index = idx;
            } else {
                log::error!("invalid mapping: \"{}.{}\" cannot be followed by a group index; ignoring rule set",
                    mlvo_sval.len(),
                    mlvo_sval);
                self.mapping.skip = true;
                return;
            }
        }

        // TODO: check array bounds
        self.mapping.mlvo_at_pos[self.mapping.num_mlvo] = Some(mlvo);

        self.mapping.defined_mlvo_mask |= 1 << mlvo_pos;
        self.mapping.num_mlvo += 1;
    }

    fn mapping_set_kccgst(&mut self, ident: &str) {
        let pos = RulesKccgst::iter().enumerate().find(|(_, kccgst)| {
            let sval = kccgst.sval();
            ident.len() >= sval.len() //TODO: equals?
                        && ident[..sval.len()] == *sval
        });

        // Not found
        let (kccgst_pos, kccgst) = match pos {
            Some(p) => p,
            None => {
                log::error!(
                    "invalid mapping: {}.{} is not a valid value here; ignoring rule set",
                    ident.len(),
                    ident
                );
                self.mapping.skip = true;
                return;
            }
        };

        if self.mapping.defined_kccgst_mask & (1 << kccgst_pos) != 0 {
            let sval = kccgst.sval();
            log::error!(
                "invalid mapping {}.{} appears twice on the same line; ignoring rule set",
                sval.len(),
                sval
            );
            self.mapping.skip = true;
            return;
        }

        // TODO: check array bounds
        self.mapping.kccgst_at_pos[self.mapping.num_kccgst] = Some(kccgst);
        self.mapping.num_kccgst += 1;
        self.mapping.defined_kccgst_mask |= 1 << kccgst_pos;
    }

    fn mapping_verify(&mut self) {
        if self.mapping.num_mlvo == 0 {
            log::debug!("invalid mapping: must have at least one value on the left hand side; ignoring rule set");
            self.mapping.skip = true;
            return;
        }
        if self.mapping.num_kccgst == 0 {
            log::debug!("invalid mapping: must have at least one value on the right hand side; ignoring rule set");
            self.mapping.skip = true;
            return;
        }

        // "The following is very stupid, but this is how it works. See the 'Notes' section in the
        // overview above"

        if (self.mapping.defined_mlvo_mask & (1 << RulesMlvo::Layout as u32)) != 0 {
            if self.mapping.layout_index.is_none() {
                if self.rmlvo.layouts.len() > 1 {
                    self.mapping.skip = true;
                    return;
                }
            } else if let Some(layout_idx) = self.mapping.layout_index {
                if self.rmlvo.layouts.len() == 1 || layout_idx >= self.rmlvo.layouts.len() {
                    self.mapping.skip = true;
                    return;
                }
            }
        }
        if (self.mapping.defined_mlvo_mask & (1 << RulesMlvo::Variant as u32)) != 0 {
            if self.mapping.variant_index.is_none() {
                if self.rmlvo.variants.len() > 1 {
                    self.mapping.skip = true;
                }
            } else if let Some(variant_idx) = self.mapping.variant_index {
                if self.rmlvo.variants.len() == 1 || variant_idx >= self.rmlvo.variants.len() {
                    self.mapping.skip = true;
                }
            }
        }
    }

    fn rule_start_new(&mut self) {
        self.rule = Rule::default();
        self.rule.skip = self.mapping.skip;
    }

    fn rule_set_mlvo_common(&mut self, ident: &str, match_type: MlvoMatchType) {
        if self.rule.mlvo_value_at_pos.len() + 1 > self.mapping.num_mlvo {
            log::error!("invalid rule: has more values than the mapping line; ignoring rule");
            self.rule.skip = true;
            return;
        }
        // TODO: check bounds
        self.rule.match_type_at_pos.push(match_type);
        self.rule.mlvo_value_at_pos.push(ident.to_owned());
    }
    fn rule_set_mlvo_wildcard(&mut self) {
        self.rule_set_mlvo_common("", MlvoMatchType::Wildcard)
    }

    fn rule_set_mlvo_group(&mut self, ident: &str) {
        self.rule_set_mlvo_common(ident, MlvoMatchType::Group)
    }

    fn rule_set_mlvo(&mut self, ident: &str) {
        self.rule_set_mlvo_common(ident, MlvoMatchType::Normal)
    }

    fn rule_set_kccgst(&mut self, ident: &str) {
        if self.rule.kccgst_value_at_pos.len() + 1 > self.mapping.num_kccgst {
            log::error!("invalid rule: has more values than the mapping line; ignoring rule");
            self.rule.skip = true;
            return;
        }

        // TODO: check bounds
        self.rule.kccgst_value_at_pos.push(ident.to_owned());
    }

    fn match_group(&self, group_name: &str, elem_to_find: &str) -> bool {
        let mut found_group = None;

        for group in self.groups.iter() {
            if group.name == group_name {
                found_group = Some(group);
                break;
            }
        }

        if let Some(found_group) = found_group {
            for element in found_group.elements.iter() {
                if elem_to_find == element {
                    return true;
                }
            }

            false
        } else {
            // rules/evded intentionally uses some undeclared
            // group names in rules (e.g. commented group definitions
            // which may be uncommented if needed).
            // So we continue silently

            false
        }
    }

    fn match_value(&self, val: &str, to: &str, match_type: MlvoMatchType) -> bool {
        use MlvoMatchType::*;
        match match_type {
            Wildcard => true,
            Group => self.match_group(val, to),
            _ => val == to,
        }
    }

    fn match_value_and_mark(
        &self,
        val: &str,
        to: &mut MatchedSval,
        match_type: MlvoMatchType,
    ) -> bool {
        let matched = self.match_value(val, to.sval.as_ref(), match_type);

        if matched {
            to.matched = true;
        }

        matched
    }

    /// This function performs %-expansion on `value`,
    /// and appends the value to `self.kccgst[category]
    fn append_expanded_kccgst_value(
        &mut self,
        category: &RulesKccgst,
        i: usize,
    ) -> Result<(), MatcherError> {
        // TODO: implement this with a Logos lexer

        let value = &self.rule.kccgst_value_at_pos[i];

        let mut expanded = String::new();

        let mut chars = value.chars().enumerate().peekable();

        let mut current_ch = chars.next().map(|(_, ch)| ch);

        while let Some(mut ch) = current_ch {
            if ch != '%' {
                expanded.push(ch);
                current_ch = chars.next().map(|(_, ch)| ch);
                continue;
            }
            ch = chars
                .next()
                .map(|(_, ch)| ch)
                .ok_or(MatcherError::LexerEarlyEOF)?;

            let mut prefix = None;
            let mut suffix = None;

            // Check for prefix
            if ['(', '+', '|', '_', '-'].contains(&ch) {
                prefix = Some(ch);
                if '(' == ch {
                    suffix = Some(')');
                }
                ch = chars
                    .next()
                    .map(|(_, ch)| ch)
                    .ok_or(MatcherError::LexerEarlyEOF)?;
            }

            // Mandatory model/layout/variant specifier
            let mlv = match ch {
                'm' => RulesMlvo::Model,
                'l' => RulesMlvo::Layout,
                'v' => RulesMlvo::Variant,
                c => return Err(MatcherError::LexerUnexpectedChar(c)),
            };

            // Check for index
            let mut index = None;
            let (i, mut ch) = chars.next().ok_or(MatcherError::LexerEarlyEOF)?;

            if ch == '[' {
                if mlv != RulesMlvo::Layout && mlv != RulesMlvo::Variant {
                    log::error!("invalid index in %-expansion; may only index layout or variant");
                    return Err(MatcherError::LexerInvalidIndex);
                }

                let (idx, consumed) = extract_layout_index(&value[i..])
                    .ok_or(MatcherError::LexerCouldNotExtractLayoutIndex)?;

                index = Some(idx);

                for _ in 0..consumed {
                    // i not assigned here because marked unused by Rust compiler
                    (_, ch) = chars.next().unwrap();
                    current_ch = Some(ch);
                }
            } else {
                current_ch = Some(ch);
            }

            // Check for suffix, if there is supposed to be one
            if let Some(sfx) = suffix {
                if current_ch != Some(sfx) {
                    return match current_ch {
                        Some(c) => Err(MatcherError::LexerUnexpectedChar(c)),
                        None => Err(MatcherError::LexerEarlyEOF),
                    };
                }

                current_ch = chars.next().map(|(_, ch)| ch);
            }

            // Get the expanded value
            let mut expanded_value = None;

            if mlv == RulesMlvo::Layout {
                if let Some(idx) = index {
                    if let Some(layout) = self.rmlvo.layouts.get_mut(idx) {
                        expanded_value = Some(layout);
                    }
                } else if index.is_none() && self.rmlvo.layouts.len() == 1 {
                    expanded_value = self.rmlvo.layouts.get_mut(0);
                }
            } else if mlv == RulesMlvo::Variant {
                if let Some(idx) = index {
                    if let Some(variant) = self.rmlvo.variants.get_mut(idx) {
                        expanded_value = Some(variant);
                    }
                } else if index.is_none() && self.rmlvo.variants.len() == 1 {
                    expanded_value = self.rmlvo.variants.get_mut(0);
                }
            } else if mlv == RulesMlvo::Model {
                expanded_value = Some(&mut self.rmlvo.model);
            }

            // If we didn't get one, skip silently.
            let expanded_value = match expanded_value {
                Some(s) if !s.sval.is_empty() => s,
                _ => continue,
            };

            if let Some(pfx) = prefix {
                expanded.push(pfx);
            }

            for c in expanded_value.sval.chars() {
                expanded.push(c);
            }

            if let Some(sfx) = suffix {
                expanded.push(sfx);
            }

            expanded_value.matched = true;
        }

        // insert if does not exist
        let to = match self.kccgst.get_mut(category) {
            Some(s) => s,
            None => {
                self.kccgst.insert(*category, String::new());
                self.kccgst.get_mut(category).unwrap()
            }
        };
        // Appends

        let ch = expanded.chars().next();
        let expanded_plus = [Some('+'), Some('|')].contains(&ch);
        let ch = to.chars().next();
        let to_plus = [Some('+'), Some('|')].contains(&ch);

        if expanded_plus || to.is_empty() {
            *to += expanded.as_str();
        } else if to_plus {
            *to = expanded + to.as_str();
        }

        Ok(())
    }

    fn rule_verify(&mut self) {
        if self.rule.mlvo_value_at_pos.len() != self.mapping.num_mlvo
            || self.rule.kccgst_value_at_pos.len() != self.mapping.num_kccgst
        {
            log::error!(
                "invalid rule: must have same number of values as mapping line; ignoring rule"
            );
            self.rule.skip = true;
        }
    }

    fn rule_apply_if_matches(&mut self) -> Result<(), MatcherError> {
        for i in 0..self.mapping.num_mlvo {
            // TODO: check index validity
            let mlvo = self.mapping.mlvo_at_pos[i];
            let value = &self.rule.mlvo_value_at_pos[i];
            let match_type = self.rule.match_type_at_pos[i];

            let mut matched = false;

            if mlvo == Some(RulesMlvo::Model) {
                let mut to_str = self.rmlvo.model.clone();
                // TODO: more functional style so don't need to clone
                matched = self.match_value_and_mark(value, &mut to_str, match_type);
                self.rmlvo.model = to_str;
            } else if mlvo == Some(RulesMlvo::Layout) {
                let idx = self.mapping.layout_index.unwrap_or(0);

                let mut to_str = self
                    .rmlvo
                    .layouts
                    .get(idx)
                    .ok_or(MatcherError::InvalidLayoutIndex(idx))?
                    .clone();
                matched = self.match_value_and_mark(value, &mut to_str, match_type);
                self.rmlvo.layouts[idx] = to_str;
            } else if mlvo == Some(RulesMlvo::Variant) {
                let idx = self.mapping.variant_index.unwrap_or(0);

                let mut to_str = self
                    .rmlvo
                    .variants
                    .get(idx)
                    .ok_or(MatcherError::InvalidVariantIndex(idx))?
                    .clone();
                matched = self.match_value_and_mark(value, &mut to_str, match_type);
                self.rmlvo.variants[idx] = to_str;
            } else if mlvo == Some(RulesMlvo::Option) {
                for i in 0..self.rmlvo.options.len() {
                    let mut to_str = self.rmlvo.options[i].clone();
                    matched = self.match_value_and_mark(value, &mut to_str, match_type);
                    self.rmlvo.options[i] = to_str;

                    if matched {
                        break;
                    }
                }
            }

            if !matched {
                return Ok(());
            }
        }
        for i in 0..self.mapping.num_kccgst {
            // TODO: reconsider these data structures
            let kccgst = self.mapping.kccgst_at_pos[i].unwrap();

            self.append_expanded_kccgst_value(&kccgst, i)?;
        }

        // If a rule matches in a rule set, the rest of the set
        // should be skipped. However, rule sets matching against
        // options may contain several legitimate rules,
        // so they are processed entirely.

        if self.mapping.defined_mlvo_mask & (1 << RulesMlvo::Option as u32) == 0 {
            self.mapping.skip = true;
        }

        Ok(())
    }
}

fn extract_layout_index(s: &str) -> Option<(LayoutIndex, usize)> {
    // "This function is pretty stupid, but works for now"
    if s.len() < 3 {
        return None;
    }

    let mut chars = s.chars();
    let c0 = chars.next().unwrap();
    let c1 = chars.next().unwrap();
    let c2 = chars.next().unwrap();
    if c0 != '[' || !c1.is_ascii_digit() || c2 != ']' {
        return None;
    }

    let c1_u32 = u32::from(c1);
    let zero_u32 = u32::from('0');

    if c1_u32 - zero_u32 < 1 || c1_u32 - zero_u32 > XKB_MAX_GROUPS.into() {
        return None;
    }

    // to zero-based index
    let layout_index = c1_u32 - zero_u32 - 1;
    let layout_index: usize = layout_index.try_into().unwrap();

    Some((layout_index, 3))
}

fn split_comma_separated_mlvo(s: Option<&str>) -> Vec<MatchedSval> {
    let s = s.unwrap_or("");

    let substrings: Vec<&str> = s.split(',').collect();

    // TODO: Make sure the array returned by this function always includes at least one value.

    let strings = match substrings.len() {
        0 => vec![""],
        _ => substrings,
    };

    strings
        .into_iter()
        .map(|s| MatchedSval {
            sval: s,
            matched: false,
        })
        .collect()
}

#[derive(PartialEq, Debug)]
enum MatcherState<'input> {
    Initial,
    Bang,
    GroupName,
    GroupElement,
    IncludeStatement,
    IncludeStatementEnd,
    MappingMlvo,
    MappingKccgst,
    RuleMlvoFirst,
    RuleMlvo,
    RuleMlvoNoTok(
        //pass lexer.next() from last iteration
        Option<Result<RulesToken<'input>, &'static str>>,
    ),
    RuleKccgst,
    Unexpected(Option<Result<RulesToken<'input>, &'static str>>),
    Finish,
}

impl<'input, 'c: 'input> Matcher<'c> {
    fn state_machine(
        &mut self,
        mut lexer: Lexer<'input, RulesToken<'input>>,
        include_depth: usize,
    ) -> Result<(), MatcherError> {
        use MatcherState::*;
        let mut state = Initial;
        while state != Finish {
            match state {
                Initial => match lexer.next() {
                    Some(Ok(RulesToken::Bang)) => state = Bang,
                    Some(Ok(RulesToken::EndOfLine)) => state = Initial,
                    None => state = Finish,
                    t => state = Unexpected(t),
                },

                Bang => match lexer.next() {
                    Some(Ok(RulesToken::GroupName(s))) => {
                        self.group_start_new(s);
                        state = GroupName
                    }
                    Some(Ok(RulesToken::Include)) => state = IncludeStatement,
                    Some(Ok(RulesToken::Identifier(s))) => {
                        self.mapping_start_new();
                        self.mapping_set_mlvo(s);
                        state = MappingMlvo;
                    }
                    t => state = Unexpected(t),
                },

                GroupName => match lexer.next() {
                    Some(Ok(RulesToken::Equals)) => state = GroupElement,
                    t => state = Unexpected(t),
                },

                GroupElement => match lexer.next() {
                    Some(Ok(RulesToken::Identifier(s))) => {
                        self.group_add_element(s)?;
                        state = GroupElement;
                    }
                    Some(Ok(RulesToken::EndOfLine)) => state = Initial,
                    t => state = Unexpected(t),
                },

                IncludeStatement => match lexer.next() {
                    Some(Ok(RulesToken::Identifier(s))) => {
                        self.include(include_depth, s)?;
                        state = IncludeStatementEnd;
                    }
                    t => state = Unexpected(t),
                },

                IncludeStatementEnd => match lexer.next() {
                    Some(Ok(RulesToken::EndOfLine)) => state = Initial,

                    t => state = Unexpected(t),
                },

                MappingMlvo => match lexer.next() {
                    Some(Ok(RulesToken::Identifier(s))) => {
                        if !self.mapping.skip {
                            self.mapping_set_mlvo(s);
                        }
                        state = MappingMlvo;
                    }
                    Some(Ok(RulesToken::Equals)) => state = MappingKccgst,
                    t => state = Unexpected(t),
                },

                MappingKccgst => match lexer.next() {
                    Some(Ok(RulesToken::Identifier(s))) => {
                        if !self.mapping.skip {
                            self.mapping_set_kccgst(s);
                        }
                        state = MappingKccgst;
                    }
                    Some(Ok(RulesToken::EndOfLine)) => {
                        if !self.mapping.skip {
                            self.mapping_verify();
                        }
                        state = RuleMlvoFirst;
                    }
                    t => state = Unexpected(t),
                },

                RuleMlvoFirst => match lexer.next() {
                    Some(Ok(RulesToken::Bang)) => state = Bang,
                    Some(Ok(RulesToken::EndOfLine)) => state = RuleMlvoFirst,
                    None => state = Finish,
                    token => {
                        self.rule_start_new();
                        state = RuleMlvoNoTok(token);
                    }
                },

                RuleMlvo => state = RuleMlvoNoTok(lexer.next()),

                RuleMlvoNoTok(token) => match token {
                    Some(Ok(RulesToken::Identifier(s))) => {
                        if !self.rule.skip {
                            self.rule_set_mlvo(s);
                        }
                        state = RuleMlvo;
                    }
                    Some(Ok(RulesToken::Star)) => {
                        if !self.rule.skip {
                            self.rule_set_mlvo_wildcard();
                        }
                        state = RuleMlvo;
                    }
                    Some(Ok(RulesToken::GroupName(s))) => {
                        if !self.rule.skip {
                            self.rule_set_mlvo_group(s);
                        }
                        state = RuleMlvo;
                    }
                    Some(Ok(RulesToken::Equals)) => state = RuleKccgst,
                    t => state = Unexpected(t),
                },

                RuleKccgst => match lexer.next() {
                    Some(Ok(RulesToken::Identifier(s))) => {
                        if !self.rule.skip {
                            self.rule_set_kccgst(s);
                        }
                        state = RuleKccgst;
                    }
                    Some(Ok(RulesToken::EndOfLine)) => {
                        if !self.rule.skip {
                            self.rule_verify();
                        }
                        if !self.rule.skip {
                            self.rule_apply_if_matches()?;
                        }
                        state = RuleMlvoFirst;
                    }
                    t => state = Unexpected(t),
                },

                Unexpected(t) => match t {
                    None => return Err(MatcherError::UnexpectedFinish),
                    Some(Ok(t)) => return Err(MatcherError::UnexpectedToken(format!("{:?}", t))),
                    Some(Err(e)) => return Err(MatcherError::LexerError(e)),
                },

                Finish => return Ok(()),
            }
        }

        Ok(())
    }
}

impl<'l> Matcher<'l> {
    fn read_rules_file(
        &mut self,
        include_depth: usize,
        mut file: std::fs::File,
        path: PathBuf,
    ) -> Result<(), MatcherError> {
        use std::io::prelude::*;
        // TODO: use map_file?
        let mut string = String::new();
        if let Err(e) = file.read_to_string(&mut string) {
            log::error!(
                "{:?}: Couldn't read rules file {:?}: {}",
                XkbMessageCode::NoId,
                path,
                e
            );
            return Err(MatcherError::CouldNotReadRulesToString { path, error: e });
        };

        // scanner_init
        let input = crate::lexer::check_supported_char_encoding(&string).map_err(|_| {
                log::error!("This could be a file encoding issue. Supported encodings must be backward compatible with ASCII");
                log::error!("E.g. ISO/CEI 8859 and UTF-8 are supported but UTF-16, UTF-32 and CP1026 are not.");
                MatcherError::WrongEncoding(path)
            })?;

        let lexer = RulesToken::lexer(input);

        // TODO: basic detection of wrong character encoding

        self.state_machine(lexer, include_depth)
    }
}

impl ComponentNames {
    pub(crate) fn from_rules(
        context: &mut Context,
        rule_names: &RuleNames,
    ) -> Result<Self, RulesCompileError> {
        let opt_file =
            context.find_file_in_xkb_path(rule_names.rules.as_ref(), XkbFileType::Rules, &mut 0);

        let mut matcher = Matcher::new(context, rule_names);

        if let Some((path, file)) = opt_file {
            let result = matcher.read_rules_file(0, file, path.clone());

            if result.is_err()
            || matcher.kccgst.get(&RulesKccgst::Keycodes).is_none() //keycodes 
            || matcher.kccgst.get(&RulesKccgst::Types).is_none() //types
            || matcher.kccgst.get(&RulesKccgst::Compat).is_none() //compat
            || matcher.kccgst.get(&RulesKccgst::Symbols).is_none()
            //symbols
            {
                log::error!(
                    "{:?}: No components returned from XKB rules {:?}",
                    XkbMessageCode::NoId,
                    path
                );

                if let Err(e) = result {
                    return Err(RulesCompileError::MatcherError(e));
                } else {
                    return Err(RulesCompileError::NoComponentsReturned(path));
                }
            }
        }

        let out = ComponentNames {
            keycodes: matcher
                .kccgst
                .get(&RulesKccgst::Keycodes)
                .cloned()
                .unwrap_or_else(|| "".into()),
            types: matcher
                .kccgst
                .get(&RulesKccgst::Types)
                .cloned()
                .unwrap_or_else(|| "".into()),
            compat: matcher
                .kccgst
                .get(&RulesKccgst::Compat)
                .cloned()
                .unwrap_or_else(|| "".into()),
            symbols: matcher
                .kccgst
                .get(&RulesKccgst::Symbols)
                .cloned()
                .unwrap_or_else(|| "".into()),
        };

        //TODO: logging

        Ok(out)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_lex_rmlvo() {
        crate::log_init!();
        let path: &'static str = "./test/data/rules/evdev";
        let string = std::fs::read_to_string(path).unwrap();
        for token in RulesToken::lexer(&string) {
            assert!(token.is_ok());
        }
    }
}

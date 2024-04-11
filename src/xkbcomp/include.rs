// Licenses from original include.c:
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
 * Copyright © 2012 Ran Benita <ran234@gmail.com>
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
use crate::context::Context;

use crate::errors::*;

use super::ast::{IncludeStmtPart, MergeMode, XkbFile, XkbFileType};

use logos::Logos;

const INCLUDE_MAX_DEPTH: u32 = 15;

impl Context {
    pub(crate) fn exceeds_include_max_depth(&self, include_depth: u32) -> bool {
        if include_depth >= INCLUDE_MAX_DEPTH {
            log::error!("Exceeded include depth threshold {}", INCLUDE_MAX_DEPTH);
            return true;
        } else {
            return false;
        }
    }
}
struct IncludeStmtPartBuilder {
    file: Option<String>,
    merge: MergeMode,
    map: Option<String>,      // should be an option
    modifier: Option<String>, // should be option
    invalid: bool,
}

impl IncludeStmtPartBuilder {
    fn new(merge: MergeMode) -> Self {
        Self {
            merge,
            file: None,
            map: None,
            modifier: None,
            invalid: false,
        }
    }

    fn build(self) -> Result<IncludeStmtPart, ParseIncludeMapError> {
        if self.invalid {
            return Err(ParseIncludeMapError::Invalid);
        }

        Ok(IncludeStmtPart {
            merge: self.merge,
            file: self.file.ok_or(ParseIncludeMapError::MapWithoutFile)?,
            map: self.map,
            modifier: self.modifier,
        })
    }
}

// TODO: determine whether A-Za-z0-9 is correct here
#[derive(Logos, Debug)]
#[logos(error = &'static str)]
enum IncludeStatementToken {
    #[regex(r"[\+\|]", |lex| lex.slice().parse().ok().map(|s: String| match s.as_str() {
        "|" => MergeMode::Augment, _ => MergeMode::Override,
    }), priority=1)]
    Merge(MergeMode),

    #[regex(r"\([^\+\|\(\)\:]+\)", |lex| lex.slice().parse().ok().map(|s: String| s[1..s.len()-1].into()), priority = 5)]
    Map(String),

    #[regex(r"\:[^\+\|\(\)]+", |lex| lex.slice().parse().ok().map(|s: String| s[1..].into()), priority = 4)]
    ExtraData(String),

    #[regex(r"[^\+\|\(\)\:]+", |lex| lex.slice().parse().ok(), priority = 3)]
    File(String),
}

fn parse_single_include(
    lexer: &mut std::iter::Peekable<logos::Lexer<IncludeStatementToken>>,
    merge_default: MergeMode,
) -> Option<Result<IncludeStmtPart, ParseIncludeMapError>> {
    use IncludeStatementToken::*;

    let mut builder = IncludeStmtPartBuilder::new(merge_default);
    // If no first token, return None
    let mut current_token = lexer.next()?;

    // process prepended merge modes
    if let Ok(Merge(merge)) = current_token {
        builder.merge = merge;
        current_token = lexer.next()?;
    }

    // process the file
    if let Ok(File(file)) = current_token {
        builder.file = Some(file);
    } else {
        // builder.build() will fail,
        // and this will be skipped
    }

    // process map and other data if available
    // check to ensure a new map was not started
    while let Some(token) = lexer.peek() {
        if let Ok(Merge(_)) = token {
            // a new include will be started
            // build and return the current one
            return Some(builder.build());
        }

        // get the next token
        current_token = lexer.next().unwrap();

        if let Ok(Map(map)) = current_token {
            if builder.map.is_none() {
                builder.map = Some(map);
            } else {
                return Some(Err(ParseIncludeMapError::MultipleMaps)); //can only have one map
            }
        } else if let Ok(ExtraData(extra_data)) = current_token {
            if builder.modifier.is_none() {
                builder.modifier = Some(extra_data);
            } else {
                return Some(Err(ParseIncludeMapError::MultipleExtraData)); // can only have one extra_data
            }
        } else {
            // Invalid token
            return Some(Err(ParseIncludeMapError::UnexpectedToken));
        }
    }

    // ran out of tokens
    Some(builder.build())
}
// Parse include statements.
// Returns a vector of Result<IncludeStmtPart,IncludeErr>,
// Each Ok(_) of which returns an IncludeStmtPart containing a file name,
// along with (possibly) a specific map in the file, and an explicit
// group designator.
pub(crate) fn parse_include_maps(
    str_inout: &str,
    merge_mode_begin: MergeMode,
) -> Vec<Result<IncludeStmtPart, ParseIncludeMapError>> {
    let mut stmts = vec![];

    let mut lexer = IncludeStatementToken::lexer(str_inout).peekable();

    while let Some(include) = parse_single_include(&mut lexer, merge_mode_begin) {
        stmts.push(include);
    }

    stmts
}

impl XkbFileType {
    fn include_dir(&self) -> &'static str {
        use XkbFileType::*;
        match self {
            Keycodes => "keycodes",
            Types => "types",
            Compat => "compat",
            Symbols => "symbols",
            Geometry => "geometry",
            Keymap => "keymap",
            Rules => "rules",
        }
    }

    fn directory_for_include(&self) -> &'static str {
        self.include_dir()
    }
}

impl Context {
    fn log_include_paths(&self) {
        if self.includes.len() > 0 {
            log::error!(
                "{:?}: {} include paths searched",
                XkbError::IncludedFileNotFound,
                self.includes.len()
            );

            for include in self.includes.iter() {
                log::error!("\t{:?}: {}", XkbError::IncludedFileNotFound, include);
            }
        } else {
            log::error!(
                "{:?}: There are no included paths to search",
                XkbError::IncludedFileNotFound
            );
        }
    }
}

use std::path::PathBuf;

impl Context {
    pub(super) fn find_file_in_xkb_path(
        &self,
        name: Option<&String>,
        file_type: XkbFileType,
        offset: &mut usize,
    ) -> Option<(PathBuf, std::fs::File)> {
        let name = name.cloned().unwrap_or_else(|| "".into());
        let type_dir = file_type.directory_for_include();

        for i in *offset..self.num_include_paths() {
            let include_path = self
                .include_path_get(i)
                .expect("Should have include path here");

            let buf = format!("{}/{}/{}", include_path, type_dir, name);

            if let Ok(metadata) = std::fs::metadata(&buf) {
                if metadata.is_file() {
                    if let Ok(file) = std::fs::File::open(buf.clone()) {
                        *offset = i;

                        return Some((buf.into(), file));
                    }
                }
            }
        }

        // Only print warnings if can't find file on the first lookup.
        if *offset == 0 {
            log::error!(
                "{:?}: Couldn't find file \"{}/{}\" in include paths",
                XkbError::IncludedFileNotFound,
                type_dir,
                name
            );

            self.log_include_paths();
        }

        None
    }
}


pub(super) fn process_include_file(
    ctx: &mut Context,
    stmt: &IncludeStmtPart,
    file_type: XkbFileType,
) -> Result<XkbFile, ProcessIncludeError> {
    let mut offset = 0;
    let (_, file) = ctx.find_file_in_xkb_path(Some(&stmt.file), file_type, &mut offset)
    .ok_or_else(|| ProcessIncludeError::NoSuchFile{ 
        path: stmt.file.clone().into(), file_type})?;

    let mut ret = None;
    let mut current_file = Some(file);

    while let Some(file) = current_file {
        let xkb_file =
            XkbFile::parse_file(ctx, file, &stmt.file, stmt.map.as_ref()
                .map(|s| s.as_str()))
                .map_err(|error| ProcessIncludeError::ParseFileFailed{path: stmt.file.clone().into(), error})?; 

        current_file = None;

        if let Some(xkb_file) = xkb_file {
            if xkb_file.file_type() != file_type {
                log::error!("{:?}: Include file of wrong type (expected {:?}, got {:?}); Include file \"{}\" ignored",
                XkbError::InvalidIncludedFile,
                file_type,
                xkb_file.file_type(),
                stmt.file);

                ret = None;
            } else {
                ret = Some(xkb_file);
                break;
            }
        }
        offset += 1;
        if let Some((_, file)) = ctx.find_file_in_xkb_path(Some(&stmt.file), file_type, &mut offset)
        {
            current_file = Some(file);
        }
    }
    // FIXME: we have to check recursive includes here (or somewhere)

    if let Some(xkb_file) = ret {
        return Ok(xkb_file);
    } else {
        if let Some(map) = stmt.map.as_ref() {
            log::error!(
                "{:?}: Couldn't process include statement for '{}({})'",
                XkbError::InvalidIncludedFile,
                stmt.file,
                map
            );
        } else {
            log::error!(
                "{:?}: Couldn't process include statement for '{}'",
                XkbError::InvalidIncludedFile,
                stmt.file
            );
        }

        return Err(ProcessIncludeError::InvalidIncludedFile(stmt.file.clone().into()));
    }
}

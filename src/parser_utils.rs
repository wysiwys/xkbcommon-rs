// This file includes some of the content from parser.y
// also includes parts of scanner.c

// license of parser.y:
/************************************************************
 Copyright (c) 1994 by Silicon Graphics Computer Systems, Inc.

 Permission to use, copy, modify, and distribute this
 software and its documentation for any purpose and without
 fee is hereby granted, provided that the above copyright
 notice appear in all copies and that both that copyright
 notice and this permission notice appear in supporting
 documentation, and that the name of Silicon Graphics not be
 used in advertising or publicity pertaining to distribution
 of the software without specific prior written permission.
 Silicon Graphics makes no representation about the suitability
 of this software for any purpose. It is provided "as is"
 without any express or implied warranty.

 SILICON GRAPHICS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SILICON
 GRAPHICS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
 THE USE OR PERFORMANCE OF THIS SOFTWARE.
// license of scanner.c
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
// additional license for rust_xkbcommon contributions:
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
use crate::errors::*;
use crate::keysyms::keysym_from_name;
use crate::xkbcomp::ast::*;

use crate::parser::XkbFileParser;

use xkeysym::Keysym;

use std::rc::Rc;

pub(crate) struct ParserParam<'p> {
    pub(super) ctx: &'p mut Context,
}

impl<'p> ParserParam<'p> {
    pub(crate) fn atom_intern(&mut self, ident: &str) -> Atom {
        self.ctx.atom_intern(ident)
    }
}

pub(crate) fn resolve_keysym(name: &str) -> Option<Keysym> {
    match name.to_lowercase().as_str() {
        "" => Some(xkeysym::NO_SYMBOL),
        "any" => Some(xkeysym::NO_SYMBOL),
        "nosymbol" => Some(xkeysym::NO_SYMBOL),
        "voidsymbol" => Some(Keysym::VoidSymbol),
        _ => keysym_from_name(name, 0),
    }
}

use thiserror::Error;

#[derive(Clone, Debug, Error)]
pub enum XkbFileParseError {
    #[error("Wrong input format")]
    WrongInputFormat,

    #[error("Could not read file {file:?} to string: {error:?}")]
    CouldNotReadToString {
        file: std::path::PathBuf,
        error: Rc<std::io::Error>,
    },

    // TODO: better error handling
    #[error("Parser encountered invalid token at location: {0:?}")]
    InvalidToken(()),

    #[error("Parser encountered unexpected token: {token} in range {span_begin:?}..{span_end:?}")]
    UnrecognizedToken {
        token: String,
        span_begin: Location,
        span_end: Location,
    },

    #[error("Parser encountered extra token: {token} in range {span_begin:?}..{span_end:?}")]
    ExtraToken {
        token: String,
        span_begin: Location,
        span_end: Location,
    },

    #[error("File ended early at location {0:?}")]
    UnrecognizedEof(Location),

    #[error("Expected: {0}")]
    User(&'static str),
}

type Location = ();
type Token = crate::lexer::Token;
type Expected = &'static str;
type ParseError = lalrpop_util::ParseError<Location, Token, Expected>;

impl From<ParseError> for XkbFileParseError {
    fn from(e: ParseError) -> Self {
        use lalrpop_util::ParseError::*;
        match e {
            InvalidToken { location: s } => Self::InvalidToken(s),
            UnrecognizedToken {
                token: (span_begin, token, span_end),
                ..
            } => Self::UnrecognizedToken {
                token: format!("{:?}", token),
                span_begin,
                span_end,
            },
            ExtraToken {
                token: (span_begin, token, span_end),
            } => Self::ExtraToken {
                token: format!("{:?}", token),
                span_begin,
                span_end,
            },
            UnrecognizedEof { location, .. } => Self::UnrecognizedEof(location),
            User { error } => Self::User(error),
        }
    }
}

impl XkbFile {
    // part from scanner.c
    pub(crate) fn parse_string(
        ctx: &mut Context,
        string: &str,
        file_name: &str,
        map: Option<&str>,
    ) -> Result<Option<XkbFile>, XkbFileParseError> {
        let mut parser_param = ParserParam { ctx };

        let mut lexer = crate::lexer::Lexer::new(string)
            .map_err(|e| {

                log::error!("This could be a file encoding issue. Supported encodings must be backward compatible with ASCII.");
                log::error!("E.g. ISO/CEI 8859 and UTF-8 are supported but UTF-16, UTF-32 and CP1026 are not.");
                e
            })?;

        let parser = XkbFileParser::new();

        // If we got a specific map, we look for it exclusively
        // and return immediately upon finding it.
        // Otherwise we need to get the default map.
        // If we find a map marked as default, we return it
        // immediately. If there are no maps marked as default,
        // we return the first map in the file.

        let mut first_file = None;

        while !lexer.is_empty() {
            lexer.reset();

            let xkb_file = match parser.parse(&mut parser_param, &mut lexer)? {
                Ok(file) => file,
                Err(_) => continue,
            };

            if let Some(map) = map {
                if xkb_file.name == map {
                    return Ok(Some(xkb_file));
                } else {
                    continue;
                }
            } else if xkb_file.flags.intersects(XkbMapFlags::MAP_IS_DEFAULT) {
                return Ok(Some(xkb_file));
            } else if first_file.is_none() {
                first_file = Some(xkb_file);
            }
        }

        if let Some(first) = first_file {
            log::warn!("{:?}: No map in include statement, but \"{}\" contains several; Using first defined map, \"{}\"",
                XkbWarning::MissingDefaultSection,
                file_name, first.name);

            return Ok(Some(first));
        }

        Ok(None)
    }

    // XkbParseFile in `scanner.c`
    pub(crate) fn parse_file(
        ctx: &mut Context,
        mut file: std::fs::File,
        file_name: &str,
        map: Option<&str>,
    ) -> Result<Option<Self>, XkbFileParseError> {
        use std::io::prelude::*;

        let mut string = String::new();
        if let Err(error) = file.read_to_string(&mut string) {
            log::error!(
                "{:?}: Couldn't read XKB file {}: {}",
                XkbMessageCode::NoId,
                file_name,
                error
            );

            return Err(XkbFileParseError::CouldNotReadToString {
                file: file_name.into(),
                error: Rc::new(error),
            });
        }

        Self::parse_string(ctx, &string, file_name, map)
    }
}

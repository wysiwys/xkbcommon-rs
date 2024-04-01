// This is some of the content from parser.y
// also includes parts of scanner.c

use std::sync::{Arc, Mutex};

use crate::atom::Atom;
use crate::errors::*;
use crate::xkbcomp::ast::*;
use crate::context::Context;
use crate::keysyms::keysym_from_name;

use crate::parser::XkbFilesParser;

use xkeysym::Keysym;

use crate::xkbcomp::ast::{Decl,VModDef};
pub(crate) struct DeclList {
    pub(crate) decl_list: Vec<Decl>,
    pub(crate) vmods: Vec<VModDef>
}

pub(crate) struct ParserParam<'p> {
    pub(super) ctx: &'p mut Context,
    pub(crate) rtrn: Option<XkbFile>,
    pub(crate) more_maps: bool,
    pub(crate) file_name: String
}

impl<'p> ParserParam<'p> {

    pub(crate) fn atom_intern(&mut self, ident: String) -> Atom {


        self.ctx.atom_intern(ident)


    }


}


pub(crate) fn resolve_keysym(name: &str) -> Option<Keysym> {

	match name.to_lowercase().as_str() {
		"" => Some(xkeysym::NO_SYMBOL),
		"any" => Some(xkeysym::NO_SYMBOL),
		"nosymbol" => Some(xkeysym::NO_SYMBOL),
		"voidsymbol" => Some(Keysym::VoidSymbol),
		_ => keysym_from_name(name, 0)

        }


}


pub(crate) fn parse_int(s: &str) -> Result<u32,()> {
	match s.parse::<u32>() {
		Ok(val) => Ok(val),
		Err(_) => Err(())
	}

}


pub(crate) fn parse_float(s: &str) -> Result<f64,()> {
	match s.parse::<f64>() {
		Ok(val) => Ok(val),
		Err(_) => Err(())
	}

}

impl XkbFile {

    pub(crate) fn parse_string(
        ctx: &mut Context,
        string: String,
        file_name: &str,
        map: Option<&str>,
    ) -> Result<Option<XkbFile>,KeymapErr> {

        let mut parser_param = ParserParam {
            ctx,
            rtrn: None,
            more_maps: false,
            file_name: file_name.into()
        };

        let lexer = crate::lexer::Lexer::new(&string);
        let parser = XkbFilesParser::new();

        // TODO:
        // If we got a specific map, we look for it exclusively
        // and return immediately upon finding it.
        // Otherwise we need to get the default map.
        // If we find a map marked as default, we return it
        // immediately. If there are no maps marked as default,
        // we return the first map in the file.
        
        let mut first_file = None;
        
        for xkb_file in parser.parse(&mut parser_param, lexer)
                                .map_err(|e| KeymapErr::ParseFailed(e))? {

            // TODO: this currently skips error'ed files. Is this correct behavior?
            if let Ok(xkb_file) = xkb_file {
                if let Some(map) = map {

                    if xkb_file.name == map {
                        return Ok(Some(xkb_file));
                    } else {
                        continue;
                    }
                }
                else {

                    if xkb_file.flags.intersects(XkbMapFlags::MAP_IS_DEFAULT) {
                        return Ok(Some(xkb_file));

                    } else if first_file.is_none() {
                        first_file = Some(xkb_file);
                    } 
                }

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

    pub(crate) fn parse_file(
        ctx: &mut Context,
        mut file: std::fs::File,
        file_name: &str,
        map: Option<&str>,
        ) -> Result<Option<Self>, KeymapErr> {

        use std::io::prelude::*;

        let mut string = String::new();
        if let Err(e) = file.read_to_string(&mut string) {

            todo!();
            return Err(todo!());
        }

        let parsed = Self::parse_string(ctx, string, file_name, map);
        if let Err(ref e) = parsed {
            log::debug!("{}: {:?}", file_name, e);
        }

        parsed


    }
}


use crate::errors::*;
use crate::context::Context;
use super::ast::{MergeMode,XkbFile,XkbFileType};
use crate::keymap::*;
use crate::rust_xkbcommon::*;

use std::sync::{Arc,Mutex};

pub(crate) use super::keymap::*;

pub(crate) struct ComponentNames {
    pub(crate) keycodes: String,
    pub(crate) types: String,
    pub(crate) compat: String,
    pub(crate) symbols: String

}

impl RuleNames {

    fn rules<'s>(&'s self) -> &'s str {

            self.rules.as_ref().map(|s| s.as_str()).unwrap_or_else(|| "")

    }
    fn model<'s>(&'s self) -> &'s str {

            self.model.as_ref().map(|s| s.as_str()).unwrap_or_else(|| "")

    }
    fn layout<'s>(&'s self) -> &'s str {

            self.layout.as_ref().map(|s| s.as_str()).unwrap_or_else(|| "")

    }
    fn variant<'s>(&'s self) -> &'s str {

            self.variant.as_ref().map(|s| s.as_str()).unwrap_or_else(|| "")

    }
    fn options<'s>(&'s self) -> &'s str {

            self.options.as_ref().map(|s| s.as_str()).unwrap_or_else(|| "")

    }

}

impl Context {

    pub(super) fn report_not_array(
        &self,
        _type: &str,
        field: &str,
        name: &str) -> XkbMessageCode {

        let err = XkbError::WrongFieldType;
        log::error!(
            "{:?}: The {:?} {} field is not an array;
            ignoring illegal assignment in {:?}",
            err,
            _type, field, name);

        return err.into();


    }

    pub(super) fn report_should_be_array(
        &self,
        _type: &str,
        field: &str,
        name: &str)
        -> Result<(),XkbMessageCode>
    {

        log::error!(
            "{:?}: Missing subscript for {:?} {:?};
            ignoring illegal assignment in {:?}",
            XkbError::ExpectedArrayEntry,
            _type, field, name);

        Err(XkbError::ExpectedArrayEntry.into())


    }
    
    pub(super) fn report_bad_type(
        &self,
        code: XkbMessageCode,
        _type: &str,
        field: &str,
        name: &str,
        wanted: &str)
    -> Result<(), XkbMessageCode> {

        log::error!(
            "{:?}: The {} {:?} field must be a {};
            ignoring illegal assignment in {:?}",
            code,
            _type, field, wanted, name);

        Err(code)


    }
    
    pub(super) fn report_bad_field(
        &self,
        _type: &str,
        field: &str,
        name: &str) {

        // TODO: return error?
        log::error!(
            "Unknown {:?} field {:?} in {:?};
            ignoring assignment to unknown field {:?}",
            
            _type, field, name, name);


    }

}

impl KeymapBuilder<TextV1> {

    /*
     * Tells whether formatting is possible
     */

    fn compile_keymap_file(&mut self, file: XkbFile) 
        -> Result<(), KeymapErr> {

        if file.file_type != XkbFileType::Keymap {
            log::error!("{:?}: Cannot compile a {:?} file alone into a keymap",
                XkbMessageCode::NoId, file.file_type);
            return Err(todo!());
        }
        
        if let Err(e) = self.compile_keymap(file, MergeMode::Override) {
            log::error!("{:?}: Failed to compile keymap", XkbMessageCode::NoId);
            return Err(e);
        }

        Ok(())



    }
    pub(crate) fn keymap_new_from_names(mut self, rmlvo: RuleNames) -> Result<Keymap,KeymapErr> {

        log::debug!("{:?}: Compiling from RMLVO: rules '{}', model '{}', layout '{}',
            variant '{}', options '{}'",
            XkbMessageCode::NoId,
            rmlvo.rules(),
            rmlvo.model(),
            rmlvo.layout(),
            rmlvo.variant(),
            rmlvo.options());

        let kccgst = match ComponentNames::from_rules(&mut self.context, &rmlvo) {
            Ok(kccgst) => kccgst,
            Err(e) => {

                log::error!("{:?}: Couldn't look up rules '{}', model '{}', layout '{}', variant '{}', options '{}'",
                    XkbMessageCode::NoId,
                    rmlvo.rules(),
                    rmlvo.model(),
                    rmlvo.layout(),
                    rmlvo.variant(),
                    rmlvo.options());

                return Err(e);
            }
        };

        log::debug!("{:?}: Compiling from KcCGST: keycodes '{}', types '{}', compat '{}', symbols '{}'",
            XkbMessageCode::NoId,
            kccgst.keycodes,
            kccgst.types,
            kccgst.compat,
            kccgst.symbols);

        let file = match XkbFile::from_components(&mut self.context, kccgst) {
            Ok(file) => file,
            Err(e) => {
                log::error!("{:?}: Failed to generate parsed XKB file from components",
                    XkbMessageCode::NoId);
                return Err(e);

            }};


        self.compile_keymap_file(file)?;

        let mut keymap = self.build()?;

        keymap.update_derived_keymap_fields()?;

        Ok(keymap)

    }

    pub(crate) fn keymap_new_from_string(mut self, string: String) -> Result<Keymap, KeymapErr> {

        let xkb_file 
            = XkbFile::parse_string(
                &mut self.context, 
                string, 
                "(input string)",
                None
                )?;

        let xkb_file = match xkb_file {
            Some(xkb_file) => xkb_file, None => return Err(KeymapErr::NoMapFound) };

        self.compile_keymap_file(xkb_file)?;

        let mut keymap = self.build()?;

        keymap.update_derived_keymap_fields()?;

        Ok(keymap)
       
    }
    pub(crate) fn keymap_new_from_file(mut self, file: std::fs::File) -> Result<Keymap, KeymapErr> {

        let xkb_file = match XkbFile::parse_file(
            &mut self.context, file,
            "(unknown file)", None)? {
            Some(file) => file,
            None => {
                log::error!("{:?}: Failed to parse input xkb file", XkbMessageCode::NoId);
                return Err(todo!());

            }
        };
        
        self.compile_keymap_file(xkb_file)?;

        let mut keymap = self.build()?;

        keymap.update_derived_keymap_fields()?;

        Ok(keymap)
    }
}


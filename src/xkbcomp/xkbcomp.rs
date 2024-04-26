use super::ast::{MergeMode, XkbFile, XkbFileType};
use crate::context::Context;
use crate::errors::*;
use crate::keymap::*;
use crate::rust_xkbcommon::*;

pub(crate) struct ComponentNames {
    pub(crate) keycodes: String,
    pub(crate) types: String,
    pub(crate) compat: String,
    pub(crate) symbols: String,
}

impl RuleNames {
    fn rules(&self) -> &str {
        self.rules.as_deref().unwrap_or("")
    }
    fn model(&self) -> &str {
        self.model.as_deref().unwrap_or("")
    }
    fn layout(&self) -> &str {
        self.layout.as_deref().unwrap_or("")
    }
    fn variant(&self) -> &str {
        self.variant.as_deref().unwrap_or("")
    }
    fn options(&self) -> &str {
        self.options.as_deref().unwrap_or("")
    }
}

impl Context {
    pub(super) fn report_not_array(&self, _type: &str, field: &str, name: &str) -> ReportedError {
        let err = XkbError::WrongFieldType;
        log::error!(
            "{:?}: The {:?} {} field is not an array;
            ignoring illegal assignment in {:?}",
            err,
            _type,
            field,
            name
        );

        ReportedError::NotArray {
            name: name.into(),
            _type: _type.into(),
            field: field.into(),
        }
    }

    pub(super) fn report_should_be_array(
        &self,
        _type: &str,
        field: &str,
        name: &str,
    ) -> ReportedError {
        log::error!(
            "{:?}: Missing subscript for {:?} {:?};
            ignoring illegal assignment in {:?}",
            XkbError::ExpectedArrayEntry,
            _type,
            field,
            name
        );

        ReportedError::ShouldBeArray {
            name: name.into(),
            _type: _type.into(),
            field: field.into(),
        }
    }

    pub(super) fn report_bad_type(
        &self,
        code: XkbMessageCode,
        _type: &str,
        field: &str,
        name: &str,
        wanted: &str,
    ) -> ReportedError {
        log::error!(
            "{:?}: The {} {:?} field must be a {};
            ignoring illegal assignment in {:?}",
            code,
            _type,
            field,
            wanted,
            name
        );

        ReportedError::BadType {
            name: name.into(),
            _type: _type.into(),
            field: field.into(),
            wanted: wanted.into(),
        }
    }

    pub(super) fn report_bad_field(&self, _type: &str, field: &str, name: &str) -> ReportedError {
        log::error!(
            "Unknown {:?} field {:?} in {:?};
            ignoring assignment to unknown field {:?}",
            _type,
            field,
            name,
            name
        );

        ReportedError::BadField {
            name: name.into(),
            _type: _type.into(),
            field: field.into(),
        }
    }
}

impl KeymapBuilder<TextV1> {
    /*
     * Tells whether formatting is possible
     */

    fn compile_keymap_file(&mut self, file: XkbFile) -> Result<(), KeymapCompileError> {
        if file.file_type != XkbFileType::Keymap {
            log::error!(
                "{:?}: Cannot compile a {:?} file alone into a keymap",
                XkbMessageCode::NoId,
                file.file_type
            );
            return Err(KeymapCompileError::OnlyPartialKeymap(file.file_type));
        }

        if let Err(e) = self.compile_keymap(file, MergeMode::Override) {
            log::error!("{:?}: Failed to compile keymap", XkbMessageCode::NoId);
            return Err(e);
        }

        Ok(())
    }
    pub(crate) fn keymap_new_from_names(
        mut self,
        rmlvo: RuleNames,
    ) -> Result<Keymap, KeymapCompileError> {
        log::debug!(
            "{:?}: Compiling from RMLVO: rules '{}', model '{}', layout '{}',
            variant '{}', options '{}'",
            XkbMessageCode::NoId,
            rmlvo.rules(),
            rmlvo.model(),
            rmlvo.layout(),
            rmlvo.variant(),
            rmlvo.options()
        );

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

                return Err(e.into());
            }
        };

        log::debug!(
            "{:?}: Compiling from KcCGST: keycodes '{}', types '{}', compat '{}', symbols '{}'",
            XkbMessageCode::NoId,
            kccgst.keycodes,
            kccgst.types,
            kccgst.compat,
            kccgst.symbols
        );

        let file = XkbFile::from_components(&mut self.context, kccgst);

        // Removed error handling because above function cannot fail
        // This is because the `calloc` failure check is not reproduced in the Rust code.
        /*
        err => {
            log::error!(
                "{:?}: Failed to generate parsed XKB file from components",
                XkbMessageCode::NoId
            );
            return Err(err);
        }
        */

        self.compile_keymap_file(file)?;

        let mut keymap = self.build();

        keymap.update_derived_keymap_fields();
        Ok(keymap)
    }

    pub(crate) fn keymap_new_from_string(
        mut self,
        string: &str,
    ) -> Result<Keymap, KeymapCompileError> {
        let xkb_file = XkbFile::parse_string(&mut self.context, string, "(input string)", None)
            .map_err(|error| KeymapCompileError::CouldNotParseString { error })?;

        let xkb_file = xkb_file.ok_or_else(|| KeymapCompileError::NoMapFoundForString)?;

        self.compile_keymap_file(xkb_file)?;

        let mut keymap = self.build();

        keymap.update_derived_keymap_fields();

        Ok(keymap)
    }
    pub(crate) fn keymap_new_from_file(
        mut self,
        file: std::fs::File,
    ) -> Result<Keymap, KeymapCompileError> {
        let xkb_file = XkbFile::parse_file(&mut self.context, file, "(unknown file)", None)
            .map_err(|error| KeymapCompileError::CouldNotParseFile { error })?
            .ok_or_else(|| {
                log::error!("{:?}: Failed to parse input xkb file", XkbMessageCode::NoId);

                KeymapCompileError::NoMapFoundForFile
            })?;

        self.compile_keymap_file(xkb_file)?;

        let mut keymap = self.build();

        keymap.update_derived_keymap_fields();

        Ok(keymap)
    }
}

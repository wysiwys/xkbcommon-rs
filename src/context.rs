// combination of context.h, context.c and context-priv.c
// context.h
/*
 * Copyright © 2012 Intel Corporation
 * Copyright © 2012 Ran Benita
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

use crate::atom::*;
use crate::config::*;
use crate::errors::*;
use crate::rust_xkbcommon::{ContextFlags, RuleNames};
use crate::utils::*;

pub mod errors {
    use thiserror::Error;

    #[derive(Debug, Error)]
    pub enum IncludePathResetDefaultsError {
        #[error("Default include paths could not be appended, but previous paths were cleared")]
        AllDefaultsFailed,
    }

    #[derive(Debug, Error)]
    pub enum IncludePathAppendError {
        #[error("The provided directory was not found: {path}")]
        DirectoryNotFound { path: String, error: std::io::Error },

        #[error("The provided file is not a directory: {0}")]
        IsNotDirectory(String),

        #[error("Do not have R_OK | X_OK eacces for {0}")]
        NoEaccesForFile(String),

        #[error("All default includes failed")]
        AllDefaultsFailed,
    }
}
use errors::*;

#[derive(Clone)]
pub struct Context {
    log_verbosity: i32,
    pub(crate) includes: Vec<String>,
    failed_includes: Vec<String>,

    atom_table: AtomTable,

    use_environment_names: bool,
    use_secure_getenv: bool,
}

#[allow(dead_code)]
impl Context {
    /// Create a new context
    pub fn new<T>(flags: T) -> Result<Self, Box<dyn std::error::Error>>
    where
        T: Into<ContextFlags>,
    {
        // convert to flags, unsetting any unknown bits
        let context_flags = flags.into();

        let mut context = Self {
            log_verbosity: 0,
            use_environment_names: !context_flags.intersects(ContextFlags::NO_ENVIRONMENT_NAMES),
            // TODO: find a way to implement secure_getenv
            use_secure_getenv: false,
            // !context_flags.intersects(ContextFlags::NO_SECURE_GETENV),
            atom_table: AtomTable::new(),

            includes: vec![],
            failed_includes: vec![],
        };

        if !context_flags.intersects(ContextFlags::NO_DEFAULT_INCLUDES)
            && context.include_path_append_default().is_err()
        {
            // TODO: ensure these paths are correct
            log::error!(
                "{:?}: failed to add default include path {}",
                XkbMessageCode::NoId,
                DFLT_XKB_CONFIG_ROOT
            );
        }

        Ok(context)
    }

    // Corresponds to `xkb_context_getenv`
    pub(crate) fn getenv(&self, name: &str) -> Option<String> {
        if self.use_secure_getenv {
            todo!()
        } else {
            std::env::var(name).ok()
        }
    }

    fn num_failed_include_paths(&self) -> usize {
        self.failed_includes.len()
    }

    fn failed_include_path_get(&self, idx: usize) -> Option<&str> {
        self.failed_includes.get(idx).map(|s| s.as_str())
    }

    pub(crate) fn atom_lookup(&self, string: &str) -> Option<Atom> {
        // This is done by accessing the table directly
        self.atom_table.atom_lookup(string)
    }

    pub(crate) fn atom_intern(&mut self, string: &str) -> Atom {
        self.atom_table.intern(string)
    }

    /// Version that returns Option
    pub(crate) fn atom_text(&self, atom: Atom) -> Option<&str> {
        self.atom_table.get(atom)
    }
    pub(crate) fn xkb_atom_text(&self, atom: Atom) -> &str {
        self.atom_table.get(atom).unwrap_or("")
    }

    fn get_default_rules(&self) -> String {
        self.use_environment_names
            .then(|| self.getenv("XKB_DEFAULT_RULES"))
            .flatten()
            .unwrap_or(DEFAULT_XKB_RULES.into())
    }

    fn get_default_model(&self) -> String {
        self.use_environment_names
            .then(|| self.getenv("XKB_DEFAULT_MODEL"))
            .flatten()
            .unwrap_or(DEFAULT_XKB_MODEL.into())
    }
    fn get_default_layout(&self) -> String {
        self.use_environment_names
            .then(|| self.getenv("XKB_DEFAULT_LAYOUT"))
            .flatten()
            .unwrap_or(DEFAULT_XKB_LAYOUT.into())
    }

    fn get_default_variant(&self) -> String {
        let mut env = None;
        let layout = self.getenv("XKB_DEFAULT_LAYOUT");

        // We don't want to inherit the variant if they haven't
        // also set a layout, since they're so closely
        // paired.
        if layout.is_some() && self.use_environment_names {
            env = self.getenv("XKB_DEFAULT_VARIANT");
        }

        env.unwrap_or(DEFAULT_XKB_VARIANT.into())
    }
    fn get_default_options(&self) -> String {
        self.use_environment_names
            .then(|| self.getenv("XKB_DEFAULT_OPTIONS"))
            .flatten()
            .unwrap_or(DEFAULT_XKB_OPTIONS.into())
    }

    pub(crate) fn sanitize_rule_names(&self, rmlvo: &mut RuleNames) {
        if rmlvo.rules == Some("".into()) || rmlvo.rules.is_none() {
            rmlvo.rules = Some(self.get_default_rules());
        }

        if rmlvo.model == Some("".into()) || rmlvo.model.is_none() {
            rmlvo.model = Some(self.get_default_model());
        }

        // Layout and variant are tied together,
        // so don't try to use one from the caller
        // and one from the environment.
        if rmlvo.layout == Some("".into()) || rmlvo.layout.is_none() {
            rmlvo.layout = Some(self.get_default_layout());
            if rmlvo.variant.is_some() {
                let variant = self.get_default_variant();
                log::warn!("{:?}: Layout not provided, but variant set to \"{}\": ignoring variant and using defaults for both: layout=\"{}\", variant=\"{}\".",
                    XkbMessageCode::NoId,
                    rmlvo.variant.as_ref().unwrap(),
                    rmlvo.layout.as_ref().unwrap(),
                    variant
                );
            }
            rmlvo.variant = Some(self.get_default_variant());
        }
        // Options can be empty,
        // so respect that if passed in
        if rmlvo.options.is_none() || rmlvo.options == Some("".into()) {
            rmlvo.options = Some(self.get_default_options());
        }
    }

    pub fn include_path_append(&mut self, path: &str) -> Result<(), IncludePathAppendError> {
        use nix::unistd::AccessFlags;

        use std::fs;

        let err: Result<(), IncludePathAppendError> = {
            let metadata =
                fs::metadata(path).map_err(|e| IncludePathAppendError::DirectoryNotFound {
                    path: path.into(),
                    error: e,
                })?;

            if !metadata.is_dir() {
                return Err(IncludePathAppendError::IsNotDirectory(path.into()));
            }

            // `check_eacces`
            let mode = AccessFlags::R_OK | AccessFlags::X_OK;
            if !check_permissions(&metadata, mode) {
                return Err(IncludePathAppendError::NoEaccesForFile(path.into()));
            }

            Ok(())
        };

        if let Err(err) = err {
            self.failed_includes.push(path.into());
            log::debug!(
                "{:?}: Include path failed: {} ({})",
                XkbMessageCode::NoId,
                path,
                err
            );

            Err(err)
        } else {
            self.includes.push(path.into());
            log::debug!("{:?}: Include path added: {}", XkbMessageCode::NoId, path);
            Ok(())
        }
    }

    pub(crate) fn include_path_get_extra_path(&self) -> String {
        self.getenv("XKB_CONFIG_EXTRA_PATH")
            .unwrap_or_else(|| DFLT_XKB_CONFIG_EXTRA_PATH.to_string())
    }

    pub(crate) fn include_path_get_system_path(&self) -> String {
        self.getenv("XKB_CONFIG_ROOT")
            .unwrap_or_else(|| DFLT_XKB_CONFIG_ROOT.to_string())
    }

    /// Append the default include directories to the context.
    pub fn include_path_append_default(&mut self) -> Result<(), IncludePathAppendError> {
        let mut success = false;

        let home = self.getenv("HOME");
        if let Some(xdg) = self.getenv("XDG_CONFIG_HOME") {
            let user_path = format!("{}/xkb", xdg);

            if self.include_path_append(&user_path).is_ok() {
                success = true;
            }
        } else if let Some(ref home) = home {
            // XDG_CONFIG_HOME fallback is $HOME/.config/
            let user_path = format!("{}/.config/xkb", home);

            if self.include_path_append(&user_path).is_ok() {
                success = true;
            }
        }

        if let Some(home) = home {
            let user_path = format!("{}/.xkb", home);
            if self.include_path_append(&user_path).is_ok() {
                success = true;
            }
        }

        let extra = self.include_path_get_extra_path();

        if self.include_path_append(&extra).is_ok() {
            success = true;
        }

        let root = self.include_path_get_system_path();

        if self.include_path_append(&root).is_ok() {
            success = true;
        }

        if success {
            Ok(())
        } else {
            Err(IncludePathAppendError::AllDefaultsFailed)
        }
    }

    /// Remove all entries in the context's include path
    pub fn include_path_clear(&mut self) {
        self.includes.clear();
        self.failed_includes.clear();
    }

    /// `include_path_clear()` + `include_path_append_default()`
    pub fn include_path_reset_defaults(&mut self) -> Result<(), IncludePathResetDefaultsError> {
        self.include_path_clear();
        self.include_path_append_default()
            .map_err(|_| IncludePathResetDefaultsError::AllDefaultsFailed)
    }

    /// Returns the number of entries in the context's include path.
    pub fn num_include_paths(&self) -> usize {
        self.includes.len()
    }

    /// Returns the given entry in the context's include path,
    /// or None if an invalid index is passed.
    pub fn include_path_get(&self, idx: usize) -> Option<&String> {
        self.includes.get(idx)
    }

    pub fn get_log_verbosity(&self) -> i32 {
        self.log_verbosity
    }
    pub fn set_log_verbosity(&mut self, verbosity: i32) {
        self.log_verbosity = verbosity;
    }
}

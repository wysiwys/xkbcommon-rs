
// combination of context.c and context-priv.c


// #include "atom.h"
// #include "messages-codes.h"

use crate::atom::*;
use crate::config::*;
use crate::utils::*;
use crate::errors::*;
use crate::rust_xkbcommon::{
    RuleNames,
    ContextFlags,
};

use std::env;

#[derive(Debug)]
pub enum ContextError {

    Nix(nix::Error),
    Std(std::io::Error)

}

#[derive(Clone)]
pub struct Context {

    //refcnt: i32,
    log_verbosity: i32,
    //user_data: *void,

    //names_dflt: XkbRuleNames,

    // TODO: data type
    pub(crate) includes: Vec<String>,
    failed_includes: Vec<String>,

    atom_table: AtomTable,

    //x11_atom_cache: Option<AtomCache>,

    //text_buffer: String, //TODO: capacity 2048?
    //text_next: usize,

    // TODO: data type
    use_environment_names: bool,
    use_secure_getenv: bool,

}

impl Context {


    /// Create a new context
    pub fn new<T>(flags: T)
        -> Result<Self, ContextError> 
        where T: Into<ContextFlags>
    {
        // TODO: take int as argument
        // TODO: logging

        // convert to flags, unsetting any unknown bits
        let context_flags = flags.into();

        let mut context = Self {
            
            log_verbosity: 0,
            use_environment_names: 
                !context_flags.intersects(ContextFlags::NO_ENVIRONMENT_NAMES),
            use_secure_getenv: false,
                // !context_flags.intersects(ContextFlags::NO_SECURE_GETENV),
           
            atom_table: AtomTable::new(),

            includes: vec![],
            failed_includes: vec![],

            // Unimplemented members

            //x11_atom_cache: None,
            //names_dflt: todo!(),
            //text_buffer: todo!(),
            //text_next: todo!(),

        };

        if !context_flags.intersects(ContextFlags::NO_DEFAULT_INCLUDES) {

            if let Err(e) = context.include_path_append_default() {

                // TODO: ensure these paths are correct
                log::error!("{:?}: failed to add default include path {}",
                    XkbMessageCode::NoId,
                    DFLT_XKB_CONFIG_ROOT);


            }


        }


        Ok(context)

        


    }

    pub(crate) fn getenv(
        &self,
        name: &str) -> Option<String> {

        if self.use_secure_getenv {
            todo!()
        } else {
            if name == "HOME" {
                match std::env::home_dir() {
                    Some(p) => p.as_os_str().to_str().map(|p| p.to_string()),
                    None => None }
            } else {
                std::env::var(name).ok()
            }
        }
    }

    fn num_failed_include_paths(&self) -> usize {
        self.failed_includes.len()
    }

    fn failed_include_path_get<'s>(
        &'s self, idx: usize)
        -> Option<&'s str> {

            self.failed_includes.get(idx).map(|s| s.as_str())

    }


    pub(crate) fn atom_lookup(&self, string: &str) -> Option<Atom> {

        // This is done by accessing the table directly
        self.atom_table.atom_lookup(string)

    }
    
    pub(crate) fn atom_intern(
        &mut self,
        string: String
    ) -> Atom {

        self.atom_table.intern(string)

    }

    pub(crate) fn xkb_atom_text<'a>(
        &'a self,
        atom: Atom) -> Option<&'a str> {

        self.atom_table.get(atom)

    }

    fn get_default_rules(&self) 
        -> String {

            if self.use_environment_names {
                match self.getenv("XKB_DEFAULT_RULES") {
                    Some(rules) => rules,
                    None => DEFAULT_XKB_RULES.into()
                }
            } else {
                DEFAULT_XKB_RULES.into()
            }

    }

    fn get_default_model(&self)
        -> String {

            if self.use_environment_names {
                match self.getenv("XKB_DEFAULT_MODEL") {
                    Some(rules) => rules,
                    None => DEFAULT_XKB_MODEL.into()
                }
            } else {
                DEFAULT_XKB_MODEL.into()
            }

    }
    fn get_default_layout(&self)
        -> String {

            if self.use_environment_names {
                match self.getenv("XKB_DEFAULT_LAYOUT") {
                    Some(rules) => rules,
                    None => DEFAULT_XKB_LAYOUT.into()
                }
            } else {
                DEFAULT_XKB_LAYOUT.into()
            }

    }

    fn get_default_variant(&self)
        -> String {

            let mut env = None;
            let layout = self.getenv("XKB_DEFAULT_LAYOUT");

            // We don't want to inherit the variant if they haven't
            // also set a layout, since they're so closely
            // paired.
            if layout.is_some() && self.use_environment_names {
                env = self.getenv("XKB_DEFAULT_VARIANT");

            }

            match env {
                Some(env) => env,
                None => DEFAULT_XKB_VARIANT.into()
            }

    }
    fn get_default_options(&self)
        -> String {

            if self.use_environment_names {
                match self.getenv("XKB_DEFAULT_OPTIONS") {
                    Some(rules) => rules,
                    None => DEFAULT_XKB_OPTIONS.into()
                }
            } else {
                DEFAULT_XKB_OPTIONS.into()
            }

    }

    pub(crate) fn sanitize_rule_names(
        &self,
        rmlvo: &mut RuleNames) {

        if rmlvo.rules == Some("".into()) 
            || rmlvo.rules.is_none() {

                rmlvo.rules = Some(self.get_default_rules());

        }
        
        if rmlvo.model == Some("".into()) 
            || rmlvo.model.is_none() {

                rmlvo.model = Some(self.get_default_model());

        }

        // Layout and variant are tied together,
        // so don't try to use one from the caller 
        // and one from the environment.
        if rmlvo.layout == Some("".into()) 
            || rmlvo.layout.is_none() {

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
        if rmlvo.options == Some("".into()) 
            || rmlvo.options.is_none() {

            rmlvo.options = Some(self.get_default_options());
        }


    }



    pub fn include_path_append(
        &mut self,
        path: &str
    ) -> Result<(),ContextError>{


        use nix::Error;
        use nix::unistd::AccessFlags;

        let mut err = ContextError::Nix(Error::ENOMEM);

        use std::fs;
        // TODO: append one directory 
        // to the context's include path

        if let Ok(metadata) = fs::metadata(path.clone()) {

            if !metadata.is_dir() {

                err = ContextError::Nix(Error::ENOTDIR);


            } else {

                let mode = AccessFlags::R_OK |
                    AccessFlags::X_OK;
                if !check_permissions(&metadata, mode.bits()) {

                    err = ContextError::Nix(Error::EACCES);
                } else {

                    self.includes.push(path.into());
                    log::debug!("{:?}: Include path added: {}", XkbMessageCode::NoId, path);

                    return Ok(());

                }


            }


        } else {
            err = ContextError::Std(
                std::io::Error::last_os_error());
        }

 
        self.failed_includes.push(path.into());
        log::debug!("{:?}: Include path failed: {} ({:?})", XkbMessageCode::NoId, path, err);

        return Err(err);

    }

    pub(crate) fn include_path_get_extra_path(
        &self) -> String { 
        
        let extra = self.getenv("XKB_CONFIG_EXTRA_PATH");
        match extra {
            Some(extra) => extra,
            None => DFLT_XKB_CONFIG_EXTRA_PATH.to_string()
        }

    }

    pub(crate) fn include_path_get_system_path(&self)
        -> String {

        
        let root = self.getenv("XKB_CONFIG_ROOT");
        match root {
            Some(root) => root,
            None => DFLT_XKB_CONFIG_ROOT.to_string()
        }

    }

    pub fn include_path_append_default(&mut self)
    -> Result<(), ContextError> {

        let mut success = false;

        let home = self.getenv("HOME");
        if let Some(xdg) = self.getenv("XDG_CONFIG_HOME") {
            let user_path = format!("{}/xkb", xdg);
            
            if self.include_path_append(&user_path).is_ok() {
                success = true;
            }
                

        } else if let Some(ref home) = home {
            // XDG_CONFIG_HOME fallback is $HOME/.config/
            let user_path = format!("{}/.config/xkb",
                home);

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
            Err(todo!())
        }
    }

    /// Remove all entries in the context's include path
    pub fn include_path_clear(&mut self) {
   
        self.includes.clear();
        self.failed_includes.clear();
    }

    /// `include_path_clear()` + `include_path_append_default()`
    pub fn include_path_reset_defaults(&mut self)
    -> Result<(), ContextError> {

        self.include_path_clear();
        self.include_path_append_default()

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


    pub fn get_log_verbosity(
        &self) -> i32 {
        self.log_verbosity
    }
    pub fn set_log_verbosity(
        &mut self, verbosity: i32) {
        self.log_verbosity = verbosity;
    }

}



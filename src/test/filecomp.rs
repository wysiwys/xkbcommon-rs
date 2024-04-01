
use crate::log_init;
use crate::test::*;
use crate::rust_xkbcommon::*;


use crate::context::Context;
use crate::errors::*;
use crate::keymap::Keymap;

use std::sync::{Arc, Mutex};


use evdev::Key;


// from filecomp.c
fn test_file(ctx: Context, path_rel: &str)
-> Result<Context, TestErr> {

    // return the context to reuse it

    match test_compile_file(ctx, path_rel) {
        Ok(keymap) => Ok(keymap.context),
        Err(e) => Err((e))
    }

}

// from filecomp.c
#[test]
fn filecomp_test_files() {

    log_init!();

    let mut context = test_get_context(TestContextFlags::empty()).unwrap();

    context = test_file(context, "keymaps/basic.xkb").unwrap();


    context = test_file(context, "keymaps/comprehensive-plus-geom.xkb").unwrap();
    
    context = test_file(context, "keymaps/quartz.xkb").unwrap();
    

    context = test_file(context, "keymaps/no-types.xkb").unwrap();
    context = test_file(context, "keymaps/no-aliases.xkb").unwrap();
    context = test_file(context, "keymaps/modmap-none.xkb").unwrap();

    context = test_file(context, "keymaps/invalid-escape-sequence.xkb").unwrap();


    assert!(test_file(context.clone(), "keymaps/divide-by-zero.xkb").is_err());
    assert!(test_file(context.clone(), "keymaps/bad.xkb").is_err());
    assert!(test_file(context.clone(), "keymaps/syntax-error.xkb").is_err());
    assert!(test_file(context.clone(), "keymaps/syntax-error2.xkb").is_err());
    assert!(test_file(context.clone(), "does not exist").is_err());


    // Test response to invalid flags and formats
    // TODO: pass a null file?
    // TODO: these tests would seemingly only work with a BufReader on stdin
    // TODO: these tests would seemingly only work with a BufReader on stdin, 
    // but the function currently takes a File.

}

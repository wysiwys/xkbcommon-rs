// based on test/filecomp.c
/*
 * Copyright © 2009 Dan Nicholson
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

use crate::log_init;
use crate::test::*;

use crate::context::Context;
use crate::errors::*;

// from filecomp.c
fn test_file(ctx: Context, path_rel: &str) -> Result<Context, TestErr> {
    // return the context to reuse it

    match test_compile_file(ctx, path_rel) {
        Ok(keymap) => Ok(keymap.context),
        Err(e) => Err(e),
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
    assert!(test_file(context.clone(), "keymaps/empty-symbol-decl.xkb").is_err());
    assert!(test_file(context.clone(), "keymaps/invalid-qualified-type-field.xkb").is_err());
    assert!(test_file(
        context.clone(),
        "keymaps/invalid-qualified-symbols-field.xkb"
    )
    .is_err());
    assert!(test_file(context.clone(), "does not exist").is_err());

    // Test response to invalid flags and formats
    // TODO: pass a null file?
    // TODO: these tests would seemingly only work with a BufReader on stdin
    // but the function currently takes a File.
}

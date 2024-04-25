/* License of `rules-file-includes.c`:
 * --------------------------------------------------
 * Copyright © 2012 Ran Benita <ran234@gmail.com>
 * Copyright © 2019 Red Hat, Inc.
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
 * --------------------------------------------------
 */

use crate::log_init;
use crate::test::*;

use std::env;

use super::rules_file::*;

#[test]
fn test_rules_file_includes() {
    log_init!();

    env::set_var("XKB_CONFIG_ROOT", "./test/data");

    let mut ctx = test_get_context(TestContextFlags::empty()).unwrap();

    let test1 = TestData {
        rules: "inc-src-simple",
        model: "my_model",
        layout: "my_layout",
        variant: "",
        options: "",

        keycodes: "my_keycodes",
        types: "default_types",
        compat: "default_compat",
        symbols: "my_symbols",

        should_fail: false,
    };

    assert!(test_rules(&mut ctx, test1));

    let test2 = TestData {
        rules: "inc-src-nested",
        model: "my_model",
        layout: "my_layout",
        variant: "",
        options: "",

        keycodes: "my_keycodes",
        types: "default_types",
        compat: "default_compat",
        symbols: "my_symbols",

        should_fail: false,
    };
    assert!(test_rules(&mut ctx, test2));

    let test3 = TestData {
        rules: "inc-src-looped",
        model: "my_model",
        layout: "my_layout",
        variant: "",
        options: "",

        keycodes: "N/A",
        types: "N/A",
        compat: "N/A",
        symbols: "N/A",

        should_fail: true,
    };

    assert!(test_rules(&mut ctx, test3));

    let test4 = TestData {
        rules: "inc-src-before-after",
        model: "before_model",
        layout: "my_layout",
        variant: "",
        options: "",

        keycodes: "my_keycodes",
        types: "default_types",
        compat: "default_compat",
        symbols: "default_symbols",

        should_fail: false,
    };
    assert!(test_rules(&mut ctx, test4));

    let test5 = TestData {
        rules: "inc-src-options",
        model: "my_model",
        layout: "my_layout",
        variant: "my_variant",
        options: "option11,my_option,colon:opt,option111",

        keycodes: "my_keycodes",
        types: "default_types",
        compat: "default_compat+substring+group(bla)|some:compat",
        symbols: "my_symbols+extra_variant+altwin(menu)",

        should_fail: false,
    };

    assert!(test_rules(&mut ctx, test5));

    let test6 = TestData {
        rules: "inc-src-loop-twice",
        model: "my_model",
        layout: "my_layout",
        variant: "",
        options: "",

        keycodes: "my_keycodes",
        types: "default_types",
        compat: "default_compat",
        symbols: "my_symbols",

        should_fail: false,
    };

    assert!(test_rules(&mut ctx, test6));

    let test7 = TestData {
        rules: "inc-no-newline",
        model: "",
        layout: "",
        variant: "",
        options: "",

        keycodes: "",
        types: "",
        compat: "",
        symbols: "",

        should_fail: true,
    };

    assert!(test_rules(&mut ctx, test7));
}

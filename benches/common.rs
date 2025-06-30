// based on test/common.c
/*
 * Copyright © 2009 Dan Nicholson <dbn.lists@gmail.com>
 * Copyright © 2012 Intel Corporation
 * Copyright © 2012 Ran Benita <ran234@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the names of the authors or their
 * institutions shall not be used in advertising or otherwise to promote the
 * sale, use or other dealings in this Software without prior written
 * authorization from the authors.
 *
 * Author: Dan Nicholson <dbn.lists@gmail.com>
 *         Daniel Stone <daniel@fooishbar.org>
 *         Ran Benita <ran234@gmail.com>
 */
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

use xkbcommon_rs::{
    error::{context::*, keymap::*},
    keycode::*,
    xkb_context::*,
};

#[cfg(feature = "server")]
use xkbcommon_rs::{keysym::*, xkb_state::*};

use xkeysym::Keysym;

#[cfg(feature = "server")]
use std::path::PathBuf;

#[cfg(test)]
#[derive(Debug)]
pub enum TestErr {
    CouldNotOpenFile,
    Keymap(KeymapCompileError),
    NoSymsForKeycode(Keycode),
    WrongKeysym { expected: Keysym, got: Keysym },
}

#[cfg(feature = "server")]
const EVDEV_OFFSET: u32 = 8;

#[derive(Debug, PartialEq)]
#[cfg(feature = "server")]
pub(crate) enum KeySeqState {
    Down,
    Repeat,
    Up,
    Both,
}

bitflags::bitflags! {

    pub(crate) struct TestContextFlags: u8 {

        const NO_FLAG = 0;
        const ALLOW_ENVIRONMENT_NAMES = (1 << 0);

    }

}

#[cfg(feature = "server")]
pub(crate) fn test_key_seq(
    keymap: &Keymap,
    interactions: Vec<(evdev::Key, KeySeqState, Keysym)>,
) -> Result<(), TestErr> {
    // TODO: make the third tuple argument a list,
    // so can test for case with more than one sym

    let mut state = State::new(keymap.clone());
    eprintln!("----");

    for (evdev_key, op, provided_keysym) in interactions {
        let kc = Keycode::new(u32::from(evdev_key.0) + EVDEV_OFFSET);
        let mut syms = state.key_get_syms(kc);
        if syms.len() == 1 {
            // with uppercase transform
            syms = vec![state.key_get_one_sym(kc).unwrap()];
        }

        use KeySeqState::*;
        if [Down, Both].contains(&op) {
            state.update_key(kc, KeyDirection::Down);
        }
        if [Up, Both].contains(&op) {
            state.update_key(kc, KeyDirection::Up);
        }

        eprint!(
            "op {:?} got {} syms for keycode {}: [",
            op,
            syms.len(),
            kc.raw()
        );

        if let Some(sym) = syms.get(0) {
            let keysym = provided_keysym;

            // TODO: restrict to 64 chars
            let ksbuf = keysym_get_name(sym).unwrap();
            eprint!("{}", ksbuf);

            if keysym != *sym {
                return Err(TestErr::WrongKeysym {
                    expected: keysym,
                    got: *sym,
                });
            }
        }
        if syms.len() == 0 {
            let keysym: Keysym = provided_keysym;
            if keysym != xkeysym::NO_SYMBOL {
                //let ksbuf = keysym_get_name(&keysym);
                return Err(TestErr::NoSymsForKeycode(kc));
            }
        }

        eprint!("]\n");
    }

    Ok(())
}

#[cfg(feature = "server")]
fn test_makedir(parent: &str, path: &str) -> String {
    let dirname = format!("{}/{}", parent, path);

    std::fs::create_dir(&dirname).unwrap();

    dirname
}

#[cfg(feature = "server")]
pub(crate) fn test_maketempdir(template: &str) -> PathBuf {
    // TODO: add Win32 case?

    let tmpdir = std::env::temp_dir().join(template);

    std::fs::create_dir(&tmpdir).unwrap();

    tmpdir
}

fn test_get_path(path_rel: &str) -> String {
    use std::env;

    let srcdir = env::var("top_srcdir").unwrap_or_else(|_| ".".into());

    if let Some(first_char) = path_rel.chars().next() {
        if first_char == '/' {
            return path_rel.to_owned();
        }
    }
    let path = format!(
        "{}/test/data{}{}",
        srcdir,
        match !path_rel.is_empty() {
            true => "/",
            false => "",
        },
        path_rel
    );

    path
}

#[cfg(feature = "server")]
pub(crate) fn test_read_file(path_rel: &str) -> Option<String> {
    let path = test_get_path(path_rel);

    let data: String = match std::fs::read_to_string(path) {
        Ok(data) => data,
        _ => return None,
    };

    Some(data)
}

pub(crate) fn test_get_context(
    test_flags: TestContextFlags,
) -> Result<Context, IncludePathAppendError> {
    use std::env;

    let mut ctx_flags = ContextFlags::NO_DEFAULT_INCLUDES;

    if test_flags.intersects(TestContextFlags::ALLOW_ENVIRONMENT_NAMES) {
        env::remove_var("XKB_DEFAULT_RULES");
        env::remove_var("XKB_DEFAULT_MODEL");
        env::remove_var("XKB_DEFAULT_LAYOUT");
        env::remove_var("XKB_DEFAULT_VARIANT");
        env::remove_var("XKB_DEFAULT_OPTIONS");
    } else {
        ctx_flags |= ContextFlags::NO_ENVIRONMENT_NAMES;
    }

    let mut ctx = Context::new(ctx_flags).unwrap();

    let path = test_get_path("");

    ctx.include_path_append(&path)?;

    Ok(ctx)
}

#[cfg(feature = "server")]
pub(crate) fn test_compile_file(context: Context, path: &str) -> Result<Keymap, TestErr> {
    let path = test_get_path(path);

    let file = match std::fs::File::open(&path) {
        Ok(file) => file,
        _ => {
            eprintln!("Failed to open path {}", path);
            return Err(TestErr::CouldNotOpenFile);
        }
    };

    let keymap = Keymap::new_from_file(context, file, KeymapFormat::TextV1, CompileFlags::empty());
    match keymap {
        Err(e) => {
            eprintln!("{:?}: Failed to compile path: {}", e, &path);
            Err(TestErr::Keymap(e))
        }
        Ok(keymap) => {
            eprintln!("Successfully compiled path: {}", &path);
            Ok(keymap)
        }
    }
}

#[cfg(feature = "server")]
pub(crate) fn test_compile_string(context: Context, string: String) -> Option<Keymap> {
    // TODO: don't pass in context this way

    let keymap = Keymap::new_from_string(context, &string, KeymapFormat::TextV1, 0);

    if keymap.is_err() {
        eprintln!("Failed to compile string");
        return None;
    }

    keymap.ok()
}

#[cfg(feature = "server")]
pub(crate) fn test_compile_rules(
    context: Context,
    rules: Option<&str>,
    model: Option<&str>,
    layout: Option<&str>,
    variant: Option<&str>,
    options: Option<&str>,
) -> Result<Keymap, KeymapCompileError> {
    let rmlvo = RuleNames {
        rules: rules.map(str::to_string),
        model: model.map(str::to_string),
        layout: layout.map(str::to_string),
        variant: variant.map(str::to_string),
        options: options.map(str::to_string),
    };

    let keymap=
    // TODO: check for "" ?
    if rules.is_none()
        && model.is_none()
        && layout.is_none()
        && variant.is_none()
        && options.is_none()
    {
        Keymap::new_from_names(context, None, 0u32)
    } else {
        Keymap::new_from_names(context, Some(rmlvo), 0)
    };

    if keymap.is_err() {
        eprintln!(
            "Failed to compile RMLVO: '{:?}', '{:?}', '{:?}', '{:?}', '{:?}'",
            rules, model, layout, variant, options
        );
    }

    keymap
}

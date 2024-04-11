/* Original license of `context.c`:
 * -----------------------------------------
 * Copyright © 2012 Intel Corporation
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
 * --------------------------------------------
 */

use crate::log_init;
use crate::rust_xkbcommon::*;
use crate::test::*;

use crate::config::*;

use crate::context::Context;

use std::env;
use std::path::*;


struct Env {
    key: String,
    value: Option<String>,
}

struct Environment {
    envs: Vec<Env>,
}

impl Environment {
    // corresponds to buffer_env
    fn add_env(&mut self, key: &str) {
        let v = env::var(key);

        let e = Env {
            key: key.into(),
            value: v.ok(),
        };

        self.envs.push(e);
    }

    fn restore_env(&mut self) {
        // removes in reverse order
        while let Some(var) = self.envs.pop() {
            if let Some(value) = var.value {
                env::set_var(var.key, value);
            } else {
                env::remove_var(var.key)
            }
        }
    }
}

struct Directories {
    dirs: Vec<PathBuf>,
}

impl Directories {
    fn make_dir(&mut self, parent: &str, path: &str) -> PathBuf {
        let dirname = test_makedir(parent, path);
        self.dirs.push(dirname.clone().into());

        dirname.into()
    }

    fn make_tmp_dir(&mut self) -> PathBuf {
        let tmpdir = test_maketempdir("xkbcommon-test.XXXXXX");
        self.dirs.push(tmpdir.clone());

        tmpdir
    }

    fn unmake_dirs(&mut self) {
        // reverse order
        while let Some(dir) = self.dirs.pop() {
            std::fs::remove_dir_all(dir).unwrap();
        }
    }
}

fn test_config_root_include_path(env: &mut Environment, dirs: &mut Directories) {
    env.add_env("XKB_CONFIG_ROOT");
    env.add_env("HOME");
    env.add_env("XDG_CONFIG_HOME");

    let tmpdir = dirs.make_tmp_dir();

    env::set_var("XKB_CONFIG_ROOT", &tmpdir);
    env::remove_var("HOME");
    env::remove_var("XDG_CONFIG_HOME");

    let ctx = Context::new(ContextFlags::empty()).unwrap();

    let nincludes = ctx.num_include_paths();

    assert!(nincludes >= 1);

    let context_path = ctx.include_path_get(nincludes - 1);

    assert!(context_path.map(|s| s.as_str()) == tmpdir.as_os_str().to_str());

    dirs.unmake_dirs();
    env.restore_env();
}

fn test_config_root_include_path_fallback(env: &mut Environment, dirs: &mut Directories) {
    let xkbdir = DFLT_XKB_CONFIG_ROOT;

    // check the dir exists
    if !Path::new(xkbdir).is_dir() {
        return;
    }

    env.add_env("XKB_CONFIG_ROOT");
    env.add_env("HOME");
    env.add_env("XDG_CONFIG_HOME");

    env::remove_var("XKB_CONFIG_ROOT");
    env::remove_var("HOME");
    env::remove_var("XDG_CONFIG_HOME");

    let ctx = Context::new(ContextFlags::empty()).unwrap();

    let nincludes = ctx.num_include_paths();

    assert!(nincludes >= 1);

    let context_path = ctx.include_path_get(nincludes - 1);

    assert!(context_path.map(|s| s.as_str()) == Some(xkbdir));

    dirs.unmake_dirs();
    env.restore_env();
}

fn test_xkbdir_include_path(env: &mut Environment, dirs: &mut Directories) {
    env.add_env("HOME");
    env.add_env("XDG_CONFIG_HOME");

    let tmpdir = dirs.make_tmp_dir();
    let xkb_path = dirs.make_dir(tmpdir.as_os_str().to_str().unwrap(), ".xkb");

    env::set_var("HOME", &tmpdir);
    env::set_var("XDG_CONFIG_HOME", tmpdir);

    let ctx = Context::new(ContextFlags::empty()).unwrap();
    assert!(ctx.num_include_paths() >= 1);

    let context_path = ctx.include_path_get(0).map(|p| p.into());

    assert_eq!(context_path, Some(xkb_path));

    dirs.unmake_dirs();
    env.restore_env();
}

fn test_xdg_include_path(env: &mut Environment, dirs: &mut Directories) {
    env.add_env("XDG_CONFIG_HOME");

    let tmpdir = dirs.make_tmp_dir();
    let xdg_path = dirs.make_dir(tmpdir.as_os_str().to_str().unwrap(), "xkb");
    env::set_var("XDG_CONFIG_HOME", tmpdir);

    let ctx = Context::new(ContextFlags::empty()).unwrap();
    assert!(ctx.num_include_paths() >= 1);

    let context_path = ctx.include_path_get(0).map(|p| p.into());

    assert_eq!(context_path, Some(xdg_path));

    dirs.unmake_dirs();
    env.restore_env();
}

fn test_xdg_include_path_fallback(env: &mut Environment, dirs: &mut Directories) {
    env.add_env("XDG_CONFIG_HOME");
    env.add_env("HOME");

    let tmpdir = dirs.make_tmp_dir();
    let xdg_root = dirs.make_dir(tmpdir.as_os_str().to_str().unwrap(), ".config");
    let xdg_path = dirs.make_dir(xdg_root.as_os_str().to_str().unwrap(), "xkb");

    env::set_var("HOME", tmpdir);
    env::remove_var("XDG_CONFIG_HOME");

    let ctx = Context::new(ContextFlags::empty()).unwrap();
    assert!(ctx.num_include_paths() >= 1);

    let context_path = ctx.include_path_get(0).map(|p| p.into());

    assert_eq!(context_path, Some(xdg_path));

    dirs.unmake_dirs();
    env.restore_env();
}

fn test_include_order(env: &mut Environment, dirs: &mut Directories) {
    env.add_env("XKB_CONFIG_ROOT");
    env.add_env("XDG_CONFIG_HOME");
    env.add_env("HOME");

    let tmpdir = dirs.make_tmp_dir();
    let xdg_path = dirs.make_dir(tmpdir.as_os_str().to_str().unwrap(), "xkb");
    let xkb_home_path = dirs.make_dir(tmpdir.as_os_str().to_str().unwrap(), ".xkb");
    let xkb_root_path = dirs.make_dir(tmpdir.as_os_str().to_str().unwrap(), "xkbroot");

    env::set_var("HOME", &tmpdir);
    env::set_var("XDG_CONFIG_HOME", &tmpdir);
    env::set_var("XKB_CONFIG_ROOT", &xkb_root_path);

    let ctx = Context::new(ContextFlags::empty()).unwrap();
    assert!(ctx.num_include_paths() >= 3);

    // XDG is first
    let context_path = ctx.include_path_get(0);
    assert_eq!(context_path.map(|p| p.into()), Some(xdg_path));

    // $HOME/.xkb is second
    let context_path = ctx.include_path_get(1);
    assert_eq!(context_path.map(|p| p.into()), Some(xkb_home_path));

    // CONFIG_ROOT is last
    let context_path = ctx.include_path_get(2);
    assert_eq!(context_path.map(|p| p.into()), Some(xkb_root_path));

    dirs.unmake_dirs();
    env.restore_env();
}

#[test]
fn test_context() {
    log_init!();

    let mut context = test_get_context(TestContextFlags::empty()).unwrap();

    assert_eq!(context.num_include_paths(), 1);

    context.include_path_append("¡NONSENSE!").unwrap_err();

    assert_eq!(context.num_include_paths(), 1);

    // TODO: in the original, `atom_intern` also takes the length as parameter.

    let atom = context.atom_intern("HELLOjunkjunkjunk".into());

    assert!(atom != 0);

    assert_eq!(
        context.xkb_atom_text(atom),
        Some("HELLOjunkjunkjunk".into())
    );

    let mut env = Environment { envs: vec![] };
    let mut dirs = Directories { dirs: vec![] };

    test_config_root_include_path(&mut env, &mut dirs);
    test_config_root_include_path_fallback(&mut env, &mut dirs);
    test_xkbdir_include_path(&mut env, &mut dirs);
    test_xdg_include_path(&mut env, &mut dirs);
    test_xdg_include_path_fallback(&mut env, &mut dirs);
    test_include_order(&mut env, &mut dirs);
}

use crate::context::*;
use crate::rust_xkbcommon::*;

use crate::keymap::*;
use crate::state::State;

use crate::keysyms::*;

use crate::errors::*;

use std::path::PathBuf;

const EVDEV_OFFSET: u32 = 8;

#[derive(Debug, PartialEq)]
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

pub(crate) fn test_key_seq(
    keymap: &Keymap,
    interactions: Vec<(evdev::Key, KeySeqState, Keysym)>,
) -> Result<(), TestErr> {
    // TODO: make the third tuple argument a list,
    // so can test for case with more than one sym

    let mut state = State::new(keymap);
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

pub(super) fn test_makedir(parent: &str, path: &str) -> String {
    let dirname = format!("{}/{}", parent, path);

    std::fs::create_dir(&dirname).unwrap();

    dirname
}

pub(super) fn test_maketempdir(template: &str) -> PathBuf {
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
        match path_rel.len() > 0 {
            true => "/",
            false => "",
        },
        path_rel
    );

    return path;
}

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
            return Err(TestErr::Keymap(e));
        }
        Ok(keymap) => {
            eprintln!("Successfully compiled path: {}", &path);
            return Ok(keymap);
        }
    }
}
pub(crate) fn test_compile_string(context: Context, string: String) -> Option<Keymap> {
    // TODO: don't pass in context this way

    let keymap = Keymap::new_from_string(context, string, KeymapFormat::TextV1, 0);

    if keymap.is_err() {
        eprintln!("Failed to compile string");
        return None;
    }

    keymap.ok()
}

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

    let keymap;
    // TODO: check for "" ?
    if rules.is_none()
        && model.is_none()
        && layout.is_none()
        && variant.is_none()
        && options.is_none()
    {
        keymap = Keymap::new_from_names(context, None, 0u32);
    } else {
        keymap = Keymap::new_from_names(context, Some(rmlvo), 0);
    }

    if keymap.is_err() {
        eprintln!(
            "Failed to compile RMLVO: '{:?}', '{:?}', '{:?}', '{:?}', '{:?}'",
            rules, model, layout, variant, options
        );
    }

    keymap
}

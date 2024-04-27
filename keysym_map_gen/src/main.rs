// - Rewrote to create a name-to-keysym lookup
/* Copyright 2022-2023 John Nunley
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

//! A generator that takes the system keysyms and compiles them into
//! a list of Rust constants.
//!
//! For some reason, the official source of truth for X11 keysyms are
//! the "keysymdef" files distributed in the X11 package. Parsing
//! these files typically involves using regexes to extract the values
//! and converting to the more important values. Why this source
//! isn't in a more conventionally parsed format, like JSON or even
//! XML, is a mystery.
//!
//! I challenged myself to write a parser that preforms most of its
//! processing in a single iterator combinator. I partially succeeded,
//! but overall I lean a little too much on the last `for_each()`.

use anyhow::Result;
use regex::Regex;
use std::{
    env,
    fmt::Write as _,
    fs,
    io::{prelude::*, BufReader, BufWriter},
    path::Path,
};

fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    // get the list of files to process
    let prefix = Path::new("/usr/include/X11/");
    let files = [
        "keysymdef.h",
        "XF86keysym.h",
        "Sunkeysym.h",
        "DECkeysym.h",
        "HPkeysym.h",
    ];

    // open the output file
    let outpath = env::args_os().nth(1).expect("output file name");
    let mut outfile = BufWriter::new(fs::File::create(outpath)?);

    write!(
        outfile,
        r#"
// This file was automatically generated using keysym_map_gen.
/* Copyright 2022-2023 John Nunley
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
 * THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 */
use xkeysym::Keysym;
"#
    )?;

    // we're looking for lines of the following form:
    // #define {some prefix}XK_{some key name} {some key code}
    // the keycode may be wrapped in _EVDEVK(*), wchich means we have to
    // add to it
    //
    // the keycode will always be in hexadecimal (0x2F) form
    let line_matcher = Regex::new(r#"^#define\s+([a-zA-Z0-9_]*XK_[A-Za-z0-9_]+)\s+(.*)"#)?;
    let evdevk = Regex::new(r#"_EVDEVK\(0x(.*)\)"#)?;
    let hex_matcher = Regex::new(r#"0x([0-9a-fA-F]+)"#)?;

    // open the file and process the lines
    let mut data = files
        .iter()
        .map(|fname| prefix.join(fname))
        .inspect(|path| tracing::info!("Opening {:?}", path))
        .map(fs::File::open)
        .filter_map(|res| match res {
            Ok(f) => Some(BufReader::new(f)),
            Err(e) => {
                tracing::error!("Unable to open file: {:?}", e);
                None
            }
        })
        .flat_map(|file| file.lines())
        .filter_map(|line| match line {
            Ok(line) => Some(line),
            Err(e) => {
                tracing::error!("Unable to read line: {:?}", e);
                None
            }
        })
        .filter_map(|line| {
            let captures = line_matcher.captures(&line)?;
            match (captures.get(1), captures.get(2)) {
                (Some(name), Some(value)) => {
                    Some((name.as_str().to_string(), value.as_str().to_string()))
                }
                _ => None,
            }
        })
        .flat_map(|(name, value)| {
            // if the value is wrapped in _EVDEVK(*), unwrap it
            let (is_evdevk, hex_value) = if let Some(captures) = evdevk.captures(&value) {
                (true, captures.get(1))
            } else {
                (
                    false,
                    hex_matcher.captures(&value).and_then(|caps| caps.get(1)),
                )
            };

            if let Some(hex_value) = hex_value {
                let hex_value = hex_value.as_str();

                // compute the value of the keysym
                let mut hex_value = u64::from_str_radix(hex_value, 16).ok()?;
                if is_evdevk {
                    hex_value += 0x10081000;
                }

                // there is a duplicate symbol somewhere in here
                if name.contains("Ydiaeresis") && hex_value == 0x100000ee {
                    return None;
                }

                // split apart the symbol at the end
                let mut parts = name.split("XK_");
                let prefix = parts.next().unwrap();
                let symbol = parts.next().unwrap();

                let needs_underscore = symbol.starts_with(|c: char| c.is_ascii_digit());

                let keysym_name = format!(
                    "{}{}{}",
                    heck::AsShoutySnakeCase(prefix),
                    if prefix.is_empty() && !needs_underscore {
                        ""
                    } else {
                        "_"
                    },
                    &symbol
                );

                let keysym_str = format!("{}{}", prefix, &symbol);

                return Some((hex_value, keysym_str, keysym_name));
            }

            None
        })
        .collect::<Vec<_>>();

    let mut name_to_keysym_icase = data.clone();
    name_to_keysym_icase.sort_by(|a, b| {
        let a_lower = a.1.to_lowercase();
        let b_lower = b.1.to_lowercase();

        match (a_lower, b_lower) {
            (p, q) if p < q => std::cmp::Ordering::Less,
            (p, q) if p > q => std::cmp::Ordering::Greater,
            _ => a.1.partial_cmp(&b.1).unwrap(),
        }
    });

    // sort data by hex value
    // TODO: ensure this sorts by raw keysym ascending
    data.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

    // The phf_map
    let mut keysym_dump = format!(
        "
use phf::phf_ordered_map;
#[allow(unreachable_patterns)]
pub(crate) static KEYSYM_TO_NAME: [(Keysym, &str); {}] = [
        (xkeysym::NO_SYMBOL, \"NoSymbol\"),\n",
        data.len() + 1
    );
    data.iter().for_each(|(_, keysym_str, keysym_name)| {
        // Write a match entry for it.
        writeln!(
            keysym_dump,
            "        (Keysym::{}, \"{}\"),",
            &keysym_name, &keysym_str
        )
        .unwrap();
    });
    // Write out the keysym dump.
    keysym_dump.push_str(
        "
];",
    );

    let mut name_to_keysym = "
#[allow(unreachable_patterns)]
pub(crate) static NAME_TO_KEYSYM: phf::OrderedMap<&'static str, Keysym> = phf_ordered_map! {
        \"NoSymbol\" => Keysym::NoSymbol,\n"
        .to_string();

    name_to_keysym_icase
        .iter()
        .for_each(|(_, keysym_str, keysym_name)| {
            // Write a match entry for it.
            writeln!(
                name_to_keysym,
                "        \"{}\" => Keysym::{},",
                &keysym_str, &keysym_name
            )
            .unwrap();
        });
    // Write out the keysym dump.
    name_to_keysym.push_str(
        "
};",
    );

    writeln!(outfile, "{name_to_keysym}\n{keysym_dump}",)?;

    Ok(())
}

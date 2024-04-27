// Includes content from atom.h (not atom.c as implementation is different)
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

pub type Atom = usize;

use indexmap::IndexSet;

#[derive(Clone)]
pub(crate) struct AtomTable {
    table: IndexSet<String>,
}

impl AtomTable {
    pub(crate) fn new() -> Self {
        Self {
            table: IndexSet::new(),
        }
    }

    pub(crate) fn atom_lookup(&self, string: &str) -> Option<Atom> {
        self.table.get_full(string).map(|(atom, _)| atom)
    }

    pub(crate) fn intern(&mut self, string: &str) -> Atom {
        if let Some((existing_atom, _)) = self.table.get_full(string) {
            return existing_atom;
        }

        let (atom, _) = self.table.insert_full(string.to_string());

        atom
    }

    pub(crate) fn get(&self, atom: Atom) -> Option<&str> {
        self.table.get_index(atom).map(|s| s.as_str())
    }
}

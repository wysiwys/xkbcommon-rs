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

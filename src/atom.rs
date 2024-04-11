pub type Atom = usize;

// TODO: use an insert-only linear probing hash table,
// if such a crate exists,
// or look up an efficient data structure for this use case.
// Alternatively, the original implementation in `context.c` can be used

#[derive(Clone)]
pub(crate) struct AtomTable {
    // TODO: don't duplicate data storage
    data: Vec<String>,
    table: std::collections::HashMap<String, Atom>,
}

impl AtomTable {
    pub(crate) fn new() -> Self {
        Self {
            data: vec![],
            table: std::collections::HashMap::new(),
        }
    }

    pub(crate) fn atom_lookup(&self, string: &str) -> Option<Atom> {
        self.table.get(string).map(|a| a + 1)
    }

    pub(crate) fn intern(&mut self, string: String) -> Atom {
        // TODO: do this more efficiently
        // follow original implementation

        if let Some(existing_atom) = self.table.get(string.as_str()) {
            return *existing_atom + 1;
        }

        self.data.push(string.clone());
        let index = self.data.len() - 1;
        self.table.insert(string, index);

        index + 1
    }

    pub(crate) fn get<'a>(&'a self, atom: Atom) -> Option<&'a str> {
        if atom == 0 {
            return None;
        }

        self.data.get(atom - 1).map(|s| s.as_str())
    }
}

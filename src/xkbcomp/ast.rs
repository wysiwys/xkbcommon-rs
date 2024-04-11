use super::include::parse_include_maps;
use crate::atom::*;
use crate::context::Context;
use crate::errors::*;
use crate::xkbcomp::xkbcomp::ComponentNames;

use xkeysym::Keysym;

// this file combines ast and ast-build (all .c and .h files)

bitflags::bitflags! {

    pub struct XkbMapFlags: u8 {
        const MAP_IS_DEFAULT = (1 << 0);
        const MAP_IS_PARTIAL = (1 << 1);
        const MAP_IS_HIDDEN = (1 << 2);
        const MAP_HAS_ALPHANUMERIC = (1 << 3);
        const MAP_HAS_MODIFIER = (1 << 4);
        const MAP_HAS_KEYPAD = (1 << 5);
        const MAP_HAS_FN = (1 << 6);
        const MAP_IS_ALTGR = (1 << 7);

    }

}
#[repr(usize)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub enum XkbFileType {
    Keycodes = 0, // FIRST_KEYMAP_FILE_TYPE
    Types = 1,
    Compat = 2,
    Symbols = 3, // LAST_KEYMAP_FILE_TYPE
    Geometry = 4,
    Keymap,
    Rules,
}

impl XkbFileType {
    pub(crate) fn first_type() -> XkbFileType {
        XkbFileType::Keycodes
    }
    pub(crate) fn last_type() -> XkbFileType {
        XkbFileType::Symbols
    }

    pub(crate) fn iter_possible() -> impl Iterator<Item = XkbFileType> {
        let start = Self::first_type() as usize;
        let end = Self::last_type() as usize + 1;

        (start..end).map(|s| Self::try_from(s).unwrap())
    }
}

impl TryFrom<usize> for XkbFileType {
    type Error = ();
    fn try_from(u: usize) -> Result<Self, Self::Error> {
        use XkbFileType::*;
        match u {
            0 => Ok(Keycodes),
            1 => Ok(Types),
            2 => Ok(Compat),
            3 => Ok(Symbols),
            4 => Ok(Geometry),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum ExprOpType {
    Value,
    Ident,
    ActionDecl,
    FieldRef,
    ArrayRef,
    KeysymList,
    ActionList,
    Add,
    Subtract,
    Multiply,
    Divide,
    Assign,
    Not,
    Negate,
    Invert,
    UnaryPlus,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum MergeMode {
    Default,
    Augment,
    Override,
    Replace,
}
impl Default for MergeMode {
    fn default() -> Self {
        MergeMode::Default
    }
}

#[derive(Debug)]
pub(crate) struct IncludeStmtPart {
    pub(crate) merge: MergeMode,
    pub(crate) file: String,
    pub(crate) map: Option<String>,
    pub(crate) modifier: Option<String>,
}

// iterate this instead of linked list
pub(crate) struct IncludeStmt {
    //common: ParseCommon,
    pub(super) stmt: String,
    pub(super) merge: MergeMode, // TODO: this is redundant and will match the first merge mode in
    // the list
    pub(super) maps: Vec<IncludeStmtPart>,
}

impl IncludeStmt {
    pub(crate) fn iter_stmts(&self) -> impl Iterator<Item = &IncludeStmtPart> {
        self.maps.iter()
    }
}

impl IncludeStmt {
    pub(crate) fn create(
        _: &Context,
        string: &str,
        merge: MergeMode,
    ) -> Self {
        let stmt = string.to_owned();

        let maps = parse_include_maps(string, merge)
            .into_iter()
            .map(|result| {
                if result.is_err() {
                    log::error!("Illegal include statement {:?}; Ignored", stmt);
                }
                result
            })
            .filter_map(|result| result.ok())
            .collect::<Vec<IncludeStmtPart>>();

        Self { merge, maps, stmt }
    }
}

// TODO: don't return `Decl` variants from `create`
#[allow(dead_code)]
pub(crate) enum Decl {
    //XkbFile(XkbFile),
    Include(IncludeStmt),
    Var(VarDef),
    VMod(VModDef),
    Interp(InterpDef),
    KeyName(KeycodeDef),
    KeyAlias(KeyAliasDef),
    KeyType(KeyTypeDef),
    Symbols(SymbolsDef),
    ModMap(ModMapDef),
    GroupCompat(GroupCompatDef),
    LedMap(LedMapDef),
    LedName(LedNameDef),
    ShapeDecl,
    SectionDecl,
    DoodadDecl,
    Skipped, //e.g. for missing keysyms
}

impl Decl {
    pub(super) fn stmt_type(&self) -> &'static str {
        use Decl::*;
        match self {
            Include(_) => "include",
            Var(_) => "var",
            VMod(_) => "vmod",
            Interp(_) => "interp",
            KeyName(_) => "keyname",
            KeyAlias(_) => "key_alias",
            KeyType(_) => "key_type",
            Symbols(_) => "symbols",
            ModMap(_) => "mod_map",
            GroupCompat(_) => "group_compat",
            LedMap(_) => "led_map",
            LedName(_) => "led_name",
            ShapeDecl => "shape_decl",
            SectionDecl => "section_decl",
            DoodadDecl => "doodad_decl",
            Skipped => "skipped", //e.g. for missing keysyms
        }
    }
}

pub(crate) enum ExprDef {
    Ident(ExprIdent),     //Unknown
    String(ExprString),   //String
    Boolean(ExprBoolean), //Boolean
    Integer(ExprInteger), //Int
    KeyName(ExprKeyName), //Keyname
    Binary(ExprBinary),   //Unknown
    Unary(ExprUnary),     //it depends
    FieldRef(ExprFieldRef),
    ArrayRef(ExprArrayRef),
    Action(ExprAction),
    Actions(ExprActionList),
    KeysymList(ExprKeysymList),
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) enum ExprValueType {
    Unknown = 0,
    Boolean,
    Int,
    Float,
    String,
    Action,
    Actions,
    Keyname,
    Symbols,
}

impl ExprDef {

    pub(crate) fn op_type(&self) -> ExprOpType {
        use ExprDef::*;
        match self {
            Ident(e) => e.op,
            String(e) => e.op,
            Boolean(e) => e.op,
            Integer(e) => e.op,
            KeyName(e) => e.op,
            Binary(e) => e.op,
            Unary(e) => e.op,
            FieldRef(e) => e.op,
            ArrayRef(e) => e.op,
            Action(e) => e.op,
            Actions(e) => e.op,
            KeysymList(e) => e.op,
        }
    }
    pub(crate) fn value_type(&self) -> ExprValueType {
        use ExprDef::*;
        match self {
            Ident(e) => e.value_type,
            String(e) => e.value_type,
            Boolean(e) => e.value_type,
            Integer(e) => e.value_type,
            KeyName(e) => e.value_type,
            Binary(e) => e.value_type,
            Unary(e) => e.value_type,
            FieldRef(e) => e.value_type,
            ArrayRef(e) => e.value_type,
            Action(e) => e.value_type,
            Actions(e) => e.value_type,
            KeysymList(e) => e.value_type,
        }
    }
}

pub(crate) struct ExprIdent {
    pub(super) op: ExprOpType,
    pub(super) value_type: ExprValueType,
    pub(super) ident: Atom,
}

impl ExprIdent {
    pub(crate) fn create(ident: Atom) -> Result<ExprDef, AstError> {
        let op = ExprOpType::Ident;
        let value_type = ExprValueType::Unknown;

        Ok(ExprDef::Ident(Self {
            op,
            value_type,
            ident,
        }))
    }
}

pub(crate) struct ExprString {
    pub(super) op: ExprOpType,
    pub(super) value_type: ExprValueType,
    pub(super) str: Atom,
}

impl ExprString {
    pub(crate) fn create(str: Atom) -> Result<ExprDef, AstError> {
        let op = ExprOpType::Value;
        let value_type = ExprValueType::String; //?

        Ok(ExprDef::String(Self {
            op,
            value_type,
            str,
        }))
    }
}

pub(crate) struct ExprBoolean {
    op: ExprOpType,
    value_type: ExprValueType,
    pub(crate) set: bool,
}
impl ExprBoolean {
    pub(super) const fn new_true() -> Self {
        let op = ExprOpType::Value;
        let value_type = ExprValueType::Boolean; //?

        Self {
            op,
            value_type,
            set: true,
        }
    }
    pub(super) const fn new_false() -> Self {
        let op = ExprOpType::Value;
        let value_type = ExprValueType::Boolean; //?

        Self {
            op,
            value_type,
            set: false,
        }
    }

    pub(crate) fn create(set: bool) -> Result<ExprDef, AstError> {
        let op = ExprOpType::Value;
        let value_type = ExprValueType::Boolean; //?

        Ok(ExprDef::Boolean(Self {
            op,
            value_type,
            set,
        }))
    }
}

pub(crate) struct ExprInteger {
    pub(super) op: ExprOpType,
    pub(super) value_type: ExprValueType,
    pub(crate) ival: i64,
}
impl ExprInteger {
    pub(crate) fn create(ival: i64) -> Result<ExprDef, AstError> {
        let op = ExprOpType::Value;
        let value_type = ExprValueType::Int; //?

        Ok(ExprDef::Integer(Self {
            op,
            value_type,
            ival,
        }))
    }
}

pub(crate) struct ExprFloat {
    _op: ExprOpType,
    _value_type: ExprValueType,
    // floats not supported;
    // this is just here for providing error messages
}
impl ExprFloat {
    pub(crate) fn create(_float: f64) -> Result<ExprDef, AstError> {
        Err(AstError::FloatNotSupported.into())
    }
}

pub(crate) struct ExprKeyName {
    op: ExprOpType,
    value_type: ExprValueType,
    pub(crate) key_name: Atom,
}
impl ExprKeyName {
    pub(crate) fn create(key_name: Atom) -> Result<ExprDef, AstError> {
        let op = ExprOpType::Value;
        let value_type = ExprValueType::Keyname; //?

        Ok(ExprDef::KeyName(Self {
            value_type,
            op,
            key_name,
        }))
    }
}

pub(crate) struct ExprBinary {
    pub(super) op: ExprOpType,
    value_type: ExprValueType,
    pub(super) left: Box<ExprDef>,
    pub(super) right: Box<ExprDef>,
}
impl ExprBinary {
    pub(crate) fn create(
        op: ExprOpType,
        left: ExprDef,
        right: ExprDef,
    ) -> Result<ExprDef, AstError> {
        let value_type = ExprValueType::Unknown;

        Ok(ExprDef::Binary(Self {
            op,
            value_type,
            left: Box::new(left),
            right: Box::new(right),
        }))
    }
}

pub(crate) struct ExprUnary {
    pub(super) op: ExprOpType,
    pub(super) value_type: ExprValueType,
    pub(super) child: Box<ExprDef>,
}
impl ExprUnary {
    pub(crate) fn create(
        op: ExprOpType,
        value_type: ExprValueType,
        child: ExprDef,
    ) -> Result<ExprDef, AstError> {
        Ok(ExprDef::Unary(Self {
            op,
            value_type,
            child: Box::new(child),
        }))
    }
}

pub(crate) struct ExprFieldRef {
    pub(super) op: ExprOpType,
    value_type: ExprValueType,
    pub(super) element: Atom,
    pub(super) field: Atom,
}
impl ExprFieldRef {
    pub(crate) fn create(element: Atom, field: Atom) -> Result<ExprDef, AstError> {
        let op = ExprOpType::FieldRef;
        let value_type = ExprValueType::Unknown;

        Ok(ExprDef::FieldRef(Self {
            op,
            value_type,
            element,
            field,
        }))
    }
}

pub(crate) struct ExprArrayRef {
    op: ExprOpType,
    pub(super) value_type: ExprValueType,
    pub(super) element: Option<Atom>,
    pub(super) field: Atom,
    pub(super) entry: Box<ExprDef>,
}
impl ExprArrayRef {
    pub(crate) fn create(
        element: Option<Atom>,
        field: Atom,
        entry: ExprDef,
    ) -> Result<ExprDef, AstError> {
        let op = ExprOpType::ArrayRef;
        let value_type = ExprValueType::Unknown;

        Ok(ExprDef::ArrayRef(Self {
            op,
            value_type,
            element,
            field,
            entry: Box::new(entry),
        }))
    }
}

pub(crate) struct ExprAction {
    pub(super) op: ExprOpType,
    pub(super) value_type: ExprValueType,
    pub(crate) name: Atom,
    pub(crate) args: Vec<Box<ExprDef>>,
}
impl ExprAction {
    pub(crate) fn create(name: Atom, args: Vec<ExprDef>) -> Result<ExprDef, AstError> {
        let op = ExprOpType::ActionDecl;
        let value_type = ExprValueType::Unknown;

        let args = args.into_iter().map(|x| Box::new(x)).collect();

        Ok(ExprDef::Action(Self {
            op,
            value_type,
            name,
            args,
        }))
    }
}

pub(crate) struct ExprActionList {
    pub(super) op: ExprOpType,
    value_type: ExprValueType,
    pub(crate) actions: Vec<Box<ExprDef>>,
}
impl ExprActionList {
    pub(crate) fn create(actions: Vec<ExprDef>) -> Result<ExprDef, AstError> {
        let op = ExprOpType::ActionList;

        let value_type = ExprValueType::Actions;

        let actions = actions
            .into_iter()
            .map(|x| {
                if let ExprDef::Action(_) = x {
                    return Ok(Box::new(x));
                } else {
                    return Err(AstError::MustBeAction.into());
                }
            })
            .collect::<Result<Vec<Box<ExprDef>>, AstError>>()?;

        Ok(ExprDef::Actions(Self {
            op,
            value_type,
            actions,
        }))
    }

}

pub(crate) struct ExprKeysymList {
    op: ExprOpType,
    value_type: ExprValueType,
    pub(crate) syms_lists: Vec<Vec<Option<Keysym>>>,
}
impl ExprKeysymList {
    pub(crate) fn create(sym: Option<Keysym>) -> Self {
        let op = ExprOpType::KeysymList;
        let value_type = ExprValueType::Symbols;

        Self {
            op,
            value_type,
            syms_lists: vec![vec![sym]],
        }
    }
    pub(crate) fn create_multi(syms: Vec<Option<Keysym>>) -> Self {
        let op = ExprOpType::KeysymList;
        let value_type = ExprValueType::Symbols;
        Self {
            op,
            value_type,
            syms_lists: vec![syms],
        }
    }

    pub(crate) fn append(&mut self, sym: Option<Keysym>) {
        self.syms_lists.push(vec![sym]);
    }

    pub(crate) fn append_multi(&mut self, mut append: Self) {
        self.syms_lists.append(&mut append.syms_lists);
    }
}

pub(crate) struct VarDef {
    pub(crate) merge: MergeMode,
    pub(super) name: Option<ExprDef>,
    pub(super) value: ExprDef,
}

impl VarDef {
    pub(crate) fn create(name: Option<ExprDef>, value: ExprDef) -> Result<Self, AstError> {
        let merge = MergeMode::default();

        Ok(Self { merge, name, value })
    }

    pub(crate) fn create_bool(ident: Atom, set: bool) -> Result<Self, AstError> {
        let name: ExprDef = ExprIdent::create(ident)?;
        let value: ExprDef = ExprBoolean::create(set)?;

        Self::create(Some(name), value)
    }
}

pub(crate) struct VModDef {
    pub(crate) merge: MergeMode,
    pub(crate) name: Atom,
    pub(super) value: Option<ExprDef>,
}

impl VModDef {
    pub(crate) fn create(name: Atom, value: Option<ExprDef>) -> Result<Self, AstError> {
        let merge = MergeMode::default();

        Ok(Self { merge, name, value })
    }
}

pub(crate) struct KeycodeDef {
    pub(crate) merge: MergeMode,
    pub(super) name: Atom,
    pub(super) value: i64,
}

impl KeycodeDef {
    pub(crate) fn create(name: Atom, value: i64) -> Result<Self, AstError> {
        let merge = MergeMode::default();
        Ok(Self { merge, name, value })
    }
}

pub(crate) struct KeyAliasDef {
    pub(crate) merge: MergeMode,
    pub(super) alias: Atom,
    pub(super) real: Atom,
}
impl KeyAliasDef {
    pub(crate) fn create(alias: Atom, real: Atom) -> Result<Self, AstError> {
        let merge = MergeMode::default();
        Ok(Self { merge, alias, real })
    }
}

pub(crate) struct KeyTypeDef {
    pub(crate) merge: MergeMode,
    pub(super) name: Atom,
    pub(super) body: Vec<VarDef>,
}

impl KeyTypeDef {
    pub(crate) fn create(name: Atom, body: Vec<VarDef>) -> Result<Self, AstError> {
        let merge = MergeMode::default();

        Ok(Self { merge, name, body })
    }
}

pub(crate) struct SymbolsDef {
    pub(crate) merge: MergeMode,
    pub(crate) key_name: Atom,
    pub(crate) symbols: Vec<VarDef>,
}

impl SymbolsDef {
    pub(crate) fn create(key_name: Atom, symbols: Vec<VarDef>) -> Result<Self, AstError> {
        let merge = MergeMode::default();

        Ok(Self {
            merge,
            key_name,
            symbols,
        })
    }
}

pub(crate) struct ModMapDef {
    pub(crate) merge: MergeMode,
    pub(crate) modifier: Atom,
    pub(crate) keys: Vec<ExprDef>,
}

impl ModMapDef {
    pub(crate) fn create(modifier: Atom, keys: Vec<ExprDef>) -> Result<Self, AstError> {
        let merge = MergeMode::default();

        //TODO: what enum variant is ExprDef? Keysymlist?
        // check this

        Ok(Self {
            merge,
            modifier,
            keys,
        })
    }
}

pub(crate) struct GroupCompatDef {
    pub(crate) merge: MergeMode,
    _group: usize, //c type: unsigned
    _def: ExprDef,
}

impl GroupCompatDef {
    pub(crate) fn create(_group: usize, val: ExprDef) -> Result<Self, AstError> {
        let merge = MergeMode::default();

        Ok(Self {
            merge,
            _group,
            _def: val,
        })
    }
}

pub(crate) struct InterpDef {
    pub(crate) merge: MergeMode,
    pub(super) sym: Option<Keysym>,
    pub(super) _match: Option<ExprDef>,
    pub(super) def: Vec<VarDef>, //VarDef
}

impl InterpDef {
    pub(crate) fn create(sym: Option<Keysym>, _match: Option<ExprDef>) -> Result<Self, AstError> {
        let merge = MergeMode::default();

        Ok(Self {
            merge,
            sym,
            _match,
            def: vec![],
        })
    }

    pub(crate) fn set_def(&mut self, def: Vec<VarDef>) {
        self.def = def;
    }
}

pub(crate) struct LedNameDef {
    pub(crate) merge: MergeMode,
    pub(super) ndx: usize,
    pub(super) name: ExprDef,
    pub(super) _virtual: bool,
}

impl LedNameDef {
    pub(crate) fn create(ndx: usize, name: ExprDef, _virtual: bool) -> Result<Self, AstError> {
        let merge = MergeMode::default();

        Ok(Self {
            merge,
            ndx,
            name,
            _virtual,
        })
    }
}

pub(crate) struct LedMapDef {
    pub(crate) merge: MergeMode,
    pub(super) name: Atom,
    pub(super) body: Vec<VarDef>,
}

impl LedMapDef {
    pub(crate) fn create(name: Atom, body: Vec<VarDef>) -> Result<Self, AstError> {
        let merge = MergeMode::default();

        Ok(Self { merge, name, body })
    }
}
pub(crate) struct XkbFile {
    pub(crate) file_type: XkbFileType,
    pub(crate) name: String,
    files: Option<Vec<XkbFile>>,
    pub(crate) defs: Vec<Decl>, //type?
    pub(crate) flags: XkbMapFlags,
}


impl XkbFile {
    pub(crate) fn create(
        file_type: XkbFileType,
        name: Option<String>,
        files: Option<Vec<XkbFile>>,
        defs: Option<Vec<Decl>>,
        flags: XkbMapFlags,
    ) -> Self {
        //TODO: escape map name
        Self {
            file_type,
            name: name.unwrap_or_else(|| "(unnamed)".to_owned()),
            files,
            defs: defs.unwrap_or_else(|| vec![]),
            flags,
        }
    }

    pub(crate) fn from_components(
        ctx: &mut Context,
        kkctgs: ComponentNames,
    ) -> Self {
        let components = [
            (XkbFileType::Keycodes, kkctgs.keycodes),
            (XkbFileType::Types, kkctgs.types),
            (XkbFileType::Compat, kkctgs.compat),
            (XkbFileType::Symbols, kkctgs.symbols),
        ];

        let mut defs = vec![];
        for (file_type, component) in components {
            let include = IncludeStmt::create(ctx, &component, MergeMode::Default);
            let decl = vec![Decl::Include(include)];

            let file = XkbFile::create(file_type, None, None, Some(decl), XkbMapFlags::empty());

            defs.push(file);
        }

        let file = XkbFile::create(
            XkbFileType::Keymap,
            None,
            Some(defs),
            None,
            XkbMapFlags::empty(),
        );

        file
    }

    pub(crate) fn take_files(&mut self) -> impl Iterator<Item = Self> {
        self.files.take().unwrap_or(vec![]).into_iter()
    }

    pub(crate) fn file_type(&self) -> XkbFileType {
        self.file_type
    }
}

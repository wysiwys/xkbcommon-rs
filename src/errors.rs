// TODO: generate message codes automatically from message-registry.yaml
// see update-message-registry.py

use crate::rust_xkbcommon::*;

pub use crate::context::ContextError;
pub use crate::state::StateError;

#[cfg(test)]
#[derive(Debug)]
pub(crate) enum TestErr {
    CouldNotOpenFile,
    Keymap(KeymapErr),
    State(StateError),
    NoSymsForKeycode(Keycode),
    WrongKeysym{
        expected: Keysym,
        got: Keysym 
    }

}
#[derive(Debug)]
pub enum KeymapErr {
    UnsupportedFormat(KeymapFormat),
    CouldNotGetKeymapString,
    UnrecognizedFlags,
    NoSuchKey,
    NoSuchType,
    KeyNoSuchGroup,
    KeyNoSuchLevel,
    AstError(AstError),
    Action(ActionErr),
    Include(IncludeErr),
    Io(std::io::Error),
    KeycodeError(KeycodeErr),
    TypeError(KeytypeErr),
    SymbolsError(SymbolsErr),
    CompatError(CompatErr),
    CouldNotCompileKeycodes,
    CouldNotCompileCompat,
    CouldNotCompileSymbols,
    CouldNotCreateString,
    NoMapFound,
    NoSuchFile,
    InvalidKeymapFormat,
    RulesNoComponentsReturned,
    XkbMessage(XkbMessageCode), // TODO: remove me
    ParseFailed(lalrpop_util::ParseError<(),crate::lexer::Token,&'static str>),
    MatchError(&'static str)
    
}
impl std::fmt::Display for KeymapErr {

    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {

        write!(f, "{:?}", self)

    }

}
impl std::error::Error for KeymapErr {


}

impl From<AstError> for KeymapErr {

    fn from(a: AstError) -> Self {
        KeymapErr::AstError(a)
    }
}

impl From<ActionErr> for KeymapErr {

    fn from(a: ActionErr) -> Self {
        KeymapErr::Action(a)
    }
}
impl From<IncludeErr> for KeymapErr {

    fn from(a: IncludeErr) -> Self {
        KeymapErr::Include(a)
    }
}
impl From<KeycodeErr> for KeymapErr {
    fn from(a: KeycodeErr) -> Self {
        KeymapErr::KeycodeError(a)
    }
}
impl From<XkbMessageCode> for KeymapErr {
    fn from(a: XkbMessageCode) -> Self {
        KeymapErr::XkbMessage(a)
    }
}

impl From<KeytypeErr> for KeymapErr {
    fn from(a: KeytypeErr) -> Self {
        KeymapErr::TypeError(a)
    }
}
#[derive(Clone,Debug)]
pub(crate) enum XkbMessageCode {
    Error(XkbError),
    Warning(XkbWarning),
    NoId

}

#[derive(Debug)]
pub(crate) enum SymbolsErr {

    IncludeFailed,
    NoGroups,
    KeyNotFound,
    GroupNotCreated,

}

impl From<SymbolsErr> for KeymapErr {

    fn from(e: SymbolsErr) -> Self {

        KeymapErr::SymbolsError(e) }
}


#[derive(Debug)]
pub(crate) enum KeytypeErr {
    InvalidSyntax,
    FailedHandleKeytypeDef

}
impl From<XkbWarning> for KeymapErr {

    fn from(w: XkbWarning) -> Self {

        KeymapErr::XkbMessage(XkbMessageCode::Warning(w))
    }
}
impl From<XkbError> for KeymapErr {

    fn from(e: XkbError) -> Self {

        KeymapErr::XkbMessage(XkbMessageCode::Error(e))
    }
}
impl From<XkbWarning> for XkbMessageCode {

    fn from(w: XkbWarning) -> Self {

        XkbMessageCode::Warning(w)
    }
}
impl From<XkbError> for XkbMessageCode {

    fn from(e: XkbError) -> Self {

        XkbMessageCode::Error(e)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum XkbWarning {
    ConflictingKeyTypePreserveEntries = 43,
    IllegalKeycodeAlias = 101,
    UnrecognizedKeysym = 107,
    UnsupportedGeometrySection = 172,
    CannotInferKeyType = 183,
    InvalidEscapeSequence = 193,
    IllegalKeyTypePreserveResult = 195,
    ConflictingKeyTypeLevelNames = 239,
    ConflictingKeyTypeMapEntry = 266,
    UndefinedKeyType = 286,
    NonBaseGroupName = 305,
    DuplicateEntry = 378,
    ConflictingKeyTypeDefinitions = 407,
    MissingDefaultSection = 433,
    ConflictingKeySymbol = 461,
    NumericKeysym = 489,
    ExtraSymbolsIgnored = 516,
    ConflictingKeyName = 523,
    UnknownCharEscapeSequence = 645,
    MultipleGroupsAtOnce = 700,
    UnsupportedSymbolsField = 711,
    UndefinedKeycode = 770,
    ConflictingModmap = 800,
    ConflictingKeyAction = 883,
    ConflictingKeyTypeMergingGroups = 893,
    MissingSymbolsGroupNameIndex = 903,
    ConflictingKeyFields = 935,
    UnresolvedKeymapSymbol = 965,
    UndeclaredModifiersInKeyType = 971


}

#[derive(Debug, Clone)]
pub(crate) enum XkbError {
    MalformedNumberLiteral = 34,
    UnsupportedModifierMask = 60,
    ExpectedArrayEntry = 77,
    UndeclaredVirtualModifier = 123,
    InsufficientBufferSize = 134,
    WrongStatementType = 150,
    InvalidIncludeStatement = 203,
    InvalidModmapEntry = 206,
    UnsupportedGroupIndex = 237,
    InvalidSetDefaultStatement = 254,
    UnsupportedShiftLevel = 312,
    IncludedFileNotFound = 338,
    UnknownOperator = 345,
    RecursiveInclude = 386,
    GlobalDefaultsWrongScope = 428,
    InvalidOperation = 478,
    AllocationError = 550,
    WrongFieldType = 578,
    InvalidRealModifier = 623,
    InvalidIncludedFile = 661,
    InvalidSyntax = 769,
    InvalidExpressionType = 784,
    InvalidValue = 796,
    UnknownField = 812,
    ConflictingKeySymbolsEntry = 901,
    InvalidIdentifier = 949,
}


#[derive(Debug)]
pub(crate) enum ActionErr {
    UnknownAction,
    IllegalFieldName
}

#[derive(Debug)]
pub(crate) enum AstError {
    FloatNotSupported,
    MustBeAction,
    IsNotAction,
    ListEmpty,
    IsNotVar,
    WrongEnumVariant,
    NotSupported,
    KeymapCreationFailed,
    NoSuchKeysym,
}


#[derive(Debug, Clone)]
pub(crate) enum IncludeErr {
    Illegal,
    MapWithoutFile,
    NoMap,
    CouldNotFindFile,
    NoMergeMode,
    InvalidFile,
    ParseIncludeNameErr
}


#[derive(Debug)]
pub(crate) enum CompatErr {

}

impl From<CompatErr> for KeymapErr {

    fn from(e: CompatErr) -> Self {

        KeymapErr::CompatError(e) }
}


#[derive(Debug)]
pub(crate) enum KeycodeErr {
    ExceedsIncludeMaxDepth,
    WrongDeclType,
    IllegalKeycode,
    IllegalIndicatorIndex,
    CouldNotResolveLhs,
    UnknownElement,
    UnknownField,

}


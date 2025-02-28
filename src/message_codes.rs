// based loosely on message-codes.h
#[derive(Clone, Debug)]
pub enum XkbMessageCode {
    Error(XkbError),
    Warning(XkbWarning),
    NoId,
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

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum XkbWarning {
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
    DeprecatedKeysym = 301,
    DeprecatedKeysymName = 302,
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
    UndeclaredModifiersInKeyType = 971,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum XkbError {
    MalformedNumberLiteral = 34,
    UnsupportedModifierMask = 60,
    ExpectedArrayEntry = 77,
    UndeclaredVirtualModifier = 123,
    InsufficientBufferSize = 134,
    WrongStatementType = 150,
    InvalidPath = 161,
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
    CannotResolveRMLVO = 595,
    InvalidRealModifier = 623,
    InvalidIncludedFile = 661,
    IncompatibleActionsAndKeysymsCount = 693,
    InvalidSyntax = 769,
    InvalidExpressionType = 784,
    InvalidValue = 796,
    UnknownField = 812,
    KeymapCompilationFailed = 822,
    ConflictingKeySymbolsEntry = 901,
    InvalidIdentifier = 949,
}

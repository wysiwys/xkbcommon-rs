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

// TODO: generate message codes automatically from message-registry.yaml
// see update-message-registry.py

pub(crate) use crate::message_codes::*;
use crate::rust_xkbcommon::*;

pub mod context {
    //! Errors for the [Context](crate::Context) struct.
    pub use crate::context::errors::*;
}
pub mod state {
    //! Errors for the [State](crate::State) struct.
    pub use crate::state::errors::*;
}

pub mod keymap {

    //! Errors for the [Keymap](crate::Keymap) struct.
    pub use super::KeymapCompileError;
}

use thiserror::Error;

use crate::parser_utils::XkbFileParseError;
use std::path::PathBuf;

use crate::keymap::ActionType;
use crate::xkbcomp::action::ActionField;
use crate::xkbcomp::ast::{ExprOpType, XkbFileType};

impl From<CompileKeycodesError> for KeymapCompileError {
    fn from(e: CompileKeycodesError) -> Self {
        KeymapCompileError::Keycodes {
            message: e.to_string(),
        }
    }
}
impl From<CompileTypesError> for KeymapCompileError {
    fn from(e: CompileTypesError) -> Self {
        KeymapCompileError::Types {
            message: e.to_string(),
        }
    }
}
impl From<CompileCompatError> for KeymapCompileError {
    fn from(e: CompileCompatError) -> Self {
        KeymapCompileError::Compat {
            message: e.to_string(),
        }
    }
}
impl From<CompileSymbolsError> for KeymapCompileError {
    fn from(e: CompileSymbolsError) -> Self {
        KeymapCompileError::Symbols {
            message: e.to_string(),
        }
    }
}

impl From<ReportedError> for HandleActionError {
    fn from(e: ReportedError) -> Self {
        Self::Configuration(e)
    }
}
#[derive(Clone, Debug, Error)]
pub(super) enum HandleActionError {
    #[error("Bad type configuration: {0:?}")]
    Configuration(ReportedError),

    #[error("Illegal field name: {0}")]
    IllegalFieldName(String),

    #[error("Invalid button code: {0}")]
    InvalidButtonCode(i64),

    #[error("Invalid count field: {0}")]
    InvalidCountField(i64),

    #[error("Screen index must be in the range 1.255; Illegal screen value {0} ignored")]
    IllegalScreenIndex(i64),

    #[error(
        "The data for a private action has {max} entries; attempt to use data[{index}] ignored"
    )]
    PrivateActionExceedMaxIndex { max: usize, index: i64 },

    #[error("All data for a private action must be between 0..255; Illegal datum {0} ignored")]
    PrivateActionIllegalDatum(i64),

    #[error("A private action must have 0..{0} data bytes; Illegam data ignored")]
    PrivateActionInvalidSize(usize),

    #[error("Default pointer button value cannot be 0")]
    DefaultPtrBtnCannotBeZero,

    #[error("In private data, array subscript must be integer.")]
    ArraySubscriptMustBeInt,

    #[error("In private data, invalid action type: {0}; action type must be within 0..255.")]
    PrivateActionTypeNotU8(i64),

    #[error("Unknown action: {0}")]
    UnknownAction(String),

    #[error("Cannot change defaults in an action definition")]
    CannotChangeDefaults,

    #[error("Unknown field name: {0}")]
    UnknownFieldName(String),

    #[error("Not an action definition: {0:?}")]
    NotAnActionDefinition(ExprOpType),

    #[error("The action type is missing")]
    ActionTypeMissing,

    // TODO: improve the clarity
    #[error("Could not resolve lhs")]
    CouldNotResolveLhs,
}
#[derive(Clone, Debug, Error)]
pub(super) enum HandleVModError {
    #[error("Too many modifiers defined")]
    TooManyModifiersDefined,

    #[error("Could not resolve mod mask")]
    CouldNotResolveModMask,

    #[error("An existing real mod has the name {0}")]
    ExistingRealModHasName(String),
}

impl From<HandleVModError> for CompileSymbolsError {
    fn from(e: HandleVModError) -> Self {
        CompileSymbolsError::VMod(e)
    }
}
#[derive(Clone, Debug, Error)]
pub(super) enum ReportedError {
    #[error("{name}: The {_type} {field} field is not an array")]
    NotArray {
        name: String,
        _type: String,
        field: String,
    },

    #[error("{name}: Missing subscript for {_type} {field}")]
    ShouldBeArray {
        name: String,
        _type: String,
        field: String,
    },

    #[error("{name}: The {_type} {field} field must be a {wanted}")]
    BadType {
        name: String,
        _type: String,
        field: String,
        wanted: String,
    },

    #[error("Unknown {_type} field {field} in {name}")]
    BadField {
        name: String,
        _type: String,
        field: String,
    },

    #[error("Value of {field:?} field must be of type {_type}")]
    ActionFieldMismatch { field: ActionField, _type: String },

    #[error("Action field {field:?} not defined for type {_type:?}")]
    ActionFieldNotDefinedForType {
        field: ActionField,
        _type: ActionType,
    },

    #[error("The {field:?} field in the {_type:?} action is not an array")]
    ActionNotArray {
        field: ActionField,
        _type: ActionType,
    },
}

#[derive(Clone, Debug, Error)]
pub(super) enum ParseIncludeMapError {
    #[error("Multiple extra data provided")]
    MultipleExtraData,

    #[error("Unexpected token")]
    UnexpectedToken,

    #[error("Multiple maps provided")]
    MultipleMaps,

    #[error("Map missing file")]
    MapWithoutFile,

    #[error("Marked invalid")]
    Invalid,
}

#[derive(Clone, Debug, Error)]
pub(super) enum ProcessIncludeError {
    #[error("Could not locate file {path} with type {file_type:?}")]
    NoSuchFile {
        path: PathBuf,
        file_type: crate::xkbcomp::ast::XkbFileType,
    },
    #[error("Parsing failed for include {path}: {error:?}")]
    ParseFileFailed {
        path: PathBuf,
        error: XkbFileParseError,
    },

    #[error("Invalid included file: {0}")]
    InvalidIncludedFile(PathBuf),
}

#[derive(Clone, Debug, Error)]
pub(crate) enum CompileKeycodesError {
    #[error("Indicator index has wrong type")]
    IndicatorBadType,

    #[error("Multiple errors: {0:?}")]
    Multiple(Vec<Self>),

    #[error("Exceeds include max depth: {0}")]
    ExceedsIncludeMaxDepth(u32),

    #[error("No keycodes file found for path {path}")]
    NoSuchKeycodesFile { path: PathBuf },

    #[error("Parsing failed for included keycodes file {path}: {error:?}")]
    ParseKeycodesFileFailed {
        path: PathBuf,
        error: XkbFileParseError,
    },

    #[error("Invalid included keycodes file: {0}")]
    InvalidIncludedKeycodesFile(PathBuf),

    // TODO: include max?
    #[error("Illegal keycode value: {0:?}")]
    IllegalKeycode(i64),

    #[error("Illegal indicator index {index} specified; must be between 1..{max}")]
    IllegalIndicatorIndex { index: LedIndex, max: usize },

    #[error("Unknown element encountered")]
    UnknownElement(String),

    #[error("Unknown field encountered")]
    UnknownField(String),

    #[error("Could not resolve lhs")]
    CouldNotResolveLhs,

    #[error("Wrong decl type: should be Include, KeyName, KeyAlias, Var, or LedName")]
    WrongDeclType,
}

impl From<ProcessIncludeError> for CompileKeycodesError {
    fn from(e: ProcessIncludeError) -> Self {
        use ProcessIncludeError::*;
        match e {
            NoSuchFile { path, .. } => Self::NoSuchKeycodesFile { path },
            ParseFileFailed { path, error } => Self::ParseKeycodesFileFailed { path, error },

            InvalidIncludedFile(path) => Self::InvalidIncludedKeycodesFile(path),
        }
    }
}

impl From<ProcessIncludeError> for CompileTypesError {
    fn from(e: ProcessIncludeError) -> Self {
        use ProcessIncludeError::*;
        match e {
            NoSuchFile { path, .. } => Self::NoSuchTypesFile { path },
            ParseFileFailed { path, error } => Self::ParseTypesFileFailed { path, error },

            InvalidIncludedFile(path) => Self::InvalidIncludedTypesFile(path),
        }
    }
}

impl From<ReportedError> for CompileTypesError {
    fn from(e: ReportedError) -> Self {
        Self::Configuration(e)
    }
}

impl From<HandleVModError> for CompileTypesError {
    fn from(e: HandleVModError) -> Self {
        Self::VMod(e)
    }
}

#[derive(Clone, Debug, Error)]
pub(crate) enum CompileTypesError {
    #[error("Exceeds include max depth")]
    ExceedsIncludeMaxDepth,

    #[error("Invalid qualifier")]
    InvalidFieldQualifier,

    #[error("Encountered multiple errors: {0:?}")]
    MultipleErrors(Vec<Self>),

    // TODO: return error from resolve_lhs?
    #[error("Could not resolve LHS")]
    CouldNotResolveLhs,

    #[error("Wrong statement type in types file: {0}")]
    WrongStatementType(&'static str),

    #[error("Bad type configuration: {0:?}")]
    Configuration(ReportedError),

    #[error("Level specifications in a key type must be integer")]
    UnsupportedShiftLevel,

    #[error("Preserve value in a key type is not a modifier mask")]
    UnsupportedModifierMask,

    #[error("Wrong field type")]
    WrongFieldType,

    #[error("Key type mask field must be a modifier mask")]
    KeyTypeMaskIsNotModMask,

    #[error("Unknown field {0} in key type")]
    UnknownField(String),

    #[error("Error processing vmod in types file: {0:?}")]
    VMod(HandleVModError),

    #[error("No types file found for path {path}")]
    NoSuchTypesFile { path: PathBuf },

    #[error("Parsing failed for included types file {path}: {error:?}")]
    ParseTypesFileFailed {
        path: PathBuf,
        error: XkbFileParseError,
    },

    #[error("Invalid included types file: {0}")]
    InvalidIncludedTypesFile(PathBuf),
}

impl From<HandleActionError> for CompileCompatError {
    fn from(e: HandleActionError) -> Self {
        use HandleActionError::*;
        match e {
            Configuration(re) => Self::Configuration(re),
            a => Self::HandleAction(a),
        }
    }
}

impl From<HandleVModError> for CompileCompatError {
    fn from(e: HandleVModError) -> Self {
        Self::VMod(e)
    }
}

impl From<ProcessIncludeError> for CompileCompatError {
    fn from(e: ProcessIncludeError) -> Self {
        use ProcessIncludeError::*;
        match e {
            NoSuchFile { path, .. } => Self::NoSuchCompatFile { path },
            ParseFileFailed { path, error } => Self::ParseCompatFileFailed { path, error },

            InvalidIncludedFile(path) => Self::InvalidIncludedCompatFile(path),
        }
    }
}

#[derive(Clone, Debug, Error)]
pub(crate) enum CompileCompatError {
    #[error("Could not resolve mod mask")]
    CouldNotResolveModMask,

    #[error("Could not resolve lhs")]
    CouldNotResolveLhs,

    #[error("Multiple errors encountered: {0:?}")]
    Multiple(Vec<Self>),

    #[error("Exceeds max include depth")]
    ExceedsIncludeMaxDepth,

    #[error("No compat file found for path {path}")]
    NoSuchCompatFile { path: PathBuf },

    #[error("Parsing failed for included compat file {path}: {error:?}")]
    ParseCompatFileFailed {
        path: PathBuf,
        error: XkbFileParseError,
    },

    #[error("Invalid included compat file: {0}")]
    InvalidIncludedCompatFile(PathBuf),

    #[error("Bad compat configuration: {0:?}")]
    Configuration(ReportedError),

    #[error("More LEDs than allowed: provided {provided}, max is {max}")]
    MoreLedsThanAllowed { provided: usize, max: usize },

    #[error("Unknown field {0} in compat map")]
    UnknownFieldInMap(String),

    #[error("Cannot set a global default value from within an interpret statement")]
    GlobalDefaultInsideInterp,

    #[error("Cannot set defaults for {0} in indicator map")]
    GlobalDefaultsWrongScope(String),

    #[error("Compat files may not include decl type: {0}")]
    WrongDeclType(String),

    #[error("Expression is missing lhs")]
    GlobalVarMissingLhs,

    #[error("Could not resolve lhs for global var")]
    GlobalVarCouldNotResolveLhs,

    #[error("Error processing vmod in compat file: {0:?}")]
    VMod(HandleVModError),

    #[error("Illegal modifier predicate: {0}")]
    IllegalModifierPredicate(String),

    #[error("Handling action failed: {0:?}")]
    HandleAction(HandleActionError),
}

// TODO: do all of them like this
impl From<ProcessIncludeError> for CompileSymbolsError {
    fn from(e: ProcessIncludeError) -> Self {
        Self::ProcessInclude(e)
    }
}

impl From<ReportedError> for CompileCompatError {
    fn from(e: ReportedError) -> Self {
        Self::Configuration(e)
    }
}

#[derive(Clone, Debug, Error)]
pub(crate) enum CompileSymbolsError {
    #[error("Global defaults wrong scope")]
    GlobalDefaultsWrongScope,

    #[error("Error handling vmod: {0:?}")]
    VMod(HandleVModError),

    #[error("No groups were created when compiling symbols")]
    NoGroupsCreated,

    #[error("Too many groups for key")]
    TooManyGroups,

    #[error("Illegal group index")]
    IllegalGroupIndex,

    #[error("Encountered the following errors: {0:?}")]
    MultipleErrors(Vec<CompileSymbolsError>),

    #[error("Process include error: {0:?}")]
    ProcessInclude(ProcessIncludeError),

    #[error("Could not set action field: {error:?}")]
    SetActionFieldFailed { error: HandleActionError },

    #[error("You must specify an index when specifying a group name")]
    IndexUnspecified,

    #[error("Illegal index in group name definition")]
    IllegalIndexInGroupNameDef,

    #[error("Group name must be string")]
    IllegalGroupName,

    #[error(
        "An explicit group was specified, but it provides a name for a group other than Group1"
    )]
    NonBaseGroupName,

    // TODO: more informative
    // maybe return Err from resolve_lhs
    #[error("Could not resolve lhs")]
    CouldNotResolveLhs,

    #[error("Invalid real modifier")]
    InvalidRealModifier,

    #[error("Wrong statement type in symbols file: {0}")]
    WrongStatementType(&'static str),

    #[error("Key not found in keycodes: {0:?}")]
    KeyNotFoundInKeycodes(Option<String>),

    #[error("Key not found in symbol map for sym {0:?}")]
    NoSuchKeyForSym(Keysym),

    #[error("Key not found in symbol map for sym {0:?}")]
    NoSuchKeyForName(String),

    #[error("Expected {expected:?}, found {found:?}")]
    WrongOpType {
        expected: ExprOpType,
        found: ExprOpType,
    },

    #[error("Duplicate symbols definition for key {key} group {group}")]
    DuplicateSymbolsDef { key: String, group: usize },

    #[error("Duplicate actions definition for key {key} group {group}")]
    DuplicateActionsDef { key: String, group: usize },

    #[error("Could not resolve string")]
    CouldNotResolveString,

    // TODO: more informative
    #[error("Unsupported group index")]
    UnsupportedGroupIndex,

    // TODO: more informative
    #[error("Expected vmod mask")]
    ExpectedVModMask,

    #[error("Illegal repeat setting")]
    IllegalRepeatSetting,

    #[error("Illegal groups wrap")]
    IllegalGroupsWrap,

    #[error("Illegal groups clamp")]
    IllegalGroupsClamp,

    #[error("Illegal group index for redirect")]
    IllegalGroupIndexForRedirect,

    #[error("Unknown field in a symbol interpretation: {0}")]
    UnknownFieldInSymInterp(String),
}

#[cfg(test)]
#[derive(Debug)]
pub enum TestErr {
    CouldNotOpenFile,
    Keymap(KeymapCompileError),
    NoSymsForKeycode(Keycode),
    WrongKeysym { expected: Keysym, got: Keysym },
}

#[derive(Clone, Debug, Error)]
pub(crate) enum NumLevelsError {
    #[error("No such group: {0}")]
    KeyNoSuchGroup(LayoutIndex),

    #[error("No such type")]
    KeyNoSuchType,
}

#[derive(Debug, Error)]
pub enum KeymapCompileError {
    #[error("Error when compiling keycodes: {message:?}")]
    Keycodes { message: String },

    #[error("Error when compiling types: {message:?}")]
    Types { message: String },

    #[error("Error when compiling compat map: {message:?}")]
    Compat { message: String },

    #[error("Error when compiling symbols: {message:?}")]
    Symbols { message: String },

    #[error("No components returned from path \"{0}\"")]
    NoComponentsReturned(PathBuf),

    #[error("Matcher error: {0:?}")]
    MatcherError(String),

    #[error("Cannot compile a {0:?} file alone into a keymap")]
    OnlyPartialKeymap(XkbFileType),

    #[error("Required section {0:?} missing from keymap")]
    RequiredSectionMissing(XkbFileType),

    #[error("Could not parse the provided string: {error:?}")]
    CouldNotParseString { error: XkbFileParseError },

    #[error("No map was found for the provided string")]
    NoMapFoundForString,

    #[error("No map was found for the provided file")]
    NoMapFoundForFile,

    #[error("Could not parse the provided file")]
    CouldNotParseFile { error: XkbFileParseError },

    #[error("Invalid keymap format: Must be TextV1")]
    InvalidKeymapFormat,

    #[error("Unrecognized CompileFlags")]
    UnrecognizedCompileFlags,
}

#[derive(Debug, Error)]
pub(crate) enum MatcherError {
    #[error("Wrong encoding provided for path {0:?}")]
    WrongEncoding(PathBuf),

    #[error("No group available to add element to")]
    NoGroupAvailable,

    // TODO: maybe just panic on these?
    #[error("Invalid layout index: {0}")]
    InvalidLayoutIndex(usize),

    #[error("Invalid variant index: {0}")]
    InvalidVariantIndex(usize),

    #[error("Tokens ended unexpectedly")]
    UnexpectedFinish,

    #[error("Matcher did not expect token: {0:?}")]
    UnexpectedToken(String),

    #[error("Lexer error: {0}")]
    LexerError(&'static str),

    #[error("%H was used in an include statement, but the HOME environment variable is not set")]
    IncludeHButNoHOME,

    #[error("unknown % format ({0}) in include statement")]
    UnknownFormatInIncludeStmt(String),

    #[error("Failed to open \"{rules}\": {error}")]
    FailedToOpenXKBRules {
        rules: String,
        error: std::io::Error,
    },

    #[error("Could not read \"{path}\" to string: {error}")]
    CouldNotReadRulesToString {
        path: PathBuf,
        error: std::io::Error,
    },

    #[error("Lexer: unexpected EOF")]
    LexerEarlyEOF,

    #[error("Lexer: unexpected character '{0}'")]
    LexerUnexpectedChar(char),

    #[error("Lexer: invalid index in %-expansion; may only index layout or variant")]
    LexerInvalidIndex,

    #[error("Could not extract layout index")]
    LexerCouldNotExtractLayoutIndex,
}
#[derive(Debug, Error)]
pub(crate) enum RulesCompileError {
    #[error("Parsing the rules failed: {0:?}")]
    MatcherError(MatcherError),

    #[error("No rules components returned from {0}")]
    NoComponentsReturned(PathBuf),
}

impl From<RulesCompileError> for KeymapCompileError {
    fn from(r: RulesCompileError) -> Self {
        use RulesCompileError::*;
        match r {
            NoComponentsReturned(p) => Self::NoComponentsReturned(p),
            MatcherError(e) => Self::MatcherError(e.to_string()),
        }
    }
}

#[derive(Debug, Error)]
pub(crate) enum AstError {
    #[error("Float is not supported")]
    FloatNotSupported,

    #[error("Cannot create action list from non-actions")]
    MustBeAction,
}

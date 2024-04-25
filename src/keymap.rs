// combines keymap.h, keymap.c, and keymap_priv.c
// also includes documentation from xkbcommon.h

use crate::atom::Atom;
use crate::context::Context;
use crate::errors::*;
use std::collections::BTreeMap;

pub(crate) const MOD_REAL_MASK_ALL: ModMask = 0x000000ff;
use crate::rust_xkbcommon::*;

/// Maximum number of allowed groups
///
/// This limit is artificially enforced. The main
/// reason it's still here is that the rules file
/// format does not support multiple groups very well,
/// and the rules shipped with `xkeyboard-config`
/// (see rules/evdev) depend on this limit extensively.
/// So just lifting this limit would cause problems for
/// people who will use more than 4 layouts.
///
// TODO: Fix the group index syntax in the rules
//format, preferably in a backwards compatible way.
//
pub const XKB_MAX_GROUPS: u8 = 4;
pub(crate) const XKB_MAX_MODS: usize = std::mem::size_of::<ModMask>() * 8;
pub(crate) const XKB_MAX_LEDS: usize = std::mem::size_of::<LedMask>() * 8;

pub(crate) const XKB_MOD_NONE: usize = 0xfffffff;

bitflags::bitflags! {

    #[derive(Copy, Clone, Debug, PartialEq)]
    pub(crate) struct ModType: u8 {
        const REAL = (1 << 0);
        const VIRT = (1 << 1);
        const BOTH = (1 << 0) | (1 << 1);
        }
}
#[repr(u8)]
#[derive(Clone, Copy, Eq, PartialEq, PartialOrd, Debug, Ord)]
pub(crate) enum ActionType {
    None = 0,
    ModSet = 1,
    ModLatch = 2,
    ModLock = 3,
    GroupSet = 4,
    GroupLatch = 5,
    GroupLock = 6,
    PtrMove = 7,
    PtrButton = 8,
    PtrLock = 9,
    PtrDefault = 10,
    Terminate = 11,
    SwitchVT = 12,
    CtrlSet = 13,
    CtrlLock = 14,
    Private = 15,
    PrivateDefinedAction(u8), //_NumEntries
}
pub(crate) const _ACTION_TYPE_NUM_ENTRIES: usize = 16;

impl From<ActionType> for u8 {
    fn from(val: ActionType) -> Self {
        use ActionType::*;
        match val {
            None => 0,
            ModSet => 1,
            ModLatch => 2,
            ModLock => 3,
            GroupSet => 4,
            GroupLatch => 5,
            GroupLock => 6,
            PtrMove => 7,
            PtrButton => 8,
            PtrLock => 9,
            PtrDefault => 10,
            Terminate => 11,
            SwitchVT => 12,
            CtrlSet => 13,
            CtrlLock => 14,
            Private => 15,
            PrivateDefinedAction(c) => c,
        }
    }
}

impl From<u8> for ActionType {
    fn from(v: u8) -> Self {
        use ActionType::*;
        match v {
            0 => None,
            1 => ModSet,
            2 => ModLatch,
            3 => ModLock,
            4 => GroupSet,
            5 => GroupLatch,
            6 => GroupLock,
            7 => PtrMove,
            8 => PtrButton,
            9 => PtrLock,
            10 => PtrDefault,
            11 => Terminate,
            12 => SwitchVT,
            13 => CtrlSet,
            14 => CtrlLock,
            15 => Private,
            val => PrivateDefinedAction(val),
        }
    }
}

bitflags::bitflags! {
    #[derive(Clone, Eq, PartialEq, Debug)]
    pub(crate) struct ActionFlags: u16 {
        const LockClear = (1 << 0);
        const LatchToLock = (1 << 1);
        const LockNoLock = (1 << 2);
        const LockNoUnlock = (1 << 3);
        const ModsLookupModMap = (1 << 4);
        const AbsoluteSwitch = (1 << 5);
        const AbsoluteX = (1 << 6);
        const AbsoluteY = (1 << 7);
        const Accel = (1 << 8);
        const SameScreen = (1 << 9);
    }
}

bitflags::bitflags! {

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub(crate) struct ActionControls: u16 {

        const Repeat = (1 << 0);
        const Slow = (1 << 1);
        const Debounce = (1 << 2);
        const Sticky = (1 << 3);
        const Mousekeys = (1 << 4);
        const MousekeysAccel = (1 << 5);
        const Ax = (1 << 6);
        const AxTimeout = (1 << 7);
        const AxFeedback = (1 << 8);
        const Bell = (1 << 9);
        const IgnoreGroupLock = (1 << 10);
    }
}

impl From<ActionControls> for i64 {
    fn from(val: ActionControls) -> Self {
        val.bits() as i64
    }
}

impl TryFrom<i64> for ActionControls {
    type Error = &'static str;
    fn try_from(i: i64) -> Result<ActionControls, Self::Error> {
        // TODO: test this works as expected

        let u = i.try_into().map_err(|_| "Could not convert from i64")?;

        Self::from_bits(u).ok_or("Could not convert to ActionControls")
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub(crate) enum MatchOperation {
    None,
    AnyOrNone,
    Any,
    All,
    Exactly,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub(crate) struct Mods {
    pub(crate) mods: ModMask, //original
    pub(crate) mask: ModMask, //computed.
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ModAction {
    pub(crate) action_type: ActionType,
    pub(crate) flags: ActionFlags,
    pub(crate) mods: Mods,
}

impl ModAction {
    fn new(action_type: ActionType) -> Self {
        Self {
            action_type,
            flags: ActionFlags::empty(),
            mods: Mods { mods: 0, mask: 0 },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct GroupAction {
    pub(crate) action_type: ActionType,
    pub(crate) flags: ActionFlags,
    pub(crate) group: Option<i32>,
}

impl GroupAction {
    fn new(action_type: ActionType) -> Self {
        Self {
            action_type,
            flags: ActionFlags::empty(),
            group: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ControlsAction {
    pub(crate) action_type: ActionType,
    pub(crate) flags: ActionFlags,
    pub(crate) ctrls: ActionControls, //TODO: where is this?
}
impl ControlsAction {
    fn new(action_type: ActionType) -> Self {
        Self {
            action_type,
            flags: ActionFlags::empty(),
            ctrls: ActionControls::empty(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct DefaultAction {
    pub(crate) action_type: ActionType,
    pub(crate) flags: ActionFlags,
    pub(crate) value: Option<i8>,
}

impl DefaultAction {
    fn new(action_type: ActionType) -> Self {
        Self {
            action_type,
            flags: ActionFlags::empty(),
            value: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SwitchScreenAction {
    pub(crate) action_type: ActionType,
    pub(crate) flags: ActionFlags,
    pub(crate) screen: Option<i8>,
}

impl SwitchScreenAction {
    fn new(action_type: ActionType) -> Self {
        Self {
            action_type,
            flags: ActionFlags::empty(),
            screen: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PointerAction {
    pub(crate) action_type: ActionType,
    pub(crate) flags: ActionFlags,
    pub(crate) x: Option<i16>,
    pub(crate) y: Option<i16>,
}

impl PointerAction {
    fn new(action_type: ActionType) -> Self {
        Self {
            action_type,
            flags: ActionFlags::empty(),
            x: None,
            y: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PointerButtonAction {
    pub(crate) action_type: ActionType,
    pub(crate) flags: ActionFlags,
    pub(crate) count: u8,
    pub(crate) button: Option<u8>,
}
impl PointerButtonAction {
    fn new(action_type: ActionType) -> Self {
        Self {
            action_type,
            flags: ActionFlags::empty(),
            count: 0,
            button: None,
        }
    }
}

pub(crate) const ACTION_DATA_LEN: usize = 7;
pub(crate) type ActionData = [Option<u8>; ACTION_DATA_LEN];

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PrivateAction {
    pub(crate) action_type: ActionType,
    pub(crate) data: ActionData,
}

impl PrivateAction {
    fn new(action_type: ActionType) -> Self {
        Self {
            action_type,
            data: [None; ACTION_DATA_LEN],
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Action {
    //ActionType(ActionType),
    Mods(ModAction),
    Group(GroupAction),
    Ctrls(ControlsAction),
    Dflt(DefaultAction),
    Screen(SwitchScreenAction),
    Ptr(PointerAction),
    Btn(PointerButtonAction),
    Private(PrivateAction),
    Terminate,
    None,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct KeyTypeEntry {
    pub(crate) level: LevelIndex,
    pub(crate) mods: Mods,
    pub(crate) preserve: Mods,
}

impl Action {
    pub(crate) fn empty_from(action_type: ActionType) -> Action {
        use ActionType::*;
        match action_type {
            ModSet => Action::Mods(ModAction::new(action_type)),
            ModLatch => Action::Mods(ModAction::new(action_type)),
            ModLock => Action::Mods(ModAction::new(action_type)),
            GroupSet => Action::Group(GroupAction::new(action_type)),
            GroupLatch => Action::Group(GroupAction::new(action_type)),
            GroupLock => Action::Group(GroupAction::new(action_type)),
            PtrMove => Action::Ptr(PointerAction::new(action_type)),
            PtrButton => Action::Btn(PointerButtonAction::new(action_type)),
            PtrLock => Action::Btn(PointerButtonAction::new(action_type)),
            PtrDefault => Action::Dflt(DefaultAction::new(action_type)),
            Terminate => Action::Terminate, // TODO: is this used for anything?
            SwitchVT => Action::Screen(SwitchScreenAction::new(action_type)),
            CtrlSet => Action::Ctrls(ControlsAction::new(action_type)),
            CtrlLock => Action::Ctrls(ControlsAction::new(action_type)),
            Private => Action::Private(PrivateAction::new(action_type)),
            None => Action::None,
            PrivateDefinedAction(_) => {
                panic!("Should not be creating empty private defined action")
            }
        }
    }
    pub(crate) fn action_type(&self) -> ActionType {
        use Action::*;
        match self {
            Mods(a) => a.action_type,
            Group(a) => a.action_type,
            Ctrls(a) => a.action_type,
            Dflt(a) => a.action_type,
            Screen(a) => a.action_type,
            Ptr(a) => a.action_type,
            Btn(a) => a.action_type,
            Private(a) => a.action_type,
            Terminate => ActionType::Terminate,
            None => ActionType::None,
        }
    }
}

impl KeyTypeEntry {
    /// Corresponds to `entry_is_active`
    /// If the virtual modifiers are not bound to anything, the entry is not active and should be
    /// skipped.
    pub(crate) fn is_active(&self) -> bool {
        self.mods.mods == 0 || self.mods.mask != 0
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct KeyType {
    pub(crate) name: Atom,
    pub(crate) mods: Mods,
    pub(crate) num_levels: LevelIndex,
    //pub(crate) num_level_names: u32,
    pub(crate) level_names: BTreeMap<usize, Atom>, //xkb_atom_t
    //pub(crate) num_entries: u32,
    pub(crate) entries: Vec<KeyTypeEntry>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct SymInterpret {
    pub(crate) sym: Option<Keysym>,
    pub(crate) match_op: MatchOperation,
    pub(crate) mods: Mods,
    pub(crate) virtual_mod: Option<ModIndex>,
    pub(crate) action: Action,
    pub(crate) level_one_only: bool,
    pub(crate) repeat: bool,
}

impl Default for SymInterpret {
    fn default() -> Self {
        Self {
            sym: None,
            match_op: MatchOperation::None, //lowest enum variant
            mods: Mods { mods: 0, mask: 0 },
            virtual_mod: None,
            action: Action::None,
            level_one_only: false,
            repeat: false,
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) struct Led {
    pub(crate) name: Option<Atom>, // xkb_atom_t
    pub(crate) which_groups: StateComponent,
    pub(crate) groups: LayoutMask,
    pub(crate) which_mods: StateComponent,
    pub(crate) mods: Mods,
    pub(crate) ctrls: ActionControls,
}

impl Default for Led {
    fn default() -> Self {
        Self {
            name: None,
            which_groups: StateComponent::empty(),
            groups: 0,
            which_mods: StateComponent::empty(),
            mods: Mods { mods: 0, mask: 0 },
            ctrls: ActionControls::empty(),
        }
    }
}

impl Led {
    pub(crate) fn new(name: Atom) -> Self {
        Self {
            name: Some(name),
            which_groups: StateComponent::empty(),
            groups: 0,
            which_mods: StateComponent::empty(),
            mods: Mods { mask: 0, mods: 0 },
            ctrls: ActionControls::empty(),
        }
    }
}
#[derive(Clone)]
pub(crate) struct KeyAlias {
    pub(crate) real: Atom,
    pub(crate) alias: Atom,
}
/*
struct Controls {
    groups_wrap: char,
    internal: Mods,
    ignore_lock: Mods,
    repeat_delay: u16,
    repeat_interval: u16,
    slow_keys_delay: u16,
    debounce_delay: u16,
    ax_options: u16,
    ax_timeous: u16,
    axt_opts_mask: u16,
    axt_opts_values: u16,
    axt_ctrls_mask: u32,
    axt_ctrls_values: u32,
}
*/

#[derive(Clone, PartialEq, Debug, Default)]
pub(crate) enum RangeExceedType {
    #[default]
    Wrap = 0,
    Saturate,
    Redirect,
}

bitflags::bitflags! {

    #[derive(Clone, PartialEq, Debug)]
    pub(crate) struct ExplicitComponents: u8 {
        const INTERP = (1 << 0);
        const VMODMAP = (1 << 1);
        const REPEAT = (1 << 2);
    }

}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct Level {
    // TODO: there is a possibility here
    // of having `action` set to Some(Action::None),
    // which is confusing
    pub(crate) action: Action,
    pub(crate) syms: Vec<Option<Keysym>>,
}

impl Level {
    // TODO: replace this with a variable?
    pub(crate) fn num_syms(&self) -> usize {
        self.syms.iter().filter(|s| s.is_some()).count()
    }

    pub(super) fn same_syms(&self, other: &Self) -> bool {
        // TODO: ensure order
        self.syms == other.syms
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Group {
    pub(crate) explicit_type: bool,
    pub(super) key_type: usize, //index of type in Keymap.types
    pub(super) levels: Vec<Level>,
}

pub(crate) struct KeyBuilder {
    pub(crate) keycode: Keycode,
    pub(crate) name: Atom,
    pub(crate) explicit: ExplicitComponents,
    pub(crate) modmap: ModMask,
    pub(crate) vmodmap: ModMask,
    pub(crate) repeats: bool,

    pub(crate) out_of_range_group_action: Option<RangeExceedType>,
    pub(crate) out_of_range_group_number: Option<LayoutIndex>,

    pub(crate) groups: Option<Vec<Group>>,
}

impl KeyBuilder {
    pub(crate) fn new(keycode: Keycode, name: Atom) -> Self {
        Self {
            keycode,
            name,
            explicit: ExplicitComponents::empty(),
            modmap: 0,
            vmodmap: 0,
            repeats: false,
            out_of_range_group_action: None,
            out_of_range_group_number: None,
            groups: None,
        }
    }

    pub(crate) fn build(self) -> Key {
        // TODO: reconsider these defaults
        // A builder might not even be needed
        Key {
            keycode: self.keycode,
            name: self.name,
            explicit: self.explicit,
            modmap: self.modmap,
            vmodmap: self.vmodmap,
            repeats: self.repeats,
            out_of_range_group_action: self.out_of_range_group_action.unwrap_or_default(),
            out_of_range_group_number: self.out_of_range_group_number.unwrap_or(0),
            groups: self.groups.unwrap_or_else(Vec::new),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Key {
    pub(super) keycode: Keycode,
    pub(crate) name: Atom,
    pub(crate) explicit: ExplicitComponents,
    pub(crate) modmap: ModMask,
    pub(crate) vmodmap: ModMask,
    pub(crate) repeats: bool,
    pub(super) out_of_range_group_action: RangeExceedType,
    pub(super) out_of_range_group_number: LayoutIndex,
    pub(super) groups: Vec<Group>,
}

impl Key {
    pub(crate) fn num_levels(
        &self,
        layout: LayoutIndex,
        keymap: &Keymap,
    ) -> Result<LevelIndex, NumLevelsError> {
        let layout = self
            .groups
            .get(layout)
            .ok_or(NumLevelsError::KeyNoSuchGroup(layout))?;

        // Get the corresponding type from the keymap
        let type_index = layout.key_type;
        let _type = keymap
            .types
            .get(type_index)
            .ok_or(NumLevelsError::KeyNoSuchType)?;

        Ok(_type.num_levels)
    }
}
#[derive(Copy, Clone, Debug)]
pub(crate) struct Mod {
    pub(crate) name: Atom,
    pub(crate) mod_type: ModType,
    pub(crate) mapping: ModMask, // vmod to real mod mapping
}

#[derive(Clone, Debug)]
pub(crate) struct ModSet {
    pub(crate) mods: Vec<Mod>,
}

impl ModSet {
    pub(crate) fn new_from_names(names: &[&str], ctx: &mut Context) -> Self {
        // see update_builtin_keymap_fields

        let mods = names
            .iter()
            .map(|name| {
                let atom = ctx.atom_intern(String::from(*name));
                Mod {
                    name: atom,
                    mod_type: ModType::REAL,
                    mapping: 0,
                }
            })
            .collect();
        Self { mods }
    }

    pub(crate) fn mod_name_to_index(&self, name: Atom, mod_type: ModType) -> Option<ModIndex> {
        self.mods
            .iter()
            .position(|_mod| _mod.name == name && _mod.mod_type.intersects(mod_type))
    }
}

#[allow(dead_code)]
#[derive(Clone)]
pub struct Keymap {
    pub(crate) context: Context,
    pub(crate) flags: CompileFlags,
    format: KeymapFormat,
    pub(super) enabled_ctrls: ActionControls,
    pub(crate) min_key_code: RawKeycode,
    pub(crate) max_key_code: RawKeycode,
    pub(crate) keys: BTreeMap<RawKeycode, Key>,

    // aliases in no particular order
    pub(crate) key_aliases: Vec<KeyAlias>,

    pub(crate) types: Vec<KeyType>,

    pub(crate) sym_interprets: Vec<SymInterpret>,

    pub(crate) mods: ModSet,

    // Not all groups must have names
    pub(crate) num_groups: usize,
    pub(crate) group_names: Vec<Atom>,

    pub(crate) leds: [Option<Led>; XKB_MAX_LEDS],

    pub(crate) keycodes_section_name: Option<String>,
    pub(crate) symbols_section_name: Option<String>,
    pub(crate) types_section_name: Option<String>,
    pub(crate) compat_section_name: Option<String>,
}

pub(crate) struct KeymapBuilder<T: KeymapFormatType> {
    pub(crate) context: Context,
    format: T,
    flags: CompileFlags,
    pub(crate) mods: ModSet,
    pub(crate) group_names: Vec<Atom>,
    pub(crate) keys: BTreeMap<RawKeycode, KeyBuilder>,
    pub(crate) key_aliases: Option<Vec<KeyAlias>>,

    pub(crate) min_key_code: Option<RawKeycode>,
    pub(crate) max_key_code: Option<RawKeycode>,

    pub(crate) leds: [Option<Led>; XKB_MAX_LEDS],
    pub(crate) sym_interprets: Option<Vec<SymInterpret>>,

    //file: Option<XkbFile>,
    pub(crate) keycodes_section_name: Option<String>,
    pub(crate) symbols_section_name: Option<String>,
    pub(crate) types_section_name: Option<String>,
    pub(crate) compat_section_name: Option<String>,

    pub(crate) types: Vec<KeyType>,
}

impl<T: KeymapFormatType> KeymapBuilder<T> {
    fn new(mut context: Context, format: T, flags: CompileFlags) -> Self {
        // Predefined (AKA real, core, X11) modifiers. The order is important!
        let builtin_mods = [
            "Shift", "Lock", "Control", "Mod1", "Mod2", "Mod3", "Mod4", "Mod5",
        ];

        let mods = ModSet::new_from_names(&builtin_mods, &mut context);

        // this was calloc'ed in the original
        Self {
            context,
            flags,
            format,
            min_key_code: None,
            max_key_code: None,
            mods,
            group_names: vec![],
            keys: BTreeMap::new(),
            key_aliases: None,
            leds: [None; XKB_MAX_LEDS],
            sym_interprets: None,
            types: vec![],
            //file: None,
            keycodes_section_name: None,
            symbols_section_name: None,
            types_section_name: None,
            compat_section_name: None,
        }
    }

    pub(crate) fn build(self) -> Keymap {
        let num_groups = self
            .keys
            .values()
            .map(|key| key.groups.as_ref().map(|g| g.len()).unwrap_or(0))
            .max()
            .unwrap_or(0);

        // simple copy of values

        Keymap {
            context: self.context, //TODO: pointer
            flags: self.flags,
            format: self.format.into(),
            enabled_ctrls: ActionControls::empty(), //TODO: substitute the actual value
            min_key_code: self.min_key_code.unwrap_or(8), //TODO: remove this default
            max_key_code: self.max_key_code.unwrap_or(255), // TODO: remove this default
            keys: self.keys.into_iter().map(|(k, v)| (k, v.build())).collect(),

            key_aliases: self.key_aliases.unwrap_or_else(Vec::new),

            types: self.types,
            sym_interprets: self.sym_interprets.unwrap_or_else(Vec::new),

            mods: self.mods,

            num_groups,
            group_names: self.group_names,

            leds: self.leds,

            keycodes_section_name: self.keycodes_section_name,
            symbols_section_name: self.symbols_section_name,
            types_section_name: self.types_section_name,
            compat_section_name: self.compat_section_name,
        }
    }
}

#[derive(Debug, Error)]
pub enum KeymapGetAsStringError {
    #[error("Invalid keymap format: Must be TextV1")]
    InvalidKeymapFormat,
}
impl Keymap {
    /// Create a keymap from RMLVO names.
    ///
    /// The primary keymap entry point: creates a new XKB keymap from a set of RMLVO (Rules + Model
    /// + Layouts + Variants + Options) names.
    ///
    /// # Arguments
    /// * `context`: The context in which to create the keymap.
    /// * `names`: The RMLVO names to use. See [RuleNames].
    /// * `flags`: Optional flags for the keymap, or 0.
    pub fn new_from_names<F>(
        context: Context,
        rmlvo: Option<RuleNames>,
        compile_flags: F,
    ) -> Result<Self, KeymapCompileError>
    where
        F: TryInto<CompileFlags> + Clone,
    {
        let mut rmlvo = match rmlvo {
            Some(r) => r,
            None => RuleNames::empty(),
        };

        let _format = KeymapFormat::TextV1;

        let flags: CompileFlags = compile_flags
            .try_into()
            .map_err(|_| KeymapCompileError::UnrecognizedCompileFlags)?;

        context.sanitize_rule_names(&mut rmlvo);

        // TextV1 is the only format available
        let keymap_builder = KeymapBuilder::new(context, TextV1, flags);

        //V1-specific option
        keymap_builder.keymap_new_from_names(rmlvo)
    }

    /// Create a keymap from a keymap string.
    ///
    /// This is just like [Keymap::new_from_file()], but instead of a file, it gets the keymap as
    /// one enormous string.
    pub fn new_from_string<T, F>(
        context: Context,
        string: String,
        format: T,
        flags_raw: F,
    ) -> Result<Self, KeymapCompileError>
    where
        T: TryInto<KeymapFormat> + Into<u32> + Clone,
        F: TryInto<CompileFlags> + Into<u32> + Clone,
    {
        // combines `new_from_string` and `new_from_buffer`
        // `new_from_buffer` would be the same function in Rust

        let _format: KeymapFormat = format.clone().try_into().map_err(|_| {
            let format_u32 = format.into();
            log::error!("Unsupported keymap format: {:?}", format_u32);
            KeymapCompileError::InvalidKeymapFormat
        })?;

        let flags: CompileFlags = flags_raw.clone().try_into().map_err(|_| {
            let flags_u32: u32 = flags_raw.into();
            log::error!("Unrecognized flags: {:?}", flags_u32);
            KeymapCompileError::UnrecognizedCompileFlags
        })?;

        //This is the only format available
        let keymap_builder = KeymapBuilder::new(context, TextV1, flags);

        //V1-specific option
        keymap_builder.keymap_new_from_string(string)
    }

    /// Get the compiled keymap as a string.
    ///
    /// You can pass in the special value [KeymapFormat::OriginalFormat] to use the format from
    /// which the keymap was originally created.
    ///
    /// The returned string may be fed back into [Keymap::new_from_string()] to get the exact same
    /// keymap (possibly in another process, etc.)
    pub fn get_as_string(
        &self,
        format: impl TryInto<KeymapFormat> + std::marker::Copy + Into<u32>,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let mut _format: KeymapFormat = match format.try_into() {
            Ok(format) => format,
            _ => {
                let format: u32 = format.into();
                log::error!("Unsupported keymap format: {}", format);
                return Err(KeymapGetAsStringError::InvalidKeymapFormat.into());
            }
        };

        if _format == KeymapFormat::OriginalFormat {
            _format = self.format;
        }

        // This is the only format available now
        self.text_v1_keymap_get_as_string()
    }

    /// Create a keymap from a keymap file.
    ///
    /// The file must contain a complete keymap. For example, in the [KeymapFormat::TextV1] format,
    /// this means the file must contain one top-level `%xkb_keymap` section, which in turn
    /// contains other required sections.
    pub fn new_from_file<T>(
        context: Context,
        file: std::fs::File,
        format: T,
        flags: CompileFlags,
    ) -> Result<Self, KeymapCompileError>
    where
        T: TryInto<KeymapFormat>,
    {
        if !flags.is_empty() {
            log::error!("Unrecognized flags: {:?}", flags);
            return Err(KeymapCompileError::UnrecognizedCompileFlags);
        }

        let format = format
            .try_into()
            .map_err(|_| KeymapCompileError::InvalidKeymapFormat)?;

        let keymap_builder = match format {
            KeymapFormat::TextV1 => KeymapBuilder::new(context, TextV1, flags),
            _ => return Err(KeymapCompileError::InvalidKeymapFormat),
        };

        keymap_builder.keymap_new_from_file(file)
    }

    pub(crate) fn xkb_key(&self, kc: RawKeycode) -> Option<&Key> {
        if kc < self.min_key_code || kc > self.max_key_code {
            None
        } else {
            self.keys.get(&kc)
        }
    }

    pub fn num_mods(&self) -> ModIndex {
        self.mods.mods.len()
    }

    pub fn mod_get_name(&self, idx: ModIndex) -> Option<String> {
        let _mod = self.mods.mods.get(idx)?;

        self.context.atom_text(_mod.name).map(|s| s.to_owned())
    }

    pub fn mod_get_index(&self, name: &str) -> Option<ModIndex> {
        let atom = self.context.atom_lookup(name)?;

        self.mods.mod_name_to_index(atom, ModType::BOTH)
    }

    pub fn num_layouts(&self) -> LayoutIndex {
        self.num_groups
    }

    pub fn layout_get_name(&self, idx: LayoutIndex) -> Option<String> {
        let name = self.group_names.get(idx)?;

        self.context.atom_text(*name).map(|s| s.to_owned())
    }

    pub fn layout_get_index(&self, name: &str) -> Option<LayoutIndex> {
        let atom = self.context.atom_lookup(name)?;

        self.group_names.iter().position(|n| *n == atom)
    }

    pub fn num_layouts_for_key(&self, kc: Keycode) -> Option<LayoutIndex> {
        let key = self.xkb_key(kc.raw())?;

        Some(key.groups.len())
    }

    pub fn num_levels_for_key(&self, kc: Keycode, layout: LayoutIndex) -> LevelIndex {
        let key = match self.xkb_key(kc.into()) {
            Some(key) => key,
            None => return 0,
        };

        let layout: i32 = match layout.try_into() {
            Ok(layout) => layout,
            _ => return 0,
        };

        let layout = crate::state::wrap_group_into_range(
            layout,
            key.groups.len(),
            &key.out_of_range_group_action,
            &key.out_of_range_group_number,
        );

        let layout = match layout {
            Some(layout) => layout,
            None => return 0,
        };

        key.num_levels(layout, self).unwrap_or(0)
    }

    pub fn num_leds(&self) -> LedIndex {
        self.leds.len()
    }

    pub fn led_get_name(&self, idx: LedIndex) -> Option<String> {
        let led = self.leds.get(idx)?.as_ref()?;

        self.context.atom_text(led.name?).map(|s| s.into())
    }

    pub fn led_get_index(&self, name: &str) -> Option<LedIndex> {
        let atom = self.context.atom_lookup(name)?;

        for (i, led) in self.leds.iter().enumerate() {
            if let Some(led) = led {
                if led.name == Some(atom) {
                    return Some(i);
                }
            }
        }

        None
    }

    pub fn key_get_mods_for_level(
        &self,
        kc: Keycode,
        layout: LayoutIndex,
        level: LevelIndex,
        masks_size: usize,
    ) -> Option<Vec<ModMask>> {
        let mut masks_out = vec![];

        let key = self.xkb_key(kc.raw())?;

        let layout: i32 = layout.try_into().ok()?;

        let layout = crate::state::wrap_group_into_range(
            layout,
            key.groups.len(),
            &key.out_of_range_group_action,
            &key.out_of_range_group_number,
        )?;

        if level >= key.groups.get(layout)?.levels.len() {
            return None;
        }

        let _type = key.groups.get(layout)?.key_type;

        let mut count = 0;

        if level == 0 {
            let mut empty_mapped = false;

            for entry in self.types.get(_type).unwrap().entries.iter() {
                if count >= masks_size {
                    break;
                }

                if entry.is_active() && entry.mods.mask == 0 {
                    empty_mapped = true;
                    break;
                }
            }

            if !empty_mapped && count < masks_size {
                count += 1;
                masks_out.push(0);
            }
        }

        // now search explicit mappings
        for entry in self.types.get(_type).unwrap().entries.iter() {
            if count >= masks_size {
                break;
            }

            if entry.is_active() && entry.level == level {
                count += 1;
                masks_out.push(entry.mods.mask);
            }
        }

        Some(masks_out)
    }

    pub fn key_get_syms_by_level(
        &self,
        kc: Keycode,
        layout_idx_orig: LayoutIndex,
        level: LevelIndex,
    ) -> Result<Vec<Keysym>, KeyGetSymsByLevelError> {
        let key = self
            .xkb_key(kc.raw())
            .ok_or(KeyGetSymsByLevelError::NoKeyForKeycode(kc))?;

        let layout_idx = layout_idx_orig
            .try_into()
            .map_err(|_| KeyGetSymsByLevelError::InvalidLayoutIndex(layout_idx_orig))?;

        let layout = crate::state::wrap_group_into_range(
            layout_idx,
            key.groups.len(),
            &key.out_of_range_group_action,
            &key.out_of_range_group_number,
        );

        let layout = layout.ok_or(KeyGetSymsByLevelError::InvalidLayoutIndex(layout_idx_orig))?;

        let num_levels = key
            .num_levels(layout, self)
            .expect("Could not get levels for this layout");

        if level >= num_levels {
            return Err(KeyGetSymsByLevelError::InvalidLevelIndex(level));
        }

        let level = key
            .groups
            .get(layout)
            .ok_or(KeyGetSymsByLevelError::InvalidLayoutIndex(layout))?
            .levels
            .get(level)
            .ok_or(KeyGetSymsByLevelError::InvalidLevelIndex(level))?;

        // TODO: is this correct?
        let syms_at_level = level.syms.clone().into_iter().flatten().collect();

        Ok(syms_at_level)
    }

    pub fn min_keycode(&self) -> Keycode {
        Keycode::new(self.min_key_code)
    }

    pub fn max_keycode(&self) -> Keycode {
        Keycode::new(self.max_key_code)
    }

    pub fn key_get_name(&self, kc: Keycode) -> Option<String> {
        let key = self.xkb_key(kc.raw())?;

        self.context.atom_text(key.name).map(|s| s.to_string())
    }

    pub fn key_by_name(&self, name: &str) -> Option<Keycode> {
        let mut atom = self.context.atom_lookup(name);

        if let Some(atom_r) = atom {
            let ratom = self.resolve_key_alias(atom_r);
            if let Some(ratom) = ratom {
                atom = Some(ratom);
            }
        }

        let atom = atom?;

        for (kc, key) in self.keys.iter() {
            if key.name == atom {
                return Some(Keycode::new(*kc));
            }
        }

        None
    }

    pub fn key_repeats(&self, kc: Keycode) -> bool {
        let key = self.xkb_key(kc.raw());

        match key {
            Some(key) => key.repeats,
            None => false,
        }
    }

    fn resolve_key_alias(&self, name: Atom) -> Option<Atom> {
        let alias = self.key_aliases.iter().find(|alias| alias.alias == name)?;

        Some(alias.real.to_owned())
    }
}

impl KeymapBuilder<TextV1> {
    // This is behavior from the
    // Keymap struct that is also
    // needed by the KeymapBuilder
    pub(crate) fn resolve_key_alias(&self, name: Atom) -> Option<Atom> {
        let key_aliases = match self.key_aliases.as_ref() {
            Some(v) => v,
            None => return None,
        };

        let alias = key_aliases.iter().find(|alias| alias.alias == name)?;

        Some(alias.real.to_owned())
    }

    // corresponds to XkbKeyByName
    pub(crate) fn get_key_by_name_mut(
        &mut self,
        name: Atom,
        use_aliases: bool,
    ) -> Option<&mut KeyBuilder> {
        let result = self
            .keys
            .iter()
            .find(|(_, k)| k.name == name)
            .map(|(kc, _)| *kc);

        match result {
            Some(kc) => self.keys.get_mut(&kc),
            None if use_aliases => {
                let new_name = self.resolve_key_alias(name)?;

                self.get_key_by_name_mut(new_name, false)
            }
            _ => None,
        }
    }
}

use thiserror::Error;

#[derive(Debug, Error)]
pub enum KeyGetSymsByLevelError {
    #[error("Keymap has no syms for the keycode {0:?}")]
    NoKeyForKeycode(Keycode),

    #[error("Provided layout index is invalid: {0}")]
    InvalidLayoutIndex(LayoutIndex),

    #[error("Provided level index is invalid: {0}")]
    InvalidLevelIndex(LevelIndex),
}

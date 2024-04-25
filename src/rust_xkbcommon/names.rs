pub struct ModName(pub &'static str);

pub struct LedName(pub &'static str);

impl ModName {
    pub const SHIFT: ModName = ModName("Shift");
    pub const CAPS: ModName = ModName("Lock");
    pub const CTRL: ModName = ModName("Control");
    pub const ALT: ModName = ModName("Mod1");
    pub const NUM: ModName = ModName("Mod2");
    pub const LOGO: ModName = ModName("Mod4");

    pub fn name(&self) -> &'static str {
        self.0
    }
}

impl LedName {
    pub const CAPS: LedName = LedName("Caps Lock");
    pub const NUM: LedName = LedName("Num Lock");
    pub const SCROLL: LedName = LedName("Scroll Lock");

    pub fn name(&self) -> &'static str {
        self.0
    }
}

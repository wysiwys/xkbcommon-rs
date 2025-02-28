// TODO: generate this as in `scripts/makekeys.py`
use crate::rust_xkbcommon::RawKeysym;

pub(crate) const DEPRECATED_KEYSYM: u16 = 0xffff;

#[derive(Clone, Copy, Default)]
pub(crate) struct DeprecatedKeysym {
    pub(crate) keysym: RawKeysym,
    pub(crate) offset: u16,
    /* Explicit deprecated aliases start index & count */
    pub(crate) explicit_index: u8,
    pub(crate) explicit_count: u8,
}
impl DeprecatedKeysym {
    const fn new(keysym: RawKeysym, offset: u16, explicit_index: u8, explicit_count: u8) -> Self {
        Self {
            keysym,
            offset,
            explicit_index,
            explicit_count,
        }
    }
}

macro_rules! deprecated_keysyms {

    ($n:expr, $({$keysym:expr, $offset:expr, $explicit_index:expr, $explicit_count:expr}),* $(,)?) => {
        {

        let mut result: [DeprecatedKeysym; $n] = [DeprecatedKeysym::new(0,0,0,0); $n];
        let mut idx = 0;
        $(


            result[idx] = DeprecatedKeysym::new($keysym, $offset, $explicit_index, $explicit_count);

            idx += 1;

        )*
        if idx != $n {
            panic!("Wrong length provided");
        }
        result
        }

    };

}
pub(crate) const DEPRECATED_KEYSYMS: [DeprecatedKeysym; 156] = deprecated_keysyms!(156,
{ 0x00000027, 934              , 0, 0 }, /* Reference: apostrophe. Deprecated: quoteright */
{ 0x00000060, 13381            , 0, 0 }, /* Reference: grave. Deprecated: quoteleft */
{ 0x000000ab, 14464            , 0, 0 }, /* Reference: guillemetleft. Deprecated: guillemotleft */
{ 0x000000ba, 21008            , 0, 0 }, /* Reference: ordmasculine. Deprecated: masculine */
{ 0x000000bb, 14478            , 0, 0 }, /* Reference: guillemetright. Deprecated: guillemotright */
{ 0x000000d0, 12179            , 0, 0 }, /* Reference: ETH. Deprecated: Eth */
{ 0x000000d8, 21453            , 0, 0 }, /* Reference: Oslash. Deprecated: Ooblique */
{ 0x000000de, 25656            , 0, 0 }, /* Reference: THORN. Deprecated: Thorn */
{ 0x000000f8, 21460            , 0, 0 }, /* Reference: oslash. Deprecated: ooblique */
{ 0x000003a2, 19252            , 0, 0 }, /* Reference: kra. Deprecated: kappa */
{ 0x000004a5, 18348            , 0, 0 }, /* Reference: kana_conjunctive. Deprecated: kana_middledot */
{ 0x000004af, 18786            , 0, 0 }, /* Reference: kana_tsu. Deprecated: kana_tu */
{ 0x000004c1, 18308            , 0, 0 }, /* Reference: kana_CHI. Deprecated: kana_TI */
{ 0x000004c2, 18777            , 0, 0 }, /* Reference: kana_TSU. Deprecated: kana_TU */
{ 0x000004cc, 18379            , 0, 0 }, /* Reference: kana_FU. Deprecated: kana_HU */
{ 0x000005e7, 1282             , 0, 0 }, /* Reference: Arabic_ha. Deprecated: Arabic_heh */
{ 0x000006a4, 26369            , 0, 0 }, /* Reference: Ukrainian_ie. Deprecated: Ukranian_je */
{ 0x000006a6, 26344            , 0, 0 }, /* Reference: Ukrainian_i. Deprecated: Ukranian_i */
{ 0x000006a7, 26395            , 0, 0 }, /* Reference: Ukrainian_yi. Deprecated: Ukranian_yi */
{ 0x000006a8, 9574             , 0, 0 }, /* Reference: Cyrillic_je. Deprecated: Serbian_je */
{ 0x000006a9, 9713             , 0, 0 }, /* Reference: Cyrillic_lje. Deprecated: Serbian_lje */
{ 0x000006aa, 9739             , 0, 0 }, /* Reference: Cyrillic_nje. Deprecated: Serbian_nje */
{ 0x000006af, 9068             , 0, 0 }, /* Reference: Cyrillic_dzhe. Deprecated: Serbian_dze */
{ 0x000006b4, 26356            , 0, 0 }, /* Reference: Ukrainian_IE. Deprecated: Ukranian_JE */
{ 0x000006b6, 26332            , 0, 0 }, /* Reference: Ukrainian_I. Deprecated: Ukranian_I */
{ 0x000006b7, 26382            , 0, 0 }, /* Reference: Ukrainian_YI. Deprecated: Ukranian_YI */
{ 0x000006b8, 9562             , 0, 0 }, /* Reference: Cyrillic_JE. Deprecated: Serbian_JE */
{ 0x000006b9, 9700             , 0, 0 }, /* Reference: Cyrillic_LJE. Deprecated: Serbian_LJE */
{ 0x000006ba, 9726             , 0, 0 }, /* Reference: Cyrillic_NJE. Deprecated: Serbian_NJE */
{ 0x000006bf, 9054             , 0, 0 }, /* Reference: Cyrillic_DZHE. Deprecated: Serbian_DZE */
{ 0x000007a5, 13841            , 0, 0 }, /* Reference: Greek_IOTAdieresis. Deprecated: Greek_IOTAdiaeresis */
{ 0x000008a2, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: topleftradical */
{ 0x000008a3, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: horizconnector */
{ 0x000008a6, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: vertconnector */
{ 0x00000aac, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: signifblank */
{ 0x00000abc, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: leftanglebracket */
{ 0x00000abd, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: decimalpoint */
{ 0x00000abe, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: rightanglebracket */
{ 0x00000aca, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: signaturemark */
{ 0x00000acc, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: leftopentriangle */
{ 0x00000acd, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: rightopentriangle */
{ 0x00000ace, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: emopencircle */
{ 0x00000acf, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: emopenrectangle */
{ 0x00000adb, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: filledrectbullet */
{ 0x00000adc, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: filledlefttribullet */
{ 0x00000add, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: filledrighttribullet */
{ 0x00000ade, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: emfilledcircle */
{ 0x00000adf, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: emfilledrect */
{ 0x00000ae0, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: enopencircbullet */
{ 0x00000ae1, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: enopensquarebullet */
{ 0x00000ae2, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: openrectbullet */
{ 0x00000ae3, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: opentribulletup */
{ 0x00000ae4, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: opentribulletdown */
{ 0x00000ae5, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: openstar */
{ 0x00000ae6, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: enfilledcircbullet */
{ 0x00000ae7, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: enfilledsqbullet */
{ 0x00000ae8, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: filledtribulletup */
{ 0x00000ae9, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: filledtribulletdown */
{ 0x00000aea, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: leftpointer */
{ 0x00000aeb, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: rightpointer */
{ 0x00000ba3, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: leftcaret */
{ 0x00000ba6, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: rightcaret */
{ 0x00000ba8, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: downcaret */
{ 0x00000ba9, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: upcaret */
{ 0x00000bc0, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: overbar */
{ 0x00000bc3, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: upshoe */
{ 0x00000bc6, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: underbar */
{ 0x00000bd6, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: downshoe */
{ 0x00000bd8, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: rightshoe */
{ 0x00000bda, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: leftshoe */
{ 0x00000ce1, 16255            , 0, 0 }, /* Reference: hebrew_bet. Deprecated: hebrew_beth */
{ 0x00000ce2, 16436            , 0, 0 }, /* Reference: hebrew_gimel. Deprecated: hebrew_gimmel */
{ 0x00000ce3, 16290            , 0, 0 }, /* Reference: hebrew_dalet. Deprecated: hebrew_daleth */
{ 0x00000ce6, 16721            , 0, 0 }, /* Reference: hebrew_zain. Deprecated: hebrew_zayin */
{ 0x00000ce7, 16278            , 0, 0 }, /* Reference: hebrew_chet. Deprecated: hebrew_het */
{ 0x00000ce8, 16652            , 0, 0 }, /* Reference: hebrew_tet. Deprecated: hebrew_teth */
{ 0x00000cf1, 16576            , 0, 0 }, /* Reference: hebrew_samech. Deprecated: hebrew_samekh */
{ 0x00000cf5, 16402            , 0, 0 }, /* Reference: hebrew_finalzade. Deprecated: hebrew_finalzadi */
{ 0x00000cf6, 16697            , 0, 0 }, /* Reference: hebrew_zade. Deprecated: hebrew_zadi */
{ 0x00000cf7, 16552            , 0, 0 }, /* Reference: hebrew_qoph. Deprecated: hebrew_kuf */
{ 0x00000cfa, 16641            , 0, 0 }, /* Reference: hebrew_taw. Deprecated: hebrew_taf */
{ 0x00000dde, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: Thai_maihanakat_maitho */
{ 0x00000eff, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: Korean_Won */
{ 0x0000fe8a, 11150            , 0, 0 }, /* Reference: dead_schwa. Deprecated: dead_small_schwa */
{ 0x0000fe8b, 11139            , 0, 0 }, /* Reference: dead_SCHWA. Deprecated: dead_capital_schwa */
{ 0x0000ff20, 19955            , 0, 0 }, /* Reference: Multi_key. Deprecated: SunCompose */
{ 0x0000ff37, 8654             , 0, 0 }, /* Reference: Codeinput. Deprecated: Kanji_Bangou, Hangul_Codeinput */
{ 0x0000ff3c, 23169            , 0, 0 }, /* Reference: SingleCandidate. Deprecated: Hangul_SingleCandidate */
{ 0x0000ff3d, 19965            , 0, 0 }, /* Reference: MultipleCandidate. Deprecated: Zen_Koho, Hangul_MultipleCandidate */
{ 0x0000ff3e, 22257            , 0, 0 }, /* Reference: PreviousCandidate. Deprecated: Mae_Koho, Hangul_PreviousCandidate */
{ 0x0000ff55, 22281            , 0, 0 }, /* Reference: Prior. Deprecated: Page_Up, SunPageUp */
{ 0x0000ff56, 20167            , 0, 0 }, /* Reference: Next. Deprecated: Page_Down, SunPageDown */
{ 0x0000ff61, 22275            , 0, 0 }, /* Reference: Print. Deprecated: SunPrint_Screen */
{ 0x0000ff65, 26514            , 0, 0 }, /* Reference: Undo. Deprecated: SunUndo */
{ 0x0000ff66, 22483            , 0, 0 }, /* Reference: Redo. Deprecated: SunAgain */
{ 0x0000ff68, 12646            , 0, 0 }, /* Reference: Find. Deprecated: SunFind */
{ 0x0000ff69, 8489             , 0, 0 }, /* Reference: Cancel. Deprecated: SunStop */
{ 0x0000ff7e, 19891            , 0, 1 }, /* Reference: Mode_switch. Non deprecated aliases: script_switch, ISO_Group_Shift, kana_switch, Arabic_switch, Greek_switch, Hebrew_switch, Hangul_switch. Deprecated: SunAltGraph */
{ 0x0000ff9a, 19187            , 0, 0 }, /* Reference: KP_Prior. Deprecated: KP_Page_Up */
{ 0x0000ff9b, 19155            , 0, 0 }, /* Reference: KP_Next. Deprecated: KP_Page_Down */
{ 0x0000ffc8, 12281            , 0, 0 }, /* Reference: F11. Deprecated: L1 */
{ 0x0000ffc9, 12285            , 0, 0 }, /* Reference: F12. Deprecated: L2 */
{ 0x0000ffca, 12289            , 0, 0 }, /* Reference: F13. Deprecated: L3 */
{ 0x0000ffcb, 12293            , 0, 0 }, /* Reference: F14. Deprecated: L4 */
{ 0x0000ffcc, 12297            , 0, 0 }, /* Reference: F15. Deprecated: L5 */
{ 0x0000ffcd, 12301            , 0, 0 }, /* Reference: F16. Deprecated: L6 */
{ 0x0000ffce, 12305            , 0, 0 }, /* Reference: F17. Deprecated: L7 */
{ 0x0000ffcf, 12309            , 0, 0 }, /* Reference: F18. Deprecated: L8 */
{ 0x0000ffd0, 12313            , 0, 0 }, /* Reference: F19. Deprecated: L9 */
{ 0x0000ffd1, 12320            , 0, 0 }, /* Reference: F20. Deprecated: L10 */
{ 0x0000ffd2, 12324            , 0, 0 }, /* Reference: F21. Deprecated: R1 */
{ 0x0000ffd3, 12328            , 0, 0 }, /* Reference: F22. Deprecated: R2 */
{ 0x0000ffd4, 12332            , 0, 0 }, /* Reference: F23. Deprecated: R3 */
{ 0x0000ffd5, 12336            , 0, 0 }, /* Reference: F24. Deprecated: R4 */
{ 0x0000ffd6, 12340            , 0, 0 }, /* Reference: F25. Deprecated: R5 */
{ 0x0000ffd7, 12344            , 0, 0 }, /* Reference: F26. Deprecated: R6 */
{ 0x0000ffd8, 12348            , 0, 0 }, /* Reference: F27. Deprecated: R7 */
{ 0x0000ffd9, 12352            , 0, 0 }, /* Reference: F28. Deprecated: R8 */
{ 0x0000ffda, 12356            , 0, 0 }, /* Reference: F29. Deprecated: R9 */
{ 0x0000ffdb, 12363            , 0, 0 }, /* Reference: F30. Deprecated: R10 */
{ 0x0000ffdc, 12367            , 0, 0 }, /* Reference: F31. Deprecated: R11 */
{ 0x0000ffdd, 12371            , 0, 0 }, /* Reference: F32. Deprecated: R12 */
{ 0x0000ffde, 12375            , 0, 0 }, /* Reference: F33. Deprecated: R13 */
{ 0x0000ffdf, 12379            , 0, 0 }, /* Reference: F34. Deprecated: R14 */
{ 0x0000ffe0, 12383            , 0, 0 }, /* Reference: F35. Deprecated: R15 */
{ 0x0100055b, 2048             , 0, 0 }, /* Reference: Armenian_accent. Deprecated: Armenian_shesht */
{ 0x0100055c, 2287             , 0, 0 }, /* Reference: Armenian_exclam. Deprecated: Armenian_amanak */
{ 0x0100055d, 2869             , 0, 0 }, /* Reference: Armenian_separation_mark. Deprecated: Armenian_but */
{ 0x0100055e, 2779             , 0, 0 }, /* Reference: Armenian_question. Deprecated: Armenian_paruyk */
{ 0x01000589, 2327             , 0, 0 }, /* Reference: Armenian_full_stop. Deprecated: Armenian_verjaket */
{ 0x0100058a, 2448             , 0, 0 }, /* Reference: Armenian_hyphen. Deprecated: Armenian_yentamna */
{ 0x010006cc, 12505            , 0, 0 }, /* Reference: Farsi_yeh. Deprecated: Arabic_farsi_yeh */
{ 0x01002247, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: notapproxeq */
{ 0x01002248, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: approxeq */
{ 0x100000a8, 17054            , 0, 0 }, /* Reference: hpmute_acute. Deprecated: mute_acute */
{ 0x100000a9, 17121            , 0, 0 }, /* Reference: hpmute_grave. Deprecated: mute_grave */
{ 0x100000aa, 17067            , 0, 0 }, /* Reference: hpmute_asciicircum. Deprecated: mute_asciicircum */
{ 0x100000ab, 17104            , 0, 0 }, /* Reference: hpmute_diaeresis. Deprecated: mute_diaeresis */
{ 0x100000ac, 17086            , 0, 0 }, /* Reference: hpmute_asciitilde. Deprecated: mute_asciitilde */
{ 0x100000af, 17011            , 0, 0 }, /* Reference: hplira. Deprecated: lira */
{ 0x100000be, 16957            , 0, 0 }, /* Reference: hpguilder. Deprecated: guilder */
{ 0x100000ee, 17158            , 0, 0 }, /* Reference: hpYdiaeresis. Deprecated: hpIO, IO */
{ 0x100000f6, 17018            , 0, 0 }, /* Reference: hplongminus. Deprecated: longminus */
{ 0x100000fc, 16911            , 0, 0 }, /* Reference: hpblock. Deprecated: block */
{ 0x1000ff6c, 17134            , 0, 0 }, /* Reference: hpReset. Deprecated: Reset */
{ 0x1000ff6d, 17142            , 0, 0 }, /* Reference: hpSystem. Deprecated: System */
{ 0x1000ff6e, 17151            , 0, 0 }, /* Reference: hpUser. Deprecated: User */
{ 0x1000ff6f, 16919            , 0, 0 }, /* Reference: hpClearLine. Deprecated: ClearLine */
{ 0x1000ff70, 16980            , 0, 0 }, /* Reference: hpInsertLine. Deprecated: InsertLine */
{ 0x1000ff71, 16944            , 0, 0 }, /* Reference: hpDeleteLine. Deprecated: DeleteLine */
{ 0x1000ff72, 16967            , 0, 0 }, /* Reference: hpInsertChar. Deprecated: InsertChar */
{ 0x1000ff73, 16931            , 0, 0 }, /* Reference: hpDeleteChar. Deprecated: DeleteChar */
{ 0x1000ff74, 16901            , 0, 0 }, /* Reference: hpBackTab. Deprecated: BackTab */
{ 0x1000ff75, 16998            , 0, 0 }, /* Reference: hpKP_BackTab. Deprecated: KP_BackTab */
{ 0x1000ff76, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: Ext16bit_L */
{ 0x1000ff77, DEPRECATED_KEYSYM, 0, 0 }, /* Deprecated: Ext16bit_R */
);

pub(crate) const EXPLICIT_DEPRECATED_ALIASES: [usize; 1] = [24029];

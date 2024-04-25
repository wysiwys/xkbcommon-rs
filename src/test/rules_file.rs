use crate::rust_xkbcommon::*;
use crate::test::*;

use crate::context::Context;

pub(super) struct TestData<'s> {
    // Rules file
    pub(super) rules: &'s str,

    // Input
    pub(super) model: &'s str,
    pub(super) layout: &'s str,
    pub(super) variant: &'s str,
    pub(super) options: &'s str,

    // Expected output
    pub(super) keycodes: &'s str,
    pub(super) types: &'s str,
    pub(super) compat: &'s str,
    pub(super) symbols: &'s str,

    // Or set this if `from_rules` should fail
    pub(super) should_fail: bool,
}

pub(super) fn test_rules(ctx: &mut Context, data: TestData) -> bool {
    eprintln!(
        "\n\nChecking: {}\t{}\t{}\t{}\t{}",
        data.rules, data.model, data.layout, data.variant, data.options
    );

    let rmlvo = RuleNames {
        rules: Some(data.rules.into()),
        model: Some(data.model.into()),
        layout: Some(data.layout.into()),
        variant: Some(data.variant.into()),
        options: Some(data.options.into()),
    };

    if data.should_fail {
        eprintln!("Expecting: FAILURE");
    } else {
        eprintln!(
            "Expecting: {}\t{}\t{}\t{}",
            &data.keycodes, &data.types, &data.compat, &data.symbols
        );
    }

    use crate::xkbcomp::xkbcomp::ComponentNames;
    let kccgst = ComponentNames::from_rules(ctx, &rmlvo);

    let kccgst = match kccgst {
        Ok(kccgst) => kccgst,
        Err(e) => {
            eprintln!("Received : FAILURE: {:?}", e);
            return data.should_fail;
        }
    };

    eprintln!(
        "Received: {}\t{}\t{}\t{}\t",
        kccgst.keycodes, kccgst.types, kccgst.compat, kccgst.symbols
    );

    let passed = kccgst.keycodes == data.keycodes
        && kccgst.types == data.types
        && kccgst.compat == data.compat
        && kccgst.symbols == data.symbols;

    passed
}

#[test]
fn rules_file_test_rules() {
    let mut context = test_get_context(TestContextFlags::empty()).unwrap();

    let utf8_with_bom = TestData {
        rules: "utf-8_with_bom",
        model: "my_model",
        variant: "my_variant",
        layout: "my_layout",
        options: "my_option",

        keycodes: "my_keycodes",
        types: "my_types",
        compat: "my_compat|some:compat",
        symbols: "my_symbols+extra_variant",

        should_fail: false,
    };

    assert!(test_rules(&mut context, utf8_with_bom));

    let utf16le_with_bom = TestData {
        rules: "utf-16le_with_bom",
        model: "my_model",
        variant: "my_variant",
        layout: "my_layout",
        options: "my_option",

        keycodes: "my_keycodes",
        types: "my_types",
        compat: "my_compat|some:compat",
        symbols: "my_symbols+extra_variant",

        should_fail: true,
    };

    assert!(test_rules(&mut context, utf16le_with_bom));

    let test1 = TestData {
        rules: "simple",
        model: "my_model",
        layout: "my_layout",
        variant: "my_variant",
        options: "my_option",

        keycodes: "my_keycodes",
        types: "my_types",
        compat: "my_compat|some:compat",
        symbols: "my_symbols+extra_variant",

        should_fail: false,
    };

    assert!(test_rules(&mut context, test1));

    let test2 = TestData {
        rules: "simple",
        model: "",
        layout: "",
        variant: "",
        options: "",

        keycodes: "default_keycodes",
        types: "default_types",
        compat: "default_compat",
        symbols: "default_symbols",

        should_fail: false,
    };

    assert!(test_rules(&mut context, test2));

    let test3 = TestData {
        rules: "groups",
        model: "pc104",
        layout: "foo",
        variant: "",
        options: "",

        keycodes: "something(pc104)",
        types: "default_types",
        compat: "default_compat",
        symbols: "default_symbols",

        should_fail: false,
    };

    assert!(test_rules(&mut context, test3));

    let test4 = TestData {
        rules: "groups",
        model: "foo",
        layout: "ar",
        variant: "bar",
        options: "",

        keycodes: "default_keycodes",
        types: "default_types",
        compat: "default_compat",
        symbols: "my_symbols+(bar)",

        should_fail: false,
    };

    assert!(test_rules(&mut context, test4));

    let test5 = TestData {
        rules: "simple",
        model: "",
        layout: "my_layout,second_layout",
        variant: "my_variant",
        options: "my_option",

        keycodes: "N/A",
        types: "N/A",
        compat: "N/A",
        symbols: "N/A",

        should_fail: true,
    };

    assert!(test_rules(&mut context, test5));

    let test6 = TestData {
        rules: "index",
        model: "",
        layout: "br,al,cn,az",
        variant: "",
        options: "some:opt",

        keycodes: "default_keycodes",
        types: "default_types",
        compat: "default_compat",
        symbols: "default_symbols+extra:1+extra:2+extra:3+extra:4",

        should_fail: false,
    };

    assert!(test_rules(&mut context, test6));

    let test7 = TestData {
        rules: "multiple-options",
        model: "my_model",
        layout: "my_layout",
        variant: "my_variant",
        options: "option3,option1,colon:opt,option11",

        keycodes: "my_keycodes",
        types: "my_types",
        compat: "my_compat+some:compat+group(bla)",
        symbols: "my_symbols+extra_variant+compose(foo)+keypad(bar)+altwin(menu)",

        should_fail: false,
    };

    assert!(test_rules(&mut context, test7));
}

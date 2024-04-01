

use crate::log_init;
use crate::test::*;
use crate::rust_xkbcommon::*;


use crate::context::Context;
use crate::errors::*;
use crate::keymap::Keymap;

use std::sync::{Arc, Mutex};


use evdev::Key;



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
    pub(super) should_fail: bool

}

pub(super) fn test_rules(ctx: &mut Context,
    data: TestData) -> bool {

    let passed: bool;
    
    eprintln!("\n\nChecking: {}\t{}\t{}\t{}\t{}",
        data.rules, data.model, data.layout,
        data.variant, data.options);

    let rmlvo = RuleNames {
        rules: Some(data.rules.into()),
        model: Some(data.model.into()),
        layout: Some(data.layout.into()),
        variant: Some(data.variant.into()),
        options: Some(data.options.into())
    };

    if data.should_fail {
        eprintln!("Expecting: FAILURE");
    } else {
        eprintln!("Expecting: {}\t{}\t{}\t{}",
            &data.keycodes, &data.types,
            &data.compat, &data.symbols);
    }

    use crate::xkbcomp::xkbcomp::ComponentNames;
    let kccgst = ComponentNames::from_rules(ctx, &rmlvo);

    let kccgst = match kccgst {
        Ok(kccgst) => kccgst,
        Err(_) => {
            eprintln!("Received : FAILURE");
            return data.should_fail;
        }
    };

    eprintln!("Received: {}\t{}\t{}\t{}\t",
        kccgst.keycodes, kccgst.types, kccgst.compat,
        kccgst.symbols);

    let passed = 
        kccgst.keycodes == data.keycodes
        && kccgst.types == data.types
        && kccgst.compat == data.compat
        && kccgst.symbols == data.symbols ;

    passed

}

#[test]
fn rules_file_test_rules() {


    let mut context = test_get_context(TestContextFlags::empty()).unwrap();

    let test1 = TestData {
        rules: "simple".into(),
        model: "my_model".into(),
        layout: "my_layout".into(),
        variant: "my_variant".into(),
        options: "my_option".into(),

        keycodes: "my_keycodes".into(),
        types: "my_types".into(),
        compat: "my_compat|some:compat".into(),
        symbols: "my_symbols+extra_variant".into(),

        should_fail: false
    };

    assert!(test_rules(&mut context, test1));

    let test2 = TestData {
        rules: "simple".into(),
        model: "".into(),
        layout: "".into(),
        variant: "".into(),
        options: "".into(),

        keycodes: "default_keycodes".into(),
        types: "default_types".into(),
        compat: "default_compat".into(),
        symbols: "default_symbols".into(),

        should_fail: false
    };

    assert!(test_rules(&mut context, test2));
    
    let test3 = TestData {
        rules: "groups".into(),
        model: "pc104".into(),
        layout: "foo".into(),
        variant: "".into(),
        options: "".into(),

        keycodes: "something(pc104)".into(),
        types: "default_types".into(),
        compat: "default_compat".into(),
        symbols: "default_symbols".into(),

        should_fail: false
    };

    assert!(test_rules(&mut context, test3));
    
    let test4 = TestData {
        rules: "groups".into(),
        model: "foo".into(),
        layout: "ar".into(),
        variant: "bar".into(),
        options: "".into(),

        keycodes: "default_keycodes".into(),
        types: "default_types".into(),
        compat: "default_compat".into(),
        symbols: "my_symbols+(bar)".into(),

        should_fail: false
    };

    assert!(test_rules(&mut context, test4));

    let test5 = TestData {
        rules: "simple".into(),
        model: "".into(),
        layout: "my_layout,second_layout".into(),
        variant: "my_variant".into(),
        options: "my_option".into(),

        keycodes: "N/A".into(),
        types: "N/A".into(),
        compat: "N/A".into(),
        symbols: "N/A".into(),

        should_fail: true,
    };
    
    assert!(test_rules(&mut context, test5));
        
    let test6 = TestData {
        rules: "index".into(),
        model: "".into(),
        layout: "br,al,cn,az".into(),
        variant: "".into(),
        options: "some:opt".into(),

        keycodes: "default_keycodes".into(),
        types: "default_types".into(),
        compat: "default_compat".into(),
        symbols: "default_symbols+extra:1+extra:2+extra:3+extra:4".into(),

        should_fail: false
    };

    assert!(test_rules(&mut context, test6));

    let test7 = TestData {
        rules: "multiple-options".into(),
        model: "my_model".into(),
        layout: "my_layout".into(),
        variant: "my_variant".into(),
        options: "option3,option1,colon:opt,option11".into(),

        keycodes: "my_keycodes".into(),
        types: "my_types".into(),
        compat: "my_compat+some:compat+group(bla)".into(),
        symbols: "my_symbols+extra_variant+compose(foo)+keypad(bar)+altwin(menu)".into(),

        should_fail: false
    };

    assert!(test_rules(&mut context, test7));
}



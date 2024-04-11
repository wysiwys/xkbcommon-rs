use xkb_rust::Context;
use xkb_rust::Keymap;
use xkb_rust::KeymapFormat;

fn main() {

    let context = Context::new(0).unwrap();

    let string = std::fs::read_to_string("../../24_xkb_rust/xkb_rust/stringcomp").unwrap();

    let _ = Keymap::new_from_string(context, string, KeymapFormat::TextV1, 0).unwrap();

    


}

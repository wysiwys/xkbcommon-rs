use xkeysym::Keysym;

pub fn keysym_to_utf8(keysym: &Keysym) -> Option<Vec<u8>> {

    let mut buf = Vec::with_capacity(4);
    // TODO: does this work?
    let c = keysym.key_char()?;

    c.encode_utf8(&mut buf);

    Some(buf)


}

pub fn keysym_to_utf32(keysym: &Keysym) -> Option<char> {
    keysym.key_char()
}

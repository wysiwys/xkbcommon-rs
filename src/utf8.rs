pub(super) fn is_valid_utf8(buf: &Vec<u8>) -> bool {

    // TODO: test this
    std::str::from_utf8(buf).is_ok()


}

pub(crate) enum KeySeqState {
    Down, Repeat, Up, Both, Next, Finish }

    
bitflags::bitflags! {

    pub(crate) struct TestContextFlags: u8 {

        const NO_FLAG = 0;
        const ALLOW_ENVIRONMENT_NAMES = 1 << 0;
    }

}

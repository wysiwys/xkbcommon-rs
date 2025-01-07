# xkbcommon-rs

[![Crates.io](https://img.shields.io/crates/v/xkbcommon-rs.svg)](https://crates.io/crates/xkbcommon-rs)
[![docs.rs](https://img.shields.io/docsrs/xkbcommon-rs/latest)](https://docs.rs/xkbcommon-rs/latest/xkbcommon_rs/)

A port of [libxkbcommon](https://github.com/xkbcommon/libxkbcommon) in safe Rust. Notably, this crate provides `Send + Sync` types for Keymap and State.

This safe Rust port intends to provide an alternative to the existing C bindings crates, e.g. for projects that are built entirely in safe Rust. It aims to release versions that match the releases of the original `libxkbcommon`.

In Cargo.toml:
```
xkbcommon-rs = "0.1.0"
```
 
 ## Use in Wayland client application

The Keymap and State provided by this crate can be used to represent keyboard state in a Wayland client. For example, `xkbcommon-rs` can be used in combination with Smithay's `wayland-client` crate.

 ### Setting up the keymap and the state
 ```rust
 use xkbcommon_rs::*;

 let keymap = Keymap::new_from_string(
     Context::new(0).unwrap(),
     string, /* Read from the OwnedFd provided by the Wayland compositor */
     KeymapFormat::TextV1,
     0).unwrap();

 let mut state = State::new(keymap);

 ```

 ### Getting keyboard symbols and updating the state


 ```rust
 // Get syms before updating state
 let sym = state.key_get_one_sym(keycode)?;

 // Update state with the parameters provided by the wl_keyboard::Event::Modifiers{..} event
 state.update_mask(
     mods_depressed, mods_latched, mods_locked,
     0, 0, group as usize);
 ```

 For more information on using `State::update_mask()` in a Wayland client, see <https://wayland-book.com/seat/keyboard.html>.

## Info

This crate is intended for use in a Wayland client. Compositor-side functionality is also provided. 

The current version corresponds to libxkbcommon version `1.7.0`. This crate strives to be as close a reimplementation of the original `libxkbcommon` library as possible, although some features are not implemented yet.

## Contributing

Feedback and PRs are welcome.

 ## Version table
| `xkbcommon-rs` version | `libxkbcommon` version |
|------------------------|------------------------|
| 0.1.1 (unreleased)     | 1.7.0                  |
| 0.1.0                  | 1.7.0                  |

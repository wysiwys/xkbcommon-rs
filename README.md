### rust_xkbcommon


A port of [libxkbcommon](https://github.com/xkbcommon/libxkbcommon) in safe Rust.

This crate is intended for use in a Wayland client or compositor.

 Corresponds to libxkbcommon version `1.7.0`.
 
 ### Use in client application

 Setting up the keymap and the state:
 ```rust
 use xkbcommon_rs::*;

 let keymap = Keymap::new_from_string(
     Context::new(0).unwrap(),
     string, /* Read from the OwnedFd provided by the Wayland compositor */
     KeymapFormat::TextV1,
     0).unwrap();

 let mut state = State::new(keymap);

 ```

 Getting keyboard symbols and updating the state:


 ```rust
 // Get syms before updating state
 let sym = state.key_get_one_sym(keycode)?;

 // Update state with the parameters provided by the wl_keyboard::Event::Modifiers{..} event
 state.update_mask(
     mods_depressed, mods_latched, mods_locked,
     0, 0, group as usize);
 ```

 For more information on using `State::update_mask()` in a Wayland client, see <https://wayland-book.com/seat/keyboard.html>.


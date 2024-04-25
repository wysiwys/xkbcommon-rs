use std::fs::*;

use nix::unistd::AccessFlags;

/// If we can, check that the permission bits of the file permit the requested access.
#[cfg(target_os = "linux")]
pub(super) fn check_permissions(meta: &Metadata, requested_mode: AccessFlags) -> bool {
    use std::os::unix::fs::PermissionsExt;

    // Use the Unix-specifix extensions to `fs::Permissions`,
    // since the regular permissions only have a read-only bit set.

    let permissions = meta.permissions();
    let actual_mode = permissions
        .mode()
        .try_into()
        .expect("Mode requested is negative");

    // truncate: leave any unknown bits unset
    let actual_mode = AccessFlags::from_bits_truncate(actual_mode);

    actual_mode.intersects(requested_mode)
}

#[cfg(not(target_os = "linux"))]
pub(super) fn check_permissions(meta: &Metadata, requested_mode: AccessFlags) -> bool {
    todo!()
}

pub(super) fn one_bit_set(x: u32) -> bool {
    x > 0 && (x & (x - 1)) == 0
}

// TODO: `open_file`

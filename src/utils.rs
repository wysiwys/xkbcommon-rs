use std::fs::*;


// TODO: original C version has directives
// for these cases. What are they for?
#[cfg(any(have_eaccess, have_euidaccess))]
pub(super) fn check_permissions(meta: &Metadata, mode: i32) -> bool {

    let permissions = meta.permissions();
    
    (permissions.mode() & mode) != 0
    

}

#[cfg(not(any(have_eaccess, have_euidaccess)))] 
pub(super) fn check_permissions(meta: &Metadata, mode: i32) -> bool {

    true

}

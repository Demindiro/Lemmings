#![no_std]
#![no_main]
#![forbid(improper_ctypes_definitions)]

extern crate lemmings_door;

use lemmings_door::{ApiId, Cookie};
use lemmings_idl_pci::Pci;

fn main() {
    let door =
        lemmings_door::door_list(Some(ApiId(Pci::ID)), Cookie(0)).expect("no PCI door found");
    let door =
        lemmings_door::door_list(Some(ApiId(Pci::ID)), Cookie(0)).expect("no PCI door found");
    todo!("{door:?}");
}

#[unsafe(no_mangle)]
extern "sysv64" fn _start() {
    main()
}

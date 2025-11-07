#![no_std]
#![no_main]
#![forbid(improper_ctypes_definitions)]

extern crate lemmings_door;

use lemmings_door::{ApiId, Cookie, Log};
use lemmings_idl_pci::Pci;

fn main() {
    let (door, _) = lemmings_door::door_find::<Pci>(Cookie(0)).expect("no PCI door found");
    let door = door.get();
    let lemmings_idl_pci::Configuration {
        base,
        segment_group,
        bus_start,
        bus_end,
    } = door.configuration();
    assert!(u8::from(bus_start) == 0 && u8::from(bus_end) == 0xff);
    let pci = unsafe { base.0.cast::<lemmings_pci::Pci>().as_ref() };
    pci.list(|bdf, hdr| {
        use core::fmt::Write;
        let vendor = hdr.vendor_id();
        let device = hdr.device_id();
        let _ = writeln!(Log::new(), "{bdf:?} -> {vendor:04x}:{device:04x}");
    });
}

#[unsafe(no_mangle)]
extern "sysv64" fn _start() {
    main()
}

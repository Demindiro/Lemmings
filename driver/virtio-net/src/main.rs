#![no_std]
#![no_main]
#![forbid(improper_ctypes_definitions)]

extern crate lemmings_door;

fn main() {
    todo!();
}

#[unsafe(no_mangle)]
extern "sysv64" fn _start() {
    main()
}

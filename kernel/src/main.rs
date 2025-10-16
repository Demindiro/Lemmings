#![no_std]
#![no_main]

mod sys;

use core::arch::asm;

fn fail(reason: &str) -> ! {
    sys::print(reason);
    unsafe {
        asm! {
            "cli",
            "hlt",
            options(noreturn),
        }
    }
}

#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo<'_>) -> ! {
    fail("!!!PANIC!!!");
}

fn main() {
    sys::print("Hello, world! I am KERNAL\n");
    unsafe {
        for k in 0..200 {
        for i in 0..200 {
            *(0xc000_0000 as *mut u32).add(k*1024).add(i) = 0xffffffff - (i + (k << 8)) as u32;
        }
        }
    }
}

#[unsafe(no_mangle)]
fn _start(sys_entry: *const ()) -> ! {
    unsafe {
        sys::ENTRY = sys_entry;
    }
    main();
    sys::exit_ok();
}

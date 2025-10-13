#![no_std]
#![no_main]

use core::arch::asm;

mod sys {
    pub fn print(_s: &str) {
    }
}

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

#[unsafe(no_mangle)]
extern "sysv64" fn _start() -> ! {
    fail("TODO");
}

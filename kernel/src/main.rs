#![no_std]
#![no_main]

#[macro_use]
mod sys;
mod page;
mod thread;
mod time;

use core::arch::asm;

mod private {
    /// This token MUST ONLY be constructed in [`_start`]!
    ///
    /// It is used to indicate a function may only be called during kernel setup.
    /// This reduced the amount of unsafe annotations required.
    pub struct KernelEntryToken(());

    impl KernelEntryToken {
        /// # Safety
        ///
        /// May only be called in [`_start`].
        pub unsafe fn new() -> Self {
            Self(())
        }
    }
}

pub use private::KernelEntryToken;

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
fn panic_handler(info: &core::panic::PanicInfo<'_>) -> ! {
    log!("[KERNEL] PANIC: {info}");
    sys::halt();
}

fn main() {
    unsafe {
        for k in 0..200 {
        for i in 0..200 {
            *(0xc000_0000 as *mut u32).add(k*1024).add(i) = 0xffffffff - (i + (k << 8)) as u32;
        }
        }
    }
}

#[unsafe(no_mangle)]
extern "sysv64" fn _start(sys_entry: *const ()) -> ! {
    // SAFETY: this is the _start function
    let token = unsafe { KernelEntryToken::new() };
    unsafe {
        sys::ENTRY = sys_entry;
    }
    let mut threads = thread::ThreadManager::new();
    threads.enter(thread::Priority::Regular, main, token);
}

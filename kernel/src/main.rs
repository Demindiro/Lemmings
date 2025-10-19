#![no_std]
#![no_main]

mod archive;
mod critical_section;
mod page;
mod thread;
mod time;
mod sync;

use lemmings_qemubios::{sys, log, dbg};

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

#[inline]
fn entry(entry: &lemmings_qemubios::Entry) -> ! {
    // SAFETY: this is the _start function
    let token = unsafe { KernelEntryToken::new() };
    let token = page::init(entry, token);
    let token = archive::init(entry, token);
    let mut threads = thread::ThreadManager::new();
    threads.enter(thread::Priority::Regular, main, token);
}

lemmings_qemubios::entry!(entry);

// compiler_builtins doesn't build???
#[unsafe(no_mangle)]
unsafe extern "C" fn memset(mut dst: *mut u8, c: i32, n: usize) -> *mut u8 {
    unsafe {
        core::arch::asm! {
            "rep stosb",
            in("al") c as u8,
            inout("rdi") dst => dst,
            inout("rcx") n => _,
        }
    }
    dst
}

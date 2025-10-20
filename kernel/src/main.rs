#![no_std]
#![no_main]
#![feature(slice_as_chunks)] // stabilized in 1.88, but Guix is on 1.85 as of writing

#[macro_use]
extern crate lemmings_qemubios;

mod archive;
mod critical_section;
mod elf;
mod page;
mod thread;
mod time;
mod sync;

use lemmings_qemubios::sys;

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
    let init = archive::root().get("init").expect("no init");
    let init = init.as_file().expect("init is not a file");
    let init = elf::load(init.data()).expect("failed to parse init");
    unsafe { core::mem::transmute::<_, extern "sysv64" fn() -> !>(init)() }
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

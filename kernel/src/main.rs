#![no_std]
#![no_main]
#![deny(improper_ctypes_definitions)]
#![feature(slice_as_chunks)] // stabilized in 1.88, but Guix is on 1.85 as of writing

#[macro_use]
extern crate lemmings_qemubios;

#[macro_use]
mod door;

mod arch;
mod archive;
mod critical_section;
mod elf;
mod ffi;
mod framebuffer;
mod page;
mod thread;
mod time;
mod sync;
mod sys;

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
    lemmings_qemubios::sys::halt();
}

// do not inline so the stupid compiler frees up stack space before entering init.
#[inline(never)]
fn main_init() -> elf::Entry {
    arch::door::register();
    archive::door::register();
    //framebuffer::door::register();
    let init = archive::root().get("init").expect("no init");
    let init = init.as_file().expect("init is not a file");
    let init = elf::load(init.data()).expect("failed to parse init");
    init
}

extern "sysv64" fn main() {
    let init = main_init();
    unsafe { core::mem::transmute::<_, extern "sysv64" fn()>(init)() }
}

#[inline]
extern "sysv64" fn entry(entry: &lemmings_qemubios::Entry) -> ! {
    unsafe {
        use lemmings_x86_64::{cr0, cr4};
        cr0::update(|x| x | cr0::WRITE_PROTECT);
        cr4::update(|x| x | cr4::FSGSBASE);
    }
    // SAFETY: this is the _start function
    let token = unsafe { KernelEntryToken::new() };
    let token = framebuffer::init(entry, token);
    let token = arch::init(entry, token);
    let token = page::init(entry, token);
    let token = archive::init(entry, token);
    let token = sys::init(entry, token);
    thread::init(main, token)
}

lemmings_qemubios::entry!(entry);

// compiler_builtins doesn't build???
#[unsafe(no_mangle)]
unsafe extern "C" fn memset(dst: *mut u8, c: i32, n: usize) -> *mut u8 {
    unsafe {
        core::arch::asm! {
            "rep stosb",
            in("al") c as u8,
            inout("rdi") dst => _,
            inout("rcx") n => _,
        }
    }
    dst
}

#[unsafe(no_mangle)]
unsafe extern "C" fn memcpy(dst: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    unsafe {
        core::arch::asm! {
            "rep movsb",
            inout("rdi") dst => _,
            inout("rsi") src => _,
            inout("rcx") n => _,
        }
    }
    dst
}

#[unsafe(no_mangle)]
unsafe extern "C" fn memcmp(mut x: *const u8, mut y: *const u8, n: usize) -> i32 {
    // rep cmpsb is slow, so do a manual loop
    unsafe {
        for _ in 0..n {
            if x.read() != y.read() {
                return i32::from(x.read()) - i32::from(y.read())
            }
            x = x.byte_add(1);
            y = y.byte_add(1);
        }
    }
    0
}

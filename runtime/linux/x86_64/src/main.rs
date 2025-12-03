#![no_std]
#![no_main]
#![deny(improper_ctypes_definitions)]
#![forbid(
    private_bounds,
    undefined_naked_function_abi,
    unsafe_op_in_unsafe_fn,
    unused_must_use
)]
#![feature(slice_as_chunks)] // stabilized in 1.88, but Guix is on 1.85 as of writing
#![feature(naked_functions)] // stabilized in 1.88

mod syscall;
#[macro_use]
mod sys;
#[macro_use]
mod door;
mod archive;
mod elf;
mod linux;
mod page;

fn load_init() {
    let init = match archive::root()
        .find("init")
        .expect("no init program present in archive")
    {
        archive::Item::Dir(_) => panic!("init program should be a file, not a directory"),
        archive::Item::File(x) => x,
    };
    let entry = elf::load(init.data()).expect("failed to load init ELF program");
    let entry = unsafe { core::mem::transmute::<_, unsafe extern "sysv64" fn()>(entry) };
    // SAFETY: pray
    unsafe { (entry)() }
}

#[panic_handler]
fn panic(info: &core::panic::PanicInfo<'_>) -> ! {
    log!("PANIC: {info}");
    unsafe {
        syscall::kill(syscall::getpid(), syscall::SIGABRT);
        // just in case...
        syscall::exit(66);
    }
}

unsafe extern "C" fn entry(env: *const usize) {
    unsafe { linux::init(env) };
    unsafe { sys::init() };
    unsafe { archive::init() };
    archive::door::register();
    load_init();
    todo!("RIP harambe");
}

#[unsafe(no_mangle)]
#[naked]
unsafe extern "C" fn _start() -> ! {
    unsafe {
        core::arch::naked_asm! {
            "mov eax, 60",
            "mov edi, 42",
            "mov rdi, rsp",
            "call {entry}",
            "ud2",
            entry = sym entry,
        }
    }
}

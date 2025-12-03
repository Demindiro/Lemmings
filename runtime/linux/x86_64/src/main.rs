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
mod archive;
mod linux;

fn load_init() {
    let res = unsafe { syscall::open("init\0".as_ptr(), syscall::O_RDONLY, 0) };
    dbg!(res);
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

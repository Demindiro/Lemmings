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

#[panic_handler]
fn panic(_: &core::panic::PanicInfo<'_>) -> ! {
    unsafe {
        syscall::kill(syscall::getpid(), syscall::SIGABRT);
        // just in case...
        syscall::exit(66);
    }
}

#[unsafe(no_mangle)]
#[naked]
unsafe extern "C" fn _start() {
    unsafe {
        core::arch::naked_asm! {
            "mov eax, 60",
            "mov edi, 42",
            "syscall",
        }
    }
}

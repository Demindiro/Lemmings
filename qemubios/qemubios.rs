#![no_std]

mod sys {
    use core::arch::asm;

    pub fn print(s: &str) {
        unsafe {
            asm! {
                "call sys_print",
                in("rcx") s.len(),
                in("rsi") s.as_ptr(),
                lateout("rcx") _,
                lateout("rdx") _,
                lateout("rsi") _,
            }
        }
    }

    pub fn exit_ok() -> ! {
        unsafe {
            asm! {
                "jmp sys_exit",
                in("edx") 0,
                options(noreturn),
            }
        }
    }

    pub fn exit_err() -> ! {
        unsafe {
            asm! {
                "jmp sys_exit",
                in("edx") 1,
                options(noreturn),
            }
        }
    }
}

#[panic_handler]
fn panic(_: &core::panic::PanicInfo<'_>) -> ! {
    sys::exit_err()
}


#[unsafe(no_mangle)]
extern "C" fn boot() -> ! {
    sys::print("Hello from Rust!\n");
    sys::exit_ok()
}

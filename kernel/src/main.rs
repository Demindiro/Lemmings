#![no_std]
#![no_main]

use core::arch::asm;

mod sys {
    pub static mut ENTRY: *const () = core::ptr::null();

    macro_rules! sys {
        ($nr:literal ["rsi" $val:expr] ["rcx" $val2:expr]) => {{
            let ret: isize;
            core::arch::asm! {
                "call [rip + {}]",
                sym ENTRY,
                in("eax") $nr,
                // spec claims it accepts a string literal token
                // but rustc disagrees, so do the stupid thing instead :)
                //
                // error: expected register class or explicit register
                //   --> src/main.rs:17:22
                //    |
                // 17 |                 $(in($reg) $val,)*
                //    |                      ^^^^
                // ...
                // 29 |         unsafe { sys!(1 ["rsi" s.as_ptr()] ["rcx" s.len()]) }
                //    |                  ------------------------------------------ in this macro invocation
                //    |
                //    = note: this error originates in the macro `sys` (in Nightly builds, run with -Z macro-backtrace for more info)
                in("rsi") $val,
                in("rcx") $val2,
                lateout("rax") ret,
                lateout("rcx") _,
                lateout("rdx") _,
                lateout("rsi") _,
                lateout("rdi") _,
            };
            ret
        }};
        (noreturn $nr:literal ["rdx" $val:expr]) => {{
            let ret: isize;
            core::arch::asm! {
                "call [rip + {}]",
                sym ENTRY,
                in("eax") $nr,
                in("rdx") $val,
                options(noreturn)
            };
            ret
        }};
    }

    pub fn print(s: &str) {
        unsafe { sys!(0 ["rsi" s.as_ptr()] ["rcx" s.len()]); }
    }

    pub fn exit_ok() -> ! {
        unsafe { sys!(noreturn 1 ["rdx" 0]); }
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

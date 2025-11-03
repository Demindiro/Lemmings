use core::arch::asm;

macro_rules! sys {
    ($nr:literal ["rsi" $val:expr] ["rcx" $val2:expr]) => {{
        let ret: isize;
        asm! {
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
        asm! {
            "call [rip + {}]",
            sym ENTRY,
            in("eax") $nr,
            in("rdx") $val,
            options(noreturn)
        };
    }};
}

#[doc(hidden)]
pub static mut ENTRY: *const () = core::ptr::null();

#[inline]
pub fn print(s: &str) {
    unsafe {
        sys!(0 ["rsi" s.as_ptr()] ["rcx" s.len()]);
    }
}

#[inline]
pub fn exit_ok() -> ! {
    unsafe {
        sys!(noreturn 1 ["rdx" 0]);
    }
}

#[inline]
pub fn halt() -> ! {
    unsafe { asm!("cli", "2: hlt", "jmp 2b", options(noreturn, nostack, nomem)) }
}

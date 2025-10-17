use core::{arch::asm, fmt};

#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {{
        use core::fmt::Write;
        let _ = writeln!($crate::sys::Log, $($arg)*);
    }};
}

#[macro_export]
macro_rules! dbg {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
        $crate::log!("[{}:{}:{}]", file!(), line!(), column!())
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                $crate::log!("[{}:{}:{}] {} = {:#?}",
                    file!(),
                    line!(),
                    column!(),
                    stringify!($val),
                    // The `&T: Debug` check happens here (not in the format literal desugaring)
                    // to avoid format literal related messages and suggestions.
                    &&tmp as &dyn core::fmt::Debug,
                );
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($($crate::dbg!($val)),+,)
    };
}

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

pub struct Log;

impl fmt::Write for Log {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        print(s);
        Ok(())
    }
}

pub fn print(s: &str) {
    unsafe { sys!(0 ["rsi" s.as_ptr()] ["rcx" s.len()]); }
}

pub fn exit_ok() -> ! {
    unsafe { sys!(noreturn 1 ["rdx" 0]); }
}

pub fn halt() -> ! {
    unsafe { asm!("cli", "2: hlt", "jmp 2b", options(noreturn)) }
}

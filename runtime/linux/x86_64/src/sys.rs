use crate::syscall;
use core::fmt::{self, Write};

#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {{
        use core::fmt::Write;
        $crate::sys::with_log(|mut log| {
            log.prefix_time();
            let _ = writeln!(&mut log, $($arg)*);
        });
    }};
}

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {{
        if option_env!("KERNEL_DEBUG").is_some() {
            // TODO auto include function name
            // ... why the hell does Rust *still* not provide a __func__ equivalent?
            use core::fmt::Write;
            $crate::sys::with_log(|mut log| {
                log.prefix_time();
                let _ = writeln!(&mut log, $($arg)*);
            });
        }
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

const STDIN_FD: i32 = 0;
const STDOUT_FD: i32 = 1;
const STDERR_FD: i32 = 2;

pub struct Log(());

struct Stderr;

impl Log {
    #[doc(hidden)]
    pub fn prefix_time(&mut self) {
        let _ = write!(self, "[TODO time] ");
    }
}

impl Write for Log {
    fn write_str(&mut self, c: &str) -> fmt::Result {
        Stderr.write_str(c)
    }
}

impl Write for Stderr {
    fn write_str(&mut self, c: &str) -> fmt::Result {
        unsafe { syscall::write(STDERR_FD, c.as_ptr(), c.len()) };
        Ok(())
    }
}

#[doc(hidden)]
pub fn with_log<R, F>(f: F) -> R
where
    F: FnOnce(Log) -> R,
{
    (f)(Log(()))
}

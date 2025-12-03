use crate::{
    door::{self, ApiId, Cookie, Table},
    syscall,
};
use core::{
    fmt::{self, Write},
    ptr::NonNull,
    slice,
};

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

macro_rules! systable {
    ($($id:literal $fn:ident)*) => {
        static TABLE: [SysFn; systable!(+ $($fn)*)] = [
            $(SysFn($fn as *const ()),)*
        ];
    };
    (+) => { 0 };
    (+ $f:ident $($fn:ident)*) => { 1+systable!(+ $($fn)*) };
}

systable! {
    0 log
    1 panic
    2 panic_begin
    3 panic_push
    4 panic_end
    5 door_list
    6 door_find
    7 door_register
}

const STDIN_FD: i32 = 0;
const STDOUT_FD: i32 = 1;
const STDERR_FD: i32 = 2;

pub struct Log(());

#[allow(dead_code)]
struct SysFn(*const ());

struct Stderr;

#[repr(C)]
struct Slice<T> {
    base: *const T,
    len: usize,
}

#[repr(C)]
struct Tuple2<A, B>(A, B);

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

impl Slice<u8> {
    unsafe fn as_str(&self) -> &str {
        let s = unsafe { slice::from_raw_parts(self.base, self.len) };
        unsafe { core::str::from_utf8_unchecked(s) }
    }
}

unsafe impl Sync for SysFn {}

#[doc(hidden)]
pub fn with_log<R, F>(f: F) -> R
where
    F: FnOnce(Log) -> R,
{
    (f)(Log(()))
}

#[allow(dead_code)]
pub fn hexdump(data: &[u8]) {
    with_log(|mut log| {
        for b in data.chunks(32) {
            for (i, b) in b.iter().enumerate() {
                if i % 4 == 0 {
                    let _ = write!(&mut log, " ");
                }
                let _ = write!(&mut log, "{b:02x}");
            }
            let _ = writeln!(&mut log);
        }
    });
}

#[inline]
pub unsafe fn init() {
    unsafe { lemmings_x86_64::set_gs(TABLE.as_ptr() as *mut _) };
}

/// # Safety
///
/// `msg_base` and `msg_len` must point to a valid UTF-8 string.
unsafe extern "sysv64" fn log(msg: Slice<u8>) {
    let msg = unsafe { msg.as_str() };
    with_log(|mut log| {
        log.prefix_time();
        let _ = log.write_str(msg);
        let _ = log.write_str("\n");
    })
}

unsafe extern "sysv64" fn panic(msg: Slice<u8>) -> ! {
    todo!("handle panic: {}", unsafe { msg.as_str() });
}

/// Begin panicking.
///
/// This version allows formatting a panic message in multiple steps,
/// which is useful for printing complex structures without preallocating a large buffer.
///
/// # Returns
///
/// A handle which must be used for [`panic_push`] and [`panic_end`]
unsafe extern "sysv64" fn panic_begin() -> *const u8 {
    todo!();
}

/// Push part of a panic message
///
/// # Returns
///
/// A new handle.
///
/// # Safety
///
/// - `panic_begin` must have been called first.
/// - `msg` must point to a valid UTF-8 string.
unsafe extern "sysv64" fn panic_push(_handle: *const u8, msg: Slice<u8>) -> *const u8 {
    todo!();
}

/// Truly start panicking.
///
/// # Safety
///
/// - `panic_begin` must have been called first.
unsafe extern "sysv64" fn panic_end(_handle: *const u8) -> ! {
    todo!();
}

unsafe extern "sysv64" fn door_list(cookie: Cookie) -> Tuple2<Option<Table>, Cookie> {
    todo!();
}

// XXX: u128 ought to be FFI-safe
#[allow(improper_ctypes_definitions)]
unsafe extern "sysv64" fn door_find(api: Option<ApiId>) -> Option<Table> {
    // somebody is going to screw up and pass 0, so always check
    let api = api.expect("door_find: API ID may not be 0");
    door::find(api)
}

unsafe extern "sysv64" fn door_register(table: Table) -> isize {
    todo!();
}

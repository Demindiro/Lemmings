use crate::{
    KernelEntryToken,
    door::{self, ApiId, Cookie, Table},
    ffi::{Slice, Tuple2},
    framebuffer,
    thread::{self, RoundRobinQueue},
};
use core::fmt::{self, Write};
use critical_section::CriticalSection;
use lemmings_spinlock::SpinLock;

/// A list of panicked threads.
static PANICKED_THREADS: SpinLock<RoundRobinQueue> = SpinLock::new(RoundRobinQueue::new());

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

pub struct Log {}

#[allow(dead_code)]
struct SysFn(*const ());

impl Log {
    pub fn prefix_time(&mut self) {
        let us = crate::time::Monotonic::now().micros();
        let (s, us) = (us / 1_000_000, us % 1_000_000);
        let _ = write!(self, "[{s:>5}.{us:06}] ");
    }
}

impl Write for Log {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        lemmings_qemubios::sys::print(s);
        Ok(())
    }
}

unsafe impl Sync for SysFn {}

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
    let msg = unsafe { msg.as_str() };
    critical_section::with(|cs| {
        let mut log = logger(cs);
        log.prefix_time();
        let _ = log.write_str("Oh no! ");
        let _ = log.write_str(msg);
        panic_halt(cs, log)
    })
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
    let _ = with_log(|mut log| {
        log.prefix_time();
        log.write_str("Oh no! ")
    });
    core::ptr::null()
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
    let msg = unsafe { msg.as_str() };
    let _ = with_log(|mut log| log.write_str(msg));
    core::ptr::null()
}

/// Truly start panicking.
///
/// # Safety
///
/// - `panic_begin` must have been called first.
unsafe extern "sysv64" fn panic_end(_handle: *const u8) -> ! {
    critical_section::with(|cs| panic_halt(cs, logger(cs)))
}

unsafe extern "sysv64" fn door_list(cookie: Cookie) -> Tuple2<Option<Table>, Cookie> {
    door::list(cookie).map_or(Tuple2(None, cookie), |(cookie, x)| Tuple2(Some(x), cookie))
}

// XXX: u128 ought to be FFI-safe
#[allow(improper_ctypes_definitions)]
unsafe extern "sysv64" fn door_find(api: Option<ApiId>) -> Option<Table> {
    // somebody is going to screw up and pass 0, so always check
    let api = api.expect("door_find: API ID may not be 0");
    door::find(api)
}

unsafe extern "sysv64" fn door_register(table: Table) -> isize {
    let res = unsafe { door::register(table) };
    match res {
        Ok(()) => 0,
        Err(door::RegisterError::Duplicate) => -1,
        Err(door::RegisterError::Full) => -2,
    }
}

pub fn with_log<F, R>(f: F) -> R
where
    F: FnOnce(Log) -> R,
{
    critical_section::with(|cs| f(logger(cs)))
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
pub fn init<'a>(token: KernelEntryToken<'a>) -> KernelEntryToken<'a> {
    unsafe { lemmings_x86_64::set_gs(TABLE.as_ptr() as *mut _) };
    token
}

fn logger(cs: CriticalSection<'_>) -> Log {
    Log {}
}

fn panic_halt(cs: CriticalSection<'_>, mut log: Log) -> ! {
    let _ = log.write_char('\n');
    drop(log);
    PANICKED_THREADS.lock(cs).enqueue(cs, thread::current());
    // the only way we can get rescheduled is if something takes
    // us out of the PANICKED_THREADS queue and forcibly unparks us.
    //
    // If they didn't fix anything, just immediately repark.
    loop {
        thread::park(cs)
    }
}

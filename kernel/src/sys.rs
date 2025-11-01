use crate::{
    KernelEntryToken,
    door::{self, ApiId, Cookie, Table},
    ffi::{Slice, Tuple2},
    framebuffer,
};
use core::{
    fmt::{self, Write},
    mem::MaybeUninit,
};
use critical_section::CriticalSection;

#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {{
        use core::fmt::Write;
        $crate::sys::with_log(|mut log| { let _ = writeln!(&mut log, $($arg)*); });
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
    2 door_list
    3 door_register
}

pub struct Log<'cs> {
    fb: framebuffer::Log<'static, 'cs>,
}

#[allow(unused)]
struct SysFn(*const ());

#[repr(C)]
struct InterfaceInfo {
    api: ApiId,
    name: Slice<u8>,
}

impl Write for Log<'_> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.fb.write_str(s);
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
        log.write_str(msg);
        log.write_str("\n");
    })
}

unsafe extern "sysv64" fn panic(msg: Slice<u8>) -> ! {
    let msg = unsafe { msg.as_str() };
    todo!("handle thread panic (message: {msg:?})");
}

// XXX: u128 ought to be FFI-safe
#[allow(improper_ctypes_definitions)]
unsafe extern "sysv64" fn door_list(
    api: Option<ApiId>,
    cookie: Cookie,
    info: Option<&mut MaybeUninit<InterfaceInfo>>,
) -> Tuple2<Option<Table>, Cookie> {
    door::list(api, cookie).map_or(Tuple2(None, cookie), |(cookie, x)| {
        info.map(|w| {
            w.write(InterfaceInfo {
                api: x.api,
                name: Slice::from(x.name),
            })
        });
        Tuple2(Some(x.table), cookie)
    })
}

// XXX: u128 ought to be FFI-safe
#[allow(improper_ctypes_definitions)]
unsafe extern "sysv64" fn door_register(api: ApiId, name: Slice<u8>, table: Table) {
    let name = unsafe { name.as_str() };
    unsafe { door::register(api, name, table) };
}

pub fn with_log<F>(f: F)
where
    F: FnOnce(Log<'_>),
{
    critical_section::with(|cs| {
        f(Log {
            fb: framebuffer::log(cs),
        })
    })
}

#[inline]
pub fn init(_: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    unsafe { lemmings_x86_64::set_gs(TABLE.as_ptr() as *mut _) };
    token
}

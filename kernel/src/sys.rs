use crate::{KernelEntryToken, door::{self, ApiId, Cookie, Table}};
use core::{mem::{self, MaybeUninit}, ptr::NonNull, num::NonZero};

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
    1 door_list
    2 door_register
}

struct SysFn(*const ());

#[repr(C)]
struct InterfaceInfo {
    api: ApiId,
    name: Slice<u8>,
}

#[repr(C)]
struct Slice<T> {
    base: NonNull<T>,
    len: usize,
}

#[repr(C)]
struct Tuple2<A, B>(A, B);

unsafe impl Sync for SysFn {}

impl<T> Slice<T> {
    unsafe fn as_slice(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.base.as_ptr(), self.len) }
    }
}

impl Slice<u8> {
    unsafe fn as_str(&self) -> &str {
        unsafe { core::str::from_utf8_unchecked(self.as_slice()) }
    }
}

impl From<&'static str> for Slice<u8> {
    fn from(s: &'static str) -> Self {
        let s = NonNull::from(s.as_bytes());
        Self {
            base: s.cast(),
            len: s.len(),
        }
    }
}

extern "sysv64" fn invalid(eax: u32) {
    panic!("invalid system call! eax={eax}");
}

/// # Safety
///
/// `msg_base` and `msg_len` must point to a valid UTF-8 string.
unsafe extern "sysv64" fn log(msg: Slice<u8>) {
    let msg = unsafe { msg.as_str() };
    lemmings_qemubios::sys::print(msg);
    lemmings_qemubios::sys::print("\n");
}

// XXX: u128 ought to be FFI-safe
#[allow(improper_ctypes_definitions)]
unsafe extern "sysv64" fn door_list(api: Option<ApiId>, cookie: Cookie, info: Option<&mut MaybeUninit<InterfaceInfo>>) -> Tuple2<Option<Table>, Cookie> {
    door::list(api, cookie)
        .map_or(Tuple2(None, cookie), |(cookie, x)| {
            info.map(|w| w.write(InterfaceInfo {
                api: x.api,
                name: Slice::from(x.name),
            }));
            Tuple2(Some(x.table), cookie)
        })
}

// XXX: u128 ought to be FFI-safe
#[allow(improper_ctypes_definitions)]
unsafe extern "sysv64" fn door_register(api: ApiId, name: Slice<u8>, table: Table) {
    let name = unsafe { name.as_str() };
    door::register(api, name, table);
}

#[inline]
pub fn init(_: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    unsafe { lemmings_x86_64::set_gs(TABLE.as_ptr() as *mut _) };
    token
}

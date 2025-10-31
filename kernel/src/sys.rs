use crate::{KernelEntryToken, door::{self, ApiId, Cookie, Table}, ffi::{Slice, Tuple2}};
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
    1 panic
    2 door_list
    3 door_register
}

struct SysFn(*const ());

#[repr(C)]
struct InterfaceInfo {
    api: ApiId,
    name: Slice<u8>,
}

unsafe impl Sync for SysFn {}

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

unsafe extern "sysv64" fn panic(msg: Slice<u8>) -> ! {
    let msg = unsafe { msg.as_str() };
    todo!("handle thread panic (message: {msg:?})");
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
    unsafe { door::register(api, name, table) };
}

#[inline]
pub fn init(_: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    unsafe { lemmings_x86_64::set_gs(TABLE.as_ptr() as *mut _) };
    token
}

#![no_std]
#![forbid(improper_ctypes_definitions)]

use core::ptr::NonNull;
use core::{
    fmt::{self, Write},
    marker::PhantomData,
    mem::MaybeUninit,
    num::NonZero,
};

#[cfg(target_arch = "x86_64")]
mod ffi {
    use core::arch::asm;

    pub unsafe fn syscall_0_1<const ID: usize>() -> u64 {
        let x: u64;
        unsafe {
            asm! {
                "call gs:[8 * {ID}]",
                ID = const ID,
                out("rax") x,
                clobber_abi("sysv64"),
            }
        }
        x
    }

    pub unsafe fn syscall_2_0<const ID: usize>(a: u64, b: u64) {
        unsafe {
            asm! {
                "call gs:[8 * {ID}]",
                ID = const ID,
                in("rdi") a,
                in("rsi") b,
                clobber_abi("sysv64"),
            }
        }
    }

    pub unsafe fn syscall_1_noreturn<const ID: usize>(a: u64) -> ! {
        unsafe {
            asm! {
                "call gs:[8 * {ID}]",
                ID = const ID,
                in("rdi") a,
                clobber_abi("sysv64"),
                options(noreturn),
            }
        }
    }

    pub unsafe fn syscall_2_noreturn<const ID: usize>(a: u64, b: u64) -> ! {
        unsafe {
            asm! {
                "call gs:[8 * {ID}]",
                ID = const ID,
                in("rdi") a,
                in("rsi") b,
                clobber_abi("sysv64"),
                options(noreturn),
            }
        }
    }

    pub unsafe fn syscall_3_1<const ID: usize>(a: u64, b: u64, c: u64) -> u64 {
        let x: u64;
        unsafe {
            asm! {
                "call gs:[8 * {ID}]",
                ID = const ID,
                in("rdi") a,
                in("rsi") b,
                in("rdx") c,
                lateout("rax") x,
                clobber_abi("sysv64"),
            }
        }
        x
    }

    pub unsafe fn syscall_4_2<const ID: usize>(a: u64, b: u64, c: u64, d: u64) -> [u64; 2] {
        let x @ y: u64;
        unsafe {
            asm! {
                "call gs:[8 * {ID}]",
                ID = const ID,
                in("rdi") a,
                in("rsi") b,
                in("rdx") c,
                in("rcx") d,
                lateout("rax") x,
                lateout("rdx") y,
                clobber_abi("sysv64"),
            }
        }
        [x, y]
    }

    pub unsafe fn syscall_5_1<const ID: usize>(a: u64, b: u64, c: u64, d: u64, e: u64) -> u64 {
        let x: u64;
        unsafe {
            asm! {
                "call gs:[8 * {ID}]",
                ID = const ID,
                in("rdi") a,
                in("rsi") b,
                in("rdx") c,
                in("rcx") d,
                in("r8") e,
                lateout("rax") x,
                clobber_abi("sysv64"),
            }
        }
        x
    }

    pub fn api_to_args(api: Option<super::ApiId>) -> [u64; 2] {
        let api = api.map_or(0, |x| x.0.get());
        [api as u64, (api >> 64) as u64]
    }
}

mod sys {
    pub const LOG: usize = 0;
    pub const PANIC: usize = 1;
    pub const DOOR_LIST: usize = 2;
    pub const DOOR_REGISTER: usize = 3;
    pub const PANIC_BEGIN: usize = 4;
    pub const PANIC_PUSH: usize = 5;
    pub const PANIC_END: usize = 6;
}

#[derive(Clone, Debug)]
pub struct HallwayIsFull;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ApiId(pub NonZero<u128>);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cookie(pub u64);

pub struct UntypedTable;

#[derive(Clone, Copy)]
pub struct Door<'table, 'name, T = UntypedTable> {
    api: ApiId,
    table: NonNull<T>,
    name: &'name str,
    _marker: PhantomData<&'table ()>,
}

struct Panic(*const u8);

#[repr(C)]
struct InterfaceInfo {
    api: ApiId,
    name_ptr: NonNull<u8>,
    name_len: usize,
}

impl<T> Door<'_, '_, T> {
    pub fn api_id(&self) -> ApiId {
        self.api
    }
}

impl<'table, 'name> Door<'table, 'name, UntypedTable> {
    fn cast<T>(self) -> Option<Door<'table, 'name, T>>
    where
        T: lemmings_idl::Api,
    {
        (self.api_id().0 == T::ID).then(|| unsafe { self.cast_unchecked::<T>() })
    }

    unsafe fn cast_unchecked<T>(self) -> Door<'table, 'name, T>
    where
        T: lemmings_idl::Api,
    {
        let Self {
            api,
            table,
            name,
            _marker,
        } = self;
        let table = table.cast::<T>();
        Door {
            api,
            table,
            name,
            _marker,
        }
    }
}

impl<'table, 'name, T> Door<'table, 'name, T> {
    pub fn get(&self) -> &'table T {
        unsafe { self.table.as_ref() }
    }
}

impl Panic {
    fn new() -> Self {
        unsafe { Self(panic_begin()) }
    }

    fn end(self) -> ! {
        unsafe { panic_end(self.0) }
    }
}

impl fmt::Write for Panic {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0 = unsafe { panic_push(self.0, s) };
        Ok(())
    }
}

impl fmt::Debug for ApiId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:032x}", self.0)
    }
}

impl<T> fmt::Debug for Door<'_, '_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(stringify!(Door))
            .field("api", &self.api)
            .field("table", &self.table)
            .field("name", &self.name)
            .finish()
    }
}

#[inline(always)]
pub fn log(msg: &str) {
    unsafe { ffi::syscall_2_0::<{ sys::LOG }>(msg.as_ptr() as _, msg.len() as _) }
}

#[inline(always)]
pub fn panic(msg: &str) -> ! {
    unsafe { ffi::syscall_2_noreturn::<{ sys::PANIC }>(msg.as_ptr() as _, msg.len() as _) }
}

#[inline(always)]
unsafe fn panic_begin() -> *const u8 {
    unsafe { ffi::syscall_0_1::<{ sys::PANIC_BEGIN }>() as _ }
}

#[inline(always)]
unsafe fn panic_push(handle: *const u8, msg: &str) -> *const u8 {
    unsafe {
        ffi::syscall_3_1::<{ sys::PANIC_PUSH }>(handle as _, msg.as_ptr() as _, msg.len() as _) as _
    }
}

#[inline(always)]
unsafe fn panic_end(handle: *const u8) -> ! {
    unsafe { ffi::syscall_1_noreturn::<{ sys::PANIC_END }>(handle as _) }
}

#[inline(always)]
pub fn door_list(api: Option<ApiId>, cookie: Cookie) -> Option<(Door<'static, 'static>, Cookie)> {
    let mut door = MaybeUninit::<InterfaceInfo>::uninit();
    let [a, b] = ffi::api_to_args(api);
    let c = cookie.0;
    let d = door.as_mut_ptr() as u64;
    let [x, y] = unsafe { ffi::syscall_4_2::<{ sys::DOOR_LIST }>(a, b, c, d) };
    let table = NonNull::new(x as *mut UntypedTable)?;
    let InterfaceInfo {
        api,
        name_ptr,
        name_len,
    } = unsafe { door.assume_init() };
    let name = unsafe { core::slice::from_raw_parts(name_ptr.as_ptr(), name_len) };
    let name = unsafe { core::str::from_utf8_unchecked(name) };
    let _marker = PhantomData;
    Some((
        Door {
            api,
            table,
            name,
            _marker,
        },
        Cookie(y),
    ))
}

#[inline(always)]
pub fn door_find<T>(cookie: Cookie) -> Option<(Door<'static, 'static, T>, Cookie)>
where
    T: lemmings_idl::Api,
{
    door_list(Some(ApiId(T::ID)), cookie).map(|(x, y)| unsafe { (x.cast_unchecked(), y) })
}

/// # Safety
///
/// `table` must be valid.
#[inline(always)]
pub unsafe fn door_register<T>(door: Door<'static, '_, T>) -> Result<(), HallwayIsFull> {
    let Door {
        api, name, table, ..
    } = door;
    let [a, b] = ffi::api_to_args(Some(api));
    let [c, d] = [name.as_ptr() as _, name.len() as _];
    let e = table.as_ptr() as u64;
    let x = unsafe { ffi::syscall_5_1::<{ sys::DOOR_REGISTER }>(a, b, c, d, e) };
    (x == 0).then_some(()).ok_or(HallwayIsFull)
}

#[panic_handler]
fn panic_handler(info: &core::panic::PanicInfo<'_>) -> ! {
    let mut panic = Panic::new();
    let _ = write!(&mut panic, "{info}");
    panic.end()
}

// compiler_builtins doesn't build???
#[unsafe(no_mangle)]
unsafe extern "C" fn memset(dst: *mut u8, c: i32, n: usize) -> *mut u8 {
    unsafe {
        core::arch::asm! {
            "rep stosb",
            in("al") c as u8,
            inout("rdi") dst => _,
            inout("rcx") n => _,
        }
    }
    dst
}

#[unsafe(no_mangle)]
unsafe extern "C" fn memcpy(dst: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    unsafe {
        core::arch::asm! {
            "rep movsb",
            inout("rdi") dst => _,
            inout("rsi") src => _,
            inout("rcx") n => _,
        }
    }
    dst
}

#[unsafe(no_mangle)]
unsafe extern "C" fn memcmp(mut x: *const u8, mut y: *const u8, n: usize) -> i32 {
    // rep cmpsb is slow, so do a manual loop
    unsafe {
        for _ in 0..n {
            if x.read() != y.read() {
                return i32::from(x.read()) - i32::from(y.read());
            }
            x = x.byte_add(1);
            y = y.byte_add(1);
        }
    }
    0
}

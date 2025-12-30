#![no_std]
#![forbid(improper_ctypes_definitions)]

use core::ptr::NonNull;
use core::{
    fmt::{self, Write},
    marker::PhantomData,
    num::NonZero,
};

#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {{
        use core::fmt::Write;
        let _ = write!($crate::Log::new(), $($arg)*);
    }};
}

#[macro_export]
macro_rules! dbg {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
        $crate::log!("[{}:{}:{}:{}]", env!("CARGO_CRATE_NAME"), file!(), line!(), column!())
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                $crate::log!("[{}:{}:{}:{}] {} = {:#?}",
                    env!("CARGO_CRATE_NAME"),
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

    pub unsafe fn syscall_1_1<const ID: usize>(a: u64) -> u64 {
        let x: u64;
        unsafe {
            asm! {
                "call gs:[8 * {ID}]",
                ID = const ID,
                in("rdi") a,
                lateout("rax") x,
                clobber_abi("sysv64"),
            }
        }
        x
    }

    pub unsafe fn syscall_1_2<const ID: usize>(a: u64) -> [u64; 2] {
        let x @ y: u64;
        unsafe {
            asm! {
                "call gs:[8 * {ID}]",
                ID = const ID,
                in("rdi") a,
                lateout("rax") x,
                lateout("rdx") y,
                clobber_abi("sysv64"),
            }
        }
        [x, y]
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

    pub unsafe fn syscall_2_1<const ID: usize>(a: u64, b: u64) -> u64 {
        let x: u64;
        unsafe {
            asm! {
                "call gs:[8 * {ID}]",
                ID = const ID,
                in("rdi") a,
                in("rsi") b,
                lateout("rax") x,
                clobber_abi("sysv64"),
            }
        }
        x
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

    pub fn api_to_args(api: Option<super::ApiId>) -> [u64; 2] {
        let api = api.map_or(0, |x| x.0.get());
        [api as u64, (api >> 64) as u64]
    }
}

mod sys {
    pub const LOG: usize = 0;
    pub const PANIC: usize = 1;
    pub const PANIC_BEGIN: usize = 2;
    pub const PANIC_PUSH: usize = 3;
    pub const PANIC_END: usize = 4;
    pub const DOOR_LIST: usize = 5;
    pub const DOOR_FIND: usize = 6;
    pub const DOOR_REGISTER: usize = 7;
}

#[derive(Clone, Debug)]
pub struct HallwayIsFull;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ApiId(pub NonZero<u128>);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Cookie(pub u64);

pub struct Log {
    n: u8,
    buf: [u8; 127],
}

#[derive(Clone, Copy)]
pub struct Door<'a> {
    table: NonNull<()>,
    _marker: PhantomData<&'a ()>,
}

struct Panic(*const u8);

impl<'a> Door<'a> {
    pub fn api_id(&self) -> ApiId {
        unsafe { self.table.cast::<ApiId>().read() }
    }

    pub fn name(&self) -> &'a str {
        unsafe { self.table.cast::<ApiId>().add(1).cast::<&'a str>().as_ref() }
    }
}

impl Log {
    pub fn new() -> Self {
        Self {
            n: 0,
            buf: [0; 127],
        }
    }

    fn push(&mut self, c: char) {
        if c == '\n' {
            return self.flush();
        }
        let n = c.len_utf8() as u8;
        let buf = self.reserve_mut(n);
        c.encode_utf8(buf);
        self.advance(n)
    }

    fn reserve_mut(&mut self, x: u8) -> &mut [u8] {
        let x = usize::from(x);
        if self.buf.len() < usize::from(self.n) + x {
            self.flush();
        }
        let n = usize::from(self.n);
        &mut self.buf[n..n + x]
    }

    fn advance(&mut self, x: u8) {
        self.n += x;
    }

    fn flush(&mut self) {
        // SAFETY: we only push chars and never tear any characters.
        let s = unsafe { core::str::from_utf8_unchecked(&self.buf[..usize::from(self.n)]) };
        log(s);
        self.n = 0;
    }
}

impl Drop for Log {
    fn drop(&mut self) {
        if self.n > 0 {
            self.flush();
        }
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

impl fmt::Write for Log {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        s.chars().for_each(|x| self.push(x));
        Ok(())
    }

    fn write_char(&mut self, c: char) -> fmt::Result {
        self.push(c);
        Ok(())
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

impl fmt::Debug for Door<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(stringify!(Door))
            .field("api", &self.api_id())
            .field("table", &self.table)
            .field("name", &self.name())
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
pub fn door_list(cookie: Cookie) -> Option<(Door<'static>, Cookie)> {
    let a = cookie.0;
    let [x, y] = unsafe { ffi::syscall_1_2::<{ sys::DOOR_LIST }>(a) };
    let _marker = PhantomData;
    NonNull::new(x as *mut ())
        .map(|table| Door { table, _marker })
        .map(|x| (x, Cookie(y)))
}

#[inline(always)]
pub fn door_find<T>() -> Option<&'static T>
where
    T: lemmings_idl::Api,
{
    let [a, b] = ffi::api_to_args(Some(ApiId(T::ID)));
    let x = unsafe { ffi::syscall_2_1::<{ sys::DOOR_FIND }>(a, b) };
    NonNull::new(x as *mut T).map(|x| unsafe { x.as_ref() })
}

/// # Safety
///
/// `table` must be valid.
#[inline(always)]
pub unsafe fn door_register<T>(door: &'static T) -> Result<(), HallwayIsFull> {
    let a = door as *const T as u64;
    let x = unsafe { ffi::syscall_1_1::<{ sys::DOOR_REGISTER }>(a) };
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
            options(nostack, preserves_flags),
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
            options(nostack, preserves_flags),
        }
    }
    dst
}

#[unsafe(no_mangle)]
unsafe extern "C" fn memmove(dst: *mut u8, src: *const u8, n: usize) -> *mut u8 {
    if src.addr() < dst.addr() {
        unsafe {
            core::arch::asm! {
                "std",
                "rep movsb",
                "cld",
                inout("rdi") dst.add(n).sub(1) => _,
                inout("rsi") src.add(n).sub(1) => _,
                inout("rcx") n => _,
                options(nostack, preserves_flags),
            }
        }
    } else {
        unsafe {
            core::arch::asm! {
                "rep movsb",
                inout("rdi") dst => _,
                inout("rsi") src => _,
                inout("rcx") n => _,
                options(nostack, preserves_flags),
            }
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

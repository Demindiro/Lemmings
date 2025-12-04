mod vdso;

use crate::syscall;
use core::{
    fmt,
    mem::{self, MaybeUninit},
    slice,
};

static mut ARGS: &'static [&'static CStr] = &[];
static mut ENV: &'static [&'static CStr] = &[];

static mut TIME: unsafe extern "C" fn(*mut u64) -> u64 = syscall_time;
static mut CLOCK_GETTIME: unsafe extern "C" fn(u32, *mut TimeSpec) -> i32 = syscall_clock_gettime;

pub const CLOCK_MONOTONIC: u32 = 1;

const AT_SYSINFO_EHDR: i32 = 33;

pub struct CStr(u8);

#[derive(Debug)]
#[repr(C)]
pub struct AuxiliaryVector {
    pub ty: i32,
    pub val: *const u8,
}

#[derive(Clone, Debug)]
pub struct CStrNotTerminated;

#[repr(C)]
pub struct LinuxDirent64 {
    inode: u64,
    offset: u64,
    record_len: u16,
    ty: u8,
    name: [u8; 0],
}

#[derive(Default)]
#[repr(C)]
pub struct Stat {
    pub dev: u64,
    pub inode: u64,
    pub num_links: u64,
    pub mode: u32,
    pub uid: u32,
    pub gid: u32,
    _pad: u32,
    pub rdev: u64,
    pub size: u64,
    pub blksize: u64,
    /// Always in terms of 512-byte blocks, regardless of [`blksize`].
    pub blocks: u64,
    pub accessed: StatTime,
    pub modified: StatTime,
    pub created: StatTime,
    _reserved: [u64; 3],
}

const _: () = assert!(mem::size_of::<Stat>() == 0x90);

#[derive(Default)]
#[repr(C)]
pub struct StatTime {
    pub secs: u64,
    pub nanos: u64,
}

#[derive(Default)]
#[repr(C)]
pub struct TimeSpec {
    pub secs: u64,
    pub nsecs: u32,
}

impl CStr {
    pub fn as_ptr(&self) -> *const u8 {
        &self.0
    }

    // FIXME no UTF-8 guarantee!!
    fn as_str(&self) -> &str {
        core::str::from_utf8(self.as_bytes()).unwrap()
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe { slice::from_raw_parts(&self.0, self.len()) }
    }

    pub fn len(&self) -> usize {
        let p @ mut q = &self.0 as *const u8;
        unsafe {
            while q.read() != 0 {
                q = q.add(1);
            }
            q.offset_from(p) as usize
        }
    }
}

impl TryFrom<&[u8]> for &CStr {
    type Error = CStrNotTerminated;

    fn try_from(s: &[u8]) -> Result<Self, Self::Error> {
        s.iter()
            .any(|x| *x == b'\0')
            .then(|| unsafe { &*(s.as_ptr() as *const CStr) })
            .ok_or(CStrNotTerminated)
    }
}

impl<const N: usize> TryFrom<&[u8; N]> for &CStr {
    type Error = CStrNotTerminated;

    fn try_from(s: &[u8; N]) -> Result<Self, Self::Error> {
        (&s[..]).try_into()
    }
}

impl LinuxDirent64 {
    pub const TY_DIR: u8 = 4;
    pub const TY_FILE: u8 = 8;

    pub fn name(&self) -> Option<&CStr> {
        <&CStr>::try_from(self.name_bytes()).ok()
    }

    pub fn record_len(&self) -> usize {
        usize::from(self.record_len)
    }

    pub fn ty(&self) -> u8 {
        self.ty
    }

    fn name_bytes(&self) -> &[u8] {
        unsafe { core::slice::from_raw_parts(self.name.as_ptr(), self.name_len()) }
    }

    fn name_len(&self) -> usize {
        self.record_len() - 2 - 8 * 2
    }
}

impl fmt::Display for CStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl fmt::Debug for CStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl fmt::Debug for LinuxDirent64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct(stringify!(LinuxDirent64))
            .field("inode", &self.inode)
            .field("offset", &self.offset)
            .field("record_len", &self.record_len())
            .field("ty", &self.ty)
            .field("name", &self.name())
            .finish()
    }
}

pub fn args() -> &'static [&'static CStr] {
    // SAFETY: nothing will write to ARGS at this point
    unsafe { ARGS }
}

pub fn env() -> &'static [&'static CStr] {
    // SAFETY: nothing will write to ENV at this point
    unsafe { ENV }
}

pub fn clock_gettime(id: u32) -> Option<TimeSpec> {
    let mut x = MaybeUninit::uninit();
    let res = unsafe { (CLOCK_GETTIME)(id, x.as_mut_ptr()) };
    (res >= 0).then(|| unsafe { x.assume_init() })
}

pub unsafe fn init(x: *const usize) {
    unsafe {
        let x = collect_args(x);
        let x = collect_env(x);
        let _ = collect_aux(x);
    }
}

unsafe fn collect_args(args: *const usize) -> *const *const CStr {
    unsafe {
        let argc = args.read();
        let argv = args.add(1).cast();
        ARGS = slice::from_raw_parts(argv, argc);
        argv.add(argc + 1).cast()
    }
}

unsafe fn collect_env(env: *const *const CStr) -> *const AuxiliaryVector {
    let mut x = env;
    unsafe {
        while !x.read().is_null() {
            x = x.add(1);
        }
        ENV = slice::from_raw_parts(env.cast(), x.offset_from(env) as usize);
        x.add(1).cast()
    }
}

unsafe fn collect_aux(mut aux: *const AuxiliaryVector) {
    loop {
        let x = unsafe { aux.read() };
        if x.ty == 0 {
            break;
        }
        aux = unsafe { aux.add(1) };
        match x.ty {
            0 => break,
            self::AT_SYSINFO_EHDR => unsafe { load_vdso(x.val) },
            _ => {}
        }
    }
}

unsafe fn load_vdso(base: *const u8) {
    let Some(base) = core::ptr::NonNull::new(base as *mut u8) else {
        log!("no vDSO?");
        return;
    };
    let tbl = unsafe { vdso::load(base) };
    tbl.time.map(|x| unsafe { TIME = x });
    tbl.clock_gettime.map(|x| unsafe { CLOCK_GETTIME = x });
}

unsafe extern "C" fn syscall_time(out: *mut u64) -> u64 {
    unsafe { syscall::time(out) }
}

unsafe extern "C" fn syscall_clock_gettime(id: u32, out: *mut TimeSpec) -> i32 {
    unsafe { syscall::clock_gettime(id, out) }
}

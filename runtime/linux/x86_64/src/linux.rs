use core::{fmt, slice};

const AT_SYSINFO_EHDR: i32 = 33;

static mut ARGS: &'static [&'static CStr] = &[];
static mut ENV: &'static [&'static CStr] = &[];
static mut AUX: &'static [AuxiliaryVector] = &[];

pub struct CStr(u8);

#[derive(Debug)]
#[repr(C)]
pub struct AuxiliaryVector {
    pub ty: i32,
    pub val: *const u8,
}

impl CStr {
    // FIXME no UTF-8 guarantee!!
    fn as_str(&self) -> &str {
        let s = unsafe { slice::from_raw_parts(&self.0, self.len()) };
        core::str::from_utf8(s).unwrap()
    }

    fn len(&self) -> usize {
        let p @ mut q = &self.0 as *const u8;
        unsafe {
            while q.read() != 0 {
                q = q.add(1);
            }
            q.offset_from(p) as usize
        }
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

pub fn args() -> &'static [&'static CStr] {
    // SAFETY: nothing will write to ARGS at this point
    unsafe { ARGS }
}

pub fn env() -> &'static [&'static CStr] {
    // SAFETY: nothing will write to ENV at this point
    unsafe { ENV }
}

pub fn aux() -> &'static [AuxiliaryVector] {
    // SAFETY: nothing will write to AUX at this point
    unsafe { AUX }
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

unsafe fn collect_aux(aux: *const AuxiliaryVector) {
    let mut x = aux;
    unsafe {
        while x.read().ty != 0 {
            x = x.add(1);
        }
        AUX = slice::from_raw_parts(aux, x.offset_from(aux) as usize);
    }
}

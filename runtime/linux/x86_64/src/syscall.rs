use crate::linux::CStr;
use core::arch::asm;

macro_rules! sys {
    ($id:literal $fn:ident($($arg:ident: $argty:ty),*$(,)?)) => {
        #[allow(dead_code)] // these functions also serve as documentation
        pub unsafe fn $fn($($arg:$argty,)*) {
            unsafe { sys!(@ $id $($arg)* _) };
        }
    };
    ($id:literal $fn:ident($($arg:ident: $argty:ty),*$(,)?) -> $ret:ty) => {
        #[allow(dead_code)] // these functions also serve as documentation
        pub unsafe fn $fn($($arg:$argty,)*) -> $ret {
            let x: $ret;
            unsafe { sys!(@ $id $($arg)* x) };
            x
        }
    };
    (@ $id:literal $ret:ident) => {
        asm! {
            "syscall",
            in("eax") $id,
            lateout("rax") $ret,
            lateout("rcx") _,
            lateout("r11") _,
            options(nostack),
        }
    };
    (@ $id:literal $a0:ident $ret:ident) => {
        asm! {
            "syscall",
            in("eax") $id,
            in("rdi") $a0,
            lateout("rax") $ret,
            lateout("rcx") _,
            lateout("r11") _,
            options(nostack),
        }
    };
    (@ $id:literal $a0:ident $a1:ident $ret:ident) => {
        asm! {
            "syscall",
            in("eax") $id,
            in("rdi") $a0,
            in("rsi") $a1,
            lateout("rax") $ret,
            lateout("rcx") _,
            lateout("r11") _,
            options(nostack),
        }
    };
    (@ $id:literal $a0:ident $a1:ident $a2:ident $ret:ident) => {
        asm! {
            "syscall",
            in("eax") $id,
            in("rdi") $a0,
            in("rsi") $a1,
            in("rdx") $a2,
            lateout("rax") $ret,
            lateout("rcx") _,
            lateout("r11") _,
            options(nostack),
        }
    };
    (@ $id:literal $a0:ident $a1:ident $a2:ident $a3:ident $ret:ident) => {
        asm! {
            "syscall",
            in("eax") $id,
            in("rdi") $a0,
            in("rsi") $a1,
            in("rdx") $a2,
            in("r10") $a3,
            lateout("rax") $ret,
            lateout("rcx") _,
            lateout("r11") _,
            options(nostack),
        }
    };
    (@ $id:literal $a0:ident $a1:ident $a2:ident $a3:ident $a4:ident $ret:ident) => {
        asm! {
            "syscall",
            in("eax") $id,
            in("rdi") $a0,
            in("rsi") $a1,
            in("rdx") $a2,
            in("r10") $a3,
            in("r8") $a4,
            lateout("rax") $ret,
            lateout("rcx") _,
            lateout("r11") _,
            options(nostack),
        }
    };
    (@ $id:literal $a0:ident $a1:ident $a2:ident $a3:ident $a4:ident $a5:ident $ret:ident) => {
        asm! {
            "syscall",
            in("eax") $id,
            in("rdi") $a0,
            in("rsi") $a1,
            in("rdx") $a2,
            in("r10") $a3,
            in("r8") $a4,
            in("r9") $a5,
            lateout("rax") $ret,
            lateout("rcx") _,
            lateout("r11") _,
            options(nostack),
        }
    };
}

pub const SIGABRT: i32 = 6;
pub const O_RDONLY: i32 = 0o00;

// implement noreturn syscalls manually
pub fn exit(status: i32) -> ! {
    unsafe {
        asm! {
            "syscall",
            in("eax") 60,
            in("edi") status,
            options(nostack, nomem, noreturn),
        }
    }
}

sys!(0 read(fd: i32, buf: *mut u8, count: usize) -> isize);
sys!(1 write(fd: i32, buf: *const u8, count: usize) -> isize);
sys!(2 open(path: *const u8, flags: i32, mode: i32) -> i32);
sys!(3 close(fd: i32) -> i32);
sys!(39 getpid() -> i32);
sys!(62 kill(pid: i32, signal: i32) -> i32);
sys!(217 getdents64(fd: i32, dirent: *mut usize, count: usize) -> i32);
sys!(257 openat(dfd: i32, path: *const u8, flags: i32, mode: i32) -> i32);

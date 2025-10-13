#![no_std]

use core::ptr::NonNull;

const KERNEL_FILENAME: &str = "opt/lemmings/kernel.elf";

mod sys {
    use core::arch::asm;

    pub struct OpenFileError;

    pub fn print(s: &str) {
        unsafe {
            asm! {
                "call sys_print",
                in("rcx") s.len(),
                in("rsi") s.as_ptr(),
                lateout("rcx") _,
                lateout("rdx") _,
                lateout("rsi") _,
            }
        }
    }

    fn exit(status: i32) -> ! {
        unsafe {
            asm! {
                "jmp sys_exit",
                in("edx") status,
                options(noreturn),
            }
        }
    }

    pub fn exit_ok() -> ! {
        exit(0)
    }

    pub fn exit_err() -> ! {
        exit(1)
    }

    pub fn open(filename: &str) -> Result<usize, OpenFileError> {
        let len: isize;
        unsafe {
            asm! {
                "call sys_open",
                inout("rcx") filename.len() => _,
                inout("rsi") filename.as_ptr() => _,
                lateout("rdi") _,
                lateout("rdx") _,
                lateout("rax") len,
            }
        }
        usize::try_from(len).map_err(|_| OpenFileError)
    }

    pub fn read(buf: &mut [u8]) {
        unsafe {
            asm! {
                "call sys_read",
                inout("rcx") buf.len() => _,
                inout("rdi") buf.as_ptr() => _,
                out("rdx") _,
                out("rax") _,
            }
        }
    }
}

mod alloc {
    use crate::*;
    use core::ops::Range;

    pub const PAGE_SIZE: usize = 4096;
    const MAX_REGIONS: usize = 8;

    pub struct Allocator {
        regions: [Range<u64>; MAX_REGIONS],
    }

    fn round_p2(x: usize, n: usize) -> usize {
        let mask = n - 1;
        (x + mask) & !mask
    }

    impl Allocator {
        pub fn new() -> Self {
            let mut regions: [_; MAX_REGIONS] = Default::default();
            // intentionally skip 0x0000 because:
            // - we use 0x0..0x1000 as stack
            // - we can't safely dereference 0x0 in Rust
            regions[0] = 0x1000..0xb0000;
            Self { regions }
        }

        pub fn alloc(&mut self, byte_count: usize) -> Option<NonNull<u8>> {
            let n = round_p2(byte_count, PAGE_SIZE) as u64;
            for r in &mut self.regions {
                if r.end - r.start >= n {
                    let s = r.start;
                    r.start += n;
                    return NonNull::new(s as *mut u8);
                }
            }
            None
        }
    }
}

mod elf {
    use crate::*;

    struct Header {
        program_entry: u64,
    }

    fn bytes_to_u64le(b: &[u8]) -> u64 {
        u64::from_le_bytes(b.try_into().expect("b.len() should be 8"))
    }

    fn parse_header(file: &[u8]) -> Header {
        let fail = |s| -> ! {
            sys::print("invalid ELF header: ");
            fail(s);
        };
        if file.len() < 64 {
            fail("truncated");
        }
        if &file[..4] != b"\x7fELF" {
            fail("bad magic");
        }
        let program_entry = bytes_to_u64le(&file[24..32]);
        if file[4] != 2 {
        }
        Header { program_entry }
    }

    /// # Returns
    ///
    /// Entry point
    pub fn load(file: &[u8]) -> NonNull<()> {
        let h = parse_header(file);
        fail("TODO map segments");
        match NonNull::new(h.program_entry as *mut ()) {
            Some(p) => p,
            None => fail("invalid ELF: entry point is 0x0?"),
        }
    }
}

fn fail(reason: &str) -> ! {
    sys::print(reason);
    sys::print("\n");
    sys::exit_err();
}

/// # Note
///
/// Start of file is guaranteed to be aligned to page boundary.
fn load_file<'a>(alloc: &'a mut alloc::Allocator, filename: &str) -> &'a [u8] {
    let len = match sys::open(filename) {
        Ok(x) => x,
        Err(sys::OpenFileError) => {
            sys::print("failed to open ");
            fail(filename);
        }
    };
    let Some(base) = alloc.alloc(len) else {
        fail("out of memory while reading file")
    };
    let buf: &mut [u8] = unsafe { core::slice::from_raw_parts_mut(base.as_ptr(), len) };
    sys::read(buf);
    buf
}

#[unsafe(no_mangle)]
extern "sysv64" fn boot() -> ! {
    let alloc = &mut alloc::Allocator::new();
    let file = load_file(alloc, KERNEL_FILENAME);
    let entry = elf::load(file);
    let entry: extern "sysv64" fn() -> ! = unsafe { core::mem::transmute(entry) };
    (entry)()
}

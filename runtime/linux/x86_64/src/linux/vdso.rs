// to keep things simple, we'll assume Linux is sane and skip a whole bunch of error checking
use crate::linux::TimeSpec;
use core::{fmt, mem, num::NonZero, ops::Range, ptr::NonNull, slice};

const PH_ENTSIZE: usize = 56;
const SYMENT_SIZE: usize = 24;

const PT_DYNAMIC: u32 = 2;

const HASH: u64 = 4;
const STRTAB: u64 = 5;
const SYMTAB: u64 = 6;
const STRSZ: u64 = 10;
const SYMENT: u64 = 11;

#[derive(Default)]
pub struct Table {
    pub time: Option<unsafe extern "C" fn(*mut u64) -> u64>,
    pub clock_gettime: Option<unsafe extern "C" fn(u32, *mut TimeSpec) -> i32>,
}

struct ProgramHeader {
    ty: u32,
    flags: u32,
    offset: usize,
    vaddr: usize,
    filesz: usize,
    memsz: usize,
    align: usize,
}

struct SymbolEntry {
    name: usize,
    info: u8,
    other: u8,
    shndx: u16,
    value: usize,
    size: usize,
}

impl ProgramHeader {
    fn from_bytes(bytes: &[u8; PH_ENTSIZE]) -> Self {
        Self {
            ty: u32(&bytes[0..4]),
            flags: u32(&bytes[4..8]),
            offset: u64_to_usize(&bytes[8..16]),
            vaddr: u64_to_usize(&bytes[16..24]),
            filesz: u64_to_usize(&bytes[32..40]),
            memsz: u64_to_usize(&bytes[40..48]),
            align: u64_to_usize(&bytes[48..56]),
        }
    }
}

impl SymbolEntry {
    fn from_bytes(bytes: &[u8; SYMENT_SIZE]) -> Self {
        Self {
            name: u32_to_usize(&bytes[..4]),
            info: bytes[4],
            other: bytes[5],
            shndx: u16(&bytes[6..8]),
            value: u64_to_usize(&bytes[8..16]),
            size: u64_to_usize(&bytes[16..24]),
        }
    }
}

pub unsafe fn load(base: NonNull<u8>) -> Table {
    // should allow a few more optimizations
    assert!(
        base.addr().get() % 4096 == 0,
        "vDSO not aligned to page boundary"
    );
    let header = unsafe { base.cast::<[u8; 64]>().as_ref() };
    let program_headers = unsafe { program_headers(base, header) };
    let dynamic = program_headers
        .iter()
        .map(ProgramHeader::from_bytes)
        .find(|x| x.ty == PT_DYNAMIC)
        .expect("vDSO: no DYNAMIC segment found");
    let dynamic = unsafe {
        let base = base.byte_add(dynamic.offset).cast::<[u8; 16]>().as_ptr();
        slice::from_raw_parts(base, dynamic.filesz / 16)
    };
    unsafe { load_symbols(base, dynamic) }
}

unsafe fn program_headers<'a>(base: NonNull<u8>, header: &[u8; 64]) -> &'a [[u8; PH_ENTSIZE]] {
    let offt = u64_to_usize(&header[32..40]);
    let entsize = usize::from(u16(&header[54..56]));
    let num = usize::from(u16(&header[56..58]));
    let base = base.cast::<[u8; PH_ENTSIZE]>().as_ptr();
    unsafe { slice::from_raw_parts(base.byte_add(offt), num) }
}

unsafe fn load_symbols<'a>(base: NonNull<u8>, dynamic: &[[u8; 16]]) -> Table {
    let mut hash @ mut symtab @ mut strtab @ mut strsz @ mut syment = 0;
    for e in dynamic {
        let val = u64(&e[8..16]).try_into().expect("u64 == usize");
        match u64(&e[0..8]) {
            self::HASH => hash = val,
            self::SYMTAB => symtab = val,
            self::SYMENT => syment = val,
            self::STRTAB => strtab = val,
            self::STRSZ => strsz = val,
            _ => {}
        }
    }
    assert_eq!(syment, SYMENT_SIZE);
    let symtab = unsafe {
        let nchain = base.byte_add(hash).cast::<u32>().add(1).read_unaligned() as usize;
        let base = base.byte_add(symtab).cast::<[u8; SYMENT_SIZE]>().as_ptr();
        slice::from_raw_parts(base, nchain)
    };
    let strings = unsafe {
        let base = base.byte_add(strtab).as_ptr();
        slice::from_raw_parts(base, strsz)
    };
    unsafe { load_symtab(base, symtab, strings) }
}

unsafe fn load_symtab(base: NonNull<u8>, symtab: &[[u8; SYMENT_SIZE]], strings: &[u8]) -> Table {
    let mut tbl = Table::default();
    for x in symtab {
        let x = SymbolEntry::from_bytes(x);
        let name = &strings[x.name..];
        let name = &name[..name
            .iter()
            .position(|x| *x == 0)
            .expect("C string not terminated")];
        // FIXME we should retrieve the actual virtual base address probably...
        match name {
            b"__vdso_time" => unsafe {
                let addr = base.byte_add(x.value);
                tbl.time = Some(mem::transmute(addr));
            },
            b"__vdso_clock_gettime" => unsafe {
                let addr = base.byte_add(x.value);
                dbg!(addr);
                tbl.clock_gettime = Some(mem::transmute(addr));
            },
            _ => {}
        }
    }
    tbl
}

#[track_caller]
#[inline(always)]
fn u64(s: &[u8]) -> u64 {
    u64::from_le_bytes(s.try_into().expect("8 bytes"))
}

#[track_caller]
#[inline(always)]
fn u32(s: &[u8]) -> u32 {
    u32::from_le_bytes(s.try_into().expect("4 bytes"))
}

#[track_caller]
#[inline(always)]
fn u32_to_usize(s: &[u8]) -> usize {
    u32(s).try_into().expect("u32 <= usize")
}

#[track_caller]
#[inline(always)]
fn u64_to_usize(s: &[u8]) -> usize {
    u64(s).try_into().expect("u64 == usize")
}

#[track_caller]
#[inline(always)]
fn u16(s: &[u8]) -> u16 {
    u16::from_le_bytes(s.try_into().expect("2 bytes"))
}

fn ceil_p2(x: usize, n: usize) -> usize {
    floor_p2(x + (n - 1), n)
}

fn floor_p2(x: usize, n: usize) -> usize {
    x & !(n - 1)
}

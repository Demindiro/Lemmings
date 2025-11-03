use crate::page;
use core::{fmt, ptr::NonNull};

const ET_DYN: [u8; 2] = [3, 0];
const ARCH: [u8; 2] = [0x3e, 0];
const PH_ENTSIZE: usize = 56;

const PT_LOAD: u32 = 1;
const PT_DYNAMIC: u32 = 2;

const RELA: u64 = 7;
const RELASZ: u64 = 8;
const RELAENT: u64 = 9;

pub mod door {
    use core::{ptr::NonNull, slice};
    use lemmings_idl_loader_elf::*;

    door! {
        [lemmings_idl_loader_elf Loader "ELF loader"]
        load
    }

    unsafe fn load(x: Load) -> LoadResult {
        let Load {
            elf_base,
            elf_len,
            reason_base,
            reason_len,
        } = x;
        let elf =
            unsafe { slice::from_raw_parts(elf_base.0.as_ptr().cast::<u8>(), elf_len.into()) };
        let reason = unsafe {
            slice::from_raw_parts_mut(
                reason_base.0.as_ptr().cast::<u8>(),
                u16::from(reason_len).into(),
            )
        };
        match super::load(elf) {
            Ok(entry) => todo!("entry"),
            Err(e) => todo!("fail {e:?}"),
        }
    }
}

pub enum LoadError {
    BadMagic,
    EntryOutOfBounds,
    Not64Bit,
    NotLittleEndian,
    NotRelocatable,
    ProgramHeadersTruncated,
    SegmentNotAligned,
    TruncatedHeader,
    UnexpectedProgramHeaderSize,
    UnsupportedArchitecture,
    UnsupportedVersion,
    UnsupportedSegmentFlags,
    VirtualSizeZero,
    OutOfMemory,
    OutOfVirtSpace,
}

#[allow(dead_code)]
pub struct Entry(NonNull<u8>);

struct ElfMapper<'a> {
    virt_base: page::Virt,
    file: &'a [u8],
    dyn_rela: &'a [[u8; 24]],
}

impl From<page::ReserveRegionError> for LoadError {
    fn from(err: page::ReserveRegionError) -> Self {
        match err {
            page::ReserveRegionError::OutOfMemory => Self::OutOfMemory,
            page::ReserveRegionError::OutOfVirtSpace => Self::OutOfVirtSpace,
        }
    }
}

impl fmt::Debug for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::BadMagic => "bad magic",
            Self::EntryOutOfBounds => "entry out of bounds",
            Self::Not64Bit => "not 64 bit",
            Self::NotLittleEndian => "not little endian",
            Self::NotRelocatable => "not relocatable (DYN)",
            Self::ProgramHeadersTruncated => "program headers truncated",
            Self::SegmentNotAligned => "segment not aligned",
            Self::TruncatedHeader => "truncated header",
            Self::UnexpectedProgramHeaderSize => "unexpected program header size",
            Self::UnsupportedArchitecture => "unsupported architecture",
            Self::UnsupportedVersion => "unsupported version",
            Self::UnsupportedSegmentFlags => "unsupported segment flags",
            Self::VirtualSizeZero => "virtual size is zero",
            Self::OutOfMemory => "out of memory",
            Self::OutOfVirtSpace => "out of virtual memory space",
        };
        f.write_str(s)
    }
}

pub fn load(file: &[u8]) -> Result<Entry, LoadError> {
    use LoadError::*;
    let header: &[u8; 64] = file
        .get(..64)
        .ok_or(TruncatedHeader)?
        .try_into()
        .expect("64 bytes");
    assert(header[..4] == *b"\x7fELF", BadMagic)?;
    assert(header[4] == 2, Not64Bit)?;
    assert(header[5] == 1, NotLittleEndian)?;
    assert(header[6] == 1, UnsupportedVersion)?;
    assert(header[20] == 1, UnsupportedVersion)?;
    //assert(header[7] == 0, UnsupportedTarget)?;
    //assert(header[8], UnsupportedAbi)?;
    assert(header[16..18] == ET_DYN, NotRelocatable)?;
    assert(header[18..20] == ARCH, UnsupportedArchitecture)?;
    let entry = u64(&header[24..32]).try_into().expect("usize == u64");
    let program_headers = program_headers(file, header)?;
    let (virt_size, dyn_rela) = parse_program_headers(file, program_headers)?;
    if entry >= virt_size {
        return Err(EntryOutOfBounds);
    }
    let mapper = ElfMapper {
        virt_base: page::reserve_region(virt_size.try_into().map_err(|_| VirtualSizeZero)?)?,
        file,
        dyn_rela,
    };
    mapper.check_segments(&program_headers)?;
    mapper.alloc_segments(&program_headers)?;
    mapper.copy_segments(&program_headers);
    mapper.patch_dynamic();
    // SAFETY:
    // - entry is between 0..virt_size and hence is guaranteed
    //   to point to somewhere inside the virtual space of the process.
    let entry = unsafe { mapper.virt_base.byte_add(entry) };
    Ok(Entry(entry))
}

fn program_headers<'a>(
    file: &'a [u8],
    header: &'a [u8; 64],
) -> Result<&'a [[u8; PH_ENTSIZE]], LoadError> {
    let offt = usize::try_from(u64(&header[32..40])).expect("usize == u64");
    let entsize = usize::from(u16(&header[54..56]));
    assert(
        entsize == PH_ENTSIZE,
        LoadError::UnexpectedProgramHeaderSize,
    )?;
    let num = usize::from(u16(&header[56..58]));
    offt.checked_add(usize::from(entsize) * usize::from(num))
        .and_then(|end| file.get(offt..end))
        .map(|slice| slice.as_chunks().0)
        .ok_or(LoadError::ProgramHeadersTruncated)
}

fn parse_program_headers<'a>(
    file: &'a [u8],
    program_headers: &[[u8; PH_ENTSIZE]],
) -> Result<(usize, &'a [[u8; 24]]), LoadError> {
    let mut virt_size = 0;
    let mut dynamic = &[][..];
    for ph in program_headers {
        match u32(&ph[..4]) {
            self::PT_LOAD => {
                virt_size = virt_size.max(u64(&ph[16..24]) + u64(&ph[40..48]));
            }
            self::PT_DYNAMIC => {
                let [offt, len] = [&ph[8..16], &ph[32..40]]
                    .map(|x| usize::try_from(u64(x)).expect("u64 == usize"));
                let end = offt + len;
                dynamic = file[offt..end].as_chunks().0;
            }
            _ => {}
        }
    }
    let rela_dyn = parse_dynamic(file, dynamic)?;
    Ok((virt_size.try_into().expect("u64 == usize"), rela_dyn))
}

fn parse_dynamic<'a>(file: &'a [u8], dynamic: &[[u8; 16]]) -> Result<&'a [[u8; 24]], LoadError> {
    let mut rela @ mut relasz @ mut relaent = 0;
    for e in dynamic {
        let val = u64(&e[8..16]).try_into().expect("u64 == usize");
        match u64(&e[0..8]) {
            self::RELA => rela = val,
            self::RELASZ => relasz = val,
            self::RELAENT => relaent = val,
            _ => {}
        }
    }
    assert!(relasz == 0 || relaent == 24);
    Ok(file[rela..rela + relasz].as_chunks().0)
}

impl<'a> ElfMapper<'a> {
    fn check_segments(&self, program_headers: &[[u8; PH_ENTSIZE]]) -> Result<(), LoadError> {
        for ph in program_headers {
            if u32(&ph[..4]) != self::PT_LOAD {
                continue;
            }
            let offset = u64(&ph[8..16]);
            let vaddr = u64(&ph[16..24]);
            let align = u64(&ph[48..56]);
            if align != 0x1000 {
                return Err(LoadError::SegmentNotAligned);
            }
            if offset % align != vaddr % align {
                return Err(LoadError::SegmentNotAligned);
            }
        }
        Ok(())
    }

    fn alloc_segments(&self, program_headers: &[[u8; PH_ENTSIZE]]) -> Result<(), LoadError> {
        for (i, ph) in program_headers.iter().enumerate() {
            if u32(&ph[..4]) != self::PT_LOAD {
                continue;
            }
            let err = match self.alloc_one_segment(ph) {
                Ok(()) => continue,
                Err(e) => e,
            };
            for _k in 0..i {
                todo!("free allocated segments");
            }
        }
        Ok(())
    }

    fn copy_segments(&self, program_headers: &[[u8; PH_ENTSIZE]]) {
        for ph in program_headers {
            if u32(&ph[..4]) != self::PT_LOAD {
                continue;
            }
            self.copy_one_segment(ph);
        }
    }

    fn alloc_one_segment(&self, ph: &[u8; PH_ENTSIZE]) -> Result<(), LoadError> {
        let flags = u32(&ph[4..8]);
        let vaddr = u64_to_usize(&ph[16..24]);
        let memsz = u64_to_usize(&ph[40..48]);

        let floor = |x| floor_p2(x, page::PAGE_SIZE);
        let ceil = |x| ceil_p2(x, page::PAGE_SIZE);
        let va = floor(vaddr);
        let va_end = ceil(vaddr + memsz);

        let [va, va_end] = [va, va_end].map(|x| unsafe { self.virt_base.byte_add(x) });

        let attr = flags_to_attr(flags).ok_or(LoadError::UnsupportedSegmentFlags)?;
        unsafe { page::map_region_zero(va..va_end, attr)? };
        Ok(())
    }

    fn copy_one_segment(&self, ph: &[u8; PH_ENTSIZE]) {
        let flags = u32(&ph[4..8]);
        let offset = u64_to_usize(&ph[8..16]);
        let vaddr = u64_to_usize(&ph[16..24]);
        let filesz = u64_to_usize(&ph[32..40]);

        let [va, va_end] = [vaddr, vaddr + filesz].map(|x| unsafe { self.virt_base.byte_add(x) });
        let src = &self.file[offset..offset + filesz];

        unsafe {
            page::copy_to_region(va, src);
        }
    }

    fn patch_dynamic(&self) {
        for _e in self.dyn_rela {
            todo!("dynamic");
        }
    }
}

impl From<page::OutOfMemory> for LoadError {
    fn from(_: page::OutOfMemory) -> Self {
        Self::OutOfMemory
    }
}

#[inline(always)]
fn assert<E>(cond: bool, error: E) -> Result<(), E> {
    cond.then_some(()).ok_or(error)
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

fn flags_to_attr(flags: u32) -> Option<page::PageAttr> {
    Some(match flags {
        0b100 => page::PageAttr::R,
        0b101 => page::PageAttr::RX,
        0b110 => page::PageAttr::RW,
        0b111 => page::PageAttr::RWX,
        _ => return None,
    })
}

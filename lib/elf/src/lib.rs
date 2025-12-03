#![no_std]
#![feature(slice_as_chunks)] // stabilized in 1.88

use core::{fmt, num::NonZero, ops::Range, ptr::NonNull};

const ET_DYN: [u8; 2] = [3, 0];
const ARCH: [u8; 2] = [0x3e, 0];
const PH_ENTSIZE: usize = 56;

const PT_LOAD: u32 = 1;
const PT_DYNAMIC: u32 = 2;

const RELA: u64 = 7;
const RELASZ: u64 = 8;
const RELAENT: u64 = 9;

pub trait Allocator {
    type ReserveError;
    type MapError;

    const PAGE_SIZE: usize;

    fn reserve(&mut self, len: NonZero<usize>) -> Result<NonNull<u8>, Self::ReserveError>;
    /// Fill the region with *zeroed* pages.
    unsafe fn alloc_region(
        &mut self,
        range: Range<NonNull<u8>>,
        attr: PageAttr,
        will_write: bool,
    ) -> Result<(), Self::MapError>;
    unsafe fn copy_to_region(&mut self, va: NonNull<u8>, src: &[u8]);
}

pub enum PageAttr {
    R,
    RW,
    RX,
    RWX,
}

pub enum LoadError<A>
where
    A: Allocator,
{
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
    ReserveRegion(A::ReserveError),
    MapRegion(A::MapError),
}

#[allow(dead_code)]
pub struct Entry(NonNull<u8>);

struct ElfMapper<'a, A> {
    virt_base: NonNull<u8>,
    file: &'a [u8],
    dyn_rela: &'a [[u8; 24]],
    alloc: A,
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

impl<A> fmt::Debug for LoadError<A>
where
    A: Allocator,
    A::ReserveError: fmt::Debug,
    A::MapError: fmt::Debug,
{
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
            Self::ReserveRegion(e) => return e.fmt(f),
            Self::MapRegion(e) => return e.fmt(f),
        };
        f.write_str(s)
    }
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

pub fn load<A>(file: &[u8], mut alloc: A) -> Result<Entry, LoadError<A>>
where
    A: Allocator,
{
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
    let entry = u64_to_usize(&header[24..32]);
    let program_headers = program_headers(file, header)?;
    let ph = || program_headers.iter().map(ProgramHeader::from_bytes);
    let (virt_size, dyn_rela) = parse_program_headers(file, ph())?;
    if entry >= virt_size {
        return Err(EntryOutOfBounds);
    }
    let mut mapper = ElfMapper {
        virt_base: alloc
            .reserve(virt_size.try_into().map_err(|_| VirtualSizeZero)?)
            .map_err(LoadError::ReserveRegion)?,
        file,
        dyn_rela,
        alloc,
    };
    mapper.check_segments(ph())?;
    mapper.alloc_segments(ph())?;
    mapper.copy_segments(ph());
    mapper.patch_dynamic();
    // SAFETY:
    // - entry is between 0..virt_size and hence is guaranteed
    //   to point to somewhere inside the virtual space of the process.
    let entry = unsafe { mapper.virt_base.byte_add(entry) };
    Ok(Entry(entry))
}

fn program_headers<'a, A>(
    file: &'a [u8],
    header: &'a [u8; 64],
) -> Result<&'a [[u8; PH_ENTSIZE]], LoadError<A>>
where
    A: Allocator,
{
    let offt = u64_to_usize(&header[32..40]);
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

fn parse_program_headers<'a, I, A>(
    file: &'a [u8],
    program_headers: I,
) -> Result<(usize, &'a [[u8; 24]]), LoadError<A>>
where
    I: Iterator<Item = ProgramHeader>,
    A: Allocator,
{
    let mut virt_size = 0;
    let mut dynamic = &[][..];
    for ph in program_headers {
        let ProgramHeader {
            ty,
            offset,
            vaddr,
            filesz,
            memsz,
            ..
        } = ph;
        match ty {
            self::PT_LOAD => {
                virt_size = virt_size.max(vaddr + memsz);
            }
            self::PT_DYNAMIC => {
                dynamic = file[offset..offset + filesz].as_chunks().0;
            }
            _ => {}
        }
    }
    let rela_dyn = parse_dynamic(file, dynamic)?;
    Ok((virt_size, rela_dyn))
}

fn parse_dynamic<'a, A>(
    file: &'a [u8],
    dynamic: &[[u8; 16]],
) -> Result<&'a [[u8; 24]], LoadError<A>>
where
    A: Allocator,
{
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

impl<'a, A> ElfMapper<'a, A>
where
    A: Allocator,
{
    fn check_segments<I>(&self, program_headers: I) -> Result<(), LoadError<A>>
    where
        I: Iterator<Item = ProgramHeader>,
    {
        for ph in program_headers.filter(|ph| ph.ty == PT_LOAD) {
            let ProgramHeader {
                offset,
                vaddr,
                align,
                ..
            } = ph;
            if align != 0x1000 {
                return Err(LoadError::SegmentNotAligned);
            }
            if offset % align != vaddr % align {
                return Err(LoadError::SegmentNotAligned);
            }
        }
        Ok(())
    }

    fn alloc_segments<I>(&mut self, program_headers: I) -> Result<(), LoadError<A>>
    where
        I: Iterator<Item = ProgramHeader>,
    {
        for (i, ph) in program_headers
            .enumerate()
            .filter(|(_, ph)| ph.ty == PT_LOAD)
        {
            let err = match self.alloc_one_segment(ph) {
                Ok(()) => continue,
                Err(e) => e,
            };
            for _k in 0..i {
                todo!("free allocated segments");
            }
            return Err(err);
        }
        Ok(())
    }

    fn copy_segments<I>(&mut self, program_headers: I)
    where
        I: Iterator<Item = ProgramHeader>,
    {
        program_headers
            .filter(|ph| ph.ty == PT_LOAD)
            .for_each(|ph| self.copy_one_segment(ph));
    }

    fn alloc_one_segment(&mut self, ph: ProgramHeader) -> Result<(), LoadError<A>> {
        let ProgramHeader {
            flags,
            vaddr,
            memsz,
            ..
        } = ph;

        let floor = |x| floor_p2(x, A::PAGE_SIZE);
        let ceil = |x| ceil_p2(x, A::PAGE_SIZE);
        let va = floor(vaddr);
        let va_end = ceil(vaddr + memsz);

        let [va, va_end] = [va, va_end].map(|x| unsafe { self.virt_base.byte_add(x) });

        let attr = flags_to_attr(flags).ok_or(LoadError::UnsupportedSegmentFlags)?;
        unsafe {
            self.alloc
                .alloc_region(va..va_end, attr, true)
                .map_err(LoadError::MapRegion)?
        };
        Ok(())
    }

    fn copy_one_segment(&mut self, ph: ProgramHeader) {
        let ProgramHeader {
            offset,
            vaddr,
            filesz,
            ..
        } = ph;
        let va = unsafe { self.virt_base.byte_add(vaddr) };
        let src = &self.file[offset..offset + filesz];
        unsafe {
            self.alloc.copy_to_region(va, src);
        }
    }

    fn patch_dynamic(&self) {
        for e in self.dyn_rela {
            let offset = u64_to_usize(&e[0..8]);
            let addend = u64_to_usize(&e[16..24]);
            // TODO we don't need to disable write protection?
            // ... this indicates we may need to clear the R/W bit ourselves,
            // which is annoying.
            unsafe {
                let p = self.virt_base.byte_add(offset).cast::<usize>();
                p.write_unaligned(self.virt_base.as_ptr().addr() + addend);
            }
        }
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

fn flags_to_attr(flags: u32) -> Option<PageAttr> {
    Some(match flags {
        0b100 => PageAttr::R,
        0b101 => PageAttr::RX,
        0b110 => PageAttr::RW,
        0b111 => PageAttr::RWX,
        _ => return None,
    })
}

#![allow(dead_code)]
pub use lemmings_x86_64::mmu::PageAttr;

use crate::{KernelEntryToken, sync::SpinLock};
use core::{
    mem,
    num::NonZero,
    ops::{self, Range},
    ptr::NonNull,
    slice,
};
use critical_section::CriticalSection;
use lemmings_qemubios::MemoryRegion;
use lemmings_x86_64::mmu;

static PAGE: SpinLock<PageManager> = SpinLock::new(PageManager::new());
static VIRT: SpinLock<VirtManager> = SpinLock::new(VirtManager::new());

static mut CONSTANT_PAGES: mem::MaybeUninit<ConstantPages> = mem::MaybeUninit::uninit();

// TODO should be moved to alloc.rs
pub mod door {
    use lemmings_idl_physical_allocator::*;

    door! {
        [lemmings_idl_physical_allocator Allocator "Physical memory allocator"]
        identity_base
        alloc64
        alloc32
        free
    }

    fn identity_base() -> IdentityBase {
        // fuck
        todo!("it's fucked Jim");
    }

    fn alloc64(Alloc64 { len, align }: Alloc64) -> MaybeRegion64 {
        if u8::from(align) > 12 {
            return NoRegion64 { len }.into();
        }
        usize::try_from((u64::from(len.clone()) + 0xfff) >> 12)
            .ok()
            .and_then(core::num::NonZero::new)
            .and_then(|count| super::alloc_4k_phys(count).ok())
            .map_or(NoRegion64 { len: len.clone() }.into(), |x| {
                Region64 {
                    base: x.0.try_into().unwrap(),
                    len,
                }
                .into()
            })
    }

    fn alloc32(Alloc32 { len, align }: Alloc32) -> MaybeRegion32 {
        if u8::from(align) > 12 {
            return NoRegion32 { len }.into();
        }
        usize::try_from((u32::from(len.clone()) + 0xfff) >> 12)
            .ok()
            .and_then(core::num::NonZero::new)
            .and_then(|count| super::alloc_4k_phys(count).ok())
            .map_or(NoRegion32 { len: len.clone() }.into(), |x| {
                Region32 {
                    base: u32::try_from(x.0).unwrap().try_into().unwrap(),
                    len,
                }
                .into()
            })
    }

    fn free(Free { base, len }: Free) {
        todo!("free base={base:?} len={len:?}");
    }
}

pub const PAGE_SIZE: usize = 4096;
pub const PAGE_MASK: usize = PAGE_SIZE - 1;

const HUGEPAGE_2M_SIZE: usize = 1 << 21;
const HUGEPAGE_1G_SIZE: usize = 1 << 30;
const HUGEPAGE_2M_MASK: usize = HUGEPAGE_2M_SIZE - 1;
const HUGEPAGE_1G_MASK: usize = HUGEPAGE_1G_SIZE - 1;

pub use lemmings_qemubios::Phys;
pub type Virt = NonNull<u8>;

#[derive(Debug)]
pub struct OutOfMemory;
pub struct OutOfVirtSpace;

pub enum ReserveRegionError {
    OutOfMemory,
    OutOfVirtSpace,
}

pub enum AllocGuardedError {
    OutOfMemory,
    OutOfVirtSpace,
}

#[repr(align(4096))]
pub struct Page;

struct PageManager {
    /// Just a simple linked list of 4K pages for now.
    head: Option<Virt>,
    /// Quick hack to make DMA allocation work
    contiguous_base: Virt,
    contiguous_num: usize,
}

struct VirtManager {
    head: Virt,
}

pub struct IdentityMapper;
struct PageAllocator;

struct ConstantPages {
    zero: [mmu::Phys<mmu::A12>; 3],
    ones: [mmu::Phys<mmu::A12>; 3],
}

impl PageManager {
    pub const fn new() -> Self {
        Self {
            head: None,
            contiguous_base: Virt::dangling(),
            contiguous_num: 0,
        }
    }

    pub fn alloc_4k(&mut self, num_4k: usize) -> Result<Virt, OutOfMemory> {
        if let Some(page) = self.head.filter(|_| num_4k == 1) {
            self.head = unsafe { page.cast::<Option<Virt>>().read() };
            return Ok(page);
        }
        if let Some(n) = self.contiguous_num.checked_sub(num_4k) {
            let page = unsafe { self.contiguous_base.byte_add(n << 12) };
            self.contiguous_num = n;
            return Ok(page);
        }
        Err(OutOfMemory)
    }
}

impl VirtManager {
    pub const fn new() -> Self {
        Self {
            head: Virt::dangling(),
        }
    }

    /// Automatically puts guard pages of minimum 4K around the region.
    pub fn reserve_region(&mut self, size: NonZero<usize>) -> Result<Virt, ReserveRegionError> {
        unsafe {
            let start = self.head.byte_add(PAGE_SIZE);
            let end = start.byte_add((size.get() + PAGE_MASK) & !PAGE_MASK);
            self.head = end.byte_add(PAGE_SIZE);
            Ok(start)
        }
    }

    /// # Safety
    ///
    /// The address range must not be actively used.
    pub unsafe fn map_range(
        &mut self,
        range: ops::Range<Virt>,
        phys: Phys,
        attr: PageAttr,
    ) -> Result<(), OutOfMemory> {
        let mut va = mmu::Virt::<mmu::A12>::new(range.start.as_ptr() as u64).unwrap();
        let mut pa = mmu::Phys::<mmu::A12>::new(phys.0).unwrap();
        let va_end = mmu::Virt::<mmu::A12>::new(range.end.as_ptr() as u64).unwrap();
        let mut root = unsafe { mmu::current_root::<mmu::L4>() };
        let va_start = va;
        while va != va_end {
            /*
            if va. & HUGEPAGE_1G_MASK == 0 {
                todo!("1G")
            }
            if va.addr().get() & HUGEPAGE_2M_MASK == 0 {
                todo!("2M")
            }
            */
            unsafe {
                root.set_4k(&IdentityMapper, &mut PageAllocator, va, pa, attr)
                    .inspect_err(|_| self.unmap_range(va_start..va, |_| ()))?;
            }
            va = va.step_next(1);
            pa = pa.step_next(1);
        }
        Ok(())
    }

    /// # Safety
    ///
    /// `will_write` must be set accurately.
    /// If it is set to `false`, constant pages will be mapped.
    pub unsafe fn map_range_zero(
        &mut self,
        range: Range<Virt>,
        attr: PageAttr,
        will_write: bool,
    ) -> Result<(), OutOfMemory> {
        let constant_pages = unsafe { (&*(&raw const CONSTANT_PAGES)).assume_init_ref() };
        let mut va = mmu::Virt::<mmu::A12>::new(range.start.as_ptr() as u64).unwrap();
        let va_end = mmu::Virt::<mmu::A12>::new(range.end.as_ptr() as u64).unwrap();
        let mut root = unsafe { mmu::current_root::<mmu::L4>() };
        let va_start = va;
        while va != va_end {
            // FIXME deadlock...
            let pa = if will_write || attr.w() {
                mmu::Phys::<mmu::A12>::new(virt_to_phys(alloc_one()?).0).unwrap()
            } else {
                //mmu::Phys::<mmu::A12>::new(virt_to_phys(alloc_one()?).0).unwrap()
                constant_pages.zero[0]
            };
            unsafe {
                root.set_4k(&IdentityMapper, &mut PageAllocator, va, pa, attr)
                    .inspect_err(|_| self.unmap_range(va_start..va, |_| ()))?;
            }
            va = va.step_next(1);
        }
        Ok(())
    }

    unsafe fn unmap_range<F>(&mut self, range: ops::Range<mmu::Virt<mmu::A12>>, f: F)
    where
        F: FnMut(Range<Phys>),
    {
        let _ = (range, f);
        todo!();
    }
}

impl mmu::PhysToVirt for IdentityMapper {
    fn virt<A>(&self, phys: mmu::Phys<A>) -> mmu::Virt<A> {
        phys.cast()
    }
}
unsafe impl mmu::PhysToPtr for IdentityMapper {}

unsafe impl mmu::PageAllocator for PageAllocator {
    type Error = OutOfMemory;

    fn alloc(&mut self) -> Result<(NonNull<u8>, mmu::Phys<mmu::A12>), Self::Error> {
        let virt = alloc_one()?;
        Ok((virt, mmu::Phys::new(virt_to_phys(virt).0).unwrap()))
    }
}

impl From<OutOfMemory> for ReserveRegionError {
    fn from(_: OutOfMemory) -> Self {
        Self::OutOfMemory
    }
}

impl From<OutOfMemory> for AllocGuardedError {
    fn from(_: OutOfMemory) -> Self {
        Self::OutOfMemory
    }
}

impl From<ReserveRegionError> for AllocGuardedError {
    fn from(x: ReserveRegionError) -> Self {
        match x {
            ReserveRegionError::OutOfMemory => Self::OutOfMemory,
            ReserveRegionError::OutOfVirtSpace => Self::OutOfVirtSpace,
        }
    }
}

fn alloc_4k_phys(count: NonZero<usize>) -> Result<Phys, OutOfMemory> {
    critical_section::with(|cs| PAGE.lock(cs).alloc_4k(count.get()).map(virt_to_phys))
}

pub fn alloc_one() -> Result<Virt, OutOfMemory> {
    critical_section::with(|cs| PAGE.lock(cs).alloc_4k(1))
}

pub fn alloc_one_guarded(attr: PageAttr) -> Result<Virt, AllocGuardedError> {
    critical_section::with(|cs| {
        let page = { PAGE.lock(cs).alloc_4k(1)? };
        {
            let mut virt = VIRT.lock(cs);
            let addr = virt.reserve_region(PAGE_SIZE.try_into().unwrap())?;
            unsafe { virt.map_range(addr..addr.byte_add(PAGE_SIZE), virt_to_phys(page), attr)? }
            Ok(addr)
        }
    })
}

/// Copy `data` to `base`.
///
/// This routine bypasses write protections protections.
pub unsafe fn copy_to_region(base: Virt, data: &[u8]) {
    // We will temporarily disable write-protection.
    // To avoid having this protection disabled while running other code,
    // disable interrupts too.
    //
    // TODO: break up large copies into smaller chunks so we don't keep
    // interrupts disabled for excessively long periods
    critical_section::with(|cs| unsafe {
        disable_write_protection(cs, || {
            base.as_ptr()
                .copy_from_nonoverlapping(data.as_ptr(), data.len())
        })
    });
}

/// Disable write protections for the duration of `f`.
pub unsafe fn disable_write_protection<F>(_cs: CriticalSection<'_>, f: F)
where
    F: FnOnce(),
{
    use lemmings_x86_64::cr0;
    let og_cr0 = unsafe { cr0::update(|x| x & !cr0::WRITE_PROTECT) };
    (f)();
    unsafe { cr0::set(og_cr0) };
}

/// Automatically puts guard pages of minimum 4K around the region.
pub fn reserve_region(size: NonZero<usize>) -> Result<Virt, ReserveRegionError> {
    critical_section::with(|cs| VIRT.lock(cs).reserve_region(size))
}

/// # Safety
///
/// The address range must not be actively used.
pub unsafe fn map_region(
    region: Range<Virt>,
    phys_base: Phys,
    attr: PageAttr,
) -> Result<(), OutOfMemory> {
    critical_section::with(|cs| unsafe { VIRT.lock(cs).map_range(region, phys_base, attr) })
}

/// # Safety
///
/// The address range must not be actively used.
///
/// `will_write` must be set accurately.
/// If it is set to `false`, constant pages will be mapped.
/// This is relevant when using [`copy_to_region`].
pub unsafe fn map_region_zero(
    region: Range<Virt>,
    attr: PageAttr,
    will_write: bool,
) -> Result<(), OutOfMemory> {
    critical_section::with(|cs| unsafe { VIRT.lock(cs).map_range_zero(region, attr, will_write) })
}

#[inline]
pub fn init<'a>(
    entry: &lemmings_qemubios::Entry,
    token: KernelEntryToken<'a>,
) -> KernelEntryToken<'a> {
    let token = init_page(entry, token);
    let token = init_virt(entry, token);
    token
}

fn init_page<'a>(
    entry: &lemmings_qemubios::Entry,
    token: KernelEntryToken<'a>,
) -> KernelEntryToken<'a> {
    let regions = unsafe { region_to_slice::<MemoryRegion>(entry.memory.list) };
    let mut head = None;
    let mut contiguous_base = Virt::dangling();
    let mut contiguous_num = 0;
    for r in regions {
        if r.start == r.end {
            continue;
        }
        if contiguous_num == 0 {
            log!("contiguous {r:?}");
            contiguous_base = phys_to_virt(r.start);
            contiguous_num = (r.end.0 - r.start.0) as usize >> 12;
            continue;
        }
        log!("linked {r:?}");
        let mut a = phys_to_virt(r.start);
        let end = phys_to_virt(r.end);
        assert!(
            a.cast::<Page>().is_aligned(),
            "start not aligned to page boundary"
        );
        assert!(
            end.cast::<Page>().is_aligned(),
            "end not aligned to page boundary"
        );
        while a < end {
            unsafe {
                a.cast::<Option<Virt>>().write(head);
            }
            head = Some(a);
            a = unsafe { a.byte_add(PAGE_SIZE) };
        }
    }
    let token = PAGE.set(
        PageManager {
            head,
            contiguous_base,
            contiguous_num,
        },
        token,
    );
    token
}

fn init_virt<'a>(
    entry: &lemmings_qemubios::Entry,
    token: KernelEntryToken<'a>,
) -> KernelEntryToken<'a> {
    let constant_pages = unsafe { &mut *(&raw mut CONSTANT_PAGES) };
    let f = |x: lemmings_qemubios::Phys| {
        mmu::Phys::<mmu::A12>::new(x.0).expect("constant page is not aligned!")
    };
    constant_pages.write(ConstantPages {
        zero: entry.paging.zero.map(f),
        ones: entry.paging.ones.map(f),
    });
    let head = entry.paging.kernel.end.0;
    let token = VIRT.set(VirtManager { head }, token);
    token
}

pub fn phys_to_virt(p: Phys) -> Virt {
    NonNull::new(p.0 as _).unwrap()
}
pub fn virt_to_phys(v: Virt) -> Phys {
    lemmings_qemubios::Phys(v.as_ptr() as _)
}

unsafe fn region_to_slice<T>(region: MemoryRegion) -> &'static [T] {
    assert!(region.start.0 <= region.end.0, "region has negative size");
    let len = (region.end.0 - region.start.0) as usize;
    assert_eq!(len % mem::size_of::<T>(), 0, "not a multiple of T");
    unsafe {
        let ptr = phys_to_virt(region.start).cast::<T>();
        slice::from_raw_parts(ptr.as_ptr(), len / mem::size_of::<T>())
    }
}

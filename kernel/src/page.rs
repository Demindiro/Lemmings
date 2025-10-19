pub use lemmings_x86_64::mmu::PageAttr;

use crate::{KernelEntryToken, sync::SpinLock};
use core::{mem, num::NonZero, ops, ptr::NonNull, slice};
use lemmings_qemubios::MemoryRegion;
use lemmings_x86_64::mmu;

static PAGE: SpinLock<PageManager> = SpinLock::new(PageManager::new());
static VIRT: SpinLock<VirtManager> = SpinLock::new(VirtManager::new());

pub const PAGE_SIZE: usize = 4096;
pub const PAGE_MASK: usize = PAGE_SIZE - 1;

const HUGEPAGE_2M_SIZE: usize = 1 << 21;
const HUGEPAGE_1G_SIZE: usize = 1 << 30;
const HUGEPAGE_2M_MASK: usize = HUGEPAGE_2M_SIZE - 1;
const HUGEPAGE_1G_MASK: usize = HUGEPAGE_1G_SIZE - 1;

pub type Phys = lemmings_qemubios::Phys;
pub type Virt = NonNull<u8>;

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
struct Page;

struct PageManager {
    /// Just a simple linked list of 4K pages for now.
    head: Option<Virt>,
}

struct VirtManager {
    head: Virt,
}

struct IdentityMapper;
struct PageAllocator;

impl PageManager {
    pub const fn new() -> Self {
        Self { head: None }
    }

    pub fn alloc_one(&mut self) -> Result<Virt, OutOfMemory> {
        let page = self.head.ok_or(OutOfMemory)?;
        self.head = unsafe { page.cast::<Option<Virt>>().read() };
        Ok(page)
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
    pub unsafe fn map_range(&mut self, range: ops::Range<Virt>, phys: Phys, attr: PageAttr) -> Result<(), OutOfMemory> {
        let mut va = mmu::Virt::<mmu::A12>::new(range.start.as_ptr() as u64).unwrap();
        let mut pa = mmu::Phys::<mmu::A12>::new(phys.0).unwrap();
        let va_end = mmu::Virt::<mmu::A12>::new(range.end.as_ptr() as u64).unwrap();
        let mut root = unsafe { mmu::current_root::<mmu::L4>() };
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
                root.set_4k(&IdentityMapper, &mut PageAllocator, va, pa, attr);
            }
            va = va.step_next(1);
            pa = pa.step_next(1);
        }
        Ok(())
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

pub fn alloc_one() -> Result<Virt, OutOfMemory> {
    critical_section::with(|cs| {
        PAGE.lock(cs).alloc_one()
    })
}

pub fn alloc_one_guarded(attr: PageAttr) -> Result<Virt, AllocGuardedError> {
    critical_section::with(|cs| {
        let page = {
            PAGE.lock(cs).alloc_one()?
        };
        {
            let mut virt = VIRT.lock(cs);
            let addr = virt.reserve_region(PAGE_SIZE.try_into().unwrap())?;
            unsafe { virt.map_range(addr..addr.byte_add(PAGE_SIZE), virt_to_phys(page), attr)? }
            Ok(addr)
        }
    })
}

/// Automatically puts guard pages of minimum 4K around the region.
pub fn reserve_region(size: NonZero<usize>) -> Result<Virt, ReserveRegionError> {
    critical_section::with(|cs| VIRT.lock(cs).reserve_region(size))
}

#[inline]
pub fn init(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    let token = init_page(entry, token);
    let token = init_virt(entry, token);
    token
}

fn init_page(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    let regions = unsafe { region_to_slice::<MemoryRegion>(entry.memory.list) };
    let mut head = None;
    for r in regions {
        if r.start == r.end {
            continue;
        }
        let mut a = phys_to_virt(r.start);
        let end = phys_to_virt(r.end);
        assert!(a.cast::<Page>().is_aligned(), "start not aligned to page boundary");
        assert!(end.cast::<Page>().is_aligned(), "end not aligned to page boundary");
        while a < end {
            unsafe { a.cast::<Option<Virt>>().write(head); }
            head = Some(a);
            a = unsafe { a.byte_add(PAGE_SIZE) };
        }
    }
    let token = PAGE.set(PageManager { head }, token);
    token
}

fn init_virt(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    let head = entry.paging.kernel.end.0;
    let token = VIRT.set(VirtManager { head }, token);
    token
}

pub fn phys_to_virt(p: Phys) -> Virt {
    NonNull::new(p.0 as _).unwrap()
}
fn virt_to_phys(v: Virt) -> Phys {
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

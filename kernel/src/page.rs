use crate::{KernelEntryToken, sync::SpinLock};
use core::{mem, ptr::NonNull, slice};
use lemmings_qemubios::MemoryRegion;

static PAGE: SpinLock<PageManager> = SpinLock::new(PageManager::new());
static VIRT: SpinLock<VirtManager> = SpinLock::new(VirtManager::new());

pub const PAGE_SIZE: usize = 4096;

pub type Phys = lemmings_qemubios::Phys;
pub type Virt = NonNull<u8>;

pub struct OutOfMemory;

#[repr(align(4096))]
struct Page;

struct PageManager {
    /// Just a simple linked list of 4K pages for now.
    head: Option<Virt>,
}

struct VirtManager {
}

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
        Self {}
    }
}

pub fn alloc_one() -> Result<Virt, OutOfMemory> {
    critical_section::with(|cs| {
        PAGE.lock(cs).alloc_one()
    })
}

#[inline]
pub fn init(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
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

fn phys_to_virt(p: Phys) -> Virt {
    NonNull::new(p.0 as _).unwrap()
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

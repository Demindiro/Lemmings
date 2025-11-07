use crate::{KernelEntryToken, page};
use core::{mem::MaybeUninit, ptr::NonNull};

static mut BASE: MaybeUninit<NonNull<[u8; 4096]>> = MaybeUninit::uninit();

pub mod door {
    use lemmings_idl_pci::*;

    door! {
        [lemmings_idl_pci Pci "PCI"]
        configuration
        acquire_dev
        release_dev
        map_msi
        unmap_msi
        wait_msi
        allocate_mmio32
        allocate_mmio64
        release_mmio
    }

    fn configuration() -> Configuration {
        let base = unsafe { super::BASE.assume_init() };
        Configuration {
            base: base.cast::<ffi::PciFunction>().into(),
            // TODO don't hardcode!
            segment_group: 0.into(),
            bus_start: 0.into(),
            bus_end: 255.into(),
        }
    }

    fn acquire_dev(x: Bdf) -> AcquireResult {
        todo!()
    }

    fn release_dev(x: Bdf) {
        todo!()
    }

    fn map_msi() -> MaybeMsiVector {
        todo!()
    }

    fn unmap_msi(x: Vector) {
        todo!()
    }

    fn wait_msi(x: Vector) {
        todo!()
    }

    fn allocate_mmio32(x: AllocateMmio32) -> MaybeAddr32 {
        todo!()
    }

    fn allocate_mmio64(x: AllocateMmio64) -> MaybeAddr64 {
        todo!()
    }

    fn release_mmio(x: Addr64) {
        todo!()
    }
}

pub fn init(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    let virt = page::phys_to_virt(entry.pcie.base).cast();
    log!(
        "PCIe configuration region at {:?} ({:?})",
        entry.pcie.base,
        virt
    );
    unsafe { BASE = MaybeUninit::new(virt) };
    token
}

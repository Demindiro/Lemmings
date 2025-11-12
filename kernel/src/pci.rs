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
        register_msi
        unregister_msi
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

    fn register_msi() -> MaybeMsi {
        crate::arch::register_msi().map_or(NoMsi { data: 0.into() }.into(), |x| {
            Msi {
                address: u64::from(x.address).try_into().unwrap(),
                data: x.data.into(),
            }
            .into()
        })
    }

    fn unregister_msi(Msi { address, data }: Msi) {
        crate::arch::unregister_msi(crate::arch::Msi {
            address: lemmings_x86_64::mmu::Phys::new(u64::from(address)).unwrap(),
            data: data.into(),
        });
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

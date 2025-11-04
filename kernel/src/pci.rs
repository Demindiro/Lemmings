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
        todo!()
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

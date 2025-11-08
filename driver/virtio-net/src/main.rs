#![no_std]
#![no_main]
#![forbid(improper_ctypes_definitions)]

extern crate lemmings_door;

use core::ptr::NonNull;
use lemmings_door::{ApiId, Cookie, Log};
use lemmings_idl_pci::Pci;
use lemmings_pci::{Header, Header0, ParsedBaseAddress};

const VENDOR_ID: u16 = 0x1af4;
const DEVICE_ID: u16 = 0x1000;

fn main() {
    let (door, _) = lemmings_door::door_find::<Pci>(Cookie(0)).expect("no PCI door found");
    let door = door.get();
    let lemmings_idl_pci::Configuration {
        base,
        segment_group,
        bus_start,
        bus_end,
    } = door.configuration();
    assert!(u8::from(bus_start) == 0 && u8::from(bus_end) == 0xff);
    let pci = unsafe { base.0.cast::<lemmings_pci::Pci>().as_ref() };
    pci.list(|_, hdr| try_device(hdr));
}

fn try_device(header: Header<'_>) {
    let Header::H0(hdr) = header else {
        return;
    };
    if hdr.vendor_id() != VENDOR_ID {
        return;
    }
    if hdr.device_id() != DEVICE_ID {
        return;
    }
    start_device(hdr);
}

fn start_device(header: &Header0) -> ! {
    let msix = lemmings_virtio_net::Msix {
        receive_queue: Some(0),
        transmit_queue: Some(1),
    };
    let bar_map = &map_bars(header);
    let dma_alloc = |x| -> Result<_, ()> {
        todo!();
    };
    let dev = unsafe { lemmings_virtio_net::Device::new(header, msix, bar_map, dma_alloc) };
    todo!("Use the device, Luke!");
}

fn map_bars(header: &Header0) -> [Option<NonNull<u8>>; 6] {
    let mut i = 0;
    let mut map = [None; 6];
    while i < 6 {
        match header.full_base_address(i) {
            None | Some(ParsedBaseAddress::IO32 { .. }) => i += 1,
            Some(ParsedBaseAddress::MMIO32 { address, .. }) => {
                map[i] = NonNull::new(address as _);
                i += 1;
            }
            Some(ParsedBaseAddress::MMIO64 { address, .. }) => {
                map[i] = NonNull::new(address as _);
                i += 2;
            }
        }
    }
    use core::fmt::Write;
    let _ = write!(Log::new(), "{map:?}");
    map
}

#[unsafe(no_mangle)]
extern "sysv64" fn _start() {
    main()
}

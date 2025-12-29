#![no_std]
#![no_main]
#![forbid(improper_ctypes_definitions, unused_must_use)]
#![feature(cell_update)] // stabilized in 1.88

extern crate lemmings_door;

use core::{
    cell::{Cell, RefCell},
    mem::{self, ManuallyDrop, MaybeUninit},
    ptr::NonNull,
};
use lemmings_allocator::Allocator;
use lemmings_door::{Log, log};
use lemmings_idl_pci::Pci;
use lemmings_pci::{Header, Header0, HeaderCommon, ParsedBaseAddress};
use lemmings_spinlock::SpinLock;
use lemmings_virtio_net::{Device, PacketHeader, PhysAddr, PhysRegion};

mod crit {
    use core::arch::asm;
    use critical_section::RawRestoreState;

    struct Impl;
    critical_section::set_impl!(Impl);

    unsafe impl critical_section::Impl for Impl {
        unsafe fn acquire() -> RawRestoreState {
            let token;
            unsafe {
                asm! {
                    "pushf",
                    "pop {0:r}",
                    "cli",
                    out(reg) token,
                    options(nomem, preserves_flags),
                }
            }
            token
        }

        unsafe fn release(token: RawRestoreState) {
            unsafe {
                asm! {
                    "push {0:r}",
                    "popf",
                    in(reg) token,
                    options(nomem),
                }
            }
        }
    }
}

mod door {
    use lemmings_idl_ethernet::*;

    pub static ETHERNET: Ethernet = imp! {
        [Ethernet "virtio-net"]
        send = send,
        recv = recv,
        mtu = mtu,
        address = address,
    };

    fn send(send: Send) -> SendResult {
        use lemmings_virtio_net as net;
        let dev = unsafe { super::device() };
        let data = send.packet;
        let data_phys = net::PhysRegion {
            base: net::PhysAddr((data.base.0.as_ptr() as u64).into()),
            size: u16::from(data.length).into(),
        };
        let hdr = core::alloc::Layout::new::<net::PacketHeader>();
        let (mut hdr, hdr_phys) =
            super::alloc(hdr.size(), hdr.align().trailing_zeros() as u8).unwrap();
        let hdr = unsafe { hdr.cast().as_mut() };
        critical_section::with(|cs| unsafe {
            dev.lock(cs).send(hdr, hdr_phys, data_phys).unwrap()
        });
        SendResult::Ok
    }
    fn recv() -> RecvResult {
        let pci = unsafe { super::PCI.assume_init() };
        let msi = unsafe { (&*(&raw const super::MSI_RX)).assume_init_read() };
        let dev = unsafe { super::device() };
        loop {
            pci.msi_wait(msi.clone().into());
            let pkt = critical_section::with(|cs| dev.lock(cs).receive_one().unwrap());
            // FIXME don't leak headers!
            if let Some((_, [hdr, pkt])) = pkt {
                critical_section::with(|cs| unsafe {
                    crate::insert_new_packet_buffer(&mut dev.lock(cs), hdr.base)
                });
                let base = core::ptr::NonNull::new(u64::from(pkt.base.0) as *mut _)
                    .expect("packet base is null");
                let len = u16::try_from(pkt.size).expect("packet length is too large");
                let packet = Packet {
                    base: base.into(),
                    length: len.try_into().expect("packet length is too small"),
                };
                return RecvOk { packet }.into();
            }
        }
    }
    fn mtu() -> Mtu {
        Mtu::try_from(1518).expect("MTU satisfies minimum")
    }
    fn address() -> Mac {
        let [a, b, c, d, e, f] = unsafe { super::MAC.map(Into::into) };
        Mac {
            abcd: MacLow { a, b, c, d },
            ef: MacHigh { e, f },
        }
    }
}

static mut DEVICE: MaybeUninit<SpinLock<Device<'static>>> = MaybeUninit::uninit();
static mut MAC: [u8; 6] = [0; 6];
static mut PCI: MaybeUninit<&'static Pci> = MaybeUninit::uninit();
static mut MSI_RX: MaybeUninit<lemmings_idl_pci::Msi> = MaybeUninit::uninit();
static mut ALLOC: MaybeUninit<Allocator> = MaybeUninit::uninit();

const VENDOR_ID: u16 = 0x1af4;
const DEVICE_ID: u16 = 0x1000;

fn main() {
    let alloc = Allocator::get().expect("no allocator found");
    unsafe { ALLOC = MaybeUninit::new(alloc) };
    let door = lemmings_door::door_find::<Pci>().expect("no PCI door found");
    unsafe { PCI = MaybeUninit::new(door) };
    let lemmings_idl_pci::Configuration {
        base,
        segment_group: _,
        bus_start,
        bus_end,
    } = door.configuration();
    assert!(u8::from(bus_start) == 0 && u8::from(bus_end) == 0xff);
    let pci = unsafe { base.0.cast::<lemmings_pci::Pci>().as_ref() };
    pci.list(|_, hdr| try_device(door, hdr));
}

fn try_device(door_pci: &Pci, header: Header<'static>) {
    let Header::H0(hdr) = header else {
        return;
    };
    if hdr.vendor_id() != VENDOR_ID {
        return;
    }
    if hdr.device_id() != DEVICE_ID {
        return;
    }
    start_device(door_pci, hdr);
}

fn start_device(door_pci: &Pci, header: &'static Header0) -> ! {
    use lemmings_idl_pci::*;

    let mut alloc_msi = || match door_pci.msi_map() {
        MaybeMsi::Msi(x) => x,
        MaybeMsi::NoMsi(_) => panic!("no MSI vectors remaining :("),
    };
    let msi_rx = alloc_msi();
    let msi_tx = alloc_msi();
    unsafe {
        MSI_RX = MaybeUninit::new(msi_rx.clone());
    };
    header.set_command(HeaderCommon::COMMAND_MMIO | HeaderCommon::COMMAND_BUS_MASTER);
    'cap: {
        for cap in header.capabilities() {
            use lemmings_pci::{
                capability::{Capability, MsiXTableInfo},
                msix,
            };
            let Some(Capability::MsiX(cap)) = cap.downcast() else {
                continue;
            };
            let mut ctrl = cap.message_control();
            let MsiXTableInfo { offset, bir } = cap.table();
            let size = usize::from(ctrl.table_size()) + 1;
            let table = header
                .full_base_address(bir.into())
                .expect("bar")
                .try_as_mmio()
                .expect("mmio");
            let table = unsafe {
                NonNull::<msix::TableEntry>::new(table as _)
                    .unwrap()
                    .byte_add(offset as _)
            };
            let table = unsafe { NonNull::slice_from_raw_parts(table, size).as_ref() };
            for (tbl, msi) in table[..2].iter().zip([msi_rx, msi_tx.clone()]) {
                let (data, address) = (msi.data.clone().into(), msi.address.clone().into());
                tbl.set_message_data(data);
                tbl.set_message_address(address);
                tbl.set_vector_control_mask(false);
            }
            ctrl.set_enable(true);
            cap.set_message_control(ctrl);
            break 'cap;
        }
        panic!("No MSI-X?");
    }
    let msix = lemmings_virtio_net::Msix {
        receive_queue: Some(0),
        transmit_queue: Some(1),
    };
    let bar_map = &map_bars(header);
    let dma_alloc = |n| alloc(n, 12).ok_or(());
    let (mut dev, mac) = unsafe { Device::new(header, msix, bar_map, dma_alloc).unwrap() };

    let (_, hdr) = alloc(mem::size_of::<PacketHeader>(), 8).unwrap();
    unsafe { insert_new_packet_buffer(&mut dev, hdr) };

    unsafe { MAC = mac.0 };
    unsafe { (&mut *(&raw mut DEVICE)).write(SpinLock::new(dev)) };

    unsafe { lemmings_door::door_register(&door::ETHERNET).unwrap() };

    let dev = unsafe { device() };
    loop {
        door_pci.msi_wait(msi_tx.clone());
        critical_section::with(|cs| {
            dev.lock(cs).collect_sent(|_, _| {
                //todo!();
            })
        });
    }
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
    map
}

unsafe fn device() -> &'static SpinLock<Device<'static>> {
    unsafe { (&*(&raw const DEVICE)).assume_init_ref() }
}

fn alloc(len: usize, align: u8) -> Option<(NonNull<u8>, PhysAddr)> {
    let alloc = unsafe { ALLOC.assume_init() };
    let ptr = alloc.alloc(len, align)?.cast::<u8>();
    Some((ptr, PhysAddr((ptr.as_ptr() as u64).into())))
}

unsafe fn insert_new_packet_buffer(dev: &mut Device<'_>, hdr: PhysAddr) {
    let (_, data) = alloc(1514, 1).unwrap();
    unsafe {
        dev.insert_buffer(
            hdr,
            PhysRegion {
                base: data,
                size: 1514,
            },
        )
        .unwrap()
    };
}

#[unsafe(no_mangle)]
extern "sysv64" fn _start() {
    main()
}

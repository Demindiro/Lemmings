#![no_std]
#![no_main]
#![forbid(improper_ctypes_definitions, unused_must_use)]
#![feature(cell_update)] // stabilized in 1.88

extern crate lemmings_door;

use core::{
    cell::{Cell, RefCell},
    mem::{self, ManuallyDrop},
    ptr::NonNull,
};
use lemmings_door::{Log, log};
use lemmings_idl_pci::Pci;
use lemmings_pci::{Header, Header0, HeaderCommon, ParsedBaseAddress};
use lemmings_virtio_net::{Packet, PhysAddr, PhysRegion};

const VENDOR_ID: u16 = 0x1af4;
const DEVICE_ID: u16 = 0x1000;

struct Dev<'a> {
    virtio: RefCell<lemmings_virtio_net::Device<'a>>,
    dma_virt: NonNull<Packet>,
    dma_phys: PhysAddr,
    /// Bitmap of packets with received data.
    rx_avail_map: Cell<u64>,
    /// Bitmap of packets available for transmission.
    tx_avail_map: Cell<u64>,
}

struct RxToken<'a, 'd> {
    dev: &'a Dev<'d>,
    index: usize,
}

struct TxToken<'a, 'd> {
    dev: &'a Dev<'d>,
    index: usize,
}

impl<'d> Dev<'d> {
    const MAX_TX_PACKET: usize = 1;
    const MAX_RX_PACKET: usize = 1;
    const MAX_PACKET: usize = Self::MAX_TX_PACKET + Self::MAX_RX_PACKET;

    fn new<E, F>(virtio: lemmings_virtio_net::Device<'d>, alloc: F) -> Result<Self, E>
    where
        F: FnOnce(usize) -> Result<(NonNull<u8>, PhysAddr), E>,
    {
        let n = mem::size_of::<Packet>() * Self::MAX_PACKET;
        let (dma_virt, dma_phys) = alloc(n)?;
        let dma_virt = dma_virt.cast();

        let s = Self {
            virtio: RefCell::new(virtio),
            dma_virt,
            dma_phys,
            rx_avail_map: Cell::new(0b01),
            tx_avail_map: Cell::new(0b10),
        };

        let map = s.rx_avail_map.get();
        s.rx_avail_map.set(0);
        for i in (0..Self::MAX_PACKET).filter(|i| map & 1 << i != 0) {
            let (mut virt, phys) = s.get(i);
            // SAFETY: we have exclusive access
            let virt = unsafe { virt.as_mut() };
            s.with_virtio(|x| unsafe { x.insert_buffer(virt, phys).unwrap() });
        }

        Ok(s)
    }

    fn collect_received(&self) -> Result<(), lemmings_virtio_net::ReceiveError> {
        self.with_virtio(|dev| {
            dev.receive(|_, phys| {
                let i =
                    u64::from(phys.base.0 - self.dma_phys.0) as usize / mem::size_of::<Packet>();
                self.rx_avail_map.update(|x| x | 1 << i);
            })
        })?;
        Ok(())
    }

    fn collect_sent(&self) -> Result<(), lemmings_virtio_net::ReceiveError> {
        self.with_virtio(|dev| {
            dev.collect_sent(|_, phys| {
                let i =
                    u64::from(phys.base.0 - self.dma_phys.0) as usize / mem::size_of::<Packet>();
                self.tx_avail_map.update(|x| x | 1 << i);
            })
        });
        Ok(())
    }

    fn with_virtio<R, F>(&self, f: F) -> R
    where
        F: FnOnce(&mut lemmings_virtio_net::Device<'_>) -> R,
    {
        let mut x = self.virtio.borrow_mut();
        (f)(&mut x)
    }

    fn get(&self, i: usize) -> (NonNull<Packet>, PhysAddr) {
        assert!(i < Self::MAX_PACKET);
        // SAFETY: index is in range
        let virt = unsafe { self.dma_virt.add(i) };
        let phys = PhysAddr(self.dma_phys.0 + (mem::size_of::<Packet>() * i) as u64);
        (virt, phys)
    }

    fn pop_rx(&self) -> RxToken<'_, 'd> {
        RxToken {
            dev: self,
            index: pop_bit(&self.rx_avail_map).into(),
        }
    }

    fn pop_tx(&self) -> TxToken<'_, 'd> {
        TxToken {
            dev: self,
            index: pop_bit(&self.tx_avail_map).into(),
        }
    }

    fn has_tx(&self) -> bool {
        self.tx_avail_map.get() != 0
    }

    fn has_rx_and_tx(&self) -> bool {
        self.tx_avail_map.get() != 0 && self.rx_avail_map.get() != 0
    }
}

impl<'d> smoltcp::phy::Device for Dev<'d> {
    type RxToken<'a>
        = RxToken<'a, 'd>
    where
        Self: 'a;
    type TxToken<'a>
        = TxToken<'a, 'd>
    where
        Self: 'a;

    // TODO what is timestamp for?
    fn receive(
        &mut self,
        _timestamp: smoltcp::time::Instant,
    ) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        self.has_rx_and_tx().then(|| (self.pop_rx(), self.pop_tx()))
    }

    fn transmit(&mut self, _timestamp: smoltcp::time::Instant) -> Option<Self::TxToken<'_>> {
        self.has_tx().then(|| self.pop_tx())
    }

    fn capabilities(&self) -> smoltcp::phy::DeviceCapabilities {
        let mut s = smoltcp::phy::DeviceCapabilities::default();
        s.max_transmission_unit = Packet::default().data.len();
        s.medium = smoltcp::phy::Medium::Ethernet;
        s
    }
}

impl smoltcp::phy::RxToken for RxToken<'_, '_> {
    fn consume<R, F>(self, f: F) -> R
    where
        F: FnOnce(&[u8]) -> R,
    {
        // forget as we don't want to release the RX packet until the device has processed it
        let s = ManuallyDrop::new(self);
        let (mut virt, phys) = s.dev.get(s.index);
        // SAFETY: we have exclusive access
        let virt = unsafe { virt.as_mut() };
        let ret = (f)(&virt.data);
        s.dev.with_virtio(|dev| unsafe {
            dev.insert_buffer(virt, phys).unwrap();
        });
        ret
    }
}

impl smoltcp::phy::TxToken for TxToken<'_, '_> {
    fn consume<R, F>(self, len: usize, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        // forget as we don't want to release the TX packet until the device has processed it
        let s = ManuallyDrop::new(self);
        let (mut virt, phys) = s.dev.get(s.index);
        let virt = unsafe { virt.as_mut() };
        let ret = f(&mut virt.data[..len]);
        s.dev.with_virtio(|dev| unsafe {
            let region = PhysRegion {
                base: phys,
                size: Packet::size_with_data(len),
            };
            dev.send(virt, region).unwrap();
        });
        ret
    }
}

impl Drop for RxToken<'_, '_> {
    fn drop(&mut self) {
        self.dev.rx_avail_map.update(|x| x | 1 << self.index);
    }
}

impl Drop for TxToken<'_, '_> {
    fn drop(&mut self) {
        self.dev.tx_avail_map.update(|x| x | 1 << self.index);
    }
}

fn main() {
    let door = lemmings_door::door_find::<Pci>().expect("no PCI door found");
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

fn try_device(door_pci: &Pci, header: Header<'_>) {
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

fn start_device(door_pci: &Pci, header: &Header0) -> ! {
    use lemmings_idl_allocator::*;
    use lemmings_idl_pci::*;

    // we'll use one MSI-X vector for now
    let msi = match door_pci.msi_map() {
        MaybeMsi::Msi(x) => x,
        MaybeMsi::NoMsi(_) => panic!("no MSI vectors remaining :("),
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
            // FIXME add identity base address!
            let table = unsafe {
                NonNull::<msix::TableEntry>::new(table as _)
                    .unwrap()
                    .byte_add(offset as _)
            };
            let table = unsafe { NonNull::slice_from_raw_parts(table, size).as_ref() };
            let (data, address) = (msi.data.clone().into(), msi.address.clone().into());
            for tbl in &table[..2] {
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
    let door = lemmings_door::door_find::<Allocator>()
        .expect("no super-experimental Page Allocator 4K door found");
    let dma_alloc = |n: usize| -> Result<_, ()> {
        let alloc = Alloc {
            len: n.into(),
            align: 12.into(),
        };
        match door.alloc(alloc) {
            MaybeRegion::Region(Region { base, len: _ }) => {
                // FIXME use identity_base
                Ok((base.0.cast(), PhysAddr((base.0.as_ptr() as u64).into())))
            }
            MaybeRegion::NoRegion(_) => todo!("oom"),
        }
    };
    let (dev, mac) =
        unsafe { lemmings_virtio_net::Device::new(header, msix, bar_map, dma_alloc).unwrap() };

    let dev = &mut Dev::new(dev, dma_alloc).unwrap();

    use smoltcp::{iface, socket, wire};
    let config = iface::Config::new(wire::HardwareAddress::Ethernet(wire::EthernetAddress(
        mac.0,
    )));
    let iface = &mut iface::Interface::new(config, dev, smoltcp::time::Instant::ZERO);

    const LEN: usize = mem::size_of::<iface::SocketStorage>() * SOCKET_NUM + 2048;
    let (mut buf @ og_buf, _) = dma_alloc(LEN).unwrap();

    const SOCKET_NUM: usize = 16;
    let sockets = buf.cast::<iface::SocketStorage>();
    buf = unsafe { buf.add(mem::size_of::<iface::SocketStorage>() * SOCKET_NUM) };
    for i in 0..SOCKET_NUM {
        unsafe { sockets.add(i).write(iface::SocketStorage::EMPTY) };
    }
    let sockets = unsafe { NonNull::slice_from_raw_parts(sockets, SOCKET_NUM).as_mut() };
    let sockets = &mut iface::SocketSet::new(sockets);

    let dhcpv4 = socket::dhcpv4::Socket::new();
    let dhcpv4 = sockets.add(dhcpv4);

    let rx = unsafe { NonNull::slice_from_raw_parts(buf, 128).as_mut() };
    buf = unsafe { buf.add(128) };
    let tx = unsafe { NonNull::slice_from_raw_parts(buf, 128).as_mut() };
    buf = unsafe { buf.add(128) };
    let rx_meta = unsafe {
        NonNull::slice_from_raw_parts(buf.cast::<socket::udp::PacketMetadata>(), 2).as_mut()
    };
    buf = unsafe { buf.add(2 * mem::size_of_val(&rx_meta[0])) };
    let tx_meta = unsafe {
        NonNull::slice_from_raw_parts(buf.cast::<socket::udp::PacketMetadata>(), 2).as_mut()
    };
    buf = unsafe { buf.add(2 * mem::size_of_val(&tx_meta[0])) };
    let [rx, tx] =
        [(rx, rx_meta), (tx, tx_meta)].map(|(x, y)| socket::udp::PacketBuffer::new(y, x));
    let udp = socket::udp::Socket::new(rx, tx);
    let udp = sockets.add(udp);

    let rx = unsafe { NonNull::slice_from_raw_parts(buf, 512).as_mut() };
    buf = unsafe { buf.add(512) };
    let tx = unsafe { NonNull::slice_from_raw_parts(buf, 512).as_mut() };
    buf = unsafe { buf.add(512) };
    let [rx, tx] = [rx, tx].map(socket::tcp::SocketBuffer::new);
    let tcp = socket::tcp::Socket::new(rx, tx);
    let tcp = sockets.add(tcp);

    assert!(unsafe { buf.offset_from(og_buf) as usize } <= LEN);

    let mut time = smoltcp::time::Instant::ZERO;
    loop {
        // FIXME proper timekeeping
        time += smoltcp::time::Duration::from_micros(1);
        //time += smoltcp::time::Duration::from_secs(1000);
        dev.collect_received().unwrap();
        dev.collect_sent().unwrap();
        match iface.poll(time, dev, sockets) {
            iface::PollResult::None => {
                door_pci.msi_wait(msi.clone());
                continue;
            }
            iface::PollResult::SocketStateChanged => {}
        }
        let dhcpv4 = sockets.get_mut::<socket::dhcpv4::Socket>(dhcpv4);
        if let Some(x) = dhcpv4.poll() {
            match x {
                socket::dhcpv4::Event::Deconfigured => log!("DHCPv4 deconfigured..."),
                socket::dhcpv4::Event::Configured(c) => {
                    log!("DHCPv4 configured!");
                    c.router
                        .map(|x| iface.routes_mut().add_default_ipv4_route(x));
                    iface.update_ip_addrs(|x| {
                        x.clear();
                        x.push(c.address.into()).unwrap();
                    });
                }
            }
        }

        let udp = sockets.get_mut::<socket::udp::Socket>(udp);
        if !udp.is_open() {
            iface.ipv4_addr().map(|x| udp.bind((x, 80)).unwrap());
        }
        if let Ok((msg, _)) = udp.recv() {
            let msg = unsafe { core::str::from_utf8_unchecked(msg) };
            lemmings_door::log(msg);
        }

        let tcp = sockets.get_mut::<socket::tcp::Socket>(tcp);
        if !tcp.is_open() {
            iface.ipv4_addr().map(|x| tcp.listen((x, 80)).unwrap());
        }
        if tcp.can_send() {
            use core::fmt::Write;
            let _ = write!(tcp, "HTTP/1.0 200 OK\r\n\r\nHello from Lemmings!\n");
            tcp.close();
        }
        let _ = tcp.recv(|x| {
            let x = core::str::from_utf8(x).unwrap();
            log!("{x}");
            (x.len(), ())
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

fn pop_bit(c: &Cell<u64>) -> u8 {
    let x = c.get();
    let i = x.trailing_zeros();
    c.set(x & !(1 << i));
    i as u8
}

#[unsafe(no_mangle)]
extern "sysv64" fn _start() {
    main()
}

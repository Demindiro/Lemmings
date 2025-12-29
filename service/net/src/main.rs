#![no_std]
#![no_main]
#![forbid(improper_ctypes_definitions, unused_must_use)]
#![feature(cell_update)] // stabilized in 1.88

extern crate lemmings_door;

use core::{
    cell::Cell,
    mem::{self, ManuallyDrop},
    ptr::NonNull,
    slice,
};
use critical_section::CriticalSection;
use lemmings_allocator::Allocator;
use lemmings_door::{Cookie, Log, log};
use lemmings_spinlock::{SpinLock, SpinLockGuard};
use smoltcp::{iface, socket, wire};

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

struct Net {
    dev: Dev,
    sockets: SpinLock<Sockets>,
}

struct PhysAddr(u64);

struct Packet {
    base: NonNull<u8>,
    len: u16,
}

struct Dev {
    ethernet: &'static lemmings_idl_ethernet::Ethernet,
    alloc: Allocator,
    rx: SpinLock<heapless::Deque<Packet, 16>>,
}

struct DevProxy<'a, 'cs> {
    ethernet: &'static lemmings_idl_ethernet::Ethernet,
    alloc: Allocator,
    rx: SpinLockGuard<'a, 'cs, heapless::Deque<Packet, 16>>,
}

struct RxToken<'a> {
    rx: &'a mut heapless::Deque<Packet, 16>,
    alloc: Allocator,
}

struct TxToken<'a> {
    ethernet: &'a lemmings_idl_ethernet::Ethernet,
    alloc: Allocator,
}

struct Sockets {
    time: smoltcp::time::Instant,
    iface: iface::Interface,
    sockets: iface::SocketSet<'static>,
    dhcpv4: iface::SocketHandle,
    tcp: iface::SocketHandle,
    udp: iface::SocketHandle,
}

impl Dev {
    fn new(ethernet: &'static lemmings_idl_ethernet::Ethernet, alloc: Allocator) -> Self {
        Self {
            ethernet,
            alloc,
            rx: Default::default(),
        }
    }

    fn proxy<'a, 'cs>(&'a self, cs: CriticalSection<'cs>) -> DevProxy<'a, 'cs> {
        let &Self {
            ethernet,
            alloc,
            ref rx,
        } = self;
        let rx = rx.lock(cs);
        DevProxy {
            ethernet,
            alloc,
            rx,
        }
    }
}

impl DevProxy<'_, '_> {
    fn insert_rx(&mut self, packet: Packet) {
        // just drop excess packets
        let _ = self.rx.push_back(packet);
    }
}

impl smoltcp::phy::Device for DevProxy<'_, '_> {
    type RxToken<'a>
        = RxToken<'a>
    where
        Self: 'a;
    type TxToken<'a>
        = TxToken<'a>
    where
        Self: 'a;

    // TODO what is timestamp for?
    fn receive(
        &mut self,
        _timestamp: smoltcp::time::Instant,
    ) -> Option<(Self::RxToken<'_>, Self::TxToken<'_>)> {
        let &mut Self {
            ethernet,
            alloc,
            ref mut rx,
        } = self;
        (!rx.is_empty()).then(|| (RxToken { rx, alloc }, TxToken { ethernet, alloc }))
    }

    fn transmit(&mut self, _timestamp: smoltcp::time::Instant) -> Option<Self::TxToken<'_>> {
        let &mut Self {
            ethernet, alloc, ..
        } = self;
        Some(TxToken { ethernet, alloc })
    }

    fn capabilities(&self) -> smoltcp::phy::DeviceCapabilities {
        let mut s = smoltcp::phy::DeviceCapabilities::default();
        s.max_transmission_unit = 1514;
        s.medium = smoltcp::phy::Medium::Ethernet;
        s
    }
}

impl smoltcp::phy::RxToken for RxToken<'_> {
    fn consume<R, F>(self, f: F) -> R
    where
        F: FnOnce(&[u8]) -> R,
    {
        let pkt = self.rx.pop_front().expect("at least 1 packet");
        (f)(pkt.data())
    }
}

impl smoltcp::phy::TxToken for TxToken<'_> {
    fn consume<R, F>(self, len: usize, f: F) -> R
    where
        F: FnOnce(&mut [u8]) -> R,
    {
        use lemmings_idl_ethernet::*;
        let length = u16::try_from(len).expect("len too large (u16)");
        let length = length.max(64);
        let length = PacketLength::try_from(length).expect("len too large (PacketLength)");
        let ptr = self.alloc.alloc_zero(1514, 1).unwrap();
        // SAFETY: it is initialized to all zeros and we have exclusive ownership
        let buf = unsafe { slice::from_raw_parts_mut(ptr.cast().as_ptr(), ptr.len()) };
        let ret = (f)(&mut buf[..len]);
        let x = Send {
            packet: Packet {
                base: ptr.cast().into(),
                length,
            },
        };
        self.ethernet.send(x);
        ret
    }
}

impl Packet {
    fn data(&self) -> &[u8] {
        unsafe { core::slice::from_raw_parts(self.base.as_ptr(), self.len.into()) }
    }
}

impl Sockets {
    const LEN: usize = mem::size_of::<iface::SocketStorage>() * Self::SOCKET_NUM + 2048;
    const SOCKET_NUM: usize = 16;

    #[inline(never)] // ensure stack space gets freed on return
    fn new(mac: [u8; 6], dev: &Dev, alloc: Allocator) -> Self {
        let config =
            iface::Config::new(wire::HardwareAddress::Ethernet(wire::EthernetAddress(mac)));
        let iface = critical_section::with(|cs| {
            iface::Interface::new(config, &mut dev.proxy(cs), smoltcp::time::Instant::ZERO)
        });

        // TODO figure out actual alignment requirements
        // 2**6 = 64 matches cache line, which definitely is sufficient for smoltcp
        let mut buf @ og_buf = alloc.alloc(Self::LEN, 6).unwrap().cast::<u8>();

        let sockets = buf.cast::<iface::SocketStorage>();
        buf = unsafe { buf.add(mem::size_of::<iface::SocketStorage>() * Self::SOCKET_NUM) };
        for i in 0..Self::SOCKET_NUM {
            unsafe { sockets.add(i).write(iface::SocketStorage::EMPTY) };
        }
        let sockets = unsafe { NonNull::slice_from_raw_parts(sockets, Self::SOCKET_NUM).as_mut() };
        let mut sockets = iface::SocketSet::new(sockets);

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

        assert!(unsafe { buf.offset_from(og_buf) as usize } <= Self::LEN);

        Self {
            time: smoltcp::time::Instant::ZERO,
            iface,
            sockets,
            dhcpv4,
            tcp,
            udp,
        }
    }

    pub fn poll(&mut self, cs: CriticalSection<'_>, dev: &Dev) -> () {
        loop {
            if let Some(wait) = self.poll_once(cs, dev) {
                return wait;
            }
        }
    }

    pub fn poll_once(&mut self, cs: CriticalSection<'_>, dev: &Dev) -> Option<()> {
        // FIXME proper time keeping
        self.time += smoltcp::time::Duration::from_millis(100);
        let poll = self
            .iface
            .poll(self.time, &mut dev.proxy(cs), &mut self.sockets);
        match poll {
            iface::PollResult::None => {
                // TODO poll_at
                return Some(());
            }
            iface::PollResult::SocketStateChanged => {}
        }

        let dhcpv4 = self.sockets.get_mut::<socket::dhcpv4::Socket>(self.dhcpv4);
        if let Some(x) = dhcpv4.poll() {
            match x {
                socket::dhcpv4::Event::Deconfigured => log!("DHCPv4 deconfigured..."),
                socket::dhcpv4::Event::Configured(c) => {
                    log!("DHCPv4 configured!");
                    c.router
                        .map(|x| self.iface.routes_mut().add_default_ipv4_route(x));
                    self.iface.update_ip_addrs(|x| {
                        x.clear();
                        x.push(c.address.into()).unwrap();
                    });
                }
            }
        }

        let udp = self.sockets.get_mut::<socket::udp::Socket>(self.udp);
        if !udp.is_open() {
            self.iface.ipv4_addr().map(|x| udp.bind((x, 80)).unwrap());
        }
        if let Ok((msg, _)) = udp.recv() {
            let msg = unsafe { core::str::from_utf8_unchecked(msg) };
            lemmings_door::log(msg);
        }

        let tcp = self.sockets.get_mut::<socket::tcp::Socket>(self.tcp);

        lemmings_door::dbg!(tcp.state());
        if !tcp.is_open() {
            self.iface.ipv4_addr().map(|x| tcp.listen((x, 80)).unwrap());
            lemmings_door::dbg!((), tcp.state());
        }
        if tcp.can_send() {
            lemmings_door::dbg!();
            use core::fmt::Write;
            let _ = write!(tcp, "HTTP/1.0 200 OK\r\n\r\nHello from Lemmings!\n");
            tcp.close();
        }
        let _ = tcp.recv(|x| {
            lemmings_door::dbg!();
            let x = core::str::from_utf8(x).unwrap();
            log!("{x}");
            (x.len(), ())
        });

        None
    }
}

fn main() -> ! {
    let (ethernet, mac) = {
        use lemmings_idl_ethernet::*;
        let door = lemmings_door::door_find::<Ethernet>().expect("no Ethernet door found");
        let Mac {
            abcd: MacLow { a, b, c, d },
            ef: MacHigh { e, f },
        } = door.address();
        (door, [a, b, c, d, e, f].map(u8::from))
    };

    let alloc = Allocator::get().expect("no Allocator door found");

    let dev = Dev::new(ethernet, alloc);
    let sockets = SpinLock::new(Sockets::new(mac, &dev, alloc));
    let net = &Net { dev, sockets };

    let threads =
        lemmings_door::door_find::<lemmings_idl_thread::Threads>().expect("no threads door");
    {
        use lemmings_idl_thread::*;
        let data = SpawnDataRef(NonNull::from(net).cast()).into();
        let entry = SpawnEntry(task_receive);
        threads.spawn(Spawn { data, entry });
    }

    loop {
        let wait = critical_section::with(|cs| net.sockets.lock(cs).poll(cs, &net.dev));
        // TODO proper sleep
        threads.park();
    }
}

/// Receive packets and put them in the device object.
unsafe extern "sysv64" fn task_receive(net: lemmings_idl_thread::ffi::MaybeSpawnDataRef) {
    use lemmings_idl_ethernet as eth;
    let net = unsafe {
        core::mem::transmute::<_, Option<NonNull<Net>>>(net)
            .expect("net must not be null")
            .as_ref()
    };
    loop {
        match net.dev.ethernet.recv() {
            eth::RecvResult::RecvOk(eth::RecvOk { packet }) => {
                let eth::Packet { base, length } = packet;
                let packet = Packet {
                    base: base.0.cast(),
                    len: length.into(),
                };
                critical_section::with(|cs| {
                    net.dev.proxy(cs).insert_rx(packet);
                    net.sockets.lock(cs).poll(cs, &net.dev);
                });
            }
            eth::RecvResult::RecvFail(eth::RecvFail { packet: _ }) => todo!("handle recv failure"),
        }
        let wait = critical_section::with(|cs| net.sockets.lock(cs).poll(cs, &net.dev));
        // TODO update timer of main poll task
    }
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

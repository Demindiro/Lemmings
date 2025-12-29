//! Based on https://docs.oasis-open.org/virtio/virtio/v1.1/cs01/virtio-v1.1-cs01.html#x1-2110003

#![no_std]
#![forbid(unused_must_use, unsafe_op_in_unsafe_fn, improper_ctypes_definitions)]
#![feature(alloc_layout_extra)] // stabilized literally 2 weeks ago

pub use lemmings_virtio::{PhysAddr, PhysRegion};

use core::{
    alloc::Layout,
    convert::TryInto,
    fmt,
    mem::{self, MaybeUninit},
    ptr::NonNull,
};
use lemmings_endian::{u16le, u32le};
use lemmings_virtio::{pci::CommonConfig, queue};

/// Device handles packets with partial checksum. This "checksum offload" is a common feature on
/// modern network cards.
#[allow(dead_code)]
const CSUM: u32 = 1 << 0;
/// Driver handles packets with partial checksum.
#[allow(dead_code)]
const GUEST_CSUM: u32 = 1 << 1;
/// Control channel offloads reconfiguration support.
#[allow(dead_code)]
const CTRL_GUEST_OFFLOADS: u32 = 1 << 2;
/// Device maximum MTU reporting is supported. If offered by the device, device advises driver
/// about the value of its maximum MTU. If negotiated, the driver uses mtu as the maximum MTU
/// value.
#[allow(dead_code)]
const MTU: u32 = 1 << 3;
/// Device has given MAC address.
const MAC: u32 = 1 << 5;
/// Driver can receive TSOv4.
#[allow(dead_code)]
const GUEST_TSO4: u32 = 1 << 7;
/// Driver can receive TSOv6.
#[allow(dead_code)]
const GUEST_TSO6: u32 = 1 << 8;
/// Driver can receive TSO with ECN.
#[allow(dead_code)]
const GUEST_ECN: u32 = 1 << 9;
/// Driver can receive UFO.
#[allow(dead_code)]
const GUEST_UFO: u32 = 1 << 10;
/// Device can receive TSOv4.
#[allow(dead_code)]
const HOST_TSO4: u32 = 1 << 11;
/// Device can receive TSOv6.
#[allow(dead_code)]
const HOST_TSO6: u32 = 1 << 12;
/// Device can receive TSO with ECN.
#[allow(dead_code)]
const HOST_ECN: u32 = 1 << 13;
/// Device can receive UFO.
#[allow(dead_code)]
const HOST_UFO: u32 = 1 << 14;
/// Driver can merge receive buffers.
#[allow(dead_code)]
const MRG_RXBUF: u32 = 1 << 15;
/// Configuration status field is available.
#[allow(dead_code)]
const STATUS: u32 = 1 << 16;
/// Control channel is available.
#[allow(dead_code)]
const CTRL_VQ: u32 = 1 << 17;
/// Control channel RX mode support.
#[allow(dead_code)]
const CTRL_RX: u32 = 1 << 18;
/// Control channel VLAN filtering.
#[allow(dead_code)]
const CTRL_VLAN: u32 = 1 << 19;
/// Driver can send gratuitous packets.
#[allow(dead_code)]
const GUEST_ANNOUNCE: u32 = 1 << 21;
/// Device supports multiqueue with automatic receive steering.
#[allow(dead_code)]
const MQ: u32 = 1 << 22;
/// Set MAC address through control channel.
#[allow(dead_code)]
const CTRL_MAC_ADDR: u32 = 1 << 23;
/// Device can process duplicated ACKs and report number of coalesced segments and duplicated ACKs.
#[allow(dead_code)]
const RSC_EXT: u32 = 1 << (61 - 32);
/// Device may act as a standby for a primary device with the same MAC address.
#[allow(dead_code)]
const STANDBY: u32 = 1 << (62 - 32);

const MAX_ETH_SIZE: usize = 1514;

/// A token for an active receive operation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RxToken(lemmings_virtio::queue::Token);

/// A token for an active transmit operation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TxToken(lemmings_virtio::queue::Token);

#[derive(Debug)]
pub enum SetupError<DmaError> {
    DmaError(DmaError),
}

#[derive(Clone)]
pub enum SendError {}

#[derive(Clone)]
pub enum ReceiveError {}

#[derive(Clone, Debug)]
pub struct Full;

pub struct Mac(pub [u8; 6]);

/// PCI MSI-X configuration.
pub struct Msix {
    /// The MSI-X vector to use for receive queue interrupts.
    pub receive_queue: Option<u16>,
    /// The MSI-X vector to use for transmit queue interrupts.
    pub transmit_queue: Option<u16>,
}

/// A driver for a virtio network (Ethernet) device.
pub struct Device<'a> {
    tx_queue: queue::Queue<'a>,
    rx_queue: queue::Queue<'a>,
    notify: lemmings_virtio::pci::Notify<'a>,
    isr: &'a lemmings_virtio::pci::Isr,
}

#[repr(C)]
struct Config {
    mac: [u8; 6],
    status: u16le,
    max_virtqueue_pairs: u16le,
    mtu: u16le,
}

#[derive(Clone, Default)]
#[repr(C)]
pub struct PacketHeader {
    flags: u8,
    gso_type: u8,
    header_length: u16le,
    gso_size: u16le,
    csum_start: u16le,
    csum_offset: u16le,
    num_buffers: u16le,
}

#[allow(dead_code)]
#[repr(C)]
struct NetworkControl {
    class: u8,
    command: u8,
    command_specific_data: [u8; 0],
    // ack: u8 after command_specific_data
}

impl Config {
    #[allow(dead_code)]
    const STATUS_LINK_UP: u16 = 1 << 0;
    #[allow(dead_code)]
    const STATUS_ANNOUNCE: u16 = 1 << 1;
}

impl PacketHeader {
    #[allow(dead_code)]
    const NEEDS_CSUM: u8 = 1 << 0;
    #[allow(dead_code)]
    const DATA_VALID: u8 = 1 << 1;
    #[allow(dead_code)]
    const RSC_INFO: u8 = 1 << 2;

    const GSO_NONE: u8 = 0;
    #[allow(dead_code)]
    const GSO_TCP4: u8 = 1;
    #[allow(dead_code)]
    const GSO_UDP: u8 = 3;
    #[allow(dead_code)]
    const GSO_TCP6: u8 = 4;
    #[allow(dead_code)]
    const GSO_ECN: u8 = 0x80;

    const SIZE: u16 = mem::size_of::<Self>() as u16;
}

impl AsRef<[u8; 6]> for Mac {
    fn as_ref(&self) -> &[u8; 6] {
        &self.0
    }
}

impl<'a> Device<'a> {
    /// Setup a network device
    ///
    /// # Safety
    ///
    /// - `pci` must be the header of a valid Virtio network device.
    /// - `dma_alloc` must return physical addresses with an alignment of 4096.
    ///
    /// # Panics
    ///
    /// If any PCI capability refers to an invalid BAR.
    // TODO 4096 is only required for legacy. Non-legacy only requires 16. Update?
    pub unsafe fn new<E, F>(
        pci: &'a lemmings_pci::Header0,
        msix: Msix,
        bar_map: &[Option<NonNull<u8>>; 6],
        mut dma_alloc: F,
    ) -> Result<(Self, Mac), SetupError<E>>
    where
        F: FnMut(usize) -> Result<(NonNull<u8>, PhysAddr), E>,
    {
        let dev = lemmings_virtio::pci::Device::new(pci, bar_map).unwrap();

        dev.common.device_status.set(CommonConfig::STATUS_RESET);
        dev.common
            .device_status
            .set(CommonConfig::STATUS_ACKNOWLEDGE);
        dev.common
            .device_status
            .set(CommonConfig::STATUS_ACKNOWLEDGE | CommonConfig::STATUS_DRIVER);

        let features = MAC;
        //let features = MAC | MRG_RXBUF;
        dev.common.device_feature_select.set(0.into());
        let features = u32le::from(features) & dev.common.device_feature.get();
        dev.common.driver_feature_select.set(0.into());
        dev.common.driver_feature.set(features);

        const VIRTIO_F_VERSION_1: u32 = 1 << (32 - 32);
        let features = VIRTIO_F_VERSION_1;
        dev.common.device_feature_select.set(1.into());
        let features = u32le::from(features) & dev.common.device_feature.get();
        assert_eq!(
            u32::from(features),
            VIRTIO_F_VERSION_1,
            "New virtio-net is required"
        );
        dev.common.driver_feature_select.set(1.into());
        dev.common.driver_feature.set(features);

        dev.common.device_status.set(
            CommonConfig::STATUS_ACKNOWLEDGE
                | CommonConfig::STATUS_DRIVER
                | CommonConfig::STATUS_FEATURES_OK,
        );
        // TODO check device status to ensure features were enabled correctly.

        // SAFETY: The caller guarantees dma_alloc returns valid physical addresses.
        let mut create_queue = |queue_idx, msix_nr| unsafe {
            queue::Queue::<'a>::new(dev.common, queue_idx, 8, msix_nr, &mut dma_alloc).map_err(
                |e| match e {
                    queue::NewQueueError::DmaError(e) => SetupError::DmaError(e),
                },
            )
        };
        let rx_queue = create_queue(0, msix.receive_queue)?;
        let tx_queue = create_queue(1, msix.transmit_queue)?;

        dev.common.device_status.set(
            CommonConfig::STATUS_ACKNOWLEDGE
                | CommonConfig::STATUS_DRIVER
                | CommonConfig::STATUS_FEATURES_OK
                | CommonConfig::STATUS_DRIVER_OK,
        );

        let mac = unsafe { Mac(dev.device.cast::<Config>().mac) };

        let s = Self {
            rx_queue,
            tx_queue,
            notify: dev.notify,
            isr: dev.isr,
        };
        Ok((s, mac))
    }

    /// Send an Ethernet packet
    ///
    /// # Safety
    ///
    /// `header_phys` and `data_phys` must remain valid for the duration of the transmission.
    /// `header_phys` must point to the same memory region as `header`.
    ///
    /// # Panics
    ///
    /// If 'data.len()` is greater than `2**32`.
    pub unsafe fn send(
        &mut self,
        header: &mut PacketHeader,
        header_phys: PhysAddr,
        data_phys: PhysRegion,
    ) -> Result<TxToken, SendError> {
        *header = PacketHeader {
            flags: 0,
            gso_type: PacketHeader::GSO_NONE,
            csum_start: 0.into(),
            csum_offset: 0.into(),
            gso_size: 0.into(),
            header_length: PacketHeader::SIZE.into(),
            num_buffers: 0.into(),
        };

        let data = [
            (header_phys, PacketHeader::SIZE.into(), false),
            (data_phys.base, data_phys.size, false),
        ];

        let tk = self
            .tx_queue
            .send(data.iter().copied())
            .expect("Failed to send data");

        // SAFETY: notify_offset is within range
        unsafe {
            self.notify.send(self.tx_queue.notify_offset());
        }

        Ok(TxToken(tk))
    }

    /// Collect tokens for sent packets.
    pub fn collect_sent<F>(&mut self, mut f: F) -> usize
    where
        F: FnMut(TxToken, PhysRegion),
    {
        self.tx_queue.collect_used(|tk, p| f(TxToken(tk), p))
    }

    /// Receive a number of Ethernet packets, if any are available
    pub fn receive<F>(&mut self, mut f: F) -> Result<usize, ReceiveError>
    where
        F: FnMut(RxToken, PhysRegion),
    {
        Ok(self.rx_queue.collect_used(|tk, p| f(RxToken(tk), p)))
    }

    /// Receive at most one Ethernet packet, if any are available.
    ///
    /// Both the header and the data are returned.
    pub fn receive_one(&mut self) -> Result<Option<(RxToken, [PhysRegion; 2])>, ReceiveError> {
        let mut buf = [PhysRegion::default(); 2];
        self.rx_queue
            .collect_one_used(&mut buf)
            .expect("enough buffers")
            .inspect(|(_, num)| assert_eq!(num.get(), 2))
            .map(|(tk, _)| (RxToken(tk), buf))
            .map(Ok)
            .transpose()
    }

    #[inline]
    pub fn was_interrupted(&self) -> bool {
        self.isr.read().queue_update()
    }

    /// Insert a buffer for the device to write RX data to
    ///
    /// # Safety
    ///
    /// `header_phys` and `data_phys` must remain valid until collected.
    pub unsafe fn insert_buffer(
        &mut self,
        header_phys: PhysAddr,
        data_phys: PhysRegion,
    ) -> Result<RxToken, Full> {
        let data = [
            (header_phys, PacketHeader::SIZE.into(), true),
            (data_phys.base, data_phys.size, true),
        ];

        let tk = self
            .rx_queue
            .send(data.iter().copied())
            .expect("Failed to send data");

        // SAFETY: notify_offset is within range
        unsafe {
            self.notify.send(self.rx_queue.notify_offset());
        }

        Ok(RxToken(tk))
    }
}

impl Drop for Device<'_> {
    fn drop(&mut self) {
        todo!("ensure the device doesn't read/write memory after being dropped");
    }
}

impl fmt::Debug for SendError {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        /*
        f.write_str(match self {
        })
        */
        Ok(())
    }
}

impl fmt::Debug for ReceiveError {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        /*
        f.write_str(match self {
        })
        */
        Ok(())
    }
}

impl fmt::Debug for PacketHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut f = f.debug_struct(stringify!(PacketHeader));
        f.field("flags", &self.flags);
        f.field("gso_type", &self.gso_type);
        let mut g = |n: &str, v: u16le| {
            f.field(n, &u16::from(v));
        };
        g("header_length", self.header_length);
        g("csum_start", self.csum_start);
        g("csum_offset", self.csum_offset);
        g("num_buffers", self.num_buffers);
        f.finish()
    }
}

impl fmt::Debug for Mac {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Mac({})", self)
    }
}

impl fmt::Display for Mac {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i, &e) in self.0.iter().enumerate() {
            if i > 0 {
                f.write_str(":")?;
            }
            write!(f, "{:02x}", e)?;
        }
        Ok(())
    }
}

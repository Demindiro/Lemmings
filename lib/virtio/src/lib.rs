#![no_std]
#![forbid(unused_must_use, unsafe_op_in_unsafe_fn, improper_ctypes_definitions)]

pub mod pci;
pub mod phys;
pub mod queue;

pub use phys::{PhysAddr, PhysMap, PhysRegion};

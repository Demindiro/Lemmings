mod sealed {
    use super::Const;
    use core::arch::asm;

    pub trait Io<Addr> {
        unsafe fn read(address: Addr) -> Self;
        unsafe fn write(address: Addr, value: Self);
    }

    macro_rules! io {
		($ty:ident $reg:tt) => {
			impl Io<u16> for $ty {
				unsafe fn read(address: u16) -> Self {
					let out;
					unsafe { asm!(concat!("in ", $reg, ", dx"), in("dx") address, out($reg) out) }
					out
				}

				unsafe fn write(address: u16, value: Self) {
					unsafe { asm!(concat!("out dx, ", $reg), in("dx") address, in($reg) value) }
				}
			}

			impl<const ADDR: u8> Io<Const<ADDR>> for $ty {
				unsafe fn read(_: Const<ADDR>) -> Self {
					let v;
					unsafe { asm!(concat!("in ", $reg, ", {ADDR}"), ADDR = const ADDR, out($reg) v) }
					v
				}

				unsafe fn write(_: Const<ADDR>, value: Self) {
					unsafe { asm!(concat!("out {ADDR}, ", $reg), ADDR = const ADDR, in($reg) value) }
				}
			}
		};
	}

    io!(u8 "al");
    io!(i8 "al");
    io!(u16 "ax");
    io!(i16 "ax");
    io!(u32 "eax");
    io!(i32 "eax");
}

pub struct Const<const ADDR: u8>;

impl<const ADDR: u8> Const<ADDR> {
    pub const ADDR: u8 = ADDR;
}

pub unsafe fn in_<T: sealed::Io<A>, A>(address: A) -> T {
    unsafe { sealed::Io::read(address) }
}

pub unsafe fn out<T: sealed::Io<A>, A>(address: A, value: T) {
    unsafe { sealed::Io::write(address, value) }
}

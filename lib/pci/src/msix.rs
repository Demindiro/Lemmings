use super::*;

#[repr(C)]
pub struct TableEntry {
    message_address_low: VolatileCell<u32le>,
    message_address_high: VolatileCell<u32le>,
    message_data: VolatileCell<u32le>,
    vector_control: VolatileCell<u32le>,
}

impl TableEntry {
    pub fn message_address(&self) -> u64 {
        let f = |n| u64::from(u32::from(n));
        f(self.message_address_low.get()) | f(self.message_address_high.get()) << 32
    }

    pub fn set_message_address(&self, address: u64) {
        self.message_address_low.set((address as u32).into());
        self.message_address_high
            .set(((address >> 32) as u32).into());
    }

    get_volatile!(message_data -> u32);
    set_volatile!(set_message_data: message_data <- u32);

    pub fn is_vector_control_masked(&self) -> bool {
        u32::from(self.vector_control.get()) & 1 > 0
    }

    pub fn set_vector_control_mask(&self, mask: bool) {
        self.vector_control.set(u32::from(mask).into())
    }
}

impl fmt::Debug for TableEntry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct(stringify!(TableEntry))
            .field(
                "message_address",
                &format_args!("0x{:016x}", self.message_address()),
            )
            .field(
                "message_data",
                &format_args!("0x{:08x}", self.message_data()),
            )
            .field("is_vector_control_masked", &self.is_vector_control_masked())
            .finish()
    }
}

pub mod door {
    use core::ptr::NonNull;

    door! {
        [0xd8112085_698f85f2_dceefb6d_4758a59f "boot framebuffer"]
        0 acquire
        1 release
        2 inuse
    }

    #[repr(C)]
    struct FrameBuffer {
        base: Option<NonNull<u8>>,
        width: u16,
        height: u16,
        stride: u16,
        format: u8,
    }

    unsafe extern "sysv64" fn acquire(disable_callback: extern "sysv64" fn()) -> FrameBuffer {
        let _ = disable_callback;
        FrameBuffer {
            base: Some(const { NonNull::new(0xc000_0000u32 as _).unwrap() }),
            width: 1024,
            height: 768,
            stride: 1024,
            format: 0,
        }
    }

    unsafe extern "sysv64" fn release() {
    }

    unsafe extern "sysv64" fn inuse() -> bool {
        todo!()
    }
}

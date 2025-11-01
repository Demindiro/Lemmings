use crate::KernelEntryToken;

#[cfg(feature = "TODO")]
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

pub fn init(entry: &lemmings_qemubios::Entry, token: KernelEntryToken) -> KernelEntryToken {
    use lemmings_qemubios::ColorFormat;
    let fb = &entry.framebuffer;
    match fb.format {
        ColorFormat::None => return token,
        ColorFormat::Rgbx8888 => {}
        ColorFormat::Bgrx8888 => {}
    }
    let [w, h, s] = [fb.width, fb.height, fb.stride].map(u32::from);
    dbg!(w, h, s);
    for y in 0..h {
        for x in 0..w {
            let b = 256 * x / w;
            let g = 256 * y / h;
            let r = (511 - (b + g)) / 2;
            let pix = r << 16 | g << 8 | b;
            unsafe {
                let p = fb.base.0 as *mut u32;
                p.byte_add((y * s) as usize).add(x as usize).write(pix);
            }
        }
    }
    token
}

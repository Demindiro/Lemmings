use core::ptr::NonNull;

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Slice<T> {
    base: NonNull<T>,
    len: usize,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Tuple2<A, B>(pub A, pub B);

impl<T> Slice<T> {
    pub unsafe fn as_slice(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.base.as_ptr(), self.len) }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn subslice(&self, range: core::ops::Range<usize>) -> Slice<T> {
        assert!(range.start <= range.end);
        assert!(range.end <= self.len);
        unsafe {
            Self {
                base: self.base.add(range.start),
                len: range.len(),
            }
        }
    }
}

impl<T> Slice<T>
where
    T: Copy
{
    pub unsafe fn copy_from_slice(&mut self, data: &[T]) {
        unsafe {
            self.base.as_ptr().copy_from_nonoverlapping(data.as_ptr(), data.len());
        }
    }
}

impl Slice<u8> {
    pub unsafe fn as_str(&self) -> &str {
        unsafe { core::str::from_utf8_unchecked(self.as_slice()) }
    }
}

impl From<&'static str> for Slice<u8> {
    fn from(s: &'static str) -> Self {
        let s = NonNull::from(s.as_bytes());
        Self {
            base: s.cast(),
            len: s.len(),
        }
    }
}

impl<T> From<Slice<T>> for NonNull<T> {
    fn from(s: Slice<T>) -> Self {
        s.base
    }
}

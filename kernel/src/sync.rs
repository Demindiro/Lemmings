use crate::KernelEntryToken;
use core::{cell::UnsafeCell, ops};
use critical_section::CriticalSection;

pub mod imp {
    use core::sync::atomic::{AtomicU8, Ordering};

    #[repr(transparent)]
    pub struct SpinLock {
        lock: AtomicU8,
    }

    pub const UNLOCKED: u8 = 0 << 0;
    pub const LOCKED: u8 = 1 << 0;

    impl SpinLock {
        pub const fn new() -> Self {
            Self {
                lock: AtomicU8::new(0),
            }
        }

        pub fn lock(&self) {
            while self
                .lock
                .compare_exchange_weak(0, LOCKED, Ordering::Acquire, Ordering::Relaxed)
                .is_err()
            {
                pause();
            }
        }

        pub fn unlock(&self) {
            self.lock.store(0, Ordering::Release);
        }
    }

    fn pause() {
        // SAFETY: _mm_pause is inherently safe
        // (why is it marked as unsafe?)
        unsafe {
            core::arch::x86_64::_mm_pause();
        }
    }
}

pub struct SpinLock<T> {
    lock: imp::SpinLock,
    value: UnsafeCell<T>,
}

pub struct SpinLockGuard<'lock, 'cs, T> {
    lock: &'lock SpinLock<T>,
    _cs: CriticalSection<'cs>,
}

impl<T> SpinLock<T> {
    pub const fn new(value: T) -> Self {
        Self {
            lock: imp::SpinLock::new(),
            value: UnsafeCell::new(value),
        }
    }

    pub fn lock<'lock, 'cs>(&'lock self, cs: CriticalSection<'cs>) -> SpinLockGuard<'lock, 'cs, T> {
        self.lock.lock();
        SpinLockGuard {
            lock: self,
            _cs: cs,
        }
    }

    pub fn set(&self, value: T, token: KernelEntryToken) -> KernelEntryToken {
        unsafe { *self.value.get() = value }
        token
    }

    pub const fn get_mut(&mut self) -> &mut T {
        self.value.get_mut()
    }
}

impl<'lock, 'cs, T> SpinLockGuard<'lock, 'cs, T> {
    /// # Safety
    ///
    /// The lock may only get disengaged once.
    ///
    /// # Warning
    ///
    /// The lock must get disengaged manually.
    pub unsafe fn into_inner_lock(self) -> &'lock imp::SpinLock {
        &core::mem::ManuallyDrop::new(self).lock.lock
    }
}

impl<T> Drop for SpinLockGuard<'_, '_, T> {
    fn drop(&mut self) {
        self.lock.lock.unlock();
    }
}

impl<T> ops::Deref for SpinLockGuard<'_, '_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.lock.value.get() }
    }
}

impl<T> ops::DerefMut for SpinLockGuard<'_, '_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.lock.value.get() }
    }
}

unsafe impl<T> Send for SpinLock<T> {}
unsafe impl<T> Sync for SpinLock<T> {}

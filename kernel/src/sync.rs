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
    pub cs: CriticalSection<'cs>,
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
        SpinLockGuard { lock: self, cs }
    }

    pub fn set<'a>(&self, value: T, token: KernelEntryToken<'a>) -> KernelEntryToken<'a> {
        unsafe { *self.value.get() = value }
        token
    }

    pub const fn get_mut(&mut self) -> &mut T {
        self.value.get_mut()
    }

    /// It isn't always practical to carry around a SpinLockGuard.
    ///
    /// As workaround, after calling [`SpinLockGuard::into_inner_lock`],
    /// this function can be called to reacquire the lock.
    ///
    /// # Safety
    ///
    /// The lock must have already been acquired before by the same caller.
    pub unsafe fn lock_unchecked<'lock, 'cs>(
        &'lock self,
        cs: CriticalSection<'cs>,
    ) -> SpinLockGuard<'lock, 'cs, T> {
        SpinLockGuard { lock: self, cs }
    }
}

impl<'lock, 'cs, T> SpinLockGuard<'lock, 'cs, T> {
    /// # Safety
    ///
    /// The lock may only get disengaged once.
    ///
    /// Once the lock is disengaged, the inner value may no longer be accessed.
    ///
    /// # Warning
    ///
    /// The lock must get disengaged manually.
    #[must_use]
    pub unsafe fn into_inner_lock(self) -> (&'lock imp::SpinLock, &'lock mut T) {
        let s = &core::mem::ManuallyDrop::new(self).lock;
        let v = unsafe { &mut *s.value.get() };
        (&s.lock, v)
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

#![allow(dead_code)]
//! This kernel supports real-time scheduling with `O(1)` time complexity
//! and no additional space overhead.

use crate::{
    KernelEntryToken,
    page::{self, PAGE_SIZE, PageAttr},
    sync::{self, SpinLock, SpinLockGuard},
};
use core::{arch::asm, cell::Cell, fmt, mem, ops, ptr::NonNull};

const PRIORITY_COUNT: usize = 4;

static mut MANAGER: mem::MaybeUninit<SpinLock<ThreadManager>> = mem::MaybeUninit::uninit();

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub enum Priority {
    Critical = 0,
    Realtime = 1,
    User = 2,
    #[default]
    Regular = 3,
}

pub enum ThreadSpawnError {
    OutOfMemory,
    OutOfVirtSpace,
}

pub struct RoundRobinQueue {
    cur: Option<RoundRobinQueueLink>,
}

#[derive(Clone)]
pub struct ThreadHandle(ThreadRef);

struct ThreadManager {
    pending: [RoundRobinQueue; PRIORITY_COUNT],
    idle_thread: ThreadHandle,
}

struct ThreadControlBlock {
    /// # Note
    ///
    /// Only valid while enqueued. Value is indeterminate while running.
    next: Cell<ThreadRef>,
    priority: Priority,
    state: Cell<ThreadState>,
    program_counter: Cell<*const u8>,
    stack_pointer: Cell<*const u8>,
}

#[repr(C, align(4096))]
struct Thread {
    stack: [Cell<mem::MaybeUninit<usize>>;
        (4096 - mem::size_of::<ThreadControlBlock>()) / mem::size_of::<usize>()],
    tcb: ThreadControlBlock,
}

/// ```
///                    wait
///          .---------------------.    .----.
///          |                     |    |    |
///          v                     |    v    | resume
///      +--------+              +--------+  |
///      | parked |          .---| active |--'
///      +--------+   notify |   +--------+
///          |               v       ^
///   notify |      +----------+     | resume/wait
///          '----->| notified |-----'
///                 +----------+
/// ```
// You may think "this should be an atomic to avoid locking".
// However, spinlocks are *very* fast.
// The reason locks are considered slow is primarily because of poor control
// over scheduling in mainstream OSes.
// Here, a spinlock will always be very fast.
// Unless there is a ridiculous amount of contention, it should not be an issue.
//
// But more importantly, lock-free algorithms are *very* hard to get right.
// Unless you can prove it is *significantly* faster, do not change this!
#[derive(Clone, Copy)]
enum ThreadState {
    Parked = 0b00,
    Active = 0b01,
    Notified = 0b11,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct ThreadRef(NonNull<Thread>);

struct RoundRobinQueueLink {
    last: ThreadRef,
    first: ThreadRef,
}

const _: () = assert!(mem::size_of::<Thread>() == PAGE_SIZE);

impl ThreadManager {
    pub const fn new(idle_thread: ThreadHandle) -> Self {
        Self {
            pending: [const { RoundRobinQueue::new() }; PRIORITY_COUNT],
            idle_thread,
        }
    }

    /// Create a new thread and put it at the *end* of the queue.
    pub fn spawn(
        &mut self,
        priority: Priority,
        entry: extern "sysv64" fn(),
    ) -> Result<(), ThreadSpawnError> {
        let thread = Thread::new(priority, entry)?;
        self.pending[priority as usize].enqueue(thread);
        Ok(())
    }

    /// Start the scheduler.
    pub fn start(&mut self, token: KernelEntryToken) -> ! {
        self.dequeue_next().0.enter(token)
    }

    /// Enqueue a thread.
    ///
    /// The thread will be added to the end of its respective queue.
    ///
    /// # Safety
    ///
    /// The thread may not already be enqueued.
    unsafe fn enqueue(&mut self, thread: ThreadHandle) {
        // FIXME we should have a better mechanism for handling the idle thread
        if thread.0 == self.idle_thread.0 {
            debug!("enqueue {:?} (ignore)", thread.0.0);
            return;
        }
        debug!("enqueue {:?}", thread.0.0);
        self.pending[thread.0.priority as usize].enqueue(thread)
    }

    /// Dequeue the next thread with the higher priority.
    fn dequeue_next(&mut self) -> ThreadHandle {
        self.pending
            .iter_mut()
            .find_map(|x| x.dequeue())
            .unwrap_or_else(|| self.idle_thread.clone())
    }
}

impl RoundRobinQueue {
    pub const fn new() -> Self {
        Self { cur: None }
    }

    pub fn enqueue(&mut self, ThreadHandle(thread): ThreadHandle) {
        let Some(cur) = self.cur.as_mut() else {
            self.cur = Some(RoundRobinQueueLink {
                first: thread,
                last: thread,
            });
            return;
        };
        // last ==> last -> thread
        cur.last.next.set(thread);
        cur.last = thread;
    }

    pub fn dequeue(&mut self) -> Option<ThreadHandle> {
        let mut cur = self.cur.take()?;
        let thread = cur.first;
        if thread != cur.last {
            // first -> next ==> next
            cur.first = thread.next.get();
            self.cur = Some(cur);
        }
        Some(ThreadHandle(thread))
    }
}

impl ThreadState {
    /// Follow a wait edge.
    ///
    /// Returns `true` if the thread should resume immediately.
    #[must_use]
    fn wait(&mut self) -> bool {
        let resume = matches!(self, Self::Notified);
        *self = if resume { Self::Active } else { Self::Parked };
        resume
    }

    /// Follow a notify edge.
    ///
    /// Returns `true` if the thread needs to be scheduled.
    /// (if `false`, the thread is already scheduled.)
    #[must_use]
    fn notify(&mut self) -> bool {
        let reschedule = matches!(self, Self::Parked);
        *self = Self::Notified;
        reschedule
    }

    /// Follow a resume edge
    fn resume(&mut self) {
        if !matches!(self, Self::Notified) {
            *self = Self::Active;
        }
    }
}

impl ThreadRef {
    // Offset such that we point right at the TCB
    // This allows more efficient encoding of memory instructions with immediates.
    const OFFSET: usize = page::PAGE_SIZE - mem::size_of::<ThreadControlBlock>();

    /// # Safety
    ///
    /// - Must be a valid pointer.
    /// - Must not be dereferenced before initialization.
    pub unsafe fn wrap(ptr: NonNull<Thread>) -> Self {
        unsafe { Self(ptr.byte_add(Self::OFFSET)) }
    }

    /// # Safety
    ///
    /// - Must be a valid pointer.
    /// - Must not be dereferenced before initialization.
    /// - Must point to the TCB!
    pub unsafe fn wrap_tcb(ptr: NonNull<ThreadControlBlock>) -> Self {
        Self(ptr.cast())
    }
}

impl Thread {
    fn new(
        priority: Priority,
        entry: extern "sysv64" fn(),
    ) -> Result<ThreadHandle, ThreadSpawnError> {
        let page = page::alloc_one_guarded(PageAttr::RW)?.cast::<Thread>();
        // SAFETY:
        // - the page we allocated is valid and writeable.
        // - we will initialize it right now.
        let thread = unsafe {
            let thread = ThreadRef::wrap(page);
            let base = page.add(1).cast::<ThreadControlBlock>().sub(1);
            base.write(ThreadControlBlock {
                next: Cell::new(thread),
                priority,
                state: Cell::new(ThreadState::Active),
                program_counter: Cell::new(entry as *const u8),
                stack_pointer: Cell::new(base.cast::<usize>().as_ptr().sub(1).cast()),
            });
            thread
        };
        // FIXME alignment?
        thread
            .stack
            .last()
            .expect("not empty")
            .set(mem::MaybeUninit::new(Thread::exit as usize));
        Ok(ThreadHandle(thread))
    }

    fn enter(&self, _token: KernelEntryToken) -> ! {
        debug!("enter {:?}", unsafe { ThreadRef::wrap(self.into()).0 });
        unsafe {
            set_current(self);
            asm! {
                "mov rsp, {sp}",
                "jmp {pc}",
                sp = in(reg) self.stack_pointer.get(),
                pc = in(reg) self.program_counter.get(),
                options(noreturn, nostack),
            }
        }
    }

    /// Enter this thread, saving the state of the current thread before entering.
    fn resume(&self, lock: SpinLockGuard<'_, '_, ThreadManager>) {
        let current = current();
        let self_ref = unsafe { ThreadRef::wrap(self.into()) };
        // FIXME this seems very wrong?
        // we shouldn't get in this situation in the first place...
        if current.0.0 == self_ref.0 {
            debug!("resume {:?} -> {:?} (ignore)", current.0.0, self_ref.0);
            return;
        }
        debug!("resume {:?} -> {:?}", current.0.0, self_ref.0);
        // SAFETY: we will disengage the lock exactly once in the assembly code
        let lock = unsafe { lock.into_inner_lock() };
        unsafe {
            set_current(self);
            asm! {
                "push rbx",
                "push rbp",
                "pushf",
                "lea {scratch}, [rip + 2f]",
                "mov [{cur} + {TCB_SP}], rsp",
                "mov [{cur} + {TCB_PC}], {scratch}",
                // we have saved the thread's state, so it is now safe to disengage the lock
                // x86 supports TSO, so a simple mov does the trick.
                // ... well, not quite because of the store buffer,
                // but stores are ordered wrt other stores *on the same core*,
                // so any changes made to the ThreadManager will be visible before this store.
                "mov byte ptr [{lock}], {UNLOCK}",
                "mov rsp, {sp}",
                "jmp {pc}",
                "2:",
                "popf",
                "pop rbp",
                "pop rbx",
                scratch = out(reg) _,
                cur = in(reg) &current.0.tcb,
                sp = in(reg) self.stack_pointer.get(),
                pc = in(reg) self.program_counter.get(),
                lock = in(reg) lock,
                TCB_SP = const mem::offset_of!(ThreadControlBlock, stack_pointer),
                TCB_PC = const mem::offset_of!(ThreadControlBlock, program_counter),
                UNLOCK = const sync::imp::UNLOCKED,
                lateout("rax") _,
                lateout("rcx") _,
                lateout("rdx") _,
                //lateout("rbx") _, // "cannot be used for inline asm"
                //lateout("rsp") _,
                //lateout("rbp") _, // "cannot be used for inline asm"
                lateout("rsi") _,
                lateout("rdi") _,
                lateout("r8") _,
                lateout("r9") _,
                lateout("r10") _,
                lateout("r11") _,
                lateout("r12") _,
                lateout("r13") _,
                lateout("r14") _,
                lateout("r15") _,
                options(preserves_flags),
            }
        }
    }

    fn exit() -> ! {
        todo!("exit thread");
    }
}

impl ops::Deref for ThreadRef {
    type Target = Thread;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        // SAFETY: the pointer is valid
        unsafe { self.0.byte_sub(Self::OFFSET).as_ref() }
    }
}

impl ops::Deref for Thread {
    type Target = ThreadControlBlock;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.tcb
    }
}

impl From<page::OutOfMemory> for ThreadSpawnError {
    fn from(_: page::OutOfMemory) -> Self {
        Self::OutOfMemory
    }
}

impl From<page::AllocGuardedError> for ThreadSpawnError {
    fn from(x: page::AllocGuardedError) -> Self {
        match x {
            page::AllocGuardedError::OutOfMemory => Self::OutOfMemory,
            page::AllocGuardedError::OutOfVirtSpace => Self::OutOfVirtSpace,
        }
    }
}

impl fmt::Debug for ThreadSpawnError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::OutOfMemory => "out of memory",
            Self::OutOfVirtSpace => "out of virtual memory space",
        };
        f.write_str(s)
    }
}

impl ThreadHandle {
    pub fn notify(&self) {
        critical_section::with(|cs| {
            let mut m = manager().lock(cs);
            if self.with_state(|x| x.notify()) {
                debug!("notify {:?} -> resume", self.0.0);
                // FIXME priority check!
                // SAFETY: according to flags, this thread is not currently queued.
                unsafe {
                    m.enqueue(self.clone());
                }
            } else {
                debug!("notify {:?} -> nop", self.0.0);
            }
        });
    }

    fn with_state<R, F>(&self, f: F) -> R
    where
        F: FnOnce(&mut ThreadState) -> R,
    {
        let mut s = self.0.state.get();
        let ret = (f)(&mut s);
        self.0.state.set(s);
        ret
    }
}

unsafe fn set_current(thread: &Thread) {
    unsafe { lemmings_x86_64::set_fs(&thread.tcb as *const _ as *mut u8) }
}

pub fn current() -> ThreadHandle {
    let tcb = lemmings_x86_64::fs().cast::<ThreadControlBlock>();
    let tcb = NonNull::new(tcb).expect("fs register should not be null");
    unsafe { ThreadHandle(ThreadRef::wrap_tcb(tcb)) }
}

/// Spawn a new thread and run it immediately.
pub fn spawn(priority: Priority, entry: extern "sysv64" fn()) -> Result<(), ThreadSpawnError> {
    let thr = Thread::new(priority, entry)?;
    critical_section::with(|cs| unsafe {
        let mut m = manager().lock(cs);
        m.enqueue(current());
        thr.0.resume(m);
    });
    Ok(())
}

/// Wait for a notification.
///
/// If a notification has already been signaled,
/// this function returns immediately.
pub fn wait() {
    critical_section::with(|cs| {
        let mut m = manager().lock(cs);
        if current().with_state(|x| x.wait()) {
            debug!("thread::wait {:?} -> return", current().0.0);
            return;
        }
        debug!("thread::wait {:?} -> yield", current().0.0);
        let next = m.dequeue_next();
        next.0.resume(m);
        debug!("thread::wait {:?} -> continue", current().0.0);
    })
}

pub fn init(entry: extern "sysv64" fn(), token: KernelEntryToken) -> ! {
    let idle_thread =
        Thread::new(Priority::Regular, entry).expect("failed to create initial/idle_thread thread");
    // SAFETY: we have exclusive access to MANAGER
    // TODO better enforcement
    let mngr = unsafe { &mut *(&raw mut MANAGER) };
    mngr.write(SpinLock::new(ThreadManager::new(idle_thread)))
        .get_mut()
        .start(token);
}

fn manager() -> &'static SpinLock<ThreadManager> {
    // SAFETY: no code outside init() accesses MANAGER with exclusive access
    // and init() should have initialized the manager.
    unsafe { (&*(&raw const MANAGER)).assume_init_ref() }
}

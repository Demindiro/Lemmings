#![allow(dead_code)]
//! This kernel supports real-time scheduling with `O(1)` time complexity
//! and no additional space overhead.

use crate::{
    KernelEntryToken,
    page::{self, PAGE_SIZE, PageAttr},
    sync::{self, SpinLock, SpinLockGuard},
};
use core::{arch::asm, cell::Cell, fmt, mem, ops, ptr::NonNull};
use critical_section::CriticalSection;

const PRIORITY_COUNT: usize = 4;

static mut MANAGER: mem::MaybeUninit<SpinLock<ThreadManager>> = mem::MaybeUninit::uninit();

pub mod door {
    use core::{mem, ptr};
    use lemmings_idl_thread::*;

    door! {
        [lemmings_idl_thread Threads "Threads"]
        spawn
        critical_section_begin
        critical_section_end
        park
        unpark
        current
    }

    fn spawn(Spawn { entry, data }: Spawn) -> SpawnResult {
        let arg = match data {
            MaybeSpawnDataRef::SpawnDataRef(x) => x.0.as_ptr().cast::<()>(),
            MaybeSpawnDataRef::NoSpawnDataRef => ptr::null(),
        };
        // SAFETY: pray to God
        let init: extern "sysv64" fn(*const ()) = unsafe { mem::transmute(entry.0) };
        super::spawn(super::Priority::Regular, init, arg).expect("failed to spawn init thread");
        Ok.into()
    }

    fn critical_section_begin() -> CriticalSection {
        let x: u32;
        unsafe { core::arch::asm!("pushf; pop {:r}", out(reg) x) };
        CriticalSection::from(x)
    }

    fn critical_section_end(x: CriticalSection) {
        let x = u32::from(x);
        unsafe { core::arch::asm!("push {:r}; popf", in(reg) x) };
    }

    fn park<'a>(x: CriticalSection) {
        // SAFETY: called inside a critical section
        let cs = unsafe { super::CriticalSection::<'a>::new() };
        super::park(cs);
        critical_section_end(x)
    }

    fn unpark(thread: Thread) {
        critical_section::with(|cs| unsafe { super::unpark(cs, handle(thread)) })
    }

    fn current() -> Thread {
        unhandle(super::current())
    }

    fn handle(thread: Thread) -> super::ThreadHandle {
        super::ThreadHandle(super::ThreadRef(thread.0.cast()))
    }

    fn unhandle(thread: super::ThreadHandle) -> Thread {
        Thread(thread.0.0.cast())
    }
}

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
    program_counter: Cell<*const u8>,
    stack_pointer: Cell<*const u8>,
}

#[repr(C, align(4096))]
struct Thread {
    stack: [Cell<mem::MaybeUninit<usize>>;
        (4096 - mem::size_of::<ThreadControlBlock>()) / mem::size_of::<usize>()],
    tcb: ThreadControlBlock,
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
        entry: extern "sysv64" fn(*const ()),
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
        entry: extern "sysv64" fn(*const ()),
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
    ///
    /// The argument will be copied to RDI.
    /// Even if the thread doesn't expect an argument, it should have stored its state
    /// on the stack, automatically overwriting it if it is not at thread start.
    fn resume(&self, arg: *const (), lock: SpinLockGuard<'_, '_, ThreadManager>) {
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
                in("rdi") arg,
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

    fn resume_noarg(&self, lock: SpinLockGuard<'_, '_, ThreadManager>) {
        // The argument is redundant but:
        // - xor r32,r32 uses renaming, so it is basically free (0 latency)
        // - the thread will overwrite it anyway
        // so no ill effects.
        self.resume(core::ptr::null(), lock)
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

unsafe fn set_current(thread: &Thread) {
    unsafe { lemmings_x86_64::set_fs(&thread.tcb as *const _ as *mut u8) }
}

pub fn current() -> ThreadHandle {
    let tcb = lemmings_x86_64::fs().cast::<ThreadControlBlock>();
    let tcb = NonNull::new(tcb).expect("fs register should not be null");
    unsafe { ThreadHandle(ThreadRef::wrap_tcb(tcb)) }
}

/// Spawn a new thread and run it immediately.
pub fn spawn(
    priority: Priority,
    entry: extern "sysv64" fn(*const ()),
    arg: *const (),
) -> Result<(), ThreadSpawnError> {
    let thr = Thread::new(priority, entry)?;
    critical_section::with(|cs| unsafe {
        let mut m = manager().lock(cs);
        m.enqueue(current());
        thr.0.resume(arg, m);
    });
    Ok(())
}

/// Park the current thread.
///
/// # Warning
///
/// If the thread is not referenced anywhere, it will leak!
pub fn park(cs: CriticalSection<'_>) {
    debug!("thread::park {:?}", current().0.0);
    let mut m = manager().lock(cs);
    let next = m.dequeue_next();
    next.0.resume_noarg(m);
    debug!("thread::park {:?} -> continue", current().0.0);
}

/// # Safety
///
/// Every unpark must be matched by a park.
pub unsafe fn unpark(cs: CriticalSection<'_>, thread: ThreadHandle) {
    debug!("thread::unpark {:?} -> {:?}", current().0.0, thread.0.0);
    let mut m = manager().lock(cs);
    // SAFETY: if the thread was parked only once, then unparking once
    // means we have exclusive access to the thread.
    unsafe { m.enqueue(thread) };
    let next = m.dequeue_next();
    next.0.resume_noarg(m);
    debug!("thread::unpark {:?} -> continue", current().0.0);
}

pub fn init(entry: extern "sysv64" fn(*const ()), token: KernelEntryToken) -> ! {
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

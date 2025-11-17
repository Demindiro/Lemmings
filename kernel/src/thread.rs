#![allow(dead_code)]
//! This kernel supports real-time scheduling with `O(1)` time complexity
//! and no additional space overhead.

use crate::{
    KernelEntryToken,
    page::{self, PageAttr},
    sync::{self, SpinLock},
};
use core::{arch::asm, fmt, mem, ops, ptr::NonNull};
use critical_section::CriticalSection;

const PRIORITY_COUNT: usize = 4;

static MANAGER: SpinLock<PriorityQueue> = SpinLock::new(PriorityQueue::new());

pub mod door {
    use core::{mem, ptr};
    use lemmings_idl_thread::*;

    door! {
        [lemmings_idl_thread Threads "Threads"]
        spawn
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

    fn park() {
        critical_section::with(super::park)
    }

    fn unpark(thread: Thread) {
        critical_section::with(|cs| super::unpark(cs, handle(thread)))
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

struct PriorityQueue {
    pending: [RoundRobinQueue; PRIORITY_COUNT],
}

struct ThreadControlBlock {
    /// # Note
    ///
    /// Only valid while enqueued. Value is indeterminate while running.
    next: ThreadRef,
    program_counter: *const u8,
    stack_pointer: *const u8,
}

#[repr(C)]
struct Thread {
    tcb: SpinLock<ThreadControlBlock>,
    priority: Priority,
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct ThreadRef(NonNull<Thread>);

struct RoundRobinQueueLink {
    last: ThreadRef,
    first: ThreadRef,
}

struct HartLocal {
    current_thread: ThreadRef,
    idle_thread: ThreadRef,
}

impl PriorityQueue {
    pub const fn new() -> Self {
        Self {
            pending: [const { RoundRobinQueue::new() }; PRIORITY_COUNT],
        }
    }

    /// Create a new thread and put it at the *end* of the queue.
    pub fn spawn(
        &mut self,
        cs: CriticalSection<'_>,
        priority: Priority,
        entry: extern "sysv64" fn(*const ()),
    ) -> Result<(), ThreadSpawnError> {
        let thread = Thread::new(priority, entry)?;
        self.pending[priority as usize].enqueue(cs, thread);
        Ok(())
    }

    /// Enqueue a thread.
    ///
    /// The thread will be added to the end of its respective queue.
    ///
    /// # Warning
    ///
    /// Enqueueing the same thread twice shouldn't lead to soundness issues,
    /// but it will almost certainly result in a deadlock sooner or later.
    fn enqueue(&mut self, cs: CriticalSection<'_>, thread: ThreadHandle) {
        debug!("enqueue {:?}", thread.0.0);
        self.pending[thread.0.priority as usize].enqueue(cs, thread)
    }

    /// Dequeue the next thread with the higher priority.
    fn dequeue(&mut self, cs: CriticalSection<'_>) -> Option<ThreadHandle> {
        self.pending.iter_mut().find_map(|x| x.dequeue(cs))
    }
}

impl RoundRobinQueue {
    pub const fn new() -> Self {
        Self { cur: None }
    }

    pub fn enqueue(&mut self, cs: CriticalSection<'_>, ThreadHandle(thread): ThreadHandle) {
        let Some(cur) = self.cur.as_mut() else {
            self.cur = Some(RoundRobinQueueLink {
                first: thread,
                last: thread,
            });
            return;
        };
        // last ==> last -> thread
        cur.last.lock(cs).next = thread;
        cur.last = thread;
    }

    pub fn dequeue(&mut self, cs: CriticalSection<'_>) -> Option<ThreadHandle> {
        let mut cur = self.cur.take()?;
        let thread = cur.first;
        if thread != cur.last {
            // first -> next ==> next
            cur.first = thread.lock(cs).next;
            self.cur = Some(cur);
        }
        Some(ThreadHandle(thread))
    }
}

impl ThreadRef {
    /// # Safety
    ///
    /// - Must be a valid pointer.
    /// - Must not be dereferenced before initialization.
    pub unsafe fn wrap(ptr: NonNull<Thread>) -> Self {
        Self(ptr.cast())
    }
}

impl Thread {
    fn new(
        priority: Priority,
        entry: extern "sysv64" fn(*const ()),
    ) -> Result<ThreadHandle, ThreadSpawnError> {
        let page = page::alloc_one_guarded(PageAttr::RW)?;
        // SAFETY:
        // - the page we allocated is valid and writeable.
        // - we will initialize it right now.
        let thread = unsafe {
            let thread = page
                .byte_add(page::PAGE_SIZE - mem::size_of::<Thread>())
                .cast::<Thread>();
            let mut stack_pointer = thread.cast::<usize>().sub(1);
            // stack must be 16-byte aligned *before* call
            // hence, ret will operate on 16*N+8
            if stack_pointer.addr().get() & 15 == 0 {
                stack_pointer = stack_pointer.sub(1);
            }
            stack_pointer.write(Thread::exit as usize);
            thread.write(Thread {
                priority,
                tcb: SpinLock::new(ThreadControlBlock {
                    next: ThreadRef::wrap(thread),
                    program_counter: entry as *const u8,
                    stack_pointer: stack_pointer.as_ptr().cast::<u8>() as *const u8,
                }),
            });
            ThreadRef::wrap(thread)
        };
        Ok(ThreadHandle(thread))
    }

    fn enter(&self, token: KernelEntryToken) -> ! {
        debug!("enter {:?}", unsafe { ThreadRef::wrap(self.into()).0 });
        let self_tcb = self.tcb.lock(token.cs());
        // (3) SAFETY: we will reaqcuire and release this lock only in (2)
        let (_, self_tcb) = unsafe { self_tcb.into_inner_lock() };
        unsafe {
            set_current(self);
            asm! {
                "mov rsp, {sp}",
                "jmp {pc}",
                sp = in(reg) self_tcb.stack_pointer,
                pc = in(reg) self_tcb.program_counter,
                options(noreturn, nostack),
            }
        }
    }

    /// Enter this thread, saving the state of the current thread before entering.
    ///
    /// The argument will be copied to RDI.
    /// Even if the thread doesn't expect an argument, it should have stored its state
    /// on the stack, automatically overwriting it if it is not at thread start.
    fn resume(&self, arg: *const (), cs: CriticalSection<'_>) {
        let current = current();
        let self_ref = unsafe { ThreadRef::wrap(self.into()) };
        debug!("resume {:?} -> {:?}", current.0.0, self_ref.0);
        // (1) SAFETY: current has the lock acquired, as it is currently running.
        let current_tcb = unsafe { current.0.tcb.lock_unchecked(cs) };
        // (2) SAFETY: we will disengage the lock exactly once in the assembly code
        let (current_lock, current_tcb) = unsafe { current_tcb.into_inner_lock() };
        let self_tcb = self.tcb.lock(cs);
        // (3) SAFETY: we will reaqcuire and release this lock only in (2)
        let (_, self_tcb) = unsafe { self_tcb.into_inner_lock() };
        unsafe {
            set_current(self);
            asm! {
                "push rbx",
                "push rbp",
                "lea {scratch}, [rip + 2f]",
                "mov [{cur} + {TCB_SP}], rsp",
                "mov [{cur} + {TCB_PC}], {scratch}",
                // we have saved the thread's state, so it is now safe to disengage the lock
                // x86 supports TSO, so a simple mov does the trick.
                // ... well, not quite because of the store buffer,
                // but stores are ordered wrt other stores *on the same core*,
                // so any changes made to the thread will be visible before this store.
                "mov byte ptr [{lock}], {UNLOCK}",
                "mov rsp, {sp}",
                "jmp {pc}",
                "2:",
                "cld",
                "pop rbp",
                "pop rbx",
                in("rdi") arg,
                scratch = out(reg) _,
                cur = in(reg) current_tcb,
                sp = in(reg) self_tcb.stack_pointer,
                pc = in(reg) self_tcb.program_counter,
                lock = in(reg) current_lock,
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
            }
        }
    }

    fn resume_noarg(&self, cs: CriticalSection<'_>) {
        // The argument is redundant but:
        // - xor r32,r32 uses renaming, so it is basically free (0 latency)
        // - the thread will overwrite it anyway
        // so no ill effects.
        self.resume(core::ptr::null(), cs)
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
        unsafe { self.0.as_ref() }
    }
}

impl ops::Deref for Thread {
    type Target = SpinLock<ThreadControlBlock>;

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

macro_rules! imp_hartlocal {
    ($vis:vis $field:ident $get:ident $set:ident) => {
        unsafe fn $set(thread: &Thread) {
            let thread = NonNull::from(thread);
            unsafe { lemmings_x86_64::fs_store::<{ mem::offset_of!(HartLocal, $field) }, _>(thread) }
        }

        $vis fn $get() -> ThreadHandle {
            unsafe {
                let t = lemmings_x86_64::fs_load::<{ mem::offset_of!(HartLocal, $field) }, _>();
                ThreadHandle(ThreadRef::wrap(t))
            }
        }
    };
}

imp_hartlocal!(pub current_thread current set_current);
imp_hartlocal!(idle_thread idle_thread set_idle_thread);

/// Spawn a new thread and run it immediately.
pub fn spawn(
    priority: Priority,
    entry: extern "sysv64" fn(*const ()),
    arg: *const (),
) -> Result<(), ThreadSpawnError> {
    let thr = Thread::new(priority, entry)?;
    critical_section::with(|cs| {
        let mut m = MANAGER.lock(cs);
        m.enqueue(cs, current());
        drop(m);
        thr.0.resume(arg, cs);
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
    let mut m = MANAGER.lock(cs);
    let next = m.dequeue(cs).unwrap_or_else(idle_thread);
    drop(m);
    next.0.resume_noarg(cs);
    debug!("thread::park {:?} -> continue", current().0.0);
}

/// # Warning
///
/// An unpark not matched by a [`park`] will eventually result in a deadlock.
pub fn unpark(cs: CriticalSection<'_>, thread: ThreadHandle) {
    debug!("thread::unpark {:?} -> {:?}", current().0.0, thread.0.0);
    let mut m = MANAGER.lock(cs);
    m.enqueue(cs, thread);
    if let Some(next) = m.dequeue(cs) {
        drop(m);
        next.0.resume_noarg(cs);
    } else {
        todo!("wtf?");
    }
    debug!("thread::unpark {:?} -> continue", current().0.0);
}

/// Entrypoint for idle threads.
///
/// This function does nothing but halt in a loop.
pub fn idle_main() -> ! {
    // SAFETY: enabling interrupts is safe at this point, as the IDT is set up
    unsafe { lemmings_x86_64::enable_interrupts() };
    loop {
        lemmings_x86_64::halt()
    }
}

pub fn init(entry: extern "sysv64" fn(*const ()), token: KernelEntryToken) -> ! {
    // TODO proper allocator
    let local = crate::page::alloc_one().expect("failed to create CPU-local data");
    unsafe { lemmings_x86_64::set_fs(local.as_ptr()) };
    let idle_thread =
        Thread::new(Priority::Regular, entry).expect("failed to create initial/idle_thread thread");
    unsafe { set_idle_thread(&idle_thread.0) }
    idle_thread.0.enter(token);
}

#![allow(dead_code)]
//! This kernel supports real-time scheduling with `O(1)` time complexity
//! and no additional space overhead.

use crate::{
    KernelEntryToken,
    page::{self, PAGE_SIZE, PageAttr},
    sync::SpinLock,
};
use core::{arch::asm, cell::Cell, fmt, mem, ops, ptr::NonNull};
use critical_section::CriticalSection;

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
    priority: Cell<Priority>,
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
        self.pending[thread.0.priority.get() as usize].enqueue(thread)
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
                priority: Cell::new(priority),
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
    fn resume(&self, _cs: CriticalSection<'_>) {
        let current = current();
        unsafe {
            set_current(self);
            asm! {
                "push rbx",
                "push rbp",
                "pushf",
                "lea {scratch}, [rip + 2f]",
                "mov [{cur} + {TCB_SP}], rsp",
                "mov [{cur} + {TCB_PC}], {scratch}",
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
                TCB_SP = const mem::offset_of!(ThreadControlBlock, stack_pointer),
                TCB_PC = const mem::offset_of!(ThreadControlBlock, program_counter),
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
    pub fn resume(&self, cs: CriticalSection<'_>) {
        self.0.resume(cs)
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

/// Park the current thread.
///
/// This removes it from the main scheduler's queues.
/// If the thread isn't referenced anywhere else, it will be lost!
///
/// If the thread is re-entered execution will resume from this function.
pub fn park(cs: CriticalSection<'_>) {
    let next = manager().lock(cs).dequeue_next();
    next.0.resume(cs)
}

/// Spawn a new thread and add it to the scheduler.
pub fn spawn(priority: Priority, entry: extern "sysv64" fn()) -> Result<(), ThreadSpawnError> {
    let thr = Thread::new(priority, entry)?;
    critical_section::with(|cs| unsafe {
        // SAFETY: we just created the thread
        manager().lock(cs).enqueue(thr)
    });
    Ok(())
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

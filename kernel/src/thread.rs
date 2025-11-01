#![allow(unused)]
//! This kernel supports real-time scheduling with `O(1)` time complexity
//! and no additional space overhead.

use crate::{
    KernelEntryToken,
    page::{self, PAGE_SIZE, PageAttr},
    sync::SpinLock,
    time::Monotonic,
};
use core::{arch::asm, cell::Cell, fmt, mem, ops, ptr::NonNull};
use critical_section::CriticalSection;

const PRIORITY_COUNT: usize = 4;

static MANAGER: SpinLock<ThreadManager> = SpinLock::new(ThreadManager::new());

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
    cur: Option<ThreadRef>,
}

pub struct ThreadHandle(ThreadRef);

struct ThreadManager {
    pending: [RoundRobinQueue; PRIORITY_COUNT],
}

struct ThreadControlBlock {
    /// # Note
    ///
    /// Only valid while enqueued. Value is indeterminate while running.
    left: Cell<ThreadRef>,
    /// # Note
    ///
    /// Ditto
    right: Cell<ThreadRef>,
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

const _: () = assert!(mem::size_of::<Thread>() == PAGE_SIZE);

impl ThreadManager {
    pub const fn new() -> Self {
        Self {
            pending: [const { RoundRobinQueue::new() }; PRIORITY_COUNT],
        }
    }

    /// Create a new thread and put it at the *end* of the queue.
    #[allow(unused)]
    pub fn spawn(
        &mut self,
        priority: Priority,
        entry: extern "sysv64" fn(),
    ) -> Result<(), ThreadSpawnError> {
        let thread = self.create(priority, entry)?;
        self.pending[priority as usize].enqueue_last(ThreadHandle(thread));
        Ok(())
    }

    /// Enter the next scheduled thread.
    ///
    /// # Warning
    ///
    /// Should only be called from [`_start`]!
    pub fn enter(
        &mut self,
        priority: Priority,
        entry: extern "sysv64" fn(),
        token: KernelEntryToken,
    ) -> ! {
        self.create(priority, entry)
            .expect("failed to create initial thread")
            .enter(token);
    }

    fn create(
        &mut self,
        priority: Priority,
        entry: extern "sysv64" fn(),
    ) -> Result<ThreadRef, ThreadSpawnError> {
        let page = page::alloc_one_guarded(PageAttr::RW)?.cast::<Thread>();
        // SAFETY:
        // - the page we allocated is valid and writeable.
        // - we will initialize it right now.
        let thread = unsafe {
            let thread = ThreadRef::wrap(page);
            let base = page.add(1).cast::<ThreadControlBlock>().sub(1);
            base.write(ThreadControlBlock {
                left: Cell::new(thread),
                right: Cell::new(thread),
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
        Ok(thread)
    }

    fn dequeue_next(&mut self) -> Option<ThreadHandle> {
        self.pending.iter_mut().find_map(|x| x.dequeue_first())
    }
}

impl RoundRobinQueue {
    pub const fn new() -> Self {
        Self { cur: None }
    }

    pub fn enqueue_last(&mut self, ThreadHandle(thread): ThreadHandle) {
        let Some(cur) = self.cur else {
            self.cur = Some(thread);
            return;
        };
        // L <-> cur <-> R ==> L <-> cur <-> thread <-> R
        thread.left.set(cur);
        thread.right.set(cur.right.get());
        cur.right.set(thread);
    }

    pub fn dequeue_first(&mut self) -> Option<ThreadHandle> {
        let cur = self.cur.take()?;
        if cur != cur.right.get() {
            // L <-> cur <-> R ==> L <-> R
            let [l, r] = [cur.left.get(), cur.right.get()];
            l.right.set(r);
            r.left.set(l);
            self.cur = Some(l);
        }
        Some(ThreadHandle(cur))
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
        unsafe { Self(ptr.cast()) }
    }
}

impl Thread {
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
    fn resume(&self, cs: CriticalSection<'_>) {
        let current = current();
        unsafe {
            set_current(self);
            asm! {
                "push rbx",
                "push rbp",
                "lea {scratch}, [rip + 2f]",
                "mov [{cur} + {TCB_SP}], rsp",
                "mov [{cur} + {TCB_PC}], {scratch}",
                "mov rsp, {sp}",
                "jmp {pc}",
                "2:",
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
    let next = MANAGER.lock(cs).dequeue_next();
    match next {
        Some(next) => next.0.resume(cs),
        None => wait(cs),
    }
}

pub fn init(entry: extern "sysv64" fn(), token: KernelEntryToken) -> ! {
    let threads = unsafe { MANAGER.get_mut_unchecked() };
    threads.enter(Priority::Regular, entry, token)
}

/// Save thread state, enable interrupts and halt until the thread is resumed.
fn wait(cs: CriticalSection<'_>) {
    let current = current();
    unsafe {
        asm! {
            // TODO should we switch to a new stack/"sleep thread"?
            // This works, but it is kinda weird that we are using the stack
            // of a parked thread...
            "push rbx",
            "push rbp",
            "lea {scratch}, [rip + 2f]",
            "mov [{cur} + {TCB_SP}], rsp",
            "mov [{cur} + {TCB_PC}], {scratch}",
            "3: sti",
            "hlt",
            "jmp 3b",
            "2:",
            "pop rbp",
            "pop rbx",
            scratch = out(reg) _,
            cur = in(reg) &current.0.tcb,
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
            options(nomem, nostack),
        }
    }
}

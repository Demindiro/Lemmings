Scheduling
==========

Not all harts in a system are automatically assigned to a scheduler.

The primary purpose is to support heterogenous systems:
Systems with harts that have incompatible feature sets.

It also accounts for ever-wider systems.
Sending a thread and associated memory to the other end of a processor
is likely less efficient than keeping it nearby.

Keeping harts outside the scheduler allows fully isolating untrusted
processes from sensitive data that may leak through CPU state.


Priorities
----------

The scheduler supports 4 priorities:

1. Critical
2. Realtime
3. User
4. Regular

Threads with a higher priority *always* run first.

If no threads are queued, the idle thread will run by default.
This thread's priority is always lower than the provided priorities.


Events
------

When a thread needs to wait on an event, it is put in a queue and "parked":
the main scheduler forgets about the thread until it is "unparked".

Generally, a thread can only wait on a single event at a time.
One thread per event should be created.

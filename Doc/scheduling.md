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

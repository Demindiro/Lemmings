[index](index.html)

Overview
========

Lemmings is focused on three aspects:

- Many lightweight threads
- Fast, low-latency IPC
- Precise control over the system

One particular feature is that all processes and threads run with *full*
privileges. This will be elaborated on in the following sections.


Many lightweight threads
------------------------

Today (at the moment of writing) "async" is all the rage when it comes
to massively concurrent I/O. However, there is no fundamental difference
between "async" and "blocking" I/O.
The difference that matters is how heavy a single task is in practice.
Threads on mainstream OSes are expensive, but this needs not be the case.

Lemmings provides very light threads, using just 4KiB per thread by default.
Thread switching is very fast too, requiring only registers to be saved
and restored. No privilege switch is required as all threads run with
full privileges.

To wait for an event, simply call the appropriate routine.
It will register and park the thread.

!!! note
    The current mechanism is under revision. It will likely be replaced with events bits and a wait() call


Fast, low-latency IPC
---------------------

Many small processes makes it easier to isolate code. This is often touted
for security advantages, but another advantage is clear division of
responsibilities.
By discarding the security aspect, we can put multiple processes in a
single address space. This removes the need for context switching and
allows calling directly into other processes, making IPC as cheap
as calling plain functions.


Precise control over the system
-------------------------------

The user can precisely control resource assignment, including which hardware
threads participate in the main scheduler and creation of address spaces.
Both of these features allow dedicating a section of the hardware entirely
to untrusted processes. In particular, assigning a core to running untrusted
processes while keeping trusted processes on other cores sidesteps issues
with Spectre entirely.

Low-level control gives the opportunity to work around system deficiencies
early in the boot process and with high precision. This should be especially
valuable for systems that violate standards.
Mainstream kernels such as Linux typically address such issues using built-in
"quirks", which specify workarounds.

It is also expected that low-level control will aid with emulating interfaces
of other systems.

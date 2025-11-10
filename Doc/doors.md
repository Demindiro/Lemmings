Doors
=====

Doors are the primary IPC mechanism.

All doors are identified by an API ID and a unique routine table.

The API ID is a 128-bit integer which corresponds to an interface description.


Interface description
---------------------

Interfaces are described by an IDL. The IDL is focused on guaranteeing both
backward and forward compatibility. To this end, reasonably precise control
over data layout and bit patterns is provided.

To further ensure API stability, the API ID is derived from the description.
Any change to the description will result in a different API ID.

!!! warning
    The IDL is in early stages. Current interfaces will be deprecated.


Discovery
---------

The system provides a "hallway" and a routine to iterate over this hallway.

The system also provides a method to register more doors to the hallway.

It is *not* possible to remove doors after registration.
The only way to clean a hallway is to wipe the entire system (through e.g. reboot).

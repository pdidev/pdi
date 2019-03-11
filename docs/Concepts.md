\page Concepts Core Concepts

%PDI supports calling libraries from the 
\ref Specification_tree "specification tree" instead of from the code.
Each library call described in the \ref Specification_tree "specification tree"
relies on:
* the \ref Data_store "data store" for data transfer,
* the \ref Event_subsystem "event subsystem" for control transfer.



\section Specification_tree Specification tree

The specification tree is a file written in the
[YAML](https://en.wikipedia.org/wiki/YAML) format and provided to %PDI at
\ref PDI_init "initialization".
The specification tree is the place where one defines which plugins to load in
order to access libraries, and what library calls to make.



\section Data_store Data store

The data store handles *data transfer* between the code and libraries.

Data transfer is the action of making data available to another part of the
code.
For example, in a function call the list of parameters determines data transfer.

In %PDI, the store offers this service.
The store is a set of references where each reference is made of:
* a unique identifier (a string),
* the address of the buffer storing the data (a pointer),
* the description of the type of the data (memory layout and interpretation),
* a RW-lock to ensure exclusive access.

Buffers referenced in the store are available for access from the libraries.
On can use them from the \ref Specification_tree "specification tree".



\section Event_subsystem Event subsystem

While the data store handles data transfer between the simulation code and
libraries, the event system handles *control* transfer.
Control transfer is the action of passing the CPU control to another part of the
code.
For example, a function call is a way to transfer control, creating a thread is
another way.

In %PDI, this is handled by events that are emitted:
* when a new reference is made available in the store,
* when a \ref PDI_event "named event" is emitted by the code,
* when accessing a non-existing reference in the store.

Libraries can be notified when an event is emitted and take action at that time.
Control transfer in %PDI is synchronous (same as a function call), but plugins
can choose to implement any other behavior on top of that such as the creation
of a thread for asynchronous execution for example.



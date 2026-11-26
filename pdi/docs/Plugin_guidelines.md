# Plugin design guidelines {#plugin_design_guidelines}

To learn how to write the code of a plugin, start with the \ref how_to_create_a_plugin "plugin creation tutorial".
This document instead gives guidelines on how to design the interface of plugins, their subtree of the specification tree, so that they fit the approach of %PDI.

## General guidelines

%PDI decouples a simulation code from the libraries that handle its data.
The application is instrumented once, in a library-agnostic way: it *shares* the buffers it produces or consumes, and it *emits events* at noteworthy points of its execution.
It describes what data it has and where it is in its execution; it never says what to do with that data.

The specification tree then says which plugins act on that data, when, and how.
Users can thus change what happens to their data (write it to HDF5 instead of NetCDF, checkpoint it, send it to an in situ analysis) by editing the YAML file alone, and the same instrumentation serves C, Fortran and Python codes alike.

Plugins are the translators between the event and buffer sharing model and libraries.
Their job is to let users express, in the specification tree, what they would otherwise have written as calls in the application, using the data and events the application already exposes.
This turns the usual library design upside down: libraries do not get to require anything from the application, every decision goes into the specification tree, and plugins have to cope with whatever the application chose to expose.

## The application

### The application proposes, the specification tree disposes

**Leave every decision about what plugins do to the specification tree.**
Whether to write or read, which data to process, at which step to act: none of this should be decided by the application, nor inferred by plugins from what the application did.
Where the decision depends on the state of the simulation, let the specification tree express it with a $-expression, as \ref Decl_HDF5_plugin "Decl'HDF5" does with `when`:

```yaml
plugins:
  decl_hdf5:
    file: output.h5
    on_event: newiter
    when: '$iter % 10 = 0'   # the simulation state drives the decision, through the specification tree
    write: [main_field]
```

**Never select a behavior from the access permissions of a share.**
A buffer shared with `PDI_OUT` is one plugins may read, and one shared with `PDI_IN` is one plugins may write, but plugins must not infer from that what they are asked to do.
The application picks a direction for its own reasons, and a plugin may need it to share a buffer as `PDI_INOUT`: adding that plugin must not change what the others do.
The behavior is selected by the specification tree; plugins then ask for the permission that behavior needs, and report an error when the share does not grant it.

**Support both data-triggered and event-triggered actions.**
Let users execute an operation either whenever the data it names is shared, or when a named event is emitted, as \ref user_code_plugin "user-code" and \ref set_value_plugin "Set_value" do with their `on_data` and `on_event` keys.
Neither the names of these data nor the names of these events are chosen by plugins: they come from the specification tree.

### The application needs no plugin-specific code

**Require no change in the application for plugins to be used.**
The application shares the data it has and emits events at meaningful points of its execution; nothing in it should be there for one plugin in particular.
Concretely, plugins should not require:

* data that only they use, such as a variable the application shares for a plugin alone and never reads itself;
* a sequence of events with a plugin-specific meaning (`myplugin_init`, `myplugin_process`, `myplugin_finalize`), when an event the application already emits would do;
* the application to read their settings from its own configuration, outside their subtree of the specification tree.

Most uses of %PDI handle cross-cutting concerns: output, checkpointing, monitoring, analysis.
The application must keep running, correctly, with any plugin removed from the specification tree.

The examples shipped with %PDI (`example/example.c`, `example/example.F90`, `example/example.py`) follow the same rule: they do exactly the same thing in three languages and share no data or event specific to one plugin.
Demonstrate plugins with a YAML file next to them, or in their own tests: well-designed plugins require no change to the example to work.

### Plugins are language-agnostic

**Do not assume the language of the application.**
A buffer is described by its %PDI type: if a type is missing, add it to the type system rather than reinterpret a buffer as something its type does not say.
For example, reinterpreting a shared buffer as a C-only type breaks the other languages: a Fortran code shares an MPI communicator as an integer (`MPI_Fint`), not as a C `MPI_Comm`.
Take such handles from a $-expression instead, and let the \ref mpi_plugin "MPI" plugin provide them correctly in every language (`communicator: $MPI_COMM_WORLD`).
Test plugins from at least C and Fortran.

## The specification tree

### The tree mirrors the library's concepts

**Model the specification tree on the objects of the library, not on the calls it takes to use them.**
Plugin users usually know the underlying library: they should recognize its concepts in the specification tree, find the setting they are looking for where they expect it, and see the keys named after the library's own vocabulary.

For example, \ref Decl_NetCDF_plugin "Decl'NetCDF" describes files, groups, variables, dimensions and attributes, with the operations to perform on them; the calls that create, define, open and close them are the plugin's business.

**Do not go beyond what the library does.**
If additional features would be nice to have, they belong in a separate library, not in plugins.
If the library has a bug, do not work around it in plugins: report it upstream.

### Plugins make simple things simple, and complex things possible

**Give every key a sensible default, and offer short forms for the common cases.**
For example, in Decl'HDF5, a list of data names is enough to write each of them to a dataset of the same name; the full form is only needed for something more specific.
Settings given at an outer level serve as defaults for the inner ones, which can override them.
The specification tree for the simple case should be short enough to be guessed, and the full specification tree still available when needed.

### Plugin inputs are $-expressions

**Accept a $-expression for every value plugins take from the specification tree.**
The rest of %PDI does so, and it lets users adapt and combine the values the simulation exposes without modifying it.
An immediate value, or a plain string naming a data, is much more restrictive:

* an iteration number given as the name of a data must be exposed with exactly the type plugins cast it to, while `$iter` accepts any integer, and `$iter + $restart_offset` a computed value;
* a file name given as a literal cannot depend on the rank or the step, unlike `file: data${rank}.h5`.

This does not apply to the data plugins act *on*, such as the buffers to write or read: the next rule applies to them instead.

### Plugins act on data in user-selected buffers

**Read and write the values plugins manipulate in buffers users name in the specification tree.**
The application allocates and shares the buffers; plugins only read or fill them.
Users can then place the data where the code needs it, without changing the simulation and without an extra copy.
This is, for example, how Set_value assigns values: to the data named in its specification tree, and how Decl'HDF5 returns the size of a dataset: into the data named by its `size_of` key.
Plugins that claim fixed names force the application to adopt them, and collide with any other use of those names.

**Check the type of buffers before writing to them.**
Plugins know the %PDI type of every buffer they are given: they must check that this type can hold what they are about to write, and report an error otherwise.
When the types do not match, convert small values, such as metadata, to the type of the buffer, but report an error for large data buffers rather than convert them.

### Construction is parse time, triggering is evaluation time

**Parse the whole specification tree in plugin constructors, and reject what is not understood.**
An unknown key is far more often a typo than a deliberate choice: report it as an error, with its location in the specification tree, rather than ignore it.
Users then find mistakes in the specification tree at start-up, rather than hours into a run when the faulty operation first triggers.
It is also the right time to parse $-expressions into `PDI::Expression` objects, so that any syntax error is caught early.

**Evaluate $-expressions only when it makes sense.**
The values they reference change during the run, and may not even be shared yet when plugins are loaded.
Most expressions should be evaluated when the action is triggered, but some may need to stay constant during the whole run.
In any case, document when each expression is evaluated.
For example, Decl'NetCDF keeps its `file`, `when` and `communicator` as expressions and evaluates them each time an operation executes.

## The data store

### Data access goes through references

**Hold a reference for as long as anybody, the library included, accesses the data.**
References hold the read and write locks that give plugins access to the buffers they read or write, and keep these buffers alive.
A raw pointer obtained from a reference is only valid while the reference is held.
When a library accesses the buffers asynchronously, or between two calls (one that registers a buffer, one that later reads or writes it), plugins must keep the references (in a container of `Ref_r` or `Ref_w`) until the library no longer uses the buffers.
A reference held in a local variable inside a loop is released at the end of each iteration, before the library uses the buffer, and other plugins can then access the buffer concurrently.

Conversely, release a reference as soon as the access is over, so that the application and the other plugins regain access to the data.

### The data type is the %PDI type

**Build the library's description of the data from its %PDI type.**
The type of a buffer is part of what the application exposes: its sizes, its layout in memory (arrays with a start and a subsize, records, pointers) and its scalar kind.
Decl'HDF5 derives the HDF5 type and memory layout of each data from it, so that any data with an HDF5 equivalent can be written as is, without the application having to copy it into a dense buffer first.
Support every type the library can represent, and report a clear error, naming the data and its type, for those it cannot.

### Plugins are composable

**Design plugins to work alongside the others, not in their place.**
Plugins should expect nothing from the application beyond the data it produces and the buffers it lets plugins fill.
What a plugin produces can then be consumed by another one, and what it consumes can be produced by another one.
The \ref serialize_plugin "Serialize" plugin, for example, takes a data the user selects and shares its serialized form under a name the user selects too, for any other plugin to use.

Make plugins work with the existing ones rather than duplicate what they do.
For example, a plugin for a compression library should neither write the compressed data to a file itself, nor hand it back to the application to write: it should share the compressed buffer, so that Decl'HDF5, Decl'NetCDF or any other plugin can write it.

## The library, errors and logging

### Plugins own the library's life cycle

**Initialize libraries in the plugins that wrap them, and finalize them there.**
The application should not have to initialize a library it does not call itself.
When the initialization needs a value from the application, such as a communicator, take it from a $-expression and initialize the library once that value becomes available, at the latest on its first use.

### Errors are reported, never hidden

**Turn every error the library reports into an exception, with the library's own message.**
Check every return code, and throw a `PDI::Error` that says what was being done, on which data and with which file or object, along with the message of the library.
%PDI then reports it to the application according to its error handling settings.
Never call `exit` or `abort`, and never let a library error go unnoticed, or merely logged.

**Log through the logger plugins are given.**
Use the levels the [contribution guide](https://github.com/pdidev/pdi/blob/main/CONTRIBUTING.md#logging) defines: `info` for the few steps that help users understand what happened, `debug` for what helps them diagnose a specification tree that does not behave as expected.

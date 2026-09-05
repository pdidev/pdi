# Trace plugin {#trace_plugin}

The trace plugin is intended to generate a trace of what happens in 
\ref Data_store "PDI data store".

The plugin is very simple; just list its name in the 
\ref Specification_tree "specification tree" and you're done.

\snippet trace/docs/trace_examples.cxx example


The plugin then generates a (somewhat verbose) trace on the standard error.

For example:
\snippet trace/docs/trace_examples.output output

Each line starting by `=>>   data becoming available in the store` means a new
piece of data has been shared.

Each line starting by `<<= data stop being available in the store` means a piece
of data is being removed from the store.

Each line starting by `!!!                            named event` means a named
event has been emitted.

\page trace_plugin Trace plugin
# Trace plugin

The trace plugin is intended to generate a trace of what happens in 
\ref Data_store "PDI data store".

The plugin is very simple; just list its name in the 
\ref Specification_tree "specification tree" and you're done.

```yaml
data: #...
metadata: #...
plugins:
  trace:
  #...
```

The plugin then generates a (somewhat verbose) trace on the standard error.

For example:
```
[PDI][Test-plugin][00:00:00] *** info: Welcome!
[PDI][Test-plugin][00:00:00] *** info: =>>   data becoming available in the store: my_data
[PDI][Test-plugin][00:00:00] *** info: <<= data stop being available in the store: my_data
[PDI][Test-plugin][00:00:00] *** info: !!!                            named event: my_event
[PDI][Test-plugin][00:00:00] *** info: Goodbye!
```

Each line starting by `=>>   data becoming available in the store` means a new
piece of data has been shared.

Each line starting by `<<= data stop being available in the store` means a piece
of data is being removed from the store.

Each line starting by `!!!                            named event` means a named
event has been emitted.

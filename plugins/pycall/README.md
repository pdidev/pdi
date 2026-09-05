# Pycall plugin {#pycall_plugin}

The Pycall plugin allows calling python scripts from yaml file, that can work either if wanted data was shared or program calls specific event.

## Configuration {#pycall_configuration}

Simple plugin build:
\snippet pycall/docs/pycall_examples.cxx overview

### Plugin tree {#pycall_plugin_tree}

The root of plugin configuration (named `pycall`), is a dictionary that contains the following keys:
- \ref logging_node
- \ref pycall_on_event
- \ref pycall_on_data

### on_event {#pycall_on_event}

This option allows to execute written python code when specific event is called.

\snippet pycall/docs/pycall_examples.cxx on_event

In this example pycall will run script on `testing` event. In the next step in `with` section alias name for data is generated. `a` will be `a_python` for execusion of the python code. Finally `exec` contains python code that will be executed on `testing` event.

There is also an option to set a list of executions:
\snippet pycall/docs/pycall_examples.cxx on_event_list

### on_data {#pycall_on_data}

This option makes it possible to execute the given Python code when a specific data is shared.

\snippet pycall/docs/pycall_examples.cxx on_data

The `on_data` is a dictionary that contains data name with the python script that will be executed on share.

## Examples {#pycall_ex}

Full example of the pycall plugin.
For simplicity, the specification tree is embedded in the code rather than kept in a separate file.

The specification tree:
\snippet pycall/docs/pycall_examples.cxx full

C code:
\snippet pycall/docs/pycall_examples.cxx example

Output from the program:
\snippet pycall/docs/pycall_examples.output output

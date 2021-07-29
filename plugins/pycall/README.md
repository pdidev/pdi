\page pycall_plugin Pycall plugin

The Pycall plugin allows calling python scripts from yaml file, that can work either if wanted data was shared or program calls specific event.

\section pycall_configuration Configuration

Simple plugin build:
```yaml
plugins:
  pycall:           // name of plugin
    on_event:       // run script on event 
      testing:      // name of an event
        with:       // data alias
        exec:       // python code which will be executed when the event is called
    on_data:        // run script on data share
      data_name:    // python code which will be executed when data_name is shared
```

\subsection pycall_plugin_tree Plugin tree

The root of plugin configuration (named `pycall`), is a dictionary that contains the following keys:
- \ref logging_node
- \ref pycall_on_event
- \ref pycall_on_data

\subsection pycall_on_event on_event

This option allows to execute written python code when specific event is called.

```yaml
data: {a: {type: array, subtype: int, size: 3}}
plugins:
  pycall:
    on_event:
      testing:
        with: { a_python: $a }
        exec: \"print(' * [P] I received    $a =',a_python); a_python[1]=7; print(' * [P] changed it to $a =',a_python);\"
```
In this example pycall will run script on `testing` event. In the next step in `with` section alias name for data is generated. `a` will be `a_python` for execusion of the python code. Finally `exec` contains python code that will be executed on `testing` event.

There is also an option to set a list of executions:
```yaml
data: {a: {type: array, subtype: int, size: 3}}
plugins:
  pycall:
    on_event:
      testing:
        - exec: #some_python_script
          with: { a: $a }
        - exec: #some_other_python_script
          with: { b: $a }
```

\subsection pycall_on_data on_data

This option allows to execute written python code when specific data is shared. 

```yaml
data: {a: {type: array, subtype: int, size: 3}}
plugins:
  pycall:
    on_data:
      a: print(' * [P] I received    $a =',a); a[1]=7; print(' * [P] changed it to $a =',a);\"
```

The `on_data` is a dictionary that contains data name with the python script that will be executed on share.

\section pycall_ex Examples

Full example of pycall plugin. For siplicity yaml file was included with c code in one file instead of creating separate files.

Yaml file named your_file:
```yaml
logging: trace
data: {a: {type: array, subtype: int, size: 3}}
plugins:
  pycall:
    on_event:
      testing:
        with: { a_python: $a }
        exec: \"print(' * [P] I received    $a =',a_python); a_python[1]=7; print(' * [P] changed it to $a =',a_python);\"
```

C++ code:
```cpp
	int a[3] = {1, 2, 3};
	
	printf(" * [C] starting with $a = [%d %d %d]\n", a[0], a[1], a[2]);
	
	PDI_share("a", a, PDI_INOUT);
	PDI_event("testing");
	PDI_reclaim("a");
	
	printf(" * [C] now I see     $a = [%d %d %d]\n", a[0], a[1], a[2]);
```
Output from the program:
```
 * [C] starting with $a = [1 2 3]
[PDI][13:51:26] *** trace: Sharing `a' Ref with rights: R = true, W = true
[PDI][13:51:26] *** trace: Calling `a' share. Callbacks to call: 0
[PDI][13:51:26] *** trace: Calling `testing' event. Callbacks to call: 1
 * [P] I received    $a = [1 2 3]
 * [P] changed it to $a = [1 7 3]
 * [C] now I see     $a = [1 7 3]
```
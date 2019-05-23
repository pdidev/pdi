\page user_code_plugin The user-code plugin

The user-code plugin enables one to call a user-defined function 
when a specified event occur or certain data becomes available.

\section conf_grammar_node Configuration grammar

The root of `user-code` plugin is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"on_data"`  (*optional*)|a \ref on_data_node |
|`"on_event"` (*optional*)|a \ref on_event_node|
|`".*"`       (*optional*)| *anything*         |

* the `on_data` key specifies the list of descriptors that, when they become available, 
will cause the specified functions to be called,
* the `on_event` key specifies the list of events on which to call the specified functions,
* additional keys are ignored.

\subsection on_data_node on_data

A \ref on_data_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref function_list_node|

* each key identifies the name of a descriptor, which will trigger specified functions when it becomes available.

\subsection on_event_node on_event

A \ref on_event_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref function_list_node|

* each key identifies the name of an event, which will trigger specified functions when it occurs.

\subsection function_list_node function_list

A \ref function_list_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref function_param_list_node|

* each key identifies the name of a function, which will be called on specified event or data.

\subsection function_param_list_node function_param_list

A \ref function_param_list_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a $-expression referencing a data|

* each key identifies the name of a descriptor alias, which will be available during function execution.

\section full_config_example_node Full configuration example
```yaml                   
data:
    desc1: int                          
    desc2: float                   
    desc3: double                  
plugins:                       
    user_code:                   
        on_data:                   
            desc1:                 
                fun1: {in: $desc2, out: $desc3}
            desc2:
                fun2: {}
                fun3: {out: $desc2}
        on_event:                  
            event1:                 
                fun2: {}
            event2:
                fun4: {param1: $desc2, param2: $desc1, param3: $desc3}
```
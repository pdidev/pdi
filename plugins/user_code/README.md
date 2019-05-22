\page user_code_plugin The user-code plugin

The user-code plugin enables one to call a user-defined function 
when a specified event occur or certain data becomes available.

# Configuration grammar

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

## on_data {#on_data_node}

A \ref on_data_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|*anything*|

* each key identifies the name of a descriptor, which will trigger specified functions when it becomes available.

## on_event {#on_event_node}

A \ref on_event_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|*anything*|

* each key identifies the name of an event, which will trigger specified functions when it occurs.

## function
# Set_value plugin {#set_value_plugin}

The Set_value plugin allows setting values to data and metadata descriptors from
the yaml file.

Here are main features of the plugins:

1. Share data - plugin will share new allocated data with given values
2. Release data - plugin will release shared data
3. Expose data - plugin will expose new allocated data with given values
4. Set data - plugin will set given values to the already shared data
5. Calling an event - plugin will call an event

## Configuration grammar {#set_value_configuration}

At its root, the Set_value configuration is made of:

|key|value|
|:--|:----|
|`"logging"` (*optional*)|a \ref logging_node|
|`"on_init"` (*optional*)|a list of \ref value_operation|
|`"on_event"` (*optional*)|an \ref on_event_map_node|
|`"on_data"` (*optional*)|an \ref on_data_map_node|
|`"on_finalize"` (*optional*)|a list of \ref value_operation|

### on_init {#on_init_list_node}

Specifies a list of operation to do in \ref PDI_init function.
`on_init` is a list of \ref value_operation s.

\snippet set_value/docs/set_value_examples.cxx grammar_on_init

### on_event {#on_event_map_node}

Specifies a map of events that will trigger the values set.
`on_event`  is a map with event name as a key and \ref value_operation list as map value.

|key|value|
|:--|:----|
|`".*"` (*optional*)|a list of \ref value_operation|

\snippet set_value/docs/set_value_examples.cxx grammar_on_event

### on_data {#on_data_map_node}

Specifies a map of data that on share will trigger the values set.
`on_data`  is a map with data/metadata name as a key and \ref value_operation as map value.

|key|value|
|:--|:----|
|`".*"` (*optional*)|a list of \ref value_operation|

\snippet set_value/docs/set_value_examples.cxx grammar_on_data

### on_finalize {#on_finalize_list_node}

Specifies a list of operation to do in \ref PDI_finalize function.
`on_finalize` is a list of \ref value_operation s.

\snippet set_value/docs/set_value_examples.cxx grammar_on_finalize

### value operation {#value_operation}
A value operation is specified as a key-value pair (a **mapping** whose content
is a single key-value pair).
It defines an operation to execute inside a \ref value_list.

|key|value|
|:--|:----|
|`"set"` (*optional*)|a \ref value_list|
|`"share"` (*optional*)|a \ref value_list|
|`"expose"` (*optional*)|a \ref value_list|
|`"release"` (*optional*)|a list with data to release|
|`"event"` (*optional*)|an event name to call|
|`"logger"` (*optional*)|a \ref logger_map|


**Share warning**  

Share is always done with read and write rights.
Plugin allocates memory by `malloc`. If you reclaim the data, you should
free it with `free` instruction.

### value list {#value_list}
Is a list of the
- \ref scalar_value,
- \ref array_value,
- \ref record_value  

as elements. The order of the elements is important. The descriptors will
be set in the same order as given in the yaml file.

### scalar value {#scalar_value}

Is a map of descriptor name as a key, and a value to set as a value:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a scalar value to set|

\snippet set_value/docs/set_value_examples.cxx example_1


### array value {#array_value}
Is a map of descriptor name as a key, and a list of values to set as a value:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a list of value to set|

\snippet set_value/docs/set_value_examples.cxx example_2


### record value {#record_value}
Is a map of descriptor name as a key, and a list of members to set as a value.
Each member consist of an inner `value_list`. The order of the members is unrestricted,
but the plugin will set the values to the members in the same order.

|key|value|
|:--|:----|
|`".*"` (*optional*)|a map with member name as key and a value to set as value|

\snippet set_value/docs/set_value_examples.cxx example_3


### logger map {#logger_map}
Defines settings for global PDI logger.
|key|value|
|:--|:----|
|`"level"` (*optional*)   |level to set to the logger|
|`"pattern"` (*optional*) |pattern to set to the logger|
|`"evaluate"` (*optional*) |if true evaluate the logger pattern|

## full configuration example {#full_config}

\snippet set_value/docs/set_value_examples.cxx example_4


## Using old values to set new {#old_to_new}
### Increment value {#increment}
The set_value plugin allows to use the old values to set new values, you can use even the same
descriptor, for example to increment a scalar. 

\snippet set_value/docs/set_value_examples.cxx example_5

After calling `init` and `increment` event, `value_int` will be equal 1, and `int_array` to [2, 3, 4].

### Getting old value {#increment_in_array}
The new value is set at the end of processing the whole descriptors. This means, that if
you want to update the array element depending on other element, the old value will be set:

\snippet set_value/docs/set_value_examples.cxx example_6

After calling `init` and `increment` event, all values in `int_array` will equal `1`. 
This is because the `int_array[0]` was updated after setting all the elements.

### Getting new value {#update_array_after_scalar}

\snippet set_value/docs/set_value_examples.cxx example_7

After calling `init` and `increment` event, all values in `int_array` will equal `1`. 
This is because the `int_scalar` is set and then the `int_array` is updated after the `int_scalar` has a new value.

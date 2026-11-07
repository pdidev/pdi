# Set_value plugin {#set_value_plugin}

The Set_value plugin sets the values of data and metadata from the YAML file.

It can:

1. share data: allocate new data, with the given values, and share it,
2. release data: release data it shared,
3. expose data: allocate new data, with the given values, and expose it,
4. set data: give new values to data that is already shared,
5. emit an event.

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

```yaml
plugins:
  set_value:
    on_init:
      - set: ... # value_list
      - share: ... # value_list
      - expose: ... # value_list
      - release: ... # value_list
      - event: ... # value
      - logger: ... # logger_map
```

### on_event {#on_event_map_node}

Specifies a map of events that will trigger the values set.
`on_event`  is a map with event name as a key and \ref value_operation list as map value.

|key|value|
|:--|:----|
|`".*"` (*optional*)|a list of \ref value_operation|

```yaml
plugins:
  set_value:
    on_event:
      event_1_name:
        - set: ... # value_list
        - share: ... # value_list
        - expose: ... # value_list
        - release: ... # value_list
        - event: ... # value
      event_2_name:
        - set: ... # value_list
        - share: ... # value_list
        - expose: ... # value_list
        - release: ... # value_list
        - event: ... # value
```

### on_data {#on_data_map_node}

Specifies a map of data that on share will trigger the values set.
`on_data`  is a map with data/metadata name as a key and \ref value_operation as map value.

|key|value|
|:--|:----|
|`".*"` (*optional*)|a list of \ref value_operation|

```yaml
metadata:
  metadata_1_name: ... # metadata_type
data:
  data_1_name: ...  # data_type
plugins:
  set_value:
    on_data:
      data_1_name:
        - set: ... # value_list
        - share: ... # value_list
        - expose: ... # value_list
        - release: ... # value_list
        - event: ... # value
      metadata_1_name:
        - set: ... # value_list
        - share: ... # value_list
        - expose: ... # value_list
        - release: ... # value_list
        - event: ... # value
```

### on_finalize {#on_finalize_list_node}

Specifies a list of operation to do in \ref PDI_finalize function.
`on_finalize` is a list of \ref value_operation s.

```yaml
plugins:
  set_value:
    on_finalize:
      - set: ... # value_list
      - share: ... # value_list
      - expose: ... # value_list
      - release: ... # value_list
      - event: ... # value
```

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


\warning
Share is always done with read and write rights.
Plugin allocates memory by `malloc`.
If you reclaim the data, you should deallocate it with `free`.

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

```yaml
metadata:
  scalar_name: int
set_value:
  on_init:
    - expose:
      - scalar_name: 42
```

### array value {#array_value}
Is a map of descriptor name as a key, and a list of values to set as a value:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a list of value to set|

```yaml
metadata:
  array_name:
    type: array
    size: 4
    subtype: int

plugins:
  set_value:
    on_init:
      - expose:
        - array_name: [2, 3, 4, 5]
```

### record value {#record_value}
Is a map of descriptor name as a key, and a list of members to set as a value.
Each member consist of an inner `value_list`. The order of the members is unrestricted,
but the plugin will set the values to the members in the same order.

|key|value|
|:--|:----|
|`".*"` (*optional*)|a map with member name as key and a value to set as value|

```yaml
metadata:
  record_name:
    type: record
    buffersize: 16
    members:
      member_1:
        disp: 0
        type: array
        size: 3
        subtype: int
      member_2:
        disp: 12
        type: int

set_value:
  on_init:
    - expose:
      - record_name:
          member_2: 3 # int member
          member_1: [1, 2, 3] # array of ints member
```

### logger map {#logger_map}
Defines settings for global PDI logger.
|key|value|
|:--|:----|
|`"level"` (*optional*)   |level to set to the logger|
|`"pattern"` (*optional*) |pattern to set to the logger|
|`"evaluate"` (*optional*) |if true evaluate the logger pattern|

## Full configuration example {#full_config}

```yaml
metadata:
  array_size: int64

data:
  record_data:
    type: record
    buffersize: 36
    members:
      scalar_data: 
        disp: 0
        type: int
      array_data:
        disp: 4
        type: array
        size: $array_size
        subtype: int

plugins:
  set_value:
    on_init:
      - expose:
        - array_size: 3
      - share:
        - record_data:
          - scalar_data: 0
          - array_data: [0, 0, 0]
    on_event:
      event_1_name:
        - set:
          - record_data:
            - scalar_data: 3
            - array_data: [1, 2, 3]
    on_finalize:
      - release: [record_data]
```

## Using old values to set new ones {#old_to_new}
### Incrementing a value {#increment}
The new values can be computed from the old ones, even those of the data being set, for example to
increment a scalar.

```yaml
data:
  value_int:
    type: int
  int_array:
    size: 3
    subtype: int
    type: array
plugins:
  set_value:
    on_event:
      init:
        - share:
          - value_int: 0
          - int_array: [1, 2, 3]
      increment:
        - set:
          - value_int: "$value_int + 1"
          - int_array: ["$int_array[0] + 1", "$int_array[1] + 1", "$int_array[2] + 1"]
    on_finalize:
      - release: [value_int, int_array]
```
After the `init` and `increment` events, `value_int` equals 1, and `int_array` [2, 3, 4].

### Getting the old value {#increment_in_array}
The new value of a data is only set once all its elements are evaluated. So when an element of an
array depends on another element of the same array, it gets the old value of that element:

```yaml
metadata:
  int_array:
    size: 3
    subtype: int
    type: array
plugins:
  set_value:
    on_event:
      init:
        - expose:
          - int_array: [0, 0, 0]
      increment:
        - expose:
          - int_array: ["$int_array[0] + 1", "$int_array[0] + 1", "$int_array[1] + 1"]
```
After the `init` and `increment` events, all the elements of `int_array` equal `1`,
because `int_array[0]` is only updated once all the elements are evaluated.

### Getting the new value {#update_array_after_scalar}

```yaml
metadata:
  int_scalar: int
  int_array:
    size: 3
    subtype: int
    type: array
plugins:
  set_value:
    on_event:
      init:
        - expose:
          - int_scalar: 0
          - int_array: [0, 0, 0]
      increment:
        - expose:
          - int_scalar: $int_scalar+1
          - int_array: ["$int_scalar", "$int_scalar", "$int_scalar"]
```
After the `init` and `increment` events, all the elements of `int_array` equal `1`,
because the data of a value list are set in order: `int_scalar` has its new value when `int_array` is evaluated.

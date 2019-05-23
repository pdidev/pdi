\page Specification_tree_ref Specification tree Reference

The root of PDI configuration is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"data"` (*optional*)|a \ref data_list_node|
|`"metadata"` (*optional*)|a \ref data_list_node|
|`"plugins"` (*optional*)|a \ref plugin_list_node|
|`".*"` (*optional*)| *anything* |

* the `data` and `metadata` sections specify the type of the data in buffers
exposed by the application; for `metadata`, PDI keeps a copy while it only
keeps references for `data`,
* the `plugin` section specifies the list of plugins to load and their
configuration,
* additional sections are ignored.

# array_type {#array_type_node}

A \ref array_type_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"array"`|
|`"size"`|a \ref intval_or_list_node|
|`"subtype"` (*optional*)|a \ref datatype_node|
|`"subsize"` (*optional*)|a \ref intval_or_list_node|
|`"start"` (*optional*)|a \ref intval_or_list_node|

A \ref array_type_node represents a potentially multi-dimensional array where:
* the value associated to the `size` key represents the size of the array in
  each dimension (C order),
* the value associated to the `subtype` key represents the type of the elements
  in the array,

# char_type {#char_type_node}

A \ref char_type_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"char"`|

A \ref char_type_node represents the C `char` datatype; it accepts no parameter.

# character_type {#character_type_node}

A \ref character_type_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"character"`|
|`"kind"` (*optional*)|an [Integer expression](\ref expression_node)|

A \ref character_type_node represents the Fortran `character` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`character(kind=...)`).

# datatype {#datatype_node}

A \ref datatype_node can be any of:
* a \ref array_type_node,
* a \ref char_type_node,
* a \ref character_type_node,
* a \ref double_type_node,
* a \ref float_type_node,
* a \ref int_type_node,
* a \ref int16_type_node,
* a \ref int32_type_node,
* a \ref int64_type_node,
* a \ref int8_type_node,
* a \ref integer_type_node,
* a \ref logical_type_node,
* a \ref real_type_node,
* a \ref record_type_node,
* a \ref simple_datatype_node.

All these are dictionaries (except for `simple_datatype`) with a `type` key
whose value disambiguate between them, other keys act as parameters to the type.
Plugins can add new datatypes that follow the same pattern.

# data_list {#data_list_node}

A \ref data_list_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref datatype_node|

* each key identifies the name of a buffer exposed to PDI associated to its
type.

# double_type {#double_type_node}

A `double` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"double"`|

A `double` is a datatype that represents the C `double` type.
It accepts no parameter.

# expression {#expression_node}

TODO

# float_type {#float_type_node}

A `float` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"float"`|

A `float` is a datatype that represents the C `float` type.
It accepts no parameter.

# int_type {#int_type_node}

A `int` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int"`|

A `int` is a datatype that represents the C `int` type.
It accepts no parameter.

# int16_type {#int16_type_node}

A `int` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int16"`|

A `int16` is a datatype that represents the C `int16_t` type from the
`<stdtypes.h>` header.
It accepts no parameter.

# int32_type {#int32_type_node}

A `int32` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int32"`|

A `int32` is a datatype that represents the C `int32_t` type from the
`<stdtypes.h>` header.
It accepts no parameter.

# int64_type {#int64_type_node}

A `int64` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int64"`|

A `int64` is a datatype that represents the C `int64_t` type from the
`<stdtypes.h>` header.
It accepts no parameter.

# int8_type {#int8_type_node}

A `int8` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int8"`|

A `int8` is a datatype that represents the C `int8_t` type from the
`<stdtypes.h>` header.
It accepts no parameter.

# integer_type {#integer_type_node}

A `integer` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"integer"`|
|`"kind"` (*optional*)|an [Integer expression](\ref expression_node)|

A `integer` represents the Fortran `integer` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`integer(kind=...)`).

# intval {#intval_node}

A \ref intval_node is... TODO.

# intval_list {#intval_list_node}

A \ref intval_list_node is a list... TODO.

# intval_or_list {#intval_or_list_node}

A \ref intval_or_list_node can be any of:
* a \ref intval_node,
* a \ref intval_list_node.

TODO

# logical_type {#logical_type_node}

A `logical` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"logical"`|
|`"kind"` (*optional*)|an [Integer expression](\ref expression_node)|

A `logical` represents the Fortran `logical` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`logical(kind=...)`).

# member_list {#member_list_node}

TODO

# plugin_list {#plugin_list_node}

A `plugin_list` is a dictionary that contains the following keys:
|key|value|
|:--|:----|
|`".*"` (*optional*)| *anything* |

* each key identifies the name of a plugin to load associated to its
configuration; the content of the configuration depends on the plugin.

# real_type {#real_type_node}

A `real` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"real"`|
|`"kind"` (*optional*)|an [Integer expression](\ref expression_node)|

A `real` represents the Fortran `real` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`real(kind=...)`).

# record_type {#record_type_node}

A `record` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"record"`|
|`"buffersize"`|a \ref intval_or_list_node|
|`"members"` (*optional*)|a \ref member_list_node|

A `record` represents a potentially multi-dimensional array where:

# simple_datatype {#simple_datatype_node}

A `simple_datatype` is interpreted as a shortcut for a dictionary with a
single key `type` whose value is the provided scalar and therefore another
\ref datatype_node.

For example, the following value:
```
"my_type"
```
is interpreted as if it was:
```
{ type: "my_type" }
```

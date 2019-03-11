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

# array {#array_node}

A \ref array_node is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"array"`|
|`"size"`|a \ref integer_or_list_node|
|`"subtype"` (*optional*)|a \ref datatype_node|
|`"subsize"` (*optional*)|a \ref integer_or_list_node|
|`"start"` (*optional*)|a \ref integer_or_list_node|

A \ref array_node represents a potentially multi-dimensional array where:
* the value associated to the `size` key represents the size of the array in
  each dimension (C order),
* the value associated to the `subtype` key represents the type of the elements
  in the array,

# char {#char_node}

A `char` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"char"`|

A `char` represents the C `char` datatype; it accepts no parameter.

# character {#character_node}

A `character` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"character"`|
|`"kind"` (*optional*)|an [Integer expression](\ref expression_node)|

A `character` represents the Fortran `character` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`character(kind=...)`).

# datatype {#datatype_node}

A `datatype` can be any of:
* a \ref array_node,
* a \ref char_node,
* a \ref character_node,
* a \ref double_node,
* a \ref float_node,
* a \ref int_node,
* a \ref int8_node,
* a \ref int16_node,
* a \ref int32_node,
* a \ref int64_node,
* a \ref integer_node,
* a \ref logical_node,
* a \ref real_node,
* a \ref record_node,
* a \ref simple_datatype_node,

All these are dictionaries (except for `simple_datatype`) with a `type` key
whose value disambiguate between them, other keys act as parameters to the type.
Plugins can add new datatypes that follow the same pattern.

# data_list {#data_list_node}

A `data_list` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref datatype_node|

* each key identifies the name of a buffer exposed to PDI associated to its
type.

# double {#double_node}

A `double` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"double"`|

A `double` is a datatype that represents the C `double` type.
It accepts no parameter.

# float {#float_node}

A `float` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"float"`|

A `float` is a datatype that represents the C `float` type.
It accepts no parameter.

# int {#int_node}

A `int` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int"`|

A `int` is a datatype that represents the C `int` type.
It accepts no parameter.

# int8 {#int8_node}

A `int8` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int8"`|

A `int8` is a datatype that represents the C `int8_t` type from the
`<stdtypes.h>` header.
It accepts no parameter.

# int16 {#int16_node}

A `int` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int16"`|

A `int16` is a datatype that represents the C `int16_t` type from the
`<stdtypes.h>` header.
It accepts no parameter.

# int32 {#int32_node}

A `int32` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int32"`|

A `int32` is a datatype that represents the C `int32_t` type from the
`<stdtypes.h>` header.
It accepts no parameter.

# int64 {#int64_node}

A `int64` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int64"`|

A `int64` is a datatype that represents the C `int64_t` type from the
`<stdtypes.h>` header.
It accepts no parameter.

# integer {#integer_node}

A `integer` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"integer"`|
|`"kind"` (*optional*)|an [Integer expression](\ref expression_node)|

A `integer` represents the Fortran `integer` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`integer(kind=...)`).

# logical {#logical_node}

A `logical` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"logical"`|
|`"kind"` (*optional*)|an [Integer expression](\ref expression_node)|

A `logical` represents the Fortran `logical` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`logical(kind=...)`).

# plugin_list {#plugin_list_node}

A `plugin_list` is a dictionary that contains the following keys:
|key|value|
|:--|:----|
|`".*"` (*optional*)| *anything* |

* each key identifies the name of a plugin to load associated to its
configuration; the content of the configuration depends on the plugin.

# real {#real_node}

A `real` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"real"`|
|`"kind"` (*optional*)|an [Integer expression](\ref expression_node)|

A `real` represents the Fortran `real` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`real(kind=...)`).

# record {#record_node}

A `record` is a dictionary that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"record"`|
|`"buffersize"`|a \ref integer_or_list_node|
|`"members"` (*optional*)|a \ref member_list_node|

A `record` represents a potentially multi-dimensional array where:

# simple_datatype {#simple_datatype_node}

A `simple_datatype` is a `scalar`.

The `simple_datatype` is interpreted as a shortcut for a dictionary with a
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

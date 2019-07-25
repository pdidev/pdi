\page Specification_tree_ref Specification tree Reference

The %PDI specification tree is expressed in
[YAML](https://en.wikipedia.org/wiki/YAML).
As such, it is a tree that contains 3 distinct kinds of nodes:
* scalars,
* sequences,
* mappings.

A **scalar** is a leaf in the tree, represented as a string.
Some form of scalars can be interpreted as a boolean, integer or floating-point
valued.
Simple examples of scalars include for example (see the
[YAML specification](https://yaml.org/spec/1.2/spec.html#id2760844) for the
complete syntax):
* `"hello"`,
* `world`,
* `3`,
* `5.7`.

A **sequence** is an ordered list of sub-nodes (that can themselves be scalars,
sequences or mappings).
Two main syntaxes are available for sequences (see the 
[YAML specification](https://yaml.org/spec/1.2/spec.html#id2759963) for the
complete syntax).
* in-line sequence: `[1, 2, 3, hello, "world"]`
* multi-line sequence:
```
- 1
- 2
- 3
- hello
- world
```

A **mapping** is an unordered list of key-value pairs (whose elements can
themselves be scalars, sequences or mappings).
Two main syntaxes are available for mapping (see the 
[YAML specification](https://yaml.org/spec/1.2/spec.html#id2759963) for the
complete syntax).
* in-line mapping: `{1: one, 2: "two", "three": 3}`
* multi-line mapping:
```
1: one
2: two
"three": 3
```


# specification tree root {#root_node}

The *specification tree root* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"data"` (*optional*)|a \ref data_map_node|
|`"metadata"` (*optional*)|a \ref data_map_node|
|`"plugins"` (*optional*)|a \ref plugin_map_node|
|`".*"` (*optional*)| *anything* |

* the `data` and `metadata` sections specify the type of the data in buffers
exposed by the application; for `metadata`, %PDI keeps a copy while it only
keeps references for `data`,
* the `plugin` section specifies the list of plugins to load and their
configuration,
* additional sections are ignored.

**Example:**

```python
metadata:
  my_metadata: int
data:
  my_data:
    type: array
    subtype: double
    size: 5
plugins:
  decl_hdf5: #...
  mpi: #...
```

# array_type {#array_type_node}

A *array_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"array"`|
|`"size"`|a \ref intexpr_or_seq_node|
|`"subtype"` (*optional*)|a \ref datatype_node|
|`"subsize"` (*optional, deprecated*)|a \ref intexpr_or_seq_node|
|`"start"` (*optional, deprecated*)|a \ref intexpr_or_seq_node|

A *array_type_node* represents a potentially multi-dimensional array where:
* the value associated to the `size` key represents the size of the array in
  each dimension (C order),
* the value associated to the `subtype` key represents the type of the elements
  in the array,
* the value associated to the `subsize` key represents the number of elements to
  actually use in each dimension (slicing), if specified it **must** have the
  same number of elements as `size`, this defaults to the full array size in
  each dimension,
* the value associated to the `start` key represents the index of the first
  element to actually use in each dimension (slicing), if specified it **must**
  have the same number of elements as `size`, this defaults to the first (0)
  element in each dimension.

**Examples:**

```python
type: array
subtype: double
size: 5
```

```python
type: array
subtype: { type: character, kind: 4 }
size: [ '$size_1d', '$size_2d' ]
```

# char_type {#char_type_node}

A *char_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"char"`|

A *char_type* represents the C `char` datatype; it accepts no parameter.

**Example:**

```python
type: char
```

# character_type {#character_type_node}

A *character_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"character"`|
|`"kind"` (*optional*)|a \ref expression_node "integer-valued $-expression"|

A *character_type_node* represents the Fortran `character` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`character(kind=...)`).

**Examples:**

```python
type: character
```

```python
type: character
kind: 4
```

# datatype {#datatype_node}

A *datatype* can be **any of**:
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

Amongst these, \ref simple_datatype_node is the only scalar.
All others are dictionaries with a `type` key used for disambiguation between
them.
Plugins can add new options that follow the same pattern.

A *datatype* represents the memory layout and interpretation for data
exposed by the user in the \ref Data_store "data store".

# data_map {#data_map_node}

A *data_map* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref datatype_node|

* each key identifies the name of a buffer exposed to %PDI associated to its
type.

**Example:**

```python
my_data_1: int
my_data_2: {type: array, subtype: double, size: 5}
```

# double_type {#double_type_node}

A *double_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"double"`|

A *double_type* represents the C `double` type.
It accepts no parameter.

**Example:**

```python
type: double
```

# $-expression {#expression_node}

A *$-expression* is a **scalar** whose content matches the following
grammar:

\include docs/expression_grammar.in.txt

The rules for evaluating an expression are close to those of BASH for example.

In addition to raw text, a `STRING_LITERAL` can contain references to the string
value of some data in the store as well as the result of an operation by
enclosing it inside a dollar-parenthesis `$()`.

An `OPERATION` can include logical and arithmetic operators grouped by
parenthesis.
The basic terms manipulated in an operation can be integer literals or
references to the integer value of some data in the store.

A `REFERENCE` is introduced by a dollar `$` sign and optionally enclosed in
curly braces `{`, `}`.
Its value is that of the data or metadata with the associated name.
It is always a good idea to have referenced values in the metadata section as it
prevents dangling references.
A direct reference is possible as well as sub-references to array elements using
the square brackets `[`, `]` operator.

The value-type of an `EXPRESSION` is as follow:
* if it's a `REFERENCE`, it has the  type of the referenced data in the store,
* if it's a `OPERATION`, it is integer-valued,
* if it's a `STRING_LITERAL`, it is string-valued.

In addition, an integer can be interpreted as a string or as a boolean value
where zero is interpreted as false and any other value as true.

**Examples:**

```python
'$my_data'
```

```python
'($my_data + 3) % 6'
```

```python
'my name is ${my_name}'
```

# float_type {#float_type_node}

A *float_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"float"`|

A \ref float_type_node represents the C `float` type.
It accepts no parameter.

**Example:**

```python
type: float
```

# int_type {#int_type_node}

A *int_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int"`|

A \ref int_type_node represents the C `int` type.
It accepts no parameter.

**Example:**

```python
type: int
```

# int16_type {#int16_type_node}

A *int16_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int16"`|

A \ref int16_type_node represents the C `int16_t` type from
the `<stdtypes.h>` header.
It accepts no parameter.

**Example:**

```python
type: int16
```

# int32_type {#int32_type_node}

A *int32_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int32"`|

A \ref int32_type_node represents the C `int32_t` type from
the `<stdtypes.h>` header.
It accepts no parameter.

**Example:**

```python
type: int32
```

# int64_type {#int64_type_node}

A *int64_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int64"`|

A *int64_type* represents the C `int64_t` type from
the `<stdtypes.h>` header.
It accepts no parameter.

**Example:**

```python
type: int64
```

# int8_type {#int8_type_node}

A *int8_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int8"`|

A \ref int8_type_node represents the C `int8_t` type from the
`<stdtypes.h>` header.
It accepts no parameter.

**Example:**

```python
type: int8
```

# integer_type {#integer_type_node}

A *integer_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"integer"`|
|`"kind"` (*optional*)|a \ref expression_node "integer-valued $-expression"|

A *integer_type* represents the Fortran `integer` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`integer(kind=...)`).
If missing, the default kind of the Fortran implementation is used.

**Examples:**

```python
type: integer
```

```python
type: integer
kind: 2
```

# intexpr_seq {#intexpr_seq_node}

A *intexpr_seq* is a **sequence** where each element of the sequence is
a \ref expression_node "integer-valued $-expression".

**Example:**

```python
[ 1, '2', '$size', '$other_size + 2' ]
```

# intexpr_or_seq {#intexpr_or_seq_node}

A *intexpr_or_seq* can be **any of**:
* a \ref expression_node "integer-valued $-expression",
* a \ref intexpr_seq_node.

In that context, a simple \ref expression_node is interpreted as a shortcut for
a sequence containing a single \ref expression_node.

For example, the following value:
```
"$x + 2"
```

is interpreted as if it was:
```
[ "$x + 2" ]
```

# logical_type {#logical_type_node}

A *logical_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"logical"`|
|`"kind"` (*optional*)|a \ref expression_node "integer-valued $-expression"|

A *logical_type* represents the Fortran `logical` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`logical(kind=...)`).
If missing, the default kind of the Fortran implementation is used.

**Examples:**

```python
type: logical
```

```python
type: logical
kind: 1
```

# member_desc {#member_desc_node}

A *member_desc* is a **mapping** that contains the following keys:
|key|value|
|:--|:----|
|`"disp"`|a \ref expression_node "integer-valued $-expression"|
|`"type"`|a `scalar`, `array` or `record` |
|`".*"` (*optional*)| *anything* |

* the value associated to the `disp` key identifies the displacement in bytes
  from the base address of the record and the address of this specific member,
* the value associated to the `type` key identifies the 
  \ref datatype_node "type" of this record, all other keys required for this
  \ref datatype_node must be present.

**Examples:**

```python
my_char:
      disp: 0
      type: int64
```

```python
    my_array:
      disp: 8
      type: array
      subtype: int64
      size: [10, 10]
```

See \ref record_type_node for more examples.

# members_map {#members_map_node}

A *members_map* is a **mapping** that contains the following keys:
|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref member_desc_node|

* each key identifies the name of a member of the record and the value
  associated to it describes the member itself.

See \ref record_type_node for an example.

# plugin_map {#plugin_map_node}

A *plugin_map* is a **mapping** that contains the following keys:
|key|value|
|:--|:----|
|`".*"` (*optional*)| *anything* |

* each key identifies the name of a plugin to load associated to its
  configuration; the content of the configuration depends on the plugin.

Have a look at the \ref Plugins "plugins" documentation to see the specification
tree they accept.

See \ref root_node for an example.

# real_type {#real_type_node}

A *real_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"real"`|
|`"kind"` (*optional*)|a \ref expression_node "integer-valued $-expression"|

A *real_type* represents the Fortran `real` datatype.
The value associated to the `kind` key corresponds to the Fortran *kind*
parameter (`real(kind=...)`).
If missing, the default kind of the Fortran implementation is used.

**Examples:**

```python
type: real
```

```python
type: real
kind: 8
```

# record_type {#record_type_node}

A *record_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"record"`|
|`"buffersize"`|a \ref expression_node "integer-valued $-expression"|
|`"members"` (*optional*)|a \ref members_map_node|

A \ref record_type_node represents a "record", *aka* C "struct", C++ "class",
Fortran "derived type" where:
* the value associated to the `buffersize` key represents the overall size of
  the record, including potential padding,
* the value associated to the `members` key lists all members of the record.

**Example:**

```python
type: record
buffersize: 8
members:
  first_int:
    disp: 0
    type: int32
  seconf_int:
    disp: 4
    type: int32
```

# simple_datatype {#simple_datatype_node}

A *simple_datatype* is a **scalar**.

It is interpreted as a shortcut for a mapping with a
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

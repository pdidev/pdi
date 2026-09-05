# Specification tree Reference {#Specification_tree_ref}

The %PDI specification tree is expressed in \subpage YAML.


## specification tree root {#root_node}

The *specification tree root* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"include"` (*optional*)     | a \ref include_or_seq_node |
|`"logging"` (*optional*)     | a \ref logging_node |
|`"types"` (*optional*)|a \ref types_map_node|
|`"metadata"` (*optional*)|a \ref data_map_node|
|`"data"` (*optional*)        | a \ref data_map_node |
|`"plugin_path"` (*optional*)|a \ref plugin_path_map_node|
|`"plugins"` (*optional*)     | a \ref plugin_map_node |
|`".*"` (*optional*)| *anything* |

* the `include` section specify other YAML configuration part of a same
  Paraconf tree. The configuration it contains should have the same format as
  the main file (with `include`, `types`,  `metadata`,  `data`,  `plugin_path`,
  and`plugins` sections. But the keys under these sections should not be
  repeated. The same file can be included twice (diamond include) but recursive
  inclusion is an error.
* the `logging` section specify logger properties
* the `metadata` and `data` sections specify the type of the data in buffers
  exposed by the application; for `metadata`, %PDI keeps a copy while it only
  keeps references for `data`,
* the `plugin_path` section specifies the path to directories where %PDI should
  search for plugins
* the `plugins` section specifies the list of plugins to load and their
  configuration,
* the `types` section specifies user-defined datatypes
* additional sections are ignored.

### Example:

\snippet doc_logging.cxx root_tree


## array_type {#array_type_node}

A *array_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"array"`|
|`"size"`|a \ref intexpr_or_seq_node|
|`"subtype"` (*optional*)|a \ref datatype_node|
|`"subsize"` (*optional, deprecated*)|a \ref intexpr_or_seq_node|
|`"start"` (*optional, deprecated*)|a \ref intexpr_or_seq_node|
|`"\+.*"` (*optional*)|anything|

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
  element in each dimension,
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx array_type

\snippet Specification_tree_ref/spec_tree_examples.cxx array_type_2


## byte_type {#byte_type_node}

A *byte_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"byte"`|
|`"\+.*"` (*optional*)|anything|

A \ref byte_type_node represents the C++ `byte` type.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

**Example:**

\snippet Specification_tree_ref/spec_tree_examples.cxx byte_type


## char_type {#char_type_node}

A *char_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"char"`|
|`"\+.*"` (*optional*)|anything|

A *char_type* represents the C `char` datatype; it accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx char_type


## character_type {#character_type_node}

A *character_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"character"`|
|`"kind"` (*optional*)|a \ref expression_node "integer-valued $-expression"|
|`"\+.*"` (*optional*)|anything|

A *character_type_node* represents the Fortran `character` datatype, where:
* the value associated to the `kind` key corresponds to the Fortran *kind*
  parameter (`character(kind=...)`),
  if missing, the default kind of the Fortran implementation is used,
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx character_type

\snippet Specification_tree_ref/spec_tree_examples.cxx character_type_2


## data_map {#data_map_node}

A *data_map* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref datatype_node|

* each key identifies the name of a buffer exposed to %PDI associated to its
type.

### Example:

\snippet PDI_doc_datatype.cxx data_map


## datatype {#datatype_node}

A *datatype* can be **any of**:
* a \ref array_type_node,
* a \ref byte_type_node,
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
* a \ref int_fast16_type_node,
* a \ref int_fast32_type_node,
* a \ref int_fast64_type_node,
* a \ref int_fast8_type_node,
* a \ref int_least16_type_node,
* a \ref int_least32_type_node,
* a \ref int_least64_type_node,
* a \ref int_least8_type_node,
* a \ref intmax_type_node,
* a \ref intptr_type_node,
* a \ref logical_type_node,
* a \ref long_type_node,
* a \ref long_type_node,
* a \ref pointer_type_node,
* a \ref ptrdiff_t_type_node,
* a \ref real_type_node,
* a \ref record_type_node,
* a \ref short_type_node,
* a \ref simple_datatype_node,
* a \ref size_t_type_node,
* a \ref struct_type_node,
* a \ref tuple_type_node,
* a \ref uint_type_node,
* a \ref uint16_type_node,
* a \ref uint32_type_node,
* a \ref uint64_type_node,
* a \ref uint8_type_node,
* a \ref uint_fast16_type_node,
* a \ref uint_fast32_type_node,
* a \ref uint_fast64_type_node,
* a \ref uint_fast8_type_node,
* a \ref uint_least16_type_node,
* a \ref uint_least32_type_node,
* a \ref uint_least64_type_node,
* a \ref uint_least8_type_node,
* a \ref uintmax_type_node,
* a \ref uintptr_type_node,
* a \ref unsigned_long_type_node,
* a \ref unsigned_long_long_type_node,
* a \ref unsigned_short_type_node,
* any user-defined datatype name.

\ref simple_datatype_node is just a string that identifies the referenced type.
All others cases are dictionaries whose `type` key identifies the type.

A *datatype* represents the memory layout and interpretation for data
exposed by the user in the \ref Data_store "data store".

Optional attributes can be added to any *datatype*.
An attribute is identified by a key that starts with the `+` character.
The value can be anything (**scalar**, **sequence** or **mapping**).

\warning
Some additional types that are not listed here might be made available by
plugins.

### Example:

\snippet PDI_doc_datatype.cxx attributes

## datatype_with_disp {#datatype_with_disp_node}

A *datatype_with_disp* is a **mapping** that must be a valid \ref datatype_node
with an additional `disp` key:
|key|value|
|:--|:----|
|`"disp"`|a \ref expression_node "integer-valued $-expression"|
|`"type"`|a type identifier|
|`".*"` (*\ref datatype_node *)|see \ref datatype_node |

* the value associated to the `disp` key specifies the offset in bytes from the
  base address of the container to this specific member,
* the other keys and values are interpreted as for a \ref datatype_node .


## double_type {#double_type_node}

A *double_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"double"`|
|`"\+.*"` (*optional*)|anything|


A *double_type* represents the C `double` type; it accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx double_type


## $-expression {#expression_node}

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
Value formatting can be applied using a FMT `format_spec` string by adding a
column `:` followed by the `format_spec` after a column just before the closing
bracket `}` (see: https://fmt.dev/latest/syntax.html#grammar-token-format_spec).
A direct reference is possible as well as sub-references to:
* array elements using the square brackets `[`, `]` operator,
* record member using dot `.` operator.

The value-type of an `EXPRESSION` is as follow:
* if it's a `REFERENCE`, it has the  type of the referenced data in the store,
* if it's a `OPERATION`, it is integer-valued,
* if it's a `STRING_LITERAL`, it is string-valued.

In addition, an integer can be interpreted as a string or as a boolean value
where zero is interpreted as false and any other value as true.

The following strings can also be interpreted as a boolean integer values:
* true (1): `y`, `Y`, `yes`, `Yes`, `YES`, `true`, `True`, `TRUE`, `on`, `On`,
  `ON`.
* false (0): `n`, `N`, `no`, `No`, `NO`, `false`, `False`, `FALSE`, `Off`,
  `Off`, `OFF`.

### Example:

\snippet PDI_doc_expression.cxx simple_reference

\snippet PDI_doc_expression.cxx operation

\snippet PDI_doc_expression.cxx subscript

\snippet PDI_doc_expression.cxx in_string

\snippet PDI_doc_expression.cxx format_int

\snippet PDI_doc_expression.cxx format_binary

\snippet PDI_doc_expression.cxx format_float

\snippet PDI_doc_expression.cxx format_string


## float_type {#float_type_node}

A *float_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"float"`|
|`"\+.*"` (*optional*)|anything|

A \ref float_type_node represents the C `float` type.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx float_type


## include {#include_node}

An *include* can be **any of**:
* a **scalar**,
* a \ref include_with_subtree_node.

In that context, a scalar is interpreted as a shortcut for a
\ref include_with_subtree_node whose `file` value is set to that scalar and `subtree`
to an empty string.

### Example:
\snippet include.cxx include_scalar

is interpreted as if it was:
\snippet include.cxx include_explicit


## include_or_seq {#include_or_seq_node}

An *include_or_seq* can be **any of**:
* a \ref include_node,
* an \ref include_seq_node.

In that context, a single \ref include_node is interpreted as a shortcut for a
sequence containing a single \ref include_node.

For example, the following:
\snippet include.cxx include_with_subtree

is interpreted as if it was:
\snippet include.cxx include_seq


## include_seq {#include_seq_node}

A *include_seq* is a **sequence** where each element of the sequence is
a \ref include_node.


## include_with_subtree {#include_with_subtree_node}

A *include_with_subtree* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"file"`    | a **scalar** representing the path of the file |
|`"subtree"` | a **scalar** representing the ypath of a subtree in the file |

The path is interpreted relative to the working directory of the execution.
The subtree ypath is expanded according to the ypath specification of paraconf:
* access to a mapping element using the dot syntax: *e.g.* `.map.key`
* access to a sequence element using square brackets (indices are 0-based):
  *e.g.* `.seq[1]`
* access to a mapping element key using braces (indices are 0-based):
  *e.g.* `.map{1}`
* access to a mapping element value by index using chevrons: *e.g.* `.map<1>`

### Example:
\snippet include.cxx include_with_subtree
or
\snippet include.cxx include_subtree_index

It references a subtree in another file.

## int_type {#int_type_node}

A *int_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int"`|
|`"\+.*"` (*optional*)|anything|

A \ref int_type_node represents the C `int` type.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int_type


## int16_type {#int16_type_node}

A *int16_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int16" or "int16_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref int16_type_node represents the C `int16_t` type from
the `<stdtypes.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int16_type


## int32_type {#int32_type_node}

A *int32_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int32"`|
|`"\+.*"` (*optional*)|anything|

A \ref int32_type_node represents the C `int32_t` type from
the `<stdtypes.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int32_type


## int64_type {#int64_type_node}

A *int64_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int64"`|
|`"\+.*"` (*optional*)|anything|

A *int64_type* represents the C `int64_t` type from
the `<stdtypes.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int64_type


## int8_type {#int8_type_node}

A *int8_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int8"`|
|`"\+.*"` (*optional*)|anything|

A \ref int8_type_node represents the C `int8_t` type from the
`<stdtypes.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int8_type


## integer_type {#integer_type_node}

A *integer_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"integer"`|
|`"kind"` (*optional*)|a \ref expression_node "integer-valued $-expression"|
|`"\+.*"` (*optional*)|anything|

A *integer_type* represents the Fortran `integer` datatype.
* The value associated to the `kind` key corresponds to the Fortran *kind*
  parameter (`integer(kind=...)`).
  If missing, the default kind of the Fortran implementation is used.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx integer_type

\snippet Specification_tree_ref/spec_tree_examples.cxx integer_type_2


## intexpr_or_seq {#intexpr_or_seq_node}

A *intexpr_or_seq* can be **any of**:
* a \ref expression_node "integer-valued $-expression",
* a \ref intexpr_seq_node.

In that context, a simple \ref expression_node is interpreted as a shortcut for
a sequence containing a single \ref expression_node.

For example, the following value:
\snippet PDI_doc_expression.cxx expr_seq_scalar

is interpreted as if it was:
\snippet PDI_doc_expression.cxx expr_seq_expanded


## intexpr_seq {#intexpr_seq_node}

A *intexpr_seq* is a **sequence** where each element of the sequence is
a \ref expression_node "integer-valued $-expression".

### Example:

\snippet PDI_doc_expression.cxx intexpr_seq


## int_fast16_type {#int_fast16_type_node}

A *int_fast16_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int_fast16" or "int_fast16_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref int_fast16_type_node represents the C `int_fast16_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int_fast16_type


## int_fast32_type {#int_fast32_type_node}

A *int_fast32_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int_fast32" or "int_fast32_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref int_fast32_type_node represents the C `int_fast32_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int_fast32_type


## int_fast64_type {#int_fast64_type_node}

A *int_fast64_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int_fast64" or "int_fast64_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref int_fast64_type_node represents the C `int_fast64_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int_fast64_type


## int_fast8_type {#int_fast8_type_node}

A *int_fast8_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int_fast8" or "int_fast8_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref int_fast8_type_node represents the C `int_fast8_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int_fast8_type


## int_least16_type {#int_least16_type_node}

A *int_least16_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int_least16" or "int_least16_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref int_least16_type_node represents the C `int_least16_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int_least16_type


## int_least32_type {#int_least32_type_node}

A *int_least32_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int_least32" or "int_least32_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref int_least32_type_node represents the C `int_least32_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int_least32_type


## int_least64_type {#int_least64_type_node}

A *int_least64_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int_least64" or "int_least64_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref int_least64_type_node represents the C `int_least64_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int_least64_type


## int_least8_type {#int_least8_type_node}

A *int_least8_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"int_least8" or "int_least8_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref int_least8_type_node represents the C `int_least8_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx int_least8_type

## intmax_type {#intmax_type_node}

A *intmax_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"intmax" or "intmax_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref intmax_type_node represents the C `intmax_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx intmax_type


## intptr_type {#intptr_type_node}

A *intptr_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"intptr" or "intptr_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref intptr_type_node represents the C `intptr_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx intptr_type


## logging {#logging_node}

A *logging* can be **any of**:
* a \ref logging_map_node,
* a \ref logging_level_node,

A *logging* is fully supported in \ref root_node and any \ref plugin_map_node .


## logging_level {#logging_level_node}

A *logging_level* is a scalar which determines verbosity level. It can be set to 
  (from the most to the least verbose): 
* `"debug"` - shows a log when a normal situation of the execution might be
  useful to understand the behavior of the library,
* `"info"` - shows a log when a normal situation of the execution is likely
  useful to understand the behavior of the library,
* `"warn"` - shows a log when a very likely invalid situation has been detected
  by the library (user input that is technically valid, but very unusual for
  example),
* `"error"` - shows a log when an invalid situation has been detected by the
  library (invalid user input, invalid hardware behaviour, etc.),
* `"off"` - logs are disabled.

Examples:

\snippet Specification_tree_ref/spec_tree_examples.cxx logging_level

* by default `level` is set to `info`


## logging_map {#logging_map_node}

A *logging_map* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"level"`  (*optional*)|a \ref logging_level_node|
|`"pattern"` (*optional*)|a logger prefix pattern of spdlog|
|`"output"` (*optional*)|a \ref logging_output_map_node|

* spdlog pattern is a string that is parsed by spdlog library,
  (see more: https://github.com/gabime/spdlog/wiki/3.-Custom-formatting)
* %PDI introduces new special flag `%{<EXPR>}`, where `<EXPR>` represents a
  \ref expression_node "string-valued $-expression"|a $-expression that will be
  evaluated just after all plugins have been initialized,
* *pattern* by default is set to (where %n is `PDI` or a plugin name):
  ```
  [%T][%n] *** %^%l%$: %v
  ```
  for serial execution and:
  ```
  [%T][%{MPI_COMM_WORLD_rank:06d}][%n] *** %^%l%$: %v
  ```
  when running application with MPI/

Example:

\snippet doc_logging.cxx logging_map


## logging_output_map {#logging_output_map_node}

A *logging_output_map* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"file"`  (*optional*)|a path of the file where to write logs|
|`"console"` (*optional*)|`on` or `off`|

* by default when `file` is defined, `console` is set to `off`.

Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx logging_output_map

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx logging_output_map_2

\snippet Specification_tree_ref/spec_tree_examples.cxx logging_output_map_3

See \ref struct_type_node for more examples.


## logical_type {#logical_type_node}

A *logical_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"logical"`|
|`"kind"` (*optional*)|a \ref expression_node "integer-valued $-expression"|
|`"\+.*"` (*optional*)|anything|

A *logical_type* represents the Fortran `logical` datatype.
* The value associated to the `kind` key corresponds to the Fortran *kind*
  parameter (`logical(kind=...)`).
  If missing, the default kind of the Fortran implementation is used.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx logical_type

\snippet Specification_tree_ref/spec_tree_examples.cxx logical_type_2


## long_type {#long_type_node}

A *long_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"long"`|
|`"\+.*"` (*optional*)|anything|

A \ref long_type_node represents the C `long` type.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

**Example:**

\snippet Specification_tree_ref/spec_tree_examples.cxx long_type


## long_long_type {#long_long_type_node}

A *long_long_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"long long"`|
|`"\+.*"` (*optional*)|anything|

A \ref long_long_type_node represents the C `long long` type.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

**Example:**

\snippet Specification_tree_ref/spec_tree_examples.cxx long_long_type


## plugin_map {#plugin_map_node}

A *plugin_map* is a **mapping** that contains the following keys:
|key|value|
|:--|:----|
|`".*"` (*optional*)| *anything* |

* each key identifies the name of a plugin to load associated to its
  configuration; the content of the configuration depends on the plugin.

Have a look at the \ref Plugins "plugins" documentation to see the specification
tree they accept.

See \ref root_node for an example.


## plugin_path {#plugin_path_map_node}

A path to directory where %PDI should search for plugins. It can be single path:

\snippet Specification_tree_ref/spec_tree_examples.cxx plugin_path

or array of paths (%PDI will take first match):

\snippet Specification_tree_ref/spec_tree_examples.cxx plugin_path_2


## pointer_type {#pointer_type_node}

A *pointer_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"pointer"`|
|`"subtype"`|a \ref datatype_node|
|`"\+.*"` (*optional*)|anything|

A \ref pointer_type_node represents a memory address where data is actually
stored (a pointer) where:
* the value associated to the `subtype` key represents the type of the
  referenced elements,
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx pointer_type

\snippet Specification_tree_ref/spec_tree_examples.cxx pointer_type_2


## ptrdiff_t_type {#ptrdiff_t_type_node}

A *ptrdiff_t_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"ptrdiff_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref ptrdiff_t_type_node represents the C `ptrdiff_t` type from the
`<stddef.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

**Example:**

\snippet Specification_tree_ref/spec_tree_examples.cxx ptrdiff_t_type

## real_type {#real_type_node}

A *real_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"real"`|
|`"kind"` (*optional*)|a \ref expression_node "integer-valued $-expression"|
|`"\+.*"` (*optional*)|anything|

A *real_type* represents the Fortran `real` datatype.
* The value associated to the `kind` key corresponds to the Fortran *kind*
  parameter (`real(kind=...)`).
  If missing, the default kind of the Fortran implementation is used.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx real_type

\snippet Specification_tree_ref/spec_tree_examples.cxx real_type_2


## record_members_map {#record_members_map_node}

A *record_members_map* is a **mapping** that contains the following keys:
|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref datatype_with_disp_node|

* each key identifies the name of a member of the record and the value
  associated to it describes the member itself.

See \ref record_type_node for an example.


## record_type {#record_type_node}

A *record_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"record"`|
|`"buffersize"`|a \ref expression_node "integer-valued $-expression"|
|`"members"` (*optional*)|a \ref record_members_map_node|
|`"\+.*"` (*optional*)|anything|

A \ref record_type_node represents a "record" where:
* the value associated to the `buffersize` key represents the overall size of
  the record, including potential padding,
* the value associated to the `members` key lists all members of the record with
  their offset,
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Examples:

\snippet Specification_tree_ref/spec_tree_examples.cxx record_type

\snippet Specification_tree_ref/spec_tree_examples.cxx record_type_2

\snippet Specification_tree_ref/spec_tree_examples.cxx record_type_3


## short_type {#short_type_node}

A *short_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"short"`|
|`"\+.*"` (*optional*)|anything|

A \ref short_type_node represents the C `short` type.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx short_type


## simple_datatype {#simple_datatype_node}

A *simple_datatype* is a **scalar**.

It is interpreted as a shortcut for a mapping with a
single key `type` whose value is the provided scalar and therefore another
\ref datatype_node.

For example, the following value:
\snippet PDI_doc_datatype.cxx type_shortcut

is interpreted as if it was:
\snippet PDI_doc_datatype.cxx type_expanded


## size_t_type {#size_t_type_node}

A *size_t_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"size_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref size_t_type_node represents the C `size_t` type from the `<stddef.h>`
header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

**Example:**

\snippet Specification_tree_ref/spec_tree_examples.cxx size_t_type


## struct_members_omap {#struct_members_omap_node}

A *struct_members_omap* is an **ordered mapping** that contains the following
keys:
|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref datatype_node|

* each key identifies the name of a member and the associated value specifies
  its type.

See \ref struct_type_node for an example.


## struct_type {#struct_type_node}

A *struct_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"struct"`|
|`"members"` (*optional*)|a \ref struct_members_omap_node|
|`"\+.*"` (*optional*)|anything|

A \ref struct_type_node represents a C "struct" or C++ "class" using the default
C memory layout, where:
* the value associated to the `members` key lists all members of the `struct` in
  order,
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:
\snippet Specification_tree_ref/spec_tree_examples.cxx struct_type

matches:
\snippet PDI_doc_datatype.cxx struct_c


## tuple_element {#tuple_element_node}

A *tuple_elements_seq* is a **sequence** where each element of the sequence is a
\ref datatype_with_disp_node where `disp` key is optional.

See \ref tuple_type_node for an example.


## tuple_elements_seq {#tuple_elements_seq_node}

A *tuple_elements_seq* is a **sequence** where each element of the sequence is 
either a \ref datatype_node or a \ref datatype_with_disp_node.

If \ref datatype_node "datatypes" with no explicit offset are used, the minimum
offset that does not lead to overlaps is used.

See \ref tuple_type_node for an example.


## tuple_type {#tuple_type_node}

A *tuple_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"tuple"`|
|`"buffersize"` (*optional*)|a \ref expression_node "integer-valued $-expression"|
|`"elements"` (*optional*)|a \ref tuple_elements_seq_node|
|`"\+.*"` (*optional*)|anything|

A \ref tuple_type_node represents a "tuple", where:
* the value associated to the `buffersize` key represents the overall size of
  the tuple including potential padding, if omitted, the minimum size that fits
  all elements is used,
* the value associated to the `elements` key lists all elements of the tuple,
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx tuple_type

\snippet Specification_tree_ref/spec_tree_examples.cxx tuple_type_2


## types_map {#types_map_node}

A *types_map* is a **mapping** that contains the following keys:
|key|value|
|:--|:----|
|`".*"` (*optional*)|a \ref datatype_node|

* each key identifies the name of new user-defined datatype and the value
  associated to it describes the type


## uint_type {#uint_type_node}
## uint16_type {#uint16_type_node}

A *uint16_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint16" or "uint16_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint16_type_node represents the C `uint16_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint16_type


## uint32_type {#uint32_type_node}

A *uint32_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint32" or "uint32_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint32_type_node represents the C `uint32_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint32_type


## uint64_type {#uint64_type_node}

A *uint64_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint64" or "uint64_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint64_type_node represents the C `uint64_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint64_type


## uint8_type {#uint8_type_node}

A *uint8_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint8" or "uint8_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint8_type_node represents the C `uint8_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint8_type


## uint_fast16_type {#uint_fast16_type_node}

A *uint_fast16_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint_fast16" or "uint_fast16_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint_fast16_type_node represents the C `uint_fast16_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint_fast16_type


## uint_fast32_type {#uint_fast32_type_node}

A *uint_fast32_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint_fast32" or "uint_fast32_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint_fast32_type_node represents the C `uint_fast32_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint_fast32_type


## uint_fast64_type {#uint_fast64_type_node}

A *uint_fast64_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint_fast64" or "uint_fast64_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint_fast64_type_node represents the C `uint_fast64_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint_fast64_type


## uint_fast8_type {#uint_fast8_type_node}

A *uint_fast8_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint_fast8" or "uint_fast8_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint_fast8_type_node represents the C `uint_fast8_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint_fast8_type


## uint_least16_type {#uint_least16_type_node}

A *uint_least16_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint_least16" or "uint_least16_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint_least16_type_node represents the C `uint_least16_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint_least16_type


## uint_least32_type {#uint_least32_type_node}

A *uint_least32_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint_least32" or "uint_least32_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint_least32_type_node represents the C `uint_least32_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint_least32_type


## uint_least64_type {#uint_least64_type_node}

A *uint_least64_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint_least64" or "uint_least64_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint_least64_type_node represents the C `uint_least64_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint_least64_type


## uint_least8_type {#uint_least8_type_node}

A *uint_least8_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uint_least8" or "uint_least8_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uint_least8_type_node represents the C `uint_least8_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uint_least8_type


## uintmax_type {#uintmax_type_node}

A *uintmax_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uintmax" or "uintmax_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uintmax_type_node represents the C `uintmax_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uintmax_type


## uintptr_type {#uintptr_type_node}

A *uintptr_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"uintptr" or "uintptr_t"`|
|`"\+.*"` (*optional*)|anything|

A \ref uintptr_type_node represents the C `uintptr_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx uintptr_type


## unsigned_long_type {#unsigned_long_type_node}

A *unsigned_long_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"unsigned long long"`|
|`"\+.*"` (*optional*)|anything|

A \ref unsigned_long_type_node represents the C `unsigned long` type.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx unsigned_long_type


## unsigned_long_long_type {#unsigned_long_long_type_node}

A *unsigned_long_long_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"unsigned long long"`|
|`"\+.*"` (*optional*)|anything|

A \ref unsigned_long_long_type_node represents the C `unsigned long long` type.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx unsigned_long_long_type


## unsigned_short_type {#unsigned_short_type_node}

A *unsigned_short_type* is a **mapping** that contains the following keys:

|key|value|
|:--|:----|
|`"type"`|`"unsigned short"`|
|`"\+.*"` (*optional*)|anything|

A \ref unsigned_short_type_node represents the C `unsigned short_t` type from the
`<stdint.h>` header.
It accepts no parameter.
* keys that start with `+` represent attributes, the associated value can be
  anything (**scalar**, **sequence** or **mapping**).

### Example:

\snippet Specification_tree_ref/spec_tree_examples.cxx unsigned_short_type

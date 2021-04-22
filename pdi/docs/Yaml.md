\page YAML YAML

# YAML Format

[YAML](https://en.wikipedia.org/wiki/YAML) is the format used to write %PDI
specification tree.
A YAML file is a text format file that represents a tree.

For example, the following file:
```yaml
tree_1:
  array_1:
    - scalar_1
    - scalar_2
  array_2: [1, 2, 3]
tree_2: {subtree_1: scalar_1, subtree_2: scalar_2}
```

Represents the following tree:
![Graphical representation of the YAML Tree](yaml_example.jpg)

This tree contains the following kinds of nodes:
* scalars,
* sequences,
* mappings.

## Scalar element

A **scalar** is a leaf in the tree, represented as a string.
Some forms of scalars can be interpreted as boolean, integer or floating-point
valued.
Simple examples of scalars include (see the
[YAML specification](https://yaml.org/spec/1.2/spec.html#id2760844) for the
complete syntax):
* `"hello"`,
* `world`,
* `3`,
* `5.7`.

Scalars are represented in pink in the graphical representation.

## Sequence sub-tree

A **sequence** is an ordered list of sub-nodes (that can themselves be scalars,
sequences, mappings, ...).
Two variants of the syntax are available for sequences (see the 
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

Sequences are represented in yellow in the graphical representation.

## Mapping sub-tree

A **mapping** is an unordered list of key-value pairs (whose elements can
themselves be scalars, sequences, mappings, ...).
There can be no duplicates in the keys and the order in which the pairs are
specified has no impact.
Two variants of the syntax are available for mapping (see the 
[YAML specification](https://yaml.org/spec/1.2/spec.html#id2759963) for the
complete syntax).
* in-line mapping: `{1: one, 2: "two", "three": 3}`
* multi-line mapping:
```
1: one
2: two
"three": 3
```

Mapping are represented in blue in the graphical representation.

## YAML advanced elements

In addition to the previous basic building blocks, YAML supports additional
advanced elements.
These advanced elements build on the basic one to provide additional meaning.

### Ordered mapping sub-tree

An **ordered mapping** is represented as a sequence of mappings containing a
single key-value pair each.
There can be no duplicates in the keys.
The sub-nodes can be scalars, sequences or mappings.
Unlike in a normal mapping, the order of elements in an ordered mapping is
meaningful.
Two variants of the syntax are available for ordered mapping (see the 
[YAML specification](https://yaml.org/spec/1.2/spec.html#id2759963) for the
complete syntax).
* in-line ordered mapping: `[{1: one}, {2: "two"}, {"three": 3}]`
* multi-line ordered mapping:
```
- 1: one
- 2: two
- "three": 3
```

# YAML Parsing with Paraconf

The PDI_init function gets as parameter a tree with `logging`, `data`,
`metadata` and `plugins` maps defined in its root. User can define its own
values in yaml and pass to %PDI only the subtree:

```yaml
duration: 0.75
size: [64, 64]
parallelism: { height: 4, width: 4 }

# only the following config will be passed to PDI
pdi_subtree:
  metadata:
    iteration: int
  data:
    main_field: double
  plugins:
    decl_hdf5:
       ...
```

C source code:
```C
PC_tree_t root = PC_parse_path("example.yaml");
PDI_init(PC_get(root, "pdi_subtree"));
```

Fortran source code:
```Fortran
type(PC_tree_t),target :: root

call PC_parse_path("example.yaml", root)
call PDI_init(PC_get(root, "pdi_subtree"))
```

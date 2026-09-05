# YAML {#YAML}

## YAML Format

[YAML](https://en.wikipedia.org/wiki/YAML) is the format used to write %PDI
specification tree.
A YAML file is a text format file that represents a tree.

For example, the following file:
\snippet Yaml/yaml_examples.cxx yaml_tree

Represents the following tree:
![Graphical representation of the YAML Tree](yaml_example.jpg)

This tree contains the following kinds of nodes:
* scalars,
* sequences,
* mappings.

### Scalar element

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

### Sequence sub-tree

A **sequence** is an ordered list of sub-nodes (that can themselves be scalars,
sequences, mappings, ...).
Two variants of the syntax are available for sequences (see the 
[YAML specification](https://yaml.org/spec/1.2/spec.html#id2759963) for the
complete syntax).
* in-line sequence: `[1, 2, 3, hello, "world"]`
* multi-line sequence:
\snippet Yaml/yaml_examples.cxx sequence

Sequences are represented in yellow in the graphical representation.

### Mapping sub-tree

A **mapping** is an unordered list of key-value pairs (whose elements can
themselves be scalars, sequences, mappings, ...).
There can be no duplicates in the keys and the order in which the pairs are
specified has no impact.
Two variants of the syntax are available for mapping (see the 
[YAML specification](https://yaml.org/spec/1.2/spec.html#id2759963) for the
complete syntax).
* in-line mapping: `{1: one, 2: "two", "three": 3}`
* multi-line mapping:
\snippet Yaml/yaml_examples.cxx mapping

Mapping are represented in blue in the graphical representation.

### YAML advanced elements

In addition to the previous basic building blocks, YAML supports additional
advanced elements.
These advanced elements build on the basic one to provide additional meaning.

#### Ordered mapping sub-tree

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
\snippet Yaml/yaml_examples.cxx ordered_mapping

## YAML Parsing with Paraconf

The PDI_init function gets as parameter a tree with `include`, `logging`,
`types`, `metadata`, `data`, `plugin_path`, and `plugins` keys defined in its
root.

\snippet Yaml/yaml_examples.cxx whole_file

C source code:
\snippet Yaml/yaml_examples.cxx init_whole_file

Fortran source code:
\snippet Yaml/yaml_examples.f90 init_whole_file

If one wants to store additional information unrelated to %PDI in the same
file, it is possible to pass only the subtree to %PDI:
\snippet Yaml/yaml_examples.cxx subtree

C source code:
\snippet Yaml/yaml_examples.cxx init_subtree

Fortran source code:
\snippet Yaml/yaml_examples.f90 init_subtree

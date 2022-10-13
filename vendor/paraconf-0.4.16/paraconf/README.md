# Paraconf: simple query language for Yaml.

Paraconf is a library that provides a simple query language to access a Yaml
tree on top of [libyaml](https://pyyaml.org/wiki/LibYAML).
Libyaml parses the Yaml tree; paraconf provides a high level API to access the
different nodes of the tree.

It ease the access to data that are exposed in a user-friendly way inside Yaml
document.
The API is simple enough that one can easily use it.
A working and commented example is provided inside the example folder.


## read and parse a Yaml file.

```
PC_tree_t a_parsed_config = PC_parse_path(char *path);
```

Creates a tree that contains all the parsed data of the file located at 'path'.
```
           .                // origin of the tree
    /      |       \
 .node0   .node1   ...      // sub-trees/leaf
 /  |  \   
...       

```

## access a specific node

### Using its name

```
PC_get(PC_tree_t some_tree, char *name_of_a_node);
```

If found, `PC_get` return the sub-tree under the node ".name" (the dot is
mandatory).

### Using its position on a tree

The nodes of a parent tree are accessed using the `{number}` or `<number>`
syntax:

```
PC_get(PC_tree_t some_tree, ".parent_name{%d}", number);
```

For instance, to access the first element at on the above illustration (the
`.node0`), the syntax is `PC_get( a_parsed_config, ".{0}");`

## Counting elements

The number of elements are given by the PC_len function (for list and map)

```
PC_len( PC_tree_t node, int *nb_of_element);
```

## Access a scalar

To recover a value depending on its type use: 
```
PC_<type>(PC_tree_t some_node, <type> *value);̀
```

Where `type` can be either: int, double, string, bool

## More

One can access each element of a list using the ̀`.list_name[<number>]` syntax
and the PC_get function.

For more details have a look at the example.

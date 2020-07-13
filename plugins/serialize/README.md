\page serialize_plugin Serialize plugin

The Serialized plugin allows to serialize shared data. The plugin will convert all
arrays from sparse to dense and dereference all the pointers.

\warning For now the plugin will only `expose` serialized data. This unables to use the plugin
with `PDI_multiexpose`. This will be implemented in future updates.

\section serialize_configuration Configuration grammar

The serialize configuration is made of only:

|key                    |value                |
|:----------------------|:--------------------|
|data name to serialize |serialized data name |

Here is an example:

```yaml
data:
  sparse_array:
    type: array
    subtype: int
    size: 8
    start: 2
    subsize: 4
plugins:
  serialize:
    sparse_array: dense_array
```
On each `sparse_array` data share, the plugin will expose serialized data under the `dense_array` name.
The `dense_array` will be of type:
```yaml
type: array
subtype: int
size: 4
```

Another example:
```yaml
data:
  pointer_to_sparse_array:
    type: pointer
    subtype:
      type: array
      subtype: int
      size: 8
      start: 2
      subsize: 4
plugins:
  serialize:
    pointer_to_sparse_array: dense_array
```
On each `pointer_to_sparse_array` data share, the plugin will expose serialized data under the `dense_array` name.
The `dense_array` again will be of type:
```yaml
type: array
subtype: int
size: 4
```

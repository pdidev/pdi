# Serialize plugin {#serialize_plugin}

The Serialize plugin serializes shared data: it converts all sparse arrays to dense
ones, and dereferences all pointers.

The serialized data is shared as long as the user data is shared.

1. When the data is shared with `PDI_OUT`, the plugin serializes it and shares the serialized data on `PDI_share`.
2. When the data is shared with `PDI_IN`, the plugin deserializes it on `PDI_reclaim`
(so that another plugin has had the time to write the serialized data, on an event for example).
3. When the data is shared with `PDI_INOUT`, the plugin does both: step 1 on `PDI_share`, and step 2 on `PDI_reclaim`.

## Configuration grammar {#serialize_configuration}

The serialize configuration is made of only:

|key                    |value                |
|:----------------------|:--------------------|
|`logging`              |\ref logging_node|
|data name to serialize |serialized data name |

## Plugin examples {#serialize_plugin_examples}

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
    logging: debug
    sparse_array: dense_array
```
Each time `sparse_array` is shared, the plugin shares its serialized version under the name `dense_array`, of type:
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
Each time `pointer_to_sparse_array` is shared, the plugin shares its serialized version under the name `dense_array`, again of type:
```yaml
type: array
subtype: int
size: 4
```

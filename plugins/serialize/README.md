# Serialize plugin {#serialize_plugin}

Serialize plugin allows to serialize shared data. The plugin will convert all
arrays from sparse to dense and dereference all pointers.

The serialized data will be shared as long the user data is shared.

1. In case of the share with `PDI_OUT`: plugin will serialize and share serialized data on `PDI_share` of given descriptor.
2. In case of the share with `PDI_IN`: plugin will deserialize data on `PDI_reclaim` of given descriptor
(to be sure that the serialized data have been writen to buffer by other plugin (e.g. done on event)).
3. In case of the share with `PDI_INOUT`: plugin will do step 1. on `PDI_share` and step 2. on `PDI_reclaim`.

## Configuration grammar {#serialize_configuration}

The serialize configuration is made of only:

|key                    |value                |
|:----------------------|:--------------------|
|`logging`              |\ref logging_node|
|data name to serialize |serialized data name |

## Plugin examples {#serialize_plugin_examples}

\snippet serialize/docs/serialize_examples.cxx sparse_array

On each `sparse_array` data share, the plugin will share serialized data under the `dense_array` name.
The `dense_array` will be of type:
\snippet serialize/docs/serialize_examples.cxx dense_type


Another example:
\snippet serialize/docs/serialize_examples.cxx pointer_to_sparse

On each `pointer_to_sparse_array` data share, the plugin will share serialized data under the `dense_array` name.
The `dense_array` again will be of type:
\snippet serialize/docs/serialize_examples.cxx dense_type_2


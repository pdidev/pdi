# The Decl'NetCDF plugin {#Decl_NetCDF_plugin}

The Decl'NetCDF plugin was created to read from and write to NetCDF files in a declarative way.

Decl'NetCDF plugin allows you to:
1. Read/write data as NetCDF variables from/to NetCDF file.
2. Define groups in the NetCDF file. Groups can be nested.
3. Read/write variables and groups attributes. **Automatically read and written when openning the file.**
4. Execute input/output operations on event.
5. Execute input/output operations on data share (when the event is not defined).
6. Define `UNLIMITED` dimension and dimensions names.
7. Read/write only part of NetCDF variable by hyperslab definition.
8. Read/write in parallel mode using MPI.

## Configuration elements {#decl_netcdf_configuration}

The root of Decl'NetCDF plugin configuration (named `decl_netcdf`), is a dictionary or a list of dictionaries that contains the following subtrees:
|key            |value                      |            |
|:--------------|:--------------------------|:-----------|
|`logging`      |\ref logging_node          |*optional*  |
|`file`         |\ref decl_netcdf_file      |*mandatory* |
|`communicator` |\ref decl_netcdf_comm      |*optional*  |
|`on_event`     |\ref decl_netcdf_on_event  |*optional*  |
|`groups`       |\ref decl_netcdf_groups    |*optional*  |
|`variables`    |\ref decl_netcdf_variables |*optional*  |
|`write`        |\ref decl_netcdf_write     |*optional*  |
|`read`         |\ref decl_netcdf_read      |*optional*  |

Configuration examples:
```yaml
plugins:
  decl_netcdf:
    file: ""file_name.nc""
    write: ...
```

```yaml
plugins:
  decl_netcdf:
    - file: ""file_name.nc""
      write: ...
    - file: "file_name_2.nc"
      write: ...
    - file: ""file_name.nc""
      read: ...
```

### file subtree {#decl_netcdf_file}

Defines name (path) of the input/output file.

|key   |value                                                |
|:-----|:----------------------------------------------------|
|`file`|string containing filename (can have `$-expressions`)|

Configuration example:
```yaml
plugins:
  decl_netcdf:
    file: "file_name_${i}.nc"
```

### communicator subtree {#decl_netcdf_comm}

\warning Feature not tested yet

Enables parallel NetCDF input/output. Defines communicator to use on read/write (all processes must have the same communicator and filename).

|key           |value                            |
|:-------------|:--------------------------------|
|`communicator`|`$-expression` to valid MPI_comm |

Configuration example:
```yaml
plugins:
  decl_netcdf:
    file: ""file_name.nc""
    communicator: $MPI_COMM_WORLD
```

### on_event subtree {#decl_netcdf_on_event}

Defines on which events the plugin reads/writes data from/to NetCDF file. The value can be either single event
name or the array of events names (both examples presented below). 

|key       |value                                              |
|:---------|:--------------------------------------------------|
|`on_event`|string or array of string that cointain event names|

Configuration examples:
```yaml
plugins:
  decl_netcdf:
    file: "file_name.nc"
    on_event: "event"
```

```yaml
plugins:
  decl_netcdf:
    file: "file_name.nc"
    on_event: ["event_1", "event_2"]
```

### when subtree {#decl_netcdf_when}

Defines the condition on which plugin will execute input/output operation on the file.

|key   |value                                               |
|:-----|:---------------------------------------------------|
|`when`|`$-expression` condition evalueated to boolean value|

Configuration example:
```yaml
plugins:
  decl_netcdf:
    file: "file_name.nc"
    when: "$i < 10"
```

### groups subtree {#decl_netcdf_groups}

Defines groups in the NetCDF file. Mainly used to read/write attributes values of the groups.

If group won't have any attributes, this subtree can be omitted. Decl'NetCDF plugin will create group by default for every variable.

|key     |value                               |
|:-------|:-----------------------------------|
|`groups`|Map of \ref decl_netcdf_groups_name |

Configuration example:
```yaml
plugins:
  decl_netcdf:
    file: "file_name.nc"
    groups:
      group1:
        attributes:
          attr1: 
            value: $value1
          attr2: 
            value: $value2
      group1/group2:
        attributes:
          attr1: 
            value: $value3
          attr2: 
            value: $value4
```

#### group name subtree {#decl_netcdf_groups_name}

|key                      |value                                                                      |
|:------------------------|:--------------------------------------------------------------------------|
|name (path) of the group |Map with `attribute` key with value as map of \ref decl_netcdf_attr |

### variables subtree {#decl_netcdf_variables}

Defines variables in the NetCDF file. Mainly used to define dimensions names and read/write attributes of the variable.

1. If no dimensions names are defined, the plugin will give them arbitrary names.

2. If the data type of variable differs from the one in descriptor this subtree is mandatory. It also allows to define `UNLIMITED` dimension (size of dimension must be then set to `0`).

3. If the variable is in group it has to be define in the variable name (`/` group notation: `group_name/varaibale_name`)

4. This subtree can be omitted. In this case, the name of NetCDF variable will be the same as the name of the descriptor and have the same type and no attributes.

|key           |value                            |
|:-------------|:--------------------------------|
|variable path |\ref decl_netcdf_variables_value |

Configuration example:
```yaml
plugins:
  decl_netcdf:
    file: "file_name.nc"
    variables:
      group1/group2/variable_name:
        type: array
        subtype: double
        size: [0, $value, $value] # 0 -> UNLIMITED dimension
        dimensions: ["time", "height", "width"]
        attributes:
          attr1: $value
```


#### variable definition subtree {#decl_netcdf_variables_value}

|key           |value                                                         |            |
|:-------------|:-------------------------------------------------------------|------------|
|type          |type of variable (defined the same way as other types in %PDI)|*optional*  |
|dimensions    |array of dimensions names of variable                         |*optional*  |
|attributes    |Map of \ref decl_netcdf_attr                                  |*optional*  |


#### attribute subtree {#decl_netcdf_attr}

|key           |value                                           |
|:-------------|:-----------------------------------------------|
|attribute name|`$-expression` value to be written as attribute |


### write subtree {#decl_netcdf_write}

The `write` subtree can have 2 definitions:

1. Just a name of descriptor to write or the list of descriptors names to write to file. In this case the name of NetCDF variable will be the same as the name of the descriptor and have the same type and no attributes.

  Configuration examples:
  ```yaml
  plugins:
    decl_netcdf:
      file: "file_name.nc"
      write: data_name
  #-------------------------------------
  plugins:
    decl_netcdf:
      file: "file_name.nc"
      write: [data_name_1, data_name_2]
  ```

2. Dictionary, with key as descriptor name and value as following subtree:

  |key                  |value                                                                |            |
  |:--------------------|:--------------------------------------------------------------------|------------|
  |`when`               |\ref decl_netcdf_io_when                                             |*optional*  |
  |`variable`           |variable name to write (may be defined in \ref decl_netcdf_variables)|*optional*  |
  |`variable_selection` |\ref decl_netcdf_var_selection                                       |*optional*  |

  Configuration example:
  ```yaml
  data:
    int_submatrix_11:
      type: array
      subtype: int
      size: [4, 4]

  plugins:
    decl_netcdf:
      file: "file_name.nc"
      variables:
          integer_matrix:
            type: array
            subtype: int
            size: [8, 8]
            dimensions: ["height", "width"]
      write:
        int_submatrix_11:
          when: $i < 10
          variable: integer_matrix
          variable_selection:  # select bottom-left submatrix 4x4
            start: [4, 4]
            subsize: [4, 4]
  ```

\warning To write a record datatype, `decl_netcdf.type` type attribute must be defined with compound type name.

  Record write configuration example:
  ```yaml
  data:
    particle:
      type: struct
      +decl_netcdf.type: particle_xyz
      members:
        - x: double
        - y: double
        - z: double

  plugins:
    decl_netcdf:
      file: "file_name.nc"
      write: [particle]
  ```

#### subtree {#decl_netcdf_var_selection}

Defines the part of file NetCDF variable where from read/to write the data. The hyperslab will be created from given `start` and `subsize` (`count`) lists.

|key                  |value                                          |            |
|:--------------------|:----------------------------------------------|------------|
|`start`              |list specifying start index for each dimension |*optional*  |
|`subsize`            |list specifying count for each dimension       |*optional*  |

#### read/write when subtree {#decl_netcdf_io_when}

Defines the condition on which plugin will read/write specific variable.

|key   |value                                               |
|:-----|:---------------------------------------------------|
|`when`|`$-expression` condition evalueated to boolean value|


### read subtree {#decl_netcdf_read}

The `read` subtree can have 2 definitions:

1. Just a name of descriptor to read or the list of descriptors names to read to file. In this case the name of NetCDF variable must be the same as the name of the descriptor and have the same type and no attributes will be read.

  Configuration examples:
  ```yaml
  plugins:
    decl_netcdf:
      file: "file_name.nc"
      read: data_name
  #-------------------------------------
  plugins:
    decl_netcdf:
      file: "file_name.nc"
      read: [data_name_1, data_name_2]
  ```

2. Dictionary, with key as descriptor name and value as following subtree:

  |key                  |value                                                                |            |
  |:--------------------|:--------------------------------------------------------------------|------------|
  |`when`               |\ref decl_netcdf_io_when                                             |*optional*  |
  |`size_of`            |Name of the variable for which we retrive its size                   |*optional*  |
  |`variable`           |variable name to read (may be defined in \ref decl_netcdf_variables) |*optional*  |
  |`variable_selection` |\ref decl_netcdf_var_selection                                       |*optional*  |

  Configuration example:
  ```yaml
  data:
    int_submatrix_11:
      type: array
      subtype: int
      size: [4, 4]
    matrix_size: {type: array, subtype: int, size: 2}

  plugins:
    decl_netcdf:
      file: "file_name.nc"
      variables:
          integer_matrix:
            type: array
            subtype: int
            size: [8, 8]
      read:
        int_submatrix_11:
          when: $i < 10
          variable: integer_matrix
          variable_selection:  # select bottom-left submatrix 4x4
            start: [4, 4]
            subsize: [4, 4]
        matrix_size:
          size_of: integer_matrix  # matrix_size = [8,8]
  ```

### Full yaml example {#decl_netcdf_full_config}

```yaml
metadata:
  var_attr: float
  group1_attr: float
  group1_data_attr: float
data:
  int_submatrix_top:
    type: array
    subtype: int
    size: [4, 8]
  int_submatrix_bottom:
    type: array
    subtype: int
    size: [4, 8]
plugins:
  decl_netcdf:
    - file: "example.nc"
      on_event: "write"
      groups:
        group_1:
          attributes:
            some_attr: $group1_attr
        group_1/data:
          attributes:
            some_attr: $group1_data_attr
      variables:
        group_1/data/int_matrix:
          type: array
          subtype: int
          size: [8, 8]
          dimensions: ["height", "width"]
          attributes:
            custom_attr: $var_attr
      write: 
        int_submatrix_top:
          variable: group_1/data/int_matrix
          variable_selection:
            start: [0, 0]
            subsize: [4, 8]
        int_submatrix_bottom:
          variable: group_1/data/int_matrix
          variable_selection:
            start: [4, 0]
            subsize: [4, 8]
    - file: "example.nc"
      on_event: "read"
      groups:
        group_1/data:
          attributes:
            some_attr: $custom_attr
      variables:
        group_1/data/int_matrix:
          type: array
          subtype: int
          size: [8, 8]
          dimensions: ["height", "width"]
          attributes:
            custom_attr: $var_attr
      read: 
        int_submatrix_top:
          variable: group_1/data/int_matrix
          variable_selection:
            start: [0, 0]
            subsize: [4, 8]
        int_submatrix_bottom:
          variable: group_1/data/int_matrix
          variable_selection:
            start: [4, 0]
            subsize: [4, 8]
```

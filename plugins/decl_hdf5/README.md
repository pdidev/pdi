# The Decl'HDF5 plugin {#Decl_HDF5_plugin}

**WARNING** This documentation is a work in progress and does not reflect the
full Decl'HDF5 potential.

The Decl'HDF5 plugin enables one to read and write data from HDF5 files in a
declarative way.
Decl'HDF5 does not support the full HDF5 feature set but offers a simple
declarative interface to access a large subset of it.

## Configuration grammar

At its root, the Decl'HDF5 configuration is made of either a single
`FILE_DESC` or a list of `FILE_DESC`s.

### FILE_DESC

A `FILE_DESC` specifies a list of actions to execute in one file.
It is specified by a key/value map that contains at least the `file`
key.
Other keys are optional.
The possible values for the keys are as follow:
* `logging`: \ref logging_node
* `file`: a string that can contain $-expressions and specified the
  name of the file this `FILE_DESC` refers to.
* `write`: a `DATA_SECTION` that defaults to an empty one.
  This `DATA_SECTION` describes writes to execute.
* `read`: a `DATA_SECTION` that defaults to an empty one.
  This `DATA_SECTION` describes reads to execute.
* `on_event`: a string identifying an event when the whole file is
  accessed.
  If not specified, each data is written when it is exposed and the file
  is opened and closed every time.
* `when`: a $-expression specifying a default condition to test before 
  executing the reads and writes of this `FILE_DESC`.
  This can be replaced by a more specific condition inside the
  `DATA_SECTION`.
* `communicator`: a $-expression referencing a MPI communicator to use
  for HDF5 parallel synchronized reads and writes.
  It defaults to MPI_COMM_SELF which stands for sequential writes.
  In case of data-triggered (vs. event-triggered) reads and writes, this
  can be replaced inside the `DATA_SECTION`.
* `datasets`: a key-value map associating a PDI type to string keys.
  Each string is the name of a dataset to create in the file on first
  access, with the type described in the value.
* `collision_policy`: a string identifying a \ref COLLISION_POLICY
* `deflate`: an integer value (from 0 to 9) defining the default deflate (GNU
  gzip) compression level to use for datasets created in this file.
  This can be overriden on a per dataset basis.
  By default, the deflate filter is deactivated.
  See https://support.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetDeflate
  for more information.
* `fletcher`: an integer value interpreted as a boolean (0 is false, non 0
  values are true) that defines whether to activate the Fletcher32 checksum
  filter by default for datasets created in this file.
  This can be overriden on a per dataset basis.
  By default, the Fletcher32 checksum filter is deactivated.
  See
  https://support.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFletcher32
  for more information.

### DATA_SECTION

The `DATA_SECTION` describes a set of I/O (read or write) to execute.
A data section can take multiple forms:
* a list of strings, each being either:
  - the name of a PDI data to write in a dataset with the same name ( e.g.
    `data`, where `data` is both a PDI data and the HDF5 dataset name)
  - the name of a PDI data to write in an attribute of the same name preceded by
    a `#` and the name of the dataset or datagroup to which the attribute
    applies ( e.g. `dset#attr`, where `attr` is both a PDI data and the HDF5
    attribute name applied to the `dset` dataset)
* a key-value map where each key is the name of a PDI data to write and
  the value is either a single `DATA_IO_DESC` or a list of
  `DATA_IO_DESC`s describing the I/O (read or write) to execute.

The first case behaves as if each data had its `DATA_IO_DESC` specified
with all default values.

### DATA_IO_DESC

A `DATA_IO_DESC` is a key-velue map describing one  I/O (read or write)
to execute.
All keys are optional and have default values.
The possible values for the keys are as follow:
* `dataset`: a $-expression identifying the name of the dataset to
  access in the file.
  If not specified this defaults to the name of the data.
  On writing, if the dataset does not exist in the file and is not
  specified in the `FILE_DESC` then a dataset with the same size as the
  memory selection is automatically created.
* `attribute`: a $-expression identifying the path of the attribute to access in
  the file. The path is specified as a `/`-separated path to the object on which
  the attribute is set (a dataset or a datagroup) followed by a `#` sign and the
  name of the attribute itself (e.g. `group/dset#attr`). If the `attribute` key
  is specified, then only the `when` key can also be specified, no other key can
  be present.
* `size_of`: a $-expression identifying the path of the dataset that size should
   be written to data descriptor.
* `when`: a $-expression specifying a condition to test before executing
  the I/O operation (read or write).
  This defaults to the value specified in the `FILE_DESC` if present
  or to unconditional I/O otherwise.
* `communicator`: a $-expression referencing a MPI communicator to use
  for HDF5 parallel synchronized I/O operation (read or write).
  Specifying communicator at this level is incompatible with
  event-triggered (vs. data-triggered)
  This defaults to the value specified in the `FILE_DESC` if present
  or to sequential (MPI_COMM_SELF) I/O otherwise.
* `memory_selection`: a `SELECTION_DESC` specifying the selection of
  data in memory to read or write.
  It defaults to selecting the whole data.
* `dataset_selection`: a `SELECTION_DESC` specifying the selection of
  data in the file data to write or read.
  This is only valid if the 
* `attributes`: a key-value map specifying the set of attributes to read from
  (respectively, write to) the file when the associated dataset is read
  (respectively, written).
  Each key is the name of an attribute of the dataset.
  Each value is a $-expression (evaluated when the dataset is accessed)
  specifying the value to the attribute.
* `collision_policy`: a string identifying a \ref COLLISION_POLICY.
* `chunking`: a \ref intexpr_or_seq_node that defines the size of the chunks
  used to the dataset in a chunked layout.
  The \ref intexpr_or_seq_node must have the same dimension as the dataset type.
  This can be overriden by the `decl_hdf5.chunking` attribute in the dataset
  type.
  By default, no chunking is activated.
  See https://support.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetChunk
  for more information.
* `deflate`: an integer value (from 0 to 9) defining the deflate (GNU gzip)
  compression level to use or `-1` for no filter for datasets created by this
  I/O.
  This can be overriden by the `decl_hdf5.deflate` attribute in the dataset
  type.
  By default, the deflate filter is deactivated.
  See https://support.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetDeflate
  for more information.
* `fletcher`: an integer value interpreted as a boolean (0 is false, non 0
  values are true) that defines whether to activate the Fletcher32 checksum
  filter by default for datasets created in this file.
  This can be overriden by the `decl_hdf5.fletcher` attribute in the dataset
  type.
  By default, the Fletcher32 checksum filter is deactivated.
  See
  https://support.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetFletcher32
  for more information.

### SELECTION_DESC

A `SELECTION_DESC` is a key-value map that describes the selection of a
subset of data from a larger set.
All keys are optional and have default values.
The possible values for the keys are as follow:
* `size` is either a single $-expression or a list of $-expressions.
  It describes the size of the selection in each dimension.
  
* `start` is either a single $-expression or a list of $-expressions.
  It describes the number of point to skip at the beginning in each dimension.
  
Memory selection default values:
* If the `size` is not specified, it defaults to size of the whole data in each
  dimension.
* If the `start` is not specified it defaults to 0 in all dimensions.

Dataset selection default values:
* If the `size` is not specified:
  * if the number of dimensions match that of the memory, the size defaults to
    that of the memory,
  * otherwise, the size default to the whole dataset.
* If the `start` is not specified it defaults to 0 in all dimensions.

### COLLISION_POLICY {#COLLISION_POLICY}

A `COLLISION_POLICY` is a string that identifies what to do when writing to a
file or dataset that already exists.
Available policies are listed below:
* `skip` - do not do anything
* `skip_and_warn` - do not do anything, only generate a warning message
* `error` - do not do anything, only throw an error
* `write_into` - [**default**] write into the existing file/dataset (potentially
  overwriting existing data in it)
* `write_into_and_warn` - write into the existing file/dataset (potentially
  overwriting existing data in it) and generate a warning message
* `replace` - delete the existing file/dataset and create a new one
* `replace_and_warn` - delete the existing file/dataset, create a new one, but
  generate a warning message

## full configuration example

```yaml
metadata: # small values for which PDI keeps a copy
  width:   int                    # per proc. width including ghost
  height:  int                    # per proc. height including ghost
  pwidth:  int                    # nb proc. in the x dim
  pheight: int                    # nb proc. in the y dim
  iter:    int                    # curent iteration id
  coord:   { type: array, subtype: int, size: 2 } # coordinate of the process as [x, y]
data:     # values that are not copied by PDI
  main_field:
    type: array
    subtype: double
    size: [$width, $height]
plugins:
  mpi: # loading MPI_Comm predefines (e.g. $MPI_COMM_WORLD)
  decl_hdf5: # a list of file to write to (can be a single element)
    file: data${coord[0]}x${coord[1]}.h5 # the file in which to write the data (required)
    collision_policy: write_into_and_warn # print a warning if file or any of dataset already exist
    on_event: newiter                    # the event that triggers these actions (default: trigger on data expose)
    when: "$iter>0 & $iter<11"           # a condition when to actually trigger the actions (default: always true)
    communicator: $MPI_COMM_SELF         # the MPI communicator used for HDF5 parallel synchronized write (default: $MPI_COMM_SELF, sequential write)
    datasets:                            # a list of datasets inside the file created on first access
      data/array: # a dataset name, datasets referenced but not defined are created just big enough to fit the data
        type: array
        subtype: double                # type of the data in the dataset
        size: [10, $width-2, $width-2] # size of the dataset
    write: # a list or map of data to write (default: empty)
      main_field: # name of the data, it contains either a list or a single write to execute
        - dataset: data/array      # a dataset name (default: the data name)
          when: "$iter>0&$iter<11" # an additional condition when to actually trigger the actions (default: always true)
          communicator: $MPI_COMM_SELF   # the MPI communicator used for HDF5 parallel synchronized write (default: that of the file)
          memory_selection:
            size:  [$width-2, $height-2] # number of elements to transfer in each dimension (default: size of the full data)
            start: [1, 1]                # coordinate of the start point in memory relative to the shared data (default: 0 in each dimensions)
          dataset_selection:
            size:  [1, $width-2, $width-2] # number of elements to transfer in each dimension, must amount to the same number as the memory selection (default: size of memory slab)
            start: [$iter, 0, 0]           # coordinate of the start point in the file relative to the dataset (default: 0 in each dimensions)
          attributes:
            size: ($width-2)*($width-2)
            width: $width-2
            height: $width-2
    read: # a list or map of data to read, similar to write (default: empty)
      - another_value
```

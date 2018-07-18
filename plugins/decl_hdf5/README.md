# The Decl'HDF5 plugin

**WARNING** This documentation is a work in progress and does not reflect the
full Decl'HDF5 potential.

The Decl'HDF5 plugin enables one to read and write data from HDF5 files in a
declarative way.
Decl'HDF5 does not support the full HDF5 feature set but offers a simple
declarative interface to access it.

## Configuration

A simple configuration would look like that:

```yaml
data:
  my_value: double
  another_value: int
plugins:
  decl_hdf5:
    file: a_file.h5 # the file in which to write the data
    write: # a list of dataset to write
      - my_value # a data reference
    read:
      - another_value # a data reference
```

This configuration would write the data `my_value` and read `another_value` from
the file `a_file.h5` as soon as they are exposed.

### conditional writing

In order not to read or write data every time it is exposed, one can use the
`when` keyword to introduce conditionality in the execution of the operation.

```yaml
metadata:
  iteration_id: int
data:
  my_value: double
  another_value: int
plugins:
  decl_hdf5:
    file: a_file.h5
    when: $iteration_id = 0
    write: [ my_value ]
    read:
      another_value: # a data reference
        when: $iteration_id = 1
```

The `when` keyword can be specified at multiple levels of the configuration file
and the last one seen takes precedence for each I/O operation.


### full config

```
metadata: # small values for which PDI keeps a copy
  width:   int                    # per proc. width including ghost
  height:  int                    # per proc. height including ghost
  pwidth:  int                    # nb proc. in the x dim
  pheight: int                    # nb proc. in the y dim
  iter:    int                    # curent iteration id
  coord:   { size: 2, type: int } # coordinate of the process as [x, y]
data:     # values that are not copied by PDI
  main_field:
    type:  double
    sizes: [$width, $height]
plugins:
  decl_hdf5: # a list of file to write to (can be a single element)
    file: data${coord[0]}x${coord[1]}.h5 # the file in which to write the data (required)
    on_event: newiter                    # the event that triggers these actions (default: trigger on data expose)
    when: "$iter>0 & $iter<11"           # a condition when to actually trigger the actions (default: always true)
    communicator: $MPI_COMM_SELF                   # the MPI communicator used for HDF5 parallel synchronized write (default: self, sequential write)
    datasets:                            # a list of datasets inside the file created on first access
      data/array: # a dataset name, datasets referenced but not defined are created just big enough to fit the data
        type: double                    # type of the data in the dataset
        sizes: [10, $width-2, $width-2] # size of the dataset
    write:                               # a list or map of data to write (default: empty)
      main_field: # name of the data, it contains either a list or a single write to execute
        - dataset: data/array      # a dataset name (default: the data name)
          when: "$iter>0&$iter<11" # a condition when to actually trigger the actions (default: that of the file)
          communicator: $MPI_COMM_SELF       # the MPI communicator used for HDF5 parallel synchronized write (default: that of the file)
          memory_selection:
            size:  [$width-2, $height-2] # number of elements to transfer in each dimension (default: size of the full data)
            start: [1, 1]                # coordinate of the start point in memory relative to the shared data (default: 0 in each dimensions)
          dataset_selection:
            size:  [1, $width-2, $width-2] # number of elements to transfer in each dimension, must amount to the same number as the memory selection (default: size of memory slab)
            start: [$iter, 0, 0]           # coordinate of the start point in the file relative to the dataset (default: 0 in each dimensions)
    read: # a list or map of data to read, similar to write (default: empty)
      - another_value
```

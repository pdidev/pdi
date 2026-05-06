# The Damaris plugin {#damaris_plugin}

**WARNING** This documentation is a work in progress and does not reflect the
full potential of this plugin.

**Add description of damaris plugin**
* For detailed information about the Damaris library, please refer to https://project.inria.fr/damaris/
  
        
## Configuration grammar

**WARNING** The following grammar will be changed in the next version of this plugin (see https://github.com/jmorice91/pdi/issues/42). For this reason, we won't go into the details.

* `communicator`: (currently not used) A $-expression referencing an MPI communicator.

* `architecture`: a key-value map.
  * `sim_name`(string): the name of the simulation  
  * `domains`(integer, default: 1) : number of blocks by sub domain
  * `dedicated`: describes what resources will be used by Damaris. Notes: for the momenet, the plugin works with either `only dedicated cores`, or `only dedicated nodes`. In the future, the mix of two modes will be supported.
    * `core`: an integer value (default 0). Number of cores per node that will be used for Damaris
    * `node`: an integer value (default 0). Number of nodes that will be used for Damaris. 

* `client_comm_get`(string) identifier of the communicator which includes all Damaris client processes
* `get_is_client` (string) identifier of data which differentiates the Damaris client processes from Damaris server processes (only needed with explicite use of Damaris client, c.f. section below)

* `datasets`: list of `DATASET_DESC`
  * `dataset`: a key value map that represent a datset that will be shared with damaris. It is composed with
    * `name`(string): name of the dataset
    * `layout`(string): name of the layout of this dataset
    * `storage`(string): name of the storage of this dataset

* `layouts`: list of `LAYOUT_DESC`.
  * `layout:` a key value map that represent a layout defined in a `dataset`. It is composed with
    * `name` (string) name of the layout
    * `type` (string) type of an element of the dataset. e.g. `double`, `int`, `float`, etc
    * `global`: Global size of the layout.
    * `dimensions`: Local (subdomain) size of the layout with ghosts layers.
    * `ghosts`: Number of ghost layers for each dimension. "," separates two dimensions and ":" separates left and right layers in each dimension
    * `depends_on`: List of data used to update the layout attributes. This is necessary because Damaris servers need this data to correctly set the `global` and `dimensions` values, on its side.

* `storages`: list of `STORAGE_DESC`.
  * `storage:` a key value map that represent the storage used to save a`dataset`. It is composed with
    * `name` (string) Name of the output file
    * `type` Type of the output file. Currently only HDF5 is supported.
    * `file_mode` (Collective or FilePerCore)
    * `files_path` Path for the output files

* `write`: list of data that will be write on the disk by damaris. Each data is composed with
  * `dataset`: The dataset in which the data will be written.
  * `position`: The starting position of the data (for each client process) with repect to the dataset.

* `log`: a key value map that specifies the logger information of Damaris. This feature is optional.
  * `file_name`(string) The beginning of the log filename. By default is the value of damaris/architecture/sim_name and the default folder is "where_you_launch_the_script/log". The suffix of the filename is `_P#proc_#iter.log` where #proc represents the MPI rank of the Damaris server process. #iter is the number of iterations.
  * `rotation_size` (integer): 5  
  * `log_level`(string): level of the logger. The value is one of: trace, debug, info, warning, error and fatal
  * `flush`(true or false): Forces the log file to be flushed if set to true .

**Question: what is the other type for a storage ( hdf5, ...)**

* the keyword `when:` can be used in `write` to define the frequency?
* For the layout structure, we can use the same definition as decl_hdf5 plugin to be homogenous  ==> modularity between plugin !!
* In decl_hdf5 plugin in some sense, layout and write can be "fusioné"?

All this points can be adressed in a new version in damaris plugin.

## How to use the Damaris plugin with a simulation?

There are two ways to initialize Damaris client from the simualtion.

### Implicit use of Damaris client
```c++

MPI_Init(...);
PDI_init(...);

PDI_expose("comm", &comm, PDI_INOUT); // <-- allow plugin to set, and return Damaris client communicator
// all simulation codes follow here
// use comm as the default communicator for the rest of the simulation
// only the Damaris client processes are available here
PDI_finalize();
MPI_Finalize(); // Damaris server processes will be finalized by the end of the damaris_plugin
```

### Explicit use of Damaris client
```c++

MPI_Init(...);
PDI_init(...);
int is_client;
PDI_expose("is_client", &is_client, PDI_INOUT); // Only Damaris client will have is_client=1
PDI_expose("comm", &comm, PDI_INOUT); // <-- allow plugin to set, and return Damaris client communicator
if(is_client) {
  // use comm as the default communicator for the rest of the simulation
  // all simulation codes should be inside this block
}
// all MPI processes are available here (both Damaris clients and servers)
PDI_finalize();
MPI_Finalize();
```

With explicit use of Damaris client, one has to add a few lines in the yaml file:
```yaml
metadata:
  is_client: int
damaris:
  get_is_client: is_client # identifier of data which differentiates the Damaris client processes from Damaris server processes
```

* [to be modified] the communicator will be split by Damaris in a communicator for the simulation code (client) 
and for damaris execution (client).
* `get_is_client`(string): name of `is_client` in PDI data store (optional). It is requested only with `is_client` is used.
* `client_comm_get` (string):  name of mpi communicator of simulation code (client) in PDI data store (requested). This communicator is defined by Damaris after the split of the communicator defined in `communicator` between client and server.


## full configuration example

```yaml
damaris:
  architecture: 
    sim_name: example  
    domains: 1
    dedicated:
      core: 1 
  client_comm_get: comm
  datasets:
    - dataset: 
        name: main_field
        layout: main_field_layout
        storage: hdf5_example  
  layouts:
    - layout: 
        name: main_field_layout
        type: double
        global: ['$psize[0]*($dsize[0]-2),$psize[1]*($dsize[1]-2)']
        dimensions: [ '$dsize[0]', '$dsize[1]' ]
        ghosts: '1:1,1:1'
        depends_on: [dsize, psize]
  storages:
    - storage:
        name: hdf5_example
        type: HDF5
        file_mode: Collective
        files_path: ./HDF5_files_damaris/
  write: 
    main_field:
      dataset: main_field 
      when: '$iter<10'
      position: ['($dsize[0]-2)*$pcoord[0]', '($dsize[1]-2)*$pcoord[1]']
  log:
    file_name: example
    rotation_size: 5
    log_level: info
    flush: true
```

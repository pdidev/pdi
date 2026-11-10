# The Damaris plugin {#damaris_plugin}

**WARNING** This documentation is a work in progress and does not reflect the
full potential of this plugin.

**Add description of damaris plugin**
* For detailed information about the Damaris library, please refer to https://project.inria.fr/damaris/
  
        
## Configuration grammar

**WARNING** The following grammar will be changed in the next version of this plugin (see https://github.com/jmorice91/pdi/issues/42). For this reason, we won't go into the details.

* `communicator`: (currently not used) A $-expression referencing an MPI communicator.

* `architecture`: a key-value map.
  * `sim_name`(string): the name of the simulation, also used as the name of output files.
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
    * `script`(string): name of the `pyscript` this dataset is passed to (optional)

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
    * `name` (string) Name of the storage
    * `type` Type of the output file. Currently only HDF5 is supported.
    * `file_mode` (Collective or FilePerCore)
    * `files_path` Path for the output files

* `pyscript`: a key value map that configures a Python script run by the Damaris server cores on the datasets that reference it through their `script` key (optional; requires Damaris built with Python support). It is composed with
  * `name` (string, required): name of the script, referenced by the datasets `script` key
  * `file` (string, required): path to the Python file to run (absolute, or relative to where the simulation is launched)
  * `frequency` (integer, default: 1): run the script every `frequency` iterations
  * `scheduler_file` (string): Dask scheduler file, to use Dask distributed from the script (optional)
  * `nthreads` (integer, default: 1): number of threads per Dask worker
  * `timeout` (integer, default: 2): Dask connection timeout, in seconds
  * `keep_workers` (`yes` or `no`, default: `no`): keep the Dask workers alive at the end of the simulation
  * `language` (default: `python`), `execution` (default: `remote`), `scope` (default: `core`), `external` (default: false), `comment`: passed as is to Damaris

  See `example/python` for a Python and a Dask example.

* `write`: list of data that will be write on the disk by damaris. Each data is composed with
  * `dataset`: The dataset in which the data will be written.
  * `position`: The starting position of the data (for each client process) with repect to the dataset.
  * `block`(integer, default: 0): Which local sub-domain of this client process the data belongs to, when `architecture/domains` is greater than 1 (i.e. a single client manages several sub-domains/patches itself). Must be between `0` and `domains - 1`. Can be left at its default when `domains = 1`.

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

## Tests

The tests are built with `-DBUILD_TESTING=ON` and run with `ctest`.

The `pyscript` tests (`PyScript.*`) are added only when Damaris is built with Python support.
The Dask test (`PyScriptDask.*`) is optional, enable it with `-DDAMARIS_PLUGIN_TEST_DASK=ON`. It
starts its own Dask scheduler, and needs:
* `DAMARIS_DASK_SCHEDULER_EXECUTABLE`: the `dask-scheduler` to use (found in `PATH` by default).
  Its directory is put first in `PATH` during the test, as Damaris starts the Dask workers with `dask worker`.
* `DAMARIS_DASK_PYTHONPATH` (optional): `;`-separated directories added to `PYTHONPATH`, so that the
  Python run by the Damaris servers finds `dask.distributed`, `damaris4py` and `mpi4py`. For example,
  with Dask and damaris4py installed in a virtual environment and mpi4py from the system:
  ```
  -DDAMARIS_DASK_SCHEDULER_EXECUTABLE=$VENV/bin/dask-scheduler
  "-DDAMARIS_DASK_PYTHONPATH=$VENV/lib/python3.12/site-packages;/usr/lib/python3/dist-packages"
  ```

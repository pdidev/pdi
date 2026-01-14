\page deisa_plugin Deisa plugin
# The DEISA plugin

**WARNING** This documentation is a work in progress and does not reflect the full DEISA plugin potential.

The DEISA plugin is used to expose data to [Dask](https://www.dask.org/).
This mechanism decouples analytics code from simulation code by providing an asynchronous pipeline for analytics.

The DEISA plugin uses the following configuration:
- `scheduler_info`: File name defined by the dask scheduler. This file contains information on how to connect to the dask cluster.
- `init_on`: Name of the PDI event called after sharing all metadata.
- `time_step`: Timestep variable.
- `deisa_arrays`: List of Deisa virtual arrays.
  - `type`: Type of the Deisa array. Usually: `array`.
  - `subtype`: Type stored in the `Deisa array.
  - `size`: Size of the Deisa array.
  - `subsize`: How the Deisa array is chunked. In other words, the size of each chunk.
  - `start`: How to find the start of each chunk.
  - `+timedim`: Define where the time dimension is. Currently, only dimension 0 is supported.
- `map_in`: Define how a local data is mapped to a Deisa array.


---


# Example: A 2D MPI space with ghost cells.

@code{.yaml}
metadata:
  nx: int               # Domain size per proc
  ny: int               # Domain size per proc
  mpi_max_coords_x: int # MPI decomposition
  mpi_max_coords_y: int # MPI decomposition
  mpi_coords_x: int     # MPI coordinate of the current process
  mpi_coords_y: int     # MPI coordinate of the current process
  nstep: int            # Time step
  max_iterations: int   # max number of iterations
data:
  main_field: { size: [ '$nx+2', '$ny+2' ], type: array, subtype: double } # Field of the current subdomain. +2 for ghost cells.
plugins:
  mpi:
  deisa:
    scheduler_info: scheduler.json
    init_on: init_PDI
    time_step: $nstep
    deisa_arrays:
      global_t:                       # Name of the Deisa array
        type: array
        subtype: double
        size: [$max_iterations, '($nx+2)*$mpi_max_coords_x', '($ny+2)*$mpi_max_coords_y'] # first dimension corresponds to time steps
        subsize: [1, '$nx+2', '$ny+2']
        start: [$nstep, '$mpi_coords_x*($nx+2)', '$mpi_coords_y*($ny+2)']
        +timedim: 0
    map_in: 
      main_field: global_t            # 'main_field' is mapped to 'global_t'
@endcode

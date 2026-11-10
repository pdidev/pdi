# description of the examples

All the examples solved the heat equation in 2D.

* `example_damaris_only.c`: In this code, we use only damaris library. The input file is `example_damaris_api.xml`.

* `example_damaris_plugin.c`: Here, pdi is used to call the damaris library with the plugin dedicated to this library. Moreover, the finalization of the server process (damaris) is done inside the damaris plugin in calling (PDI_finalize, MPI_finalize). The input file is `example_damaris_plugin.yml`.

* `example_damaris_plugin_with_is_client.c`: In this case, pdi is used to call the damaris library with the plugin dedicated to this library. Moreover, the finalization of the server process (damaris) is done in the simulation code. The input file is `example_damaris_plugin_with_is_client.yml`.

* `python/3dmesh_py.c`: a 3D cube is split between the client processes and, at each iteration, passed to a Python script run by the Damaris server cores through the `pyscript` configuration. Two input files are provided: `3dmesh_py.yml` (Python only, script `3dmesh_py.py`) and `3dmesh_dask.yml` (Python and Dask distributed, script `3dmesh_dask.py`). See `python/README.md`.

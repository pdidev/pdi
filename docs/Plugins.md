# Plugins {#Plugins}

|Plugin                                               |Description                                                        |
|:----------------------------------------------------|:------------------------------------------------------------------|
|\subpage Decl_HDF5_plugin "Decl'HDF5 plugin"         |Read and write data from HDF5 files in a declarative way.          |
|\subpage Decl_NetCDF_plugin "Decl'NetCDF plugin"     |Read and write data from NetCDF files in a declarative way.        |
|Deisa plugin                                         |Expose data to a Dask analysis script.                             |
|\subpage mpi_plugin "MPI plugin"                     |Enables MPI support in %PDI and plugins.                           |
|\subpage pycall_plugin "Pycall plugin"               |Call python scripts from C application                             |
|\subpage serialize_plugin "Serialize plugin"         |Serializes and deserializes shared data.                           |
|\subpage set_value_plugin "Set Value plugin"         |Set values to data and metadata from yaml file.                    |
|\subpage trace_plugin "Trace plugin"                 |Generate a trace of what happens in %PDI data store.               |
|\subpage user_code_plugin "user-code plugin"         |Call your function on event or when data becomes available.        |

To learn how to create your own plugin see: \subpage how_to_create_plugin "How to create a plugin"

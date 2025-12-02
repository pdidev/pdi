PDI Catalyst Plugin
===================

This PDI plugin pushes PDI shared data to the Catalyst 2 API. The goal is to leverage the numerous Catalyst implementations like [Catalyst-ParaView](https://gitlab.kitware.com/paraview/paraview) or [Catalyst-ADIOS2](https://gitlab.kitware.com/paraview/adioscatalyst), helping massive data analysis and visualization at exascale.

# Build Instructions

 - Build and Install [PDI](https://pdi.dev/master/index.html)
 - Build and Install [Catalyst](https://gitlab.kitware.com/paraview/catalyst), with MPI support.
 - Configure with CMake with variables:
   * `PDI_DIR` points to `pdi/install/folder/share/pdi/cmake`
   * `paraconf_DIR` points to `pdi/install/folder/share/paraconf/cmake`
   * `catalyst_DIR` points to `catalyst/install/folder/lib/cmake/catalyst-2.0`
   * optional: `BUILD_TESTING=ON` to build the example test
   * in case of you used vendored version of libraries during your PDI build, instead of system libraries, you may have to define additional PDI dependencies locations. For example `spdlog_DIR` CMake variable for the spdlog library.
 - Build with `make` or `ninja`

# Running the Test

The test executable expects the config yaml file as arguments.

To use the Catalyst-ParaView implementation, you should also set the following environment variables:
 - `CATALYST_IMPLEMENTATION_NAME=paraview`
 - `CATALYST_IMPLEMENTATION_PATHS=path/to/paraview/install/lib/catalyst`

and likely add the catalyst lib folder to `LD_LIBRARY_PATH` if the catalyst library is installed in a non-standard location.

# Design Considerations

*This is a work-in-progress. This paragraph is subject to change.*

PDI describes data through a [Specification Tree](https://pdi.dev/master/Concepts.html#Specification_tree), written in the YAML format and provided to PDI at initialization. Catalyst describes data with [Conduit](https://llnl-conduit.readthedocs.io/en/latest/index.html) nodes and [Mesh Blueprint](https://llnl-conduit.readthedocs.io/en/latest/blueprint_mesh.html#) protocol, provided at execution.

Both protocols are very similars, because they are just hierarchical dictionnary with metadata about the shared memories.
However, Catalyst requires additional semantic about meanings of the data, to map the memory chunk to mesh description (structured mesh, unstructured mesh, image data, AMR, etc.).
The current approach is to add this semantic to the PDI Specification Tree under the `catalyst` key. See the [example file](test/pdi.yml.in) for actual implementation.

PDI is very flexible about the timing of the data sharing using an advanced event mechanism, whereas Catalyst needs all data at the same point in time.
So, the user of this plugin should set an event name referenced by the `on_event` key in the yaml config, in order to trigger the call to `catalyst_execute`. Data should have been shared either before the event or during the event using the `PDI_multi_expose` function.

Internally, `catalyst_initialize` is called by `PDI_Init` and `catalyst_finalize` is called by `PDI_finalize`.


In the sub-tree corresponding to the catalyst plugin, a double quoted value is evaluated as a string.

In the specification tree, the `PDI_data_array` key indicates that the conduit node data should be set as external pointer to a data array from the PDI data store. The value of this key corresponds to the name of the data in PDI data store. There is several keys to describe this array like `size`, `offset`, `stride` to try to match every possible memory layout cases. In this case, these integers values are evaluated as conduit index type.

By default other integer are evaluated as `long`. Excepted if the integer value depend on a data defined in PDI data store as `numXPoints`
in this example:
```yaml
dims: { i: '$numXPoints', j: '60', k: 44 }
```
Be careful, if you compile conduit with 32-bits index (option `CONDUIT_INDEX_32`), you recommand to define a metadata/data for the index and pass the data as `i` in the previous example.

In the case of real value, the value is evaluated as `double`. Excepted if the real value depend on a data defined in PDI data store.


# License

This repository is under the Apache 2.0 license, see NOTICE and LICENSE file.

The test case is a modification of the Catalyst2 CxxFullExample code from the ParaView source code, licenced under BSD-3-Clauses.

Developed by Kitware SAS (Kitware Europe), motivated by the [NumPEx](https://numpex.org/) program.

Reach us at https://www.kitware.com/contact/

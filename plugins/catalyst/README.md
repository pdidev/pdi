PDI Catalyst Plugin
===================

This PDI plugin pushes PDI shared data to the Catalyst 2 API. The goal is to leverage the numerous Catalyst implementations like [Catalyst-ParaView](https://gitlab.kitware.com/paraview/paraview) or [Catalyst-ADIOS2](https://gitlab.kitware.com/paraview/adioscatalyst), helping massive data analysis and visualization at exascale.

# Build Instructions

 - Build and Install [PDI](https://pdi.dev/master/index.html)
 - Build and Install [Catalyst](https://gitlab.kitware.com/paraview/catalyst)
 - Configure with CMake with variables:
   * `PDI_DIR` points to `pdi/install/folder/share/pdi/cmake`
   * `paraconf_DIR` points to `pdi/install/folder/share/paraconf/cmake`
   * `Catalyst_DIR` points to `catalyst/install/folder/lib/cmake/catalyst-2.0`
   * optionnal: `BUILD_TESTING=ON` to build the example test
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

PDI is very flexible about the timing of the data sharing using an advanced event mechanism, whereas Catalyst needs all data at the same point in time. So, the user of this plugin should share all the data at once with the `PDI_multi_expose` function associated to the "catalyst_execute" event.

# License

This repository is under the Apache 2.0 license, see NOTICE and LICENSE file.

The test case is a modification of the Catalyst2 CxxFullExample code from the ParaView source code, licenced under BSD-3-Clauses.

Developed by Kitware SAS (Kitware Europe), motivated by the [NumPEx](https://numpex.org/) program.

Reach us at https://www.kitware.com/contact/

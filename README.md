# PDI: the Portable Data Interface.

PDI is a library that aims to decouple high-performance simulation codes from
Input/Output concerns.
It offers a declarative application programming interface that enables codes to
expose the buffers in which they store data and to notify PDI of significant
steps of the simulation.
It supports a plugin system to make existing libraries such as HDF5, SIONlib or
FTI available to codes, potentially mixed in a single execution.

This approach makes it possible to describe the I/O operations in a dedicated
YAML file instead of interleaving them with the simulation code and thus to
improve their portability and maintainability.
The public plugin API offered by PDI is general and simple enough that one can
easily add support for the best suited library for its use-case (problem size,
IO type, hardware used, etc.)

## Getting the source

You can find a list of release at 
https://gitlab.maisondelasimulation.fr/jbigot/pdi/tags

For example, you can get release 0.3.4 (but look for the latest release at
https://gitlab.maisondelasimulation.fr/jbigot/pdi/tags )

```
wget https://gitlab.maisondelasimulation.fr/jbigot/pdi/-/archive/0.3.4/pdi-0.3.4.tar.bz2
tar -xf pdi-0.3.4.tar.bz2
mv pdi-0.3.4 pdi
```

## Compilation

if the sources are stored in the folder `pdi`:
```
cd pdi
cmake \
        -DUSE_DEFAULT=EMBEDDED \
        -DCMAKE_INSTALL_PREFIX=/usr/ \
        .
make install
```

## Prerequisites

### PDI

PDI depends on:
  * cmake, version >= 3.5
  * a C-99 and C++-14 compiler (gcc-5.4 is tested)
  * a POSIX compatible OS (linux with GNU libc-2.27 is tested)
  * paraconf (distributed with PDI, pass the `-DUSE_DEFAULT=EMBEDDED` or
  `-DUSE_PARACONF=EMBEDDED` option to cmake to use the embedded version)
  * libyaml (distributed with PDI, pass the `-DUSE_DEFAULT=EMBEDDED` or
  `-DUSE_YAML=EMBEDDED` option to cmake to use the embedded version)
  * spdlog (distributed with PDI, pass the `-DUSE_DEFAULT=EMBEDDED` or
  `-DUSE_SPDLOG=EMBEDDED` option to cmake to use the embedded version)

### Fortran support

Fortran support is enabled by default, pass the `-DENABLE_FORTRAN=OFF` option to
cmake to disable it.

PDI Fortran support depends on:
  * a Fortran-2003 compiler (gfort-5.4 is tested)
  * a python and bash interpreter at compilation
  * BPP (distributed with PDI, pass the `-DUSE_DEFAULT=EMBEDDED` or
  `-DUSE_BPP=EMBEDDED` option to cmake to use the embedded version)

### Python support

Python support is not marked stable yet. Pass the `-DENABLE_UNSTABLE=ON` or
`-DENABLE_PYTHON=ON` option to cmake to enable it.

PDI Fortran support depends on:
  * a python3 installation with development headers
  * pybind11 (distributed with PDI, pass the `-DUSE_DEFAULT=EMBEDDED` or
  `-DUSE_PYBIND11=EMBEDDED` option to cmake to use the embedded version)

### pdicfg_validate

PDI configuration validation tool is enabled by default, pass the
`-DBUILD_CFG_VALIDATOR=OFF` option to cmake to disable it.

PDI configuration validation tool depends on:
  * a python3 installation
  * python yaml support (distributed with PDI, pass the `-DUSE_DEFAULT=EMBEDDED`
  or `-DUSE_PYYAML=EMBEDDED` option to cmake to use the embedded version)

### Decl'HDF5 plugin

The decl'HDF5 plugin is enabled by default, pass the
`-DBUILD_DECL_HDF5_PLUGIN=OFF` option to cmake to disable it.

The decl'HDF5 plugin depends on:
  * HDF5 (either sequential or parallel compatible with the chosen MPI)

### Decl'SION plugin

The decl'SION plugin is not marked stable yet. Pass the `-DENABLE_UNSTABLE=ON`
or `-DBUILD_DECL_SION_PLUGIN=ON` option to cmake to enable it.

The decl'SION plugin depends on:
  * SIONlib compatible with the chosen MPI

### FTI plugin

FTI support is currently disabled waiting for the new-style FTI plugin. Pass the
`-DBUILD_FTI_PLUGIN=ON` option to cmake to enable it anyway (at your own risk).

The FTI plugin depends on:
  * FTI (distributed with PDI, pass the `-DUSE_DEFAULT=EMBEDDED` or
  `-DUSE_FTI=EMBEDDED` option to cmake to use the embedded version)

### MPI plugin

The MPI plugin is enabled by default, pass the `-DBUILD_MPI_PLUGIN=OFF` option
to cmake to disable it.

The MPI plugin depends on:
  * a MPI-2 library (openmpi-1.10.2 is tested)

### Tests

Tests are enabled by default, pass the `-DBUILD_TESTING=OFF` option to cmake to
disable them.

Tests depend on:
  * cmake, version >= 3.10
  * gtest and gmock (embedded versions used by default, pass the
  `-DUSE_GTEST=SYSTEM` option to cmake to use the system version)

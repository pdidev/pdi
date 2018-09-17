# PDI: the Portable Data Interface.

PDI is a library that aims to decouple high-performance simulation codes from Input/Output concerns.
It offers a declarative application programming interface that enables codes to expose the buffers in which they store data and to notify PDI of significant steps of the simulation.
It supports a plugin system to make existing libraries such as HDF5, SIONlib or FTI available to codes, potentially mixed in a single execution.

This approach makes it possible to describe the I/O operations in a dedicated YAML file instead of interleaving them with the simulation code and thus to improve their portability and maintainability.
The public plugin API offered by PDI is general and simple enough that one can easily add support for the best suited library for its use-case (problem size, IO type, hardware used, etc.)


## Prerequisites

PDI depends on:
  * cmake, version >= 3.5
  * a C-99 and C++-14 compiler (gcc-5.4 is tested)
  * a POSIX compatible OS (linux with GNU libc-2.27 is tested)
  * a MPI-2 library (openmpi-1.10.2 is tested)
  * paraconf (distributed with PDI, pass the `-DUSE_PARACONF=EMBEDDED` option to cmake to use the embedded version)
  * libyaml (distributed with PDI, pass the `-DUSE_YAML=EMBEDDED` option to cmake to use the embedded version)
  * spdlog (distributed with PDI, pass the `-DUSE_SPDLOG=EMBEDDED` option to cmake to use the embedded version)

PDI Fortran support depends on:
  * a Fortran-2003 compiler (gfort-5.4 is tested)
  * a python and bash interpreter at compilation
  * BPP (distributed with PDI, pass the `-DUSE_BPP=EMBEDDED` option to cmake to use it)

Tests depend on:
  * cmake, version >= 3.10
  * gtest and gmock (distributed with PDI, pass the `-DUSE_GTEST=EMBEDDED` option to cmake to use the embedded version)

Plugins:
  * Decl'HDF5 plugin depends on:
    * HDF5 (either sequential or parallel compatible with the chosen MPI)
  * FTI plugin depends on:
    * FTI (distributed with PDI, pass the `-DUSE_FTI=EMBEDDED` option to cmake to use the embedded version)
  * Decl'SION plugin depends on:
    * SIONlib

## Getting the source

### Latest release

You can find a list of release at https://gitlab.maisondelasimulation.fr/jbigot/pdi/tags

For example, you can get release 0.2.0

```
wget https://gitlab.maisondelasimulation.fr/jbigot/pdi/-/archive/0.2.0/pdi-0.2.0.tar.bz2
tar -xf pdi-0.2.0.tar.bz2
mv pdi-0.2.0 pdi
```

### From GIT

You can fetch the latest source from git .

```
git clone --recursive https://gitlab.maisondelasimulation.fr/jbigot/pdi.git
```

## Compilation

if the sources are stored in the folder `pdi`:
```
cd pdi
cmake \
        -DUSE_BPP=EMBEDDED \
        -DUSE_FTI=EMBEDDED \
        -DUSE_GTEST=EMBEDDED \
        -DUSE_PARACONF=EMBEDDED \
        -DUSE_SPDLOG=EMBEDDED \
        -DUSE_YAML=EMBEDDED \
        -DCMAKE_INSTALL_PREFIX=/usr/ \
        .
make install
```

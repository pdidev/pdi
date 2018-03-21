# PDI: the Portable Data Interface.

PDI is a library that aims to decouple high-performance simulation codes from Input/Output concerns.
It offers a declarative application programming interface that enables codes to expose the buffers in which they store data and to notify PDI of significant steps of the simulation.
It supports a plugin system to make existing libraries such as HDF5, SIONlib or FTI available to codes, potentially mixed in a single execution.

This approach makes it possible to describe the I/O operations in a dedicated YAML file instead of interleaving them with the simulation code and thus to improve their portability and maintainability.
The public plugin API offered by PDI is general and simple enough that one can easily add support for the best suited library for its use-case (problem size, IO type, hardware used, etc.)


## Prerequisites

PDI depends on:
  * cmake, version >= 3.5 (version >= 3.10 if you want to run tests)
  * a C-99 and C++-14 compiler (gcc-5.4 is tested)
  * a POSIX compatible OS (linux is tested)
  * a MPI library
  * paraconf (distributed with PDI, pass the `-USE_SYSTEM_PARACONF=ON` option to cmake to use an external one)
  * libyaml (distributed with paraconf, pass the `-DUSE_SYSTEM_YAML=OFF` option to cmake to use it)

PDI Fortran support depends on:
  * a Fortran-2003 compiler (gfort-5.4 is tested)
  * BPP (distributed with PDI)
  * a python and bash interpreter at compilation

Plugins:
  * the decl'HDF5 plugin require a version of HDF5 compatible with the chosen MPI (either sequential or parallel)
  * the FTI plugin depends on the FTI library that is distributed together with PDI in the `vendor` directory
  * the decl'SION plugin depends on SIONlib

## Getting the source

As of now, the source has to be fetched from git .

```
git clone --recursive 
```

## Compilation

if the sources are in the folder pdi:

```
cd pdi
cmake -DUSE_SYSTEM_YAML=OFF -DCMAKE_INSTALL_PREFIX=/usr/ .. 
make
make doc
make install
```

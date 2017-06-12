# PDI: the Parallel Data Interface.

PDI is a library that aims to decouple high-performance simulation codes from Input/Output concerns.
It offers a declarative application programming interface that enables codes to expose the buffers in which they store data and to notify PDI of significant steps of the simulation.
It supports a plugin system to make existing libraries such as HDF5, SIONlib or FTI available to codes, potentially mixed in a single execution.

This approach makes it possible to describe the I/O operations in a dedicated YAML file instead of interleaving them with the simulation code and thus to improve their portability and maintainability.
The public plugin API offered by PDI is general and simple enough that one can easily add support for the best suited library for its use-case (problem size, IO type, hardware used, etc.)


## Existing plugins
* HDF5 (one-file-per-process)
* FTI
* SIONlib 
* User Code
* ... (see TODO.md for a list of future plug-ins)


## Prerequisites

To build the library one needs:
  * cmake, version >= 3.1
  * a C compiler (gcc and icc are tested).
  * a MPI library

PDI also requires the BPP tool and Paraconf library that are distributed together with PDI in the `vendor` directory.
  * paraconf depends on libyaml. By default paraconf uses the system libyaml but it also embedds a copy that can be used by passing the `-DUSE_SYSTEM_YAML=OFF` option to cmake.

Fortran support:
  * a working Fortran compiler with `iso_c_binding` support is required.

Plugins:
  * the HDF5 plugin require a version of HDF5 compatible with the chosen MPI (and Fortran compiler if enabled)
  * the FTI plugin depends on the FTI library that is distributed together with PDI in the `vendor` directory.

## Getting the source

As of now, the source has to be fetched from git .


```
git clone --recursive 
```

## Compilation

if the sources are in the folder pdi:

```
cd pdi
mkdir build
cd build
cmake -DUSE_SYSTEM_YAML=OFF .. 
make
make install
```

## Documentation
A doxygen documentation is provided in the docs folder along with a makefile.
Invoking make will produce a `Reference_manual.pdf`.


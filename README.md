# The PDI distribution

This is the PDI source distribution, including PDI (the PDI Data Interface) and
its bindings for Fortran and python, the PDI plugins, the PDI configuration
validator, examples and tests, as well as most dependencies of all those.

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

# Documentation

The on-line PDI documentation is available at https://pdi.julien-bigot.fr

# Installation

**The recommended approach to install PDI is to use the stable pre-compiled binary
packages for Debian, Fedora and Ubuntu available at
https://github.com/pdidev/pkgs/tree/repo .**

On other distributions, in case you do not have root acces, or if you want a
more recent version, the **%PDI source distribution** can be easily downloaded
and compiled.

If you have the following dependencies:
* [cmake](https://cmake.org), version >= 3.5,
* a C 99, C++ 14 and Fortran 95 compiler ([gcc](https://gcc.gnu.org/) 5.4 is
  tested),
* a POSIX compatible OS with a [bash](https://www.gnu.org/software/bash/) and
  [python](https://www.python.org/) interpreter
  ([linux](https://www.kernel.org/) with
  [GNU libc](https://www.gnu.org/software/libc/) 2.27 is tested),
* a MPI implementation.

you can install the default PDI source distribution using the following
instructions (replace `-DCMAKE_INSTALL_PREFIX=/usr/` by the actual place where
you want to install):
```
wget https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/archive/1.1.0/pdi-1.1.0.tar.bz2
tar -xjf pdi-1.1.0.tar.bz2
cd pdi-1.1.0
cmake -DCMAKE_INSTALL_PREFIX=/usr/ .
make -j install
```

If this does not work, if you want to select the features you want or which
dependencies to install, please look at the 
[advanced installation instructions](https://pdi.julien-bigot.fr/master/Installation.html).

# Content

The PDI distribution is made of the following submodules:
* `CMakeLists.txt`, `LICENSE`, `cmake/`, `README.md`: distribution specific files,
* `pdi/` : the PDI library,
* `plugins/decl_hdf5/`: the Decl'HDF5 plugin,
* `plugins/decl_netcdf/`: the Decl'NetCDF plugin,
* `plugins/flowvr/`: the FlowVR plugin,
* `plugins/mpi/`: the MPI plugin,
* `plugins/test/`: the Test plugin (deprecated),
* `plugins/user_code/`: the user-code plugin,
* `plugins/decl_sion/`: the Decl'SION plugin,
* `plugins/fti/`: the FTI plugin,
* `plugins/pycall/`: the Pycall plugin,
* `plugins/serialize/`: the serialize plugin,
* `plugins/set_value/`: the Set value plugin,
* `plugins/trace/`: the Trace plugin,
* `tools/pdicfg_validator/`: the PDIcfg-validator tool,
* `example/`: PDI examples,
* `tests/`: tests that combine multiple plugins,
* `tutorial/`: the PDI tutorial,
* `vendor/`: source for PDI dependencies.

# The PDI distribution

This is the PDI distribution, including PDI (the Portable Data Interface), some
plugins and their dependencies.

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

If you have the following dependencies:
  * a POSIX compatible OS with a bash and python interpreter,
  * cmake-3.5+, a C99, C++-14 and Fortran-2003 compiler,
  * HDF5 and MPI,

you can install the default PDI distribution using the following instructions 
(replace `-DCMAKE_INSTALL_PREFIX=/usr/` by the actual place where you want to
install):
```
wget https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/archive/0.6.0/pdi-0.6.0.tar.bz2
tar -xjf pdi-0.6.0.tar.bz2
cd pdi-0.6.0
cmake -DCMAKE_INSTALL_PREFIX=/usr/ .
make install
```

If this does not work, if you want to select the features you want or which
dependencies to install, please look at the 
[advanced installation instructions](https://pdi.julien-bigot.fr/master/Installation.html).

# Content

The PDI distribution is made of the following submodules:
* `CMakeLists.txt`, `LICENSE`, `cmake/`, `README.md`: distribution specific files,
* `pdi/` : the PDI library,
* `plugins/decl_hdf5/`: the Decl'HDF5 plugin,
* `plugins/flowvr/`: the FlowVR plugin,
* `plugins/mpi/`: the MPI plugin,
* `plugins/test/`: the Test plugin (deprecated),
* `plugins/user_code/`: the user-code plugin,
* `plugins/decl_sion/`: the Decl'SION plugin,
* `plugins/fti/`: the FTI plugin,
* `plugins/pycall/`: the Pycall plugin,
* `plugins/trace/`: the Trace plugin,
* `tools/pdicfg_validator/`: the PDIcfg-validator tool,
* `example/`: PDI examples,
* `hands_on/`: the PDI hands-on,
* `vendor/`: source for PDI dependencies.

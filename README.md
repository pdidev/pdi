# The PDI distribution

This is the PDI source distribution, including PDI (the PDI Data Interface) and
its bindings for Fortran and python, the PDI plugins, the PDI configuration
validator, examples and tests, as well as many dependencies of all those.

PDI is a library that aims to decouple high-performance simulation codes from
Input/Output concerns.
It offers a declarative application programming interface that enables codes to
expose the buffers in which they store data and to notify PDI of significant
steps of the simulation.
It supports a plugin system to make existing libraries such as HDF5, NetCDF or
Python available to codes, potentially mixed in a single execution.

This approach makes it possible to describe the I/O operations in a dedicated
YAML file instead of interleaving them with the simulation code and thus to
improve their portability and maintainability.
The public plugin API offered by PDI is general and simple enough that one can
easily add support for the best suited library for its use-case (problem size,
IO type, hardware used, etc.)

## Documentation

The on-line PDI documentation is available at https://pdi.dev

## Installation

**The recommended approach to install PDI is to use the stable pre-compiled
binary packages for Debian, Fedora and Ubuntu available at
https://github.com/pdidev/pkgs/tree/repo .**

On other distributions, in case you do not have root access, or if you want a
more recent version, [**PDI spack recipe**](https://github.com/pdidev/spack) can
be used.

In case you don't want to use either option, you can install PDI source
distribution using the dedicated
[installation instructions](https://pdi.julien-bigot.fr/master/Installation.html).

## Content

PDI distribution is made of the following submodules:
* `CMakeLists.txt`, `LICENSE`, `cmake/`, `README.md`: distribution specific files,
* `pdi/` : the PDI library,
* `plugins/decl_hdf5/`: the Decl'HDF5 plugin,
* `plugins/decl_netcdf/`: the Decl'NetCDF plugin,
* `plugins/deisa/`: the Deisa plugin,
* `plugins/mpi/`: the MPI plugin,
* `plugins/pycall/`: the Pycall plugin,
* `plugins/serialize/`: the serialize plugin,
* `plugins/set_value/`: the Set value plugin,
* `plugins/test/`: the Test plugin (deprecated),
* `plugins/trace/`: the Trace plugin,
* `plugins/user_code/`: the user-code plugin,
* `example/`: PDI examples,
* `tests/`: tests that combine multiple plugins,
* `tutorial/`: the PDI tutorial,
* `vendor/`: source for vendored PDI dependencies.

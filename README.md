# The PDI distribution

This is the PDI source distribution, including PDI (the PDI Data Interface), its
bindings for Fortran and python, the PDI plugins, examples and tests,
documentation, as well as many dependencies of all those.

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

**The recommended approach to install PDI is to use the stable [pre-compiled binary packages for Debian, Fedora and Ubuntu](https://repo.pdi.dev).**

On other distributions, or in case you do not have root access,
[**PDI spack recipe**](https://github.com/pdidev/spack) can be used.

In case you don't want to use either option, you can install PDI source
distribution using the dedicated
[installation instructions](https://pdi.dev/main/Installation.html).

## Contributing

Contributions are welcome!
The [contributing guide](CONTRIBUTING.md) describes how to set up a development
build, run the test suite, find your way around the source tree and submit your
work, as well as the coding conventions the project follows.

Bugs and feature requests are tracked at
https://github.com/pdidev/pdi/issues and development is discussed on the PDI
slack channel at https://pdidev.slack.com

## License

PDI is distributed under the 3-clause BSD license, see the [LICENSE](LICENSE)
file.

## Content

Most of the files in this repository make up the PDI project itself.
However a few sub-directories contain independent projects that can be used on
their own.

* `mock_pdi/`: a header-only no-op implementation of the PDI API that can be
  copied into user codes to build and run without PDI,
* `example/`: the PDI examples, that can be built against an installed PDI,
* `tutorial/`: the PDI tutorial, that can be built against an installed PDI.

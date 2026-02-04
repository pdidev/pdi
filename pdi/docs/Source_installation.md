# Installation of %PDI source distribution {#Source_installation}

\warning
The recommended approach to install %PDI is to use either the pre-compiled
binary packages for Debian, Fedora and Ubuntu or Spack recipe, see 
\ref Installation "the main installation page".

The %PDI source distribution includes:
* the %PDI core and its bindings for Fortran and python,
* many %PDI plugins,
* examples and tests,
* most dependencies of all the above.

## Downloading PDI source distribution {#downloading_distribution}

To download the sources, have a look at the list of all releases at 
https://github.com/pdidev/pdi/releases/

For example, release 1.10.0 can be downloaded from
https://github.com/pdidev/pdi/archive/refs/tags/1.10.0.tar.gz

## Default installation {#default_installation}

\attention
The *default %PDI source distribution* consists in a subset of the %PDI source
distribution considered stable enough for production use.

Installing the default %PDI source distribution is fairly easy.
Most dependencies are embedded in the distribution and the only required
external dependencies are:
* a POSIX compatible OS such as GNU/linux,
* [cmake](https://cmake.org/) version 3.22 or above,
* a C 99, C++ 17 and Fortran 03 compiler such as
  - [gcc](https://gcc.gnu.org/) 11 or above,
  - [clang](https://clang.llvm.org/) 14 or above,
* a [python](https://www.python.org/) interpreter, version 3.10 or above,
* a [bash](https://www.gnu.org/software/bash/) interpreter,
* a [MPI](https://www.mpi-forum.org/) 2 implementation such as
  - [openmpi](https://www.open-mpi.org/) 4.1 or above,
  - [mpich](https://www.mpich.org/) 4.0 or above.

\attention
This list of dependencies can be further reduced or extended by changing the
set of features compiled.

For example, release 1.10.0 can be installed by following these instructions (but
look for the latest release at
https://github.com/pdidev/pdi/releases ):

```bash
wget https://github.com/pdidev/pdi/archive/refs/tags/1.10.0.tar.gz
tar -xjf 1.10.0.tar.bz2
mkdir 1.10.0/build
cd 1.10.0/build
cmake -DCMAKE_INSTALL_PREFIX="${HOME}/.local/" ..   # configuration
make install   # compilation and installation
```

\attention
The `cmake` command must be followed by the path to the `pdi` directory, here
`..` because we have just created and moved to the `build` subdirectory.

\attention
The `-DCMAKE_INSTALL_PREFIX="${HOME}/.local/"` flag is used to specify where to
install the distribution, flags are
\ref cmake_config "discussed in more detail below".

If the default installation fails or if you need an installation better tailored
to your needs, keep reading.

## Configuration {#cmake_config}

The %PDI source distribution is compiled using CMake.
The `cmake` command accepts options to configure the distribution with flags.
The syntax is `-D<FLAG>=<VALUE>` to set `FLAG` value to `VALUE`.

For example, the installation directory can be changed with the following command:
```bash
cmake -DCMAKE_INSTALL_PREFIX=/home/user/ ..
```

The following general flags are useful to configure the distribution as a whole.

|Flag                  |Default     |Description|
|:---------------------|:-----------|:----------|
|`CMAKE_INSTALL_PREFIX`|`/usr/local`|The path where to install the distribution.|
|`CMAKE_PREFIX_PATH`   |            |A semicolon-separated list of prefix where to look for %PDI dependencies in addition to system path.|
|`DIST_PROFILE`        |`User`      |Sets the default values of other flags. The possible values are `User` for the %PDI use profile and `Devel` for the developer profile.|
|`USE_DEFAULT`         |`AUTO`      |Whether to compile the embedded versions of the dependencies. The possible values are `SYSTEM` to use the system versions, `EMBEDDED` to compile the version provided in the distribution and `AUTO` to prefer a system version but fall-back on the embedded version if unavailable.|
|`BUILD_UNSTABLE`      |`OFF`       |Whether to build the unstable parts of the distribution, either `ON` or `OFF`.|
|`CMAKE_BUILD_TYPE`    |`Release`   |Optimization level and debug verbosity. The possible values are `Release` and `Debug`.|
|`PDI_PLUGIN_PATH`     |same as PDI |Path where to install all plugins. If not defined, will install relative to PDI.|

The following flags define which features of the distribution to enable or not.

|Flag                       |Default|Description|
|:--------------------------|:------|:----------|
|`BUILD_BENCHMARKING`       |`ON`   |Build the benchmarks.|
|`BUILD_DECL_HDF5_PLUGIN`   |`ON`   |Build the Decl'HDF5 plug-in.|
|`BUILD_DECL_NETCDF_PLUGIN` |`ON`   |Build the Decl'NetCDF plug-in.|
|`BUILD_FORTRAN`            |`ON`   |Build the Fortran interface.|
|`BUILD_HDF5_PARALLEL`      |`ON`   |Build the parallel version of the Decl'HDF5 plugin instead of the sequential one.|
|`BUILD_JSON_PLUGIN`        |`OFF`  |Build the Json plug-in.|
|`BUILD_MPI_PLUGIN`         |`ON`   |Build the MPI plug-in.|
|`BUILD_NETCDF_PARALLEL`    |`ON`   |Build the parallel version of the Decl'NetCDF plugin instead of the sequential one.|
|`BUILD_TESTING`            |`ON`   |Build the tests.|
|`BUILD_TRACE_PLUGIN`       |`ON`   |Build the Trace plug-in.|
|`BUILD_USER_CODE_PLUGIN`   |`ON`   |Build the User-code plug-in.|
|`BUILD_DOCUMENTATION`      |`OFF`  |Build the documentation website. (devel profile)|
|`BUILD_PYCALL_PLUGIN`      |`OFF`  |Build Pycall plug-in. (unstable)|
|`BUILD_PYTHON`             |`OFF`  |Build the Python interface. (unstable)|


The following flags define whether to:
* use the preinstalled version of a dependency (`SYSTEM`), it will be looked for
  in the system directories and those specified by the `CMAKE_PREFIX_PATH` list,
* compile the version of the dependency provided in the distribution
  (`EMBEDDED`),
* compile another version of the dependency (the path to the source archive),
* use the preinstalled version of a dependency if available and fallback on the
  compilation of the version of the dependency provided in the distribution if
  no preinstalled version is found.

|Flag           |Default   |Description|
|:--------------|:---------|:----------|
|`USE_benchmark`|`EMBEDDED`|the [Benchmark](https://github.com/google/benchmark) library.|
|`USE_Doxygen`  |`AUTO`    |the [doxygen](http://www.doxygen.nl/) tool.|
|`USE_GTest`    |`EMBEDDED`|the [googletest](https://github.com/google/googletest) library.|
|`USE_HDF5`     |`AUTO`    |the [HDF5](https://www.hdfgroup.org/solutions/hdf5/) library.|
|`USE_JSON`     |`AUTO`    |the [Json](https://github.com/nlohmann/json/) library.|
|`USE_paraconf` |`AUTO`    |the [paraconf](https://github.com/pdidev/paraconf) library.|
|`USE_pybind11` |`AUTO`    |the [pybind11](https://pybind11.readthedocs.io/en/stable) library.|
|`USE_spdlog`   |`AUTO`    |the [spdlog](https://github.com/gabime/spdlog) library.|
|`USE_yaml`     |`AUTO`    |the [yaml](https://github.com/jbigot/zpp) library.|
|`USE_Zpp`      |`EMBEDDED`|the [zpp](https://github.com/jbigot/zpp) preprocessor.|


The following flags define where to install %PDI, those prefixed with `CMAKE_`
are provided and documented by the
[GNUInstallDirs](https://cmake.org/cmake/help/v3.22/module/GNUInstallDirs.html)
cmake module.



|Flag          |Default   |Description|
|:-------------|:---------|:----------|
|`INSTALL_CMAKEDIR`|`PDIDATADIR/cmake`|Cmake modules.|
|`INSTALL_FMODDIR`|`LIBDIR/pdi/finclude/${COMPILER_VERSION}`|Fortran modules|
|`INSTALL_PDIDATADIR`|`DATADIR/pdi`|PDI data|
|`INSTALL_PDIPLUGINDIR`|`LIBDIR/pdi/plugins_${PDI_VERSION}`|PDI plugins|
|`CMAKE_INSTALL_BINDIR`|see [GNUInstallDirs](https://cmake.org/cmake/help/v3.22/module/GNUInstallDirs.html)|user executables|
|`CMAKE_INSTALL_DATADIR`|see [GNUInstallDirs](https://cmake.org/cmake/help/v3.22/module/GNUInstallDirs.html)|read-only architecture-independent data|
|`CMAKE_INSTALL_DOCDIR`|see [GNUInstallDirs](https://cmake.org/cmake/help/v3.22/module/GNUInstallDirs.html)|documentation root|
|`CMAKE_INSTALL_INCLUDEDIR`|see [GNUInstallDirs](https://cmake.org/cmake/help/v3.22/module/GNUInstallDirs.html)|C header files|
|`CMAKE_INSTALL_LIBDIR`|see [GNUInstallDirs](https://cmake.org/cmake/help/v3.22/module/GNUInstallDirs.html)|object code libraries|
|`CMAKE_INSTALL_PREFIX`|see [CMake doc](https://cmake.org/cmake/help/v3.22/variable/CMAKE_INSTALL_PREFIX.html)|Installation base|

## List of dependencies {#dependecies}

Here is a list of all dependencies required by one feature or another.
All dependencies are provided in the distribution unless specified otherwise.

Dependencies of **%PDI**:

* a POSIX compatible OS such as GNU/linux,
* **[cmake](https://cmake.org/) version 3.22 or above (not provided)**,
* **a C 99 and C++ 17 compiler (not provided)** such as
  - [gcc](https://gcc.gnu.org/) 11 or above,
  - [clang](https://clang.llvm.org/) 14 or above,
* the [paraconf](https://github.com/pdidev/paraconf) library version 1.0 or above (provided),
* the [libyaml](https://pyyaml.org/wiki/LibYAML) library version 0.2.2 or above (provided),
* the [spdlog](https://github.com/gabime/spdlog) library version 1.9 or above (provided).

Additional dependencies for **the Fortran API**:

* the PDI library,
* **a Fortran 03 compiler (not provided)** such as
  - [gcc](https://gcc.gnu.org/) 11 or above,
* **a [python](https://www.python.org/) interpreter, version 3.10 or above (not provided)**.

Additional dependencies for **the Python support**:

* the PDI library,
* **the [python](https://www.python.org/) development environment version 3.10 or above (not provided)**,
* the [pybind11](https://pybind11.readthedocs.io/en/stable) library version 2.9 or above.

Dependencies of **the Decl'HDF5 plugin**:

* the PDI library,
* the [HDF5](https://www.hdfgroup.org/solutions/hdf5/) library version 1.10 or above (provided),
* **a MPI implementation for the parallel version of the plugin (not provided)**, such as
  - [openmpi](https://www.open-mpi.org/) 4.1 or above,
  - [mpich](https://www.mpich.org/) 4.0 or above.

Dependencies of **the Decl'NetCDF plugin**:

* the PDI library,
* the [NetCDF](https://www.unidata.ucar.edu/software/netcdf/) library version 4.8 or above (provided),
* the [HDF5](https://www.hdfgroup.org/solutions/hdf5/) library version 1.10 or above (provided),
* **a MPI implementation for the parallel version of the plugin (not provided)**, such as
  - [openmpi](https://www.open-mpi.org/) 4.1 or above,
  - [mpich](https://www.mpich.org/) 4.0 or above.

Dependencies of **the JSON plugin**:

* the PDI library,
* the [Json](https://github.com/nlohmann/json/) library version 3.9 or above (provided).
  
Dependencies of **the MPI plugin**:

* the PDI library,
* **a MPI-2 implementation (not provided)** such as
  - [openmpi](https://www.open-mpi.org/) 4.1 or above,
  - [mpich](https://www.mpich.org/) 4.0 or above.

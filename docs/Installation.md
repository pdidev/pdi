\page Installation Installation

PDI is distributed along some plugins and most of its dependencies in what is
known as the **PDI distribution**.
It can be easily compiled from its source code.


# Default PDI installation

Installing the *default PDI distribution* is fairly easy.
Most dependencies are embedded in the distribution and the only required
external dependencies are:
  * a POSIX compatible OS with a python3 interpreter,
  * CMake-3.10 together with a C-99, a C++-14 and a Fortran-2003 compiler,
  * the HDF5 and MPI libraries for the plugins.
  
For example, release 0.5.1 can be installed by following these instructions (but
look for the latest release at
https://gitlab.maisondelasimulation.fr/jbigot/pdi/tags)

```bash
wget https://gitlab.maisondelasimulation.fr/jbigot/pdi/-/archive/0.5.1/pdi-0.5.1.tar.bz2
tar -xjf pdi-0.5.1.tar.bz2
cd pdi-0.5.1
cmake -DUSE_DEFAULT=EMBEDDED -DCMAKE_INSTALL_PREFIX=/usr/ .
make install
```

If the default installation fails or if you need an installation better tailored
to your needs, keep reading.


# Downloading the distribution

As of now, PDI must be compiled from source.

There are two options to download PDI sources:
* download a tarball,
* clone the git repository.


## Downloading a tarball

You can look at the list of all releases from 
https://gitlab.maisondelasimulation.fr/jbigot/pdi/tags
or download `master` from
https://gitlab.maisondelasimulation.fr/jbigot/pdi/-/archive/master/pdi-master.tar.bz2 .

For example, `master` can be downloaded and extracted by following these
instructions:
```bash
https://gitlab.maisondelasimulation.fr/jbigot/pdi/-/archive/master/pdi-master.tar.bz2
tar -xjf pdi-master.tar.bz2
cd pdi-master
```


## Cloning the GIT repository

You can look at the list of all branches from
https://gitlab.maisondelasimulation.fr/jbigot/pdi/branches .

For example, `master` can be cloned from git by following these instructions:
```bash
git clone https://gitlab.maisondelasimulation.fr/jbigot/pdi.git
cd pdi
```


# Configuration and compilation

The PDI distribution is compiled using Cmake.
In addition to PDI itself, the distribution includes:
* the `Decl'HDF5` plugin,
* the `Decl'SION` plugin,
* the `FTI` plugin,
* the `MPI` plugin.

PDI depends on:
* [cmake](https://cmake.org), version >= 3.5
* a C-99 and C++-14 compiler ([gcc](https://gcc.gnu.org/) 5.4 is tested)
* a POSIX compatible OS ([linux](https://www.kernel.org/) with
  [GNU libc](https://www.gnu.org/software/libc/) 2.27 is tested)
* [paraconf](https://gitlab.maisondelasimulation.fr/jbigot/libparaconf)
  (distributed with PDI, see below)
* [libyaml](https://pyyaml.org/wiki/LibYAML) (distributed with PDI, see below)
* [spdlog](https://github.com/gabime/spdlog) (distributed with PDI, see below)

Once inside `pdi` directory, the compilation can be done using the following
commands:
```bash
mkdir build
cd build
cmake ..
make -j
make install
```

The `cmake` command is used for configuration.
It accepts options to enable or disable each element of the distribution and to
configure them with the `-D` syntax.

For example, the installation directory can be changed with the following
command:
```bash
cmake -DCMAKE_INSTALL_PREFIX=/usr/ ..
```

\warning the `cmake` command must be followed by the path to the `pdi`
directory, here `..` because we have just created and moved to the `build`
subdirectory.

The following boolean build options are available, each option should be set to
either `ON` or `OFF`:
<dl>
<dt>`ENABLE_UNSTABLE`</dt>
   <dd>Build all features by default including those not stable yet</dd>
<dt>`BUILD_CFG_VALIDATOR`</dt>
   <dd>Build config validation script</dd>
<dt>`BUILD_DECL_HDF5_PLUGIN`</dt>
   <dd>Build decl'HDF5 plug-in</dd>
<dt>`BUILD_DECL_SION_PLUGIN`</dt>
   <dd>Build decl'SION plug-in</dd>
<dt>`BUILD_DOCUMENTATION`</dt>
   <dd>Build documentation</dd>
<dt>`BUILD_MPI_PLUGIN`</dt>
   <dd>Build MPI plug-in</dd>
<dt>`BUILD_FLOWVR_PLUGIN`</dt>
   <dd>Build FlowVR plug-in</dd>
<dt>`BUILD_TESTING`</dt>
   <dd>Build tests</dd>
<dt>`ENABLE_FORTRAN`</dt>
   <dd>Enable Fortran support</dd>
<dt>`ENABLE_PYTHON`</dt>
   <dd>Enable Python support</dd>
</dl>

The following dependency inclusion options are available, each one can be set to
either `SYSTEM`, `EMBEDDED` or a path:
<dl>
<dt>`USE_DEFAULT`</dt>
   <dd>Default version of libraries to use</dd>
<dt>`USE_BPP`</dt>
   <dd>version of BPP to use</dd>
<dt>`USE_FLOWVR`</dt>
   <dd>version of FlowVR to use</dd>
<dt>`USE_GTEST`</dt>
   <dd>version of Gtest to use</dd>
<dt>`USE_PARACONF`</dt>
   <dd>version of paraconf to use</dd>
<dt>`USE_PYBIND11`</dt>
   <dd>version of pybind11 to use</dd>
<dt>`USE_PYYAML`</dt>
   <dd>version of PyYAML to use</dd>
<dt>`USE_SPDLOG`</dt>
   <dd>version of spdlog to use</dd>
<dt>`USE_YAML`</dt>
   <dd>version of yaml to use</dd>
</dl>


## Fortran support

Fortran support is enabled by default, pass the `-DENABLE_FORTRAN=OFF` option to
cmake to disable it.

PDI Fortran support depends on:
  * a Fortran-2003 compiler ([GNU Fortran](https://gcc.gnu.org/fortran/) 5.4 is
    tested)
  * a [python](https://www.python.org/) and
    [bash](https://www.gnu.org/software/bash/) interpreter at compilation
  * [BPP](https://gitlab.maisondelasimulation.fr/jbigot/bpp) (distributed with
    PDI, pass the `-DUSE_DEFAULT=EMBEDDED` or `-DUSE_BPP=EMBEDDED` option to
    cmake to use the embedded version)


## PDI configuration validation tool

PDI configuration validation tool (pdicfg_validate) is enabled by default, pass
the `-DBUILD_CFG_VALIDATOR=OFF` option to cmake to disable it.

PDI configuration validation tool depends on:
  * a [python](https://www.python.org/)3 interpreter
  * [python YAML](https://pyyaml.org/) support (distributed with PDI, pass the
    `-DUSE_DEFAULT=EMBEDDED` or `-DUSE_PYYAML=EMBEDDED` option to cmake to use
    the embedded version)


## Tests

Tests are enabled by default, pass the `-DBUILD_TESTING=OFF` option to cmake to
disable them.

Tests depend on:
  * [cmake](https://cmake.org), version >= 3.10
  * [Google Test framework](https://github.com/google/googletest) (the embedded
    version is used by default, pass the `-DUSE_GTEST=SYSTEM` option to cmake to
    use the system version)


## Python support

Python support is **not marked stable yet**. Pass the `-DENABLE_UNSTABLE=ON` or
`-DENABLE_PYTHON=ON` option to cmake to enable it.

PDI Fortran support depends on:
  * a [python](https://www.python.org/)3 installation with development headers
  * [pybind11](https://pybind11.readthedocs.io/en/stable/) (distributed with
    PDI, pass the `-DUSE_DEFAULT=EMBEDDED` or `-DUSE_PYBIND11=EMBEDDED` option
    to cmake to use the embedded version)


## Decl'HDF5 plugin

The decl'HDF5 plugin is enabled by default, pass the
`-DBUILD_DECL_HDF5_PLUGIN=OFF` option to cmake to disable it.

The decl'HDF5 plugin depends on:
  * HDF5 (either sequential or parallel compatible with the chosen MPI)


## Decl'SION plugin

The decl'SION plugin is not marked stable yet. Pass the `-DENABLE_UNSTABLE=ON`
or `-DBUILD_DECL_SION_PLUGIN=ON` option to cmake to enable it.

The decl'SION plugin depends on:
  * SIONlib compatible with the chosen MPI


## FTI plugin

FTI support is currently disabled waiting for the new-style FTI plugin. Pass the
`-DBUILD_FTI_PLUGIN=ON` option to cmake to enable it anyway (at your own risk).

The FTI plugin depends on:
  * FTI (distributed with PDI, pass the `-DUSE_DEFAULT=EMBEDDED` or
  `-DUSE_FTI=EMBEDDED` option to cmake to use the embedded version)


## MPI plugin

The MPI plugin is enabled by default, pass the `-DBUILD_MPI_PLUGIN=OFF` option
to cmake to disable it.

The MPI plugin depends on:
  * a MPI-2 library (openmpi-1.10.2 is tested)

\page Installation Installation

%PDI is distributed along some plugins and most of its dependencies in what is
known as the **%PDI distribution**.
It can be easily compiled from its source code.


\section downloading_distribution Downloading PDI distribution

As of now, %PDI must be compiled from source.

To download the sources, have a look at the list of all releases from:  

https://gitlab.maisondelasimulation.fr/pdidev/pdi/tags

For example, release 0.6.1 can be downloaded by following these instructions:
```bash
wget https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/archive/0.6.1/pdi-0.6.1.tar.bz2
tar -xjf pdi-0.6.1.tar.bz2 && rm pdi-0.6.1.tar.bz2
```

\section default_installation Default installation

Installing the *default %PDI distribution* is fairly easy.
Most dependencies are embedded in the distribution and the only required
external dependencies are:
   * [cmake](https://cmake.org), version >= 3.5,
   * a C-99 and C++-14 compiler ([gcc](https://gcc.gnu.org/) 5.4 is tested),
   * a POSIX compatible OS ([linux](https://www.kernel.org/) with
   [GNU libc](https://www.gnu.org/software/libc/) 2.27 is tested).
  
For example, release 0.6.1 can be installed by following these instructions (but
look for the latest release at
https://gitlab.maisondelasimulation.fr/pdidev/pdi/tags):

```bash
mkdir pdi-0.6.1/build && cd pdi-0.6.1/build
cmake ..       # configuration
make install   # compilation and installation
```

\warning The `cmake` command must be followed by the path to the `pdi`
directory, here `..` because we have just created and moved to the `build`
subdirectory.

If the default installation fails or if you need an installation better tailored
to your needs, keep reading.

\section configuration Configuration

The %PDI distribution is compiled using CMake.
In addition to %PDI itself, the distribution includes %PDI plugins.

The `cmake` command is used for configuration.
It accepts options to enable or disable each element of the distribution and to
configure them with the `-D` syntax (set `VALUE` to the `FLAG` by: `-DFLAG=VALUE`).

For example, the installation directory can be changed with the following command:
```bash
cmake -DCMAKE_INSTALL_PREFIX=/home/user/ ..
```

\subsection list_of_compilation_flags PDI compilation flags

|Flag           |Default value |Description                                  |
|:--------------|:-------------|:--------------------------------------------|
|`DIST_PROFILE` |`User`        |Compilation profile (`Devel` for developers).|

\subsubsection user_profile_plugin_flags User profile plugin flags
|Flag                     |Default value |Description              |
|:------------------------|:-------------|:------------------------|
|`BUILD_DECL_HDF5_PLUGIN` |`ON`          |Build decl'HDF5 plug-in. |
|`BUILD_MPI_PLUGIN`       |`ON`          |Build MPI plug-in.       |
|`BUILD_FLOWVR_PLUGIN`    |`OFF`         |Build FlowVR plug-in.    |
|`BUILD_FTI_PLUGIN`       |`OFF`         |Build FTI plug-in.       |
|`BUILD_PYCALL_PLUGIN`    |`OFF`         |Build Pycall plug-in.    |
|`BUILD_DECL_SION_PLUGIN` |`OFF`         |Build decl'SION plug-in. |

\subsubsection user_profile_other_flags User profile other flags
|Flag                   |Default value |Description                                                  |
|:----------------------|:-------------|:------------------------------------------------------------|
|`BUILD_FORTRAN`        |`ON`          |Enable Fortran support.                                      |
|`BUILD_PYTHON`         |`OFF`         |Enable Python support.                                       |
|`BUILD_DOCUMENTATION`  |`OFF`         |Build documentation.                                         |
|`BUILD_TESTING`        |`OFF`         |Build tests.                                                 |
|`BUILD_CFG_VALIDATOR`  |`OFF`         |Build config validation script.                              |
|`BUILD_UNSTABLE`       |`OFF`         |Build all features by default including those not stable yet.|

\subsubsection user_dependency_flags Dependency inclusion options

|Flag                   |Default Value       | Description                                          |
|:----------------------|:-------------------|:-----------------------------------------------------|
|`USE_DEFAULT`          |`AUTO`              |Detect missing dependencies and build them with %PDI. |

|Flag                   |Value               | Description                                  |
|:----------------------|:-------------------|:---------------------------------------------|
|`USE_DEFAULT`          |`SYSTEM`/`EMBEDDED` |Use system libraries or build them with %PDI. |
|`USE_YAML`             |`SYSTEM`/`EMBEDDED` |Use system libyaml or build it with %PDI.     |
|`USE_PARACONF`         |`SYSTEM`/`EMBEDDED` |Use system libparaconf or build it with %PDI. |
|`USE_SPDLOG`           |`SYSTEM`/`EMBEDDED` |Use system spdlog or build it with %PDI.      |
|`USE_HDF5`             |`SYSTEM`/`EMBEDDED` |Use system HDF5 or build it with %PDI.        |
|`USE_FTI`              |`SYSTEM`/`EMBEDDED` |Use system FTI or build it with %PDI.         |
|`USE_FLOWVR`           |`SYSTEM`/`EMBEDDED` |Use system FlowVR or build it with %PDI.      |
|`USE_SIONLIB`          |`SYSTEM`/`EMBEDDED` |Use system SIONlib or build it with %PDI.     |
|`USE_PYBIND11`         |`SYSTEM`/`EMBEDDED` |Use system pybind11 or build it with %PDI.    |


\subsubsection pdi_dependecies PDI dependencies
|Dependency  |Distributed with %PDI |Source                                                           |
|:-----------|:--------------------|:-----------------------------------------------------------------|
|`paraconf`  |`YES`                |[Link](https://gitlab.maisondelasimulation.fr/jbigot/libparaconf) |
|`libyaml`   |`YES`                |[Link](https://pyyaml.org/wiki/LibYAML)                           |
|`spdlog`    |`YES`                |[Link](https://github.com/gabime/spdlog)                          |

\subsubsection fortan_support Fortran support

%PDI Fortran support dependencies:
|Dependency              |Distributed with %PDI |Source                                                    |
|:-----------------------|:---------------------|:---------------------------------------------------------|
|`Fortran-2003 compiler` |`NO`                  |[Link](https://gcc.gnu.org/fortran/)                      |
|`python`                |`NO`                  |[Link](https://www.python.org/)                           |
|`bash`                  |`NO`                  |[Link](https://www.gnu.org/software/bash/)                |
|`BPP`                   |`YES`                 |[Link](https://gitlab.maisondelasimulation.fr/jbigot/bpp) |

\subsubsection python_support Python support

Python support is **marked as unstable**.

PDI Python support dependencies:
|Dependency              |Distributed with %PDI |Source                                                    |
|:-----------------------|:---------------------|:---------------------------------------------------------|
|`python`                |`NO`                  |[Link](https://www.python.org/)                           |
|`pybind11`              |`YES`                 |[Link](https://pybind11.readthedocs.io/en/stable)         |

\subsection decl_hdf5_plugin Decl'HDF5 plugin

Decl'HDF5 plugin dependencies:
|Dependency              |Distributed with %PDI |Source                                                    |
|:-----------------------|:---------------------|:---------------------------------------------------------|
|`HDF5`                  |`YES`                  |[Link](https://www.hdfgroup.org/solutions/hdf5/)         |

To build parallel HDF5 with PDI use flag:
|Flag                  |Value |
|:---------------------|:-----|
|`BUILD_HDF5_PARALLEL` |`YES` |

\subsection mpi_plugin MPI plugin

MPI plugin dependencies:
|Dependency |Distributed with %PDI |Source                                                                |
|:----------|:---------------------|:---------------------------------------------------------------------|
|`MPI`      |`NO`                  |[OpenMPI](https://www.open-mpi.org/), [MPICH](https://www.mpich.org/) |

\subsection flowvr_plugin FlowVR plugin

FlowVR plugin is **marked as unstable**.

FlowVR plugin dependencies:
|Dependency |Distributed with %PDI |Source                                           |
|:----------|:---------------------|:------------------------------------------------|
|`FlowVR`   |`YES`                 |[Link](https://gitlab.inria.fr/flowvr/flowvr-ex) |

\subsection fti_plugin FTI plugin

FTI plugin is **marked as unstable**.

FTI plugin dependencies:
|Dependency              |Distributed with %PDI |Source                                  |
|:-----------------------|:---------------------|:---------------------------------------|
|`FTI`                   |`YES`                  |[Link](https://github.com/leobago/fti) |

\subsection pycall_plugin Pycall plugin

Pycall plugin is **marked as unstable**.

Pycall plugin dependencies:
|Dependency              |Distributed with %PDI |Source                                  |
|:-----------------------|:---------------------|:---------------------------------------|
|`python`                |`NO`                  |[Link](https://www.python.org/)                           |
|`pybind11`              |`YES`                 |[Link](https://pybind11.readthedocs.io/en/stable)         |


\subsection decl_sion_plugin Decl'SION plugin

Decl'SION plugin is **marked as unstable**.

Decl'SION plugin dependencies:
|Dependency              |Distributed with %PDI |Source                                                                                      |
|:-----------------------|:---------------------|:-------------------------------------------------------------------------------------------|
|`SIONlib`               |`YES`                  |[Link](https://www.fz-juelich.de/ias/jsc/EN/Expertise/Support/Software/SIONlib/_node.html) |

\subsection tests_dependencies Tests

Tests dependencies:
|Dependency              |Distributed with %PDI |Source                                       |
|:-----------------------|:---------------------|:--------------------------------------------|
|`Google Test framework` |`YES`                 |[Link](https://github.com/google/googletest) |
|`cmake >= 3.10`         |`NO`                  |[Link](https://www.cmake.org/)               |

\subsection documentation_dependencies Documentation

Documentation dependencies:
|Dependency          |Distributed with %PDI |Source                         |
|:-------------------|:---------------------|:------------------------------|
|`doxygen >= 1.8.15` |`YES`                 |[Link](http://www.doxygen.nl/) |

\subsection pdiconf_validator PDI configuration validation tool

%PDI configuration validation tool dependencies:
|Dependency              |Distributed with %PDI |Source                          |
|:-----------------------|:---------------------|:-------------------------------|
|`python`                |`NO`                  |[Link](https://www.python.org/) |
|`python YAML`           |`NO`                  |[Link](https://pyyaml.org/)     |

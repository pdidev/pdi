# %PDI usage {#PDI_usage}

After \ref Installation step, %PDI must be used in the application, that
is written either in C/C++, Fortran or Pyhton.

## Preparing the environment {#preparing_the_environment}

### PDI was installed in default location {#pdi_in_default_location}

The library and plugins should be found without any problem, but a good approach is to
make sure that all environment variables are set correctly. To do this, there are 3 options:

1. `source pdirun` - sets up current environment variables
2. `pdirun bash` - creates new shell with environment variables set up
3. `pdirun command` - runs a `command` with all environment variables set up

To have the environment always prepared for using %PDI, the first option (`source pdirun`)
can be added to `~/.bash_profile` file. After that, any new shell will have environment
variables set up.

### PDI was installed in custom location {#pdi_not_in_default_location}

To prepare environment in this case there are 3 options:

1. `source PDI_path/share/pdi/env.bash` - sets up current environment variables
2. `PDI_path/bin/pdirun bash` - creates new shell with environment variables set up
3. `PDI_path/bin/pdirun command` - runs a `command` with all environment variables set up

where `PDI_path` is the path where %PDI was installed.

To have the environment always prepared for using %PDI, the first option (`source PDI_path/share/pdi/env.bash`)
can be added to `~/.bash_profile` file. After that, any new shell will have environment
variables set up.

## Compilation of an application {#compilation_of__app}

If source files (of application that uses %PDI) and specification tree file are ready, the compilation step can be made.
For C make sure that source files that use %PDI API are including `pdi.h` header file.
For Fortran make sure that source files that use %PDI API are using `%PDI` module file (`USE %PDI`).

%PDI can be disabled by using the `no-pdi` directory (see section ["How to deactivate PDI"](#deactivate_pdi)).

### Compiling by hand {#compiling_by_hand}

To compile application, linker flag `-lpdi` must be used.
For C it would look like this:

```bash
gcc source_files.c -o exec_file -lpdi
```

For Fortran it would look like this:

```bash
gfortran source_files.f90 -o exec_file -lpdi
```

### Compiling with cmake {#compiling_with_cmake}

If source files (of application that uses %PDI) and specification tree file are ready, the compilation step can be made.

#### C/C++ compilation {#c_cpp_compiling_with_cmake_application}

To compile C/C++ application, cmake must find `C` component from %PDI package.
Then the `PDI::PDI_C` library must be linked to the target:

```cmake
find_package(PDI REQUIRED COMPONENTS C)

add_executable(exec_file source_files.c)
target_link_libraries(exec_file PDI::PDI_C)
```

#### Fortran compilation {#fortan_compiling_by_cmake_application}

To compile Fortran application, cmake must find `f90` component from %PDI package.
Then the `PDI::PDI_f90` library must be linked to the target:

```cmake
find_package(PDI REQUIRED COMPONENTS f90)

add_executable(exec_file source_files.c)
target_link_libraries(exec_file PDI::PDI_f90)
```

## Running an application {#running_app}

%PDI is a shared library. That means that all environment variables
must be set not only on compilation, but also when running the program.

Every used plugin in application needs to be found by %PDI. It will search for
plugins in 4 steps (it will use the first plugin found):

1. `PDI_PLUGIN_PATH` environment variable that is colon separated list with paths,
2. `plugin_path` subtree in specification tree: \ref plugin_path_map_node,
3. Relative path of used %PDI shared object `libpdi.so`,
4. `LD_LIBRARY_PATH` environment variable that is colon separated list.

## How to deactivate PDI {#deactivate_pdi}

### Using the no-pdi directory with an available Paraconf

You may use the no-pdi mock folder instead of the real PDI to compile your 
application. The only modification you need to make is to add the 
CMAKE_PREFIX_PATH option to your CMake command :
```bash
cmake . -your-usual-cmake-compile-options -DCMAKE_PREFIX_PATH="${CMAKE_PREFIX_PATH:+${CMAKE_PREFIX_PATH}:}<path-to>/no-pdi"
```
The no-pdi directory can be copied out of the pdi repository, for convenience.

If Paraconf is missing from your system, you need to remove all calls to it 
from your application.
This contains its inclusion (`#include <paraconf.h>` in C/C++, for example), 
its use by PDI via `PDI_init`, and direct calls to Paraconf API.

%PDI can be re-enabled by reverting those modifications, 
using cmake again without specifying a path to no-pdi.

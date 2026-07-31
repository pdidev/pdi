# %PDI usage {#PDI_usage}

After \ref Installation step, %PDI must be used in the application, that
is written either in C/C++, Fortran or Pyhton.

## Preparing the environment {#preparing_the_environment}

If you installed PDI to the system default location (such as `/usr/` under linux), the library and plugins should be found without any problem.
But you can always follow the directions for \ref pdi_not_in_default_location "installation in a custom location" to ensure everything is found.

### %PDI was installed in custom location {#pdi_not_in_default_location}

To prepare environment in this case there are 3 options:

1. `source <path/to/pdi/root>/share/pdi/env.bash` - sets up current environment variables
2. `<path/to/pdi/root>/bin/pdirun bash` - creates new shell with environment variables set up
3. `<path/to/pdi/root>/bin/pdirun command` - runs a `command` with all environment variables set up

where `<path/to/pdi/root>` is the root of %PDI installation.

To have the environment always setup for %PDI, the first option (`source PDI_path/share/pdi/env.bash`) can be added to `~/.bash_profile` file.
After that, any new shell will have the environment set up.

## Compilation of an application {#compilation_of__app}

For C make sure that source files that use %PDI API include the `pdi.h` header (`#include <pdi.h>`).
For Fortran make sure that source files that use %PDI use `PDI` module (`USE PDI`).

The recommended way to compile your application is based on cmake.

\remark
If you have instrumented your code using %PDI but no %PDI installation is available, you can disable it using the \ref deactivate_pdi "MockPDI approach".

### C/C++ compilation {#c_cpp_compiling_with_cmake_application}

To compile C/C++ applications, cmake must find the `C` component from %PDI package and the `PDI::PDI_C` target must be linked to the application target:
Because in order to initialize %PDI, you also have to use Paraconf, you should do the same for Paraconf component `C` and target `paraconf::paraconf`.
```CMake
find_package(paraconf REQUIRED COMPONENTS C)
find_package(PDI REQUIRED COMPONENTS C)

add_executable(exec_file source_files.c)
target_link_libraries(exec_file PRIVATE PDI::PDI_C paraconf::paraconf)
```

### Fortran compilation {#fortan_compiling_by_cmake_application}

To compile Fortran applications, cmake must find the `C` component from %PDI and the `PDI::PDI_f90` target must be linked to the application target.
Because in order to initialize %PDI, you also have to use Paraconf, you should do the same for Paraconf component `f90` and target `paraconf::paraconf_f90`.
```CMake
find_package(paraconf REQUIRED COMPONENTS f90)
find_package(PDI REQUIRED COMPONENTS f90)

add_executable(exec_file source_files.c)
target_link_libraries(exec_file PRIVATE PDI::PDI_f90 paraconf::paraconf_f90)
```

## Running the application {#running_app}

%PDI is a shared library.
That means that all environment variables must be set not only on compilation, but also when running the program.

Every used plugin in application needs to be found by %PDI.
%PDI will search for plugins in 4 steps (it will use the first plugin found):

1. `PDI_PLUGIN_PATH` environment variable that is colon separated list with paths,
2. `plugin_path` subtree in specification tree: \ref plugin_path_map_node,
3. the directory relative to the installation of %PDI shared library `libpdi.so`,
4. `LD_LIBRARY_PATH` environment variable that is colon separated list.

## Deactivating %PDI with MockPDI {#deactivate_pdi}

You may use the `mock_pdi` directory instead of the real %PDI to compile your application.
This directory contains a [mock](https://en.wikipedia.org/wiki/Mock_object) %PDI implementation that you can use instead of the real %PDI, if you prefer not or can not to install it on your specific system.
The recommandation is to copy the `mock_pdi` dirctory into your own application directory so as to be able to use it when required.

\remark
As of now, only the C component of %PDI is provided by the mock %PDI.
We do not provide any way to mock the Fortran or Python API of %PDI yet.

There are two ways the mock %PDI can be used: "SubdirMock" and "MockFind".
1. The recommended way is to use "SubdirMock", as it is more robust and will also support working without Paraconf if that's an option for you.
However, "SubdirMock" requires you to modify your root `CMakeLists.txt`.
2. The "MockFind" approach is less robust, and only mocks %PDI so other libraries such as Paraconf remain required even if they were used for %PDI only, but it is non-invasive and requires zero modification to your project.

### The recommended way: "SubdirMock" {#SubdirMock}

\remark
This is the recommended way to mock %PDI.
It is robust and also support working without Paraconf if that's an option for you.
However, it requires you to modify your root `CMakeLists.txt`.
I you do not or can not do it, have a look at the \ref MockFind "\"MockFind\" approach"

To use "SubdirMock", your need to introduce a new cmake option in your project.
For example, in the %PDI `example`, the option is called `DISABLE_PDI`.
```CMake
option(DISABLE_PDI "Disable the use of both PDI and Paraconf in the project" OFF)
```

Then, where you used to look for %PDI, you will have to replace the call to `find_package` by a call to `add_subdirectory`.
As parameter, pass the location where you copied the `mock_pdi` directory in your project.

```CMake
if("${DISABLE_PDI}")
	add_subdirectory(<path/to>/mock_pdi mock_pdi)
else()
	find_package(PDI REQUIRED)
endif()
```

With this approach, your code should be able to compile without %PDI.
Please note, that it will still require Paraconf however.

\remark
In your code, you can check and handle the case where %PDI is disabled specifically by checking the `WITHOUT_PDI` macro:
```C
#ifdef WITHOUT_PDI
do_something_specific_for_mock_pdi();
#endif
```

#### Without Paraconf

If you want to go further and make Paraconf optional too:
1. set `PDI_MOCK_PARACONF_TARGET` to true before the `add_subdirectory` call, and
2. put Paraconf `find_package` in the same place as the one for %PDI that is not called in case your disabling option is set.

```CMake
if("${DISABLE_PDI}")
	set(PDI_MOCK_PARACONF_TARGET TRUE)
	add_subdirectory(<path/to>/mock_pdi mock_pdi)
else()
	find_package(paraconf REQUIRED)
	find_package(PDI REQUIRED)
endif()
```
\remark
Again, in your code, you can check the `WITHOUT_PDI` macro.
This time, you can also check and handle the case where Paraconf is disabled by checking the `WITHOUT_PARACONF` macro.

This will take care of your cmake.
But while the `paraconf::paraconf` target will be provided that way, Paraconf itself will not be mocked.
You will therefore have to `#ifdef` out any use you make of Paraconf in your code.

The two uses you most definitely make in your code are
1. the one to include Paraconf, and
2. the one to provide the `PC_tree_t` to %PDI.
These should be `#ifdef`ed out.

```C
#ifndef WITHOUT_PARACONF
#include <paraconf.h>
#endif
```

```C
#ifndef WITHOUT_PARACONF
	PDI_init(PC_parse_path(config_file]));
#endif
```

And if your only use of Paraconf is for %PDI, that's all you have to do.

If you use Paraconf beyond %PDI, you will have to `#ifdef` out those usages too.
Here you are on your own to provide sensible values instead of the one read from YAML by Paraconf.

For example:
```C++
	double duration;
#ifndef WITHOUT_PARACONF
	PC_double(PC_get(conf, ".duration"), &duration);
#else
	// if we don't have paraconf available, we use 10 as duration, because... why not.
	duration = 10;
#endif
```

You can find a full example of this approach in the `example` directory.

### The non-invasive way: "MockFind" {#MockFind}

\remark
This is way to mock %PDI that is non-invasive and require zero modification to either your code or cmake files.
However, it is less robust than \ref SubdirMock "the \"SubdirMock\" approach", and does not support mocking Paraconf.
Hence the recommended approach is \ref SubdirMock \"SubdirMock\".

The only modification you need to make is to add the `PDI_ROOT` option to your CMake command to point to `mock_pdi`:
```bash
cmake --your-usual-cmake-compile-options -DPDI_ROOT="<path-to>/mock_pdi" 
```

\warning
"MockFind" only mocks %PDI and not Paraconf, so you still need a functional Paraconf if only to pass the required parameter to `PDI_init`'

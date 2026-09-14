#=============================================================================
# Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the names of CEA, nor the names of the contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written  permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================



### Project header

cmake_minimum_required(VERSION 3.22...4.2)
project(PDI LANGUAGES NONE)

## Version handling
file(READ VERSION PDI_VERSION)
string(STRIP "${PDI_VERSION}" PDI_VERSION)
string(REGEX MATCH "([0-9A-Za-z]+)\\.([0-9A-Za-z]+)\\.([0-9A-Za-z]+)(-([0-9A-Za-z]+))?" "\\1;\\2;\\3" "${PDI_VERSION}")
set(PDI_VERSION_MAJOR "${CMAKE_MATCH_1}")
set(PDI_VERSION_MINOR "${CMAKE_MATCH_2}")
set(PDI_VERSION_PATCH "${CMAKE_MATCH_3}")
set(PDI_VERSION_MODIF "${CMAKE_MATCH_5}")
if("git" STREQUAL "${PDI_VERSION_MODIF}" OR "alpha" STREQUAL "${PDI_VERSION_MODIF}")
	execute_process(COMMAND "${CMAKE_CURRENT_LIST_DIR}/cmake/version-uid" "${PDI_VERSION_MODIF}" WORKING_DIRECTORY "${CMAKE_CURRENT_LIST_DIR}" OUTPUT_VARIABLE PDI_VERSION_MODIF OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()
set(PDI_VERSION "${PDI_VERSION_MAJOR}.${PDI_VERSION_MINOR}.${PDI_VERSION_PATCH}")
set(PDI_VERSION "${PDI_VERSION}${PDI_VERSION_MODIF}")



## Required versions of the dependencies

# PDIConfig.cmake.in reads these too, so that an installed PDI asks for the same versions it was built against.
# pdi/docs/Source_installation.md is the authoritative list: keep both in sync.
set(PDI_REQUIRED_DOXYGEN_VERSION  1.9)
set(PDI_REQUIRED_HDF5_VERSION     1.10)
set(PDI_REQUIRED_JSON_VERSION     3.9)
set(PDI_REQUIRED_NETCDF_VERSION   4.8)
set(PDI_REQUIRED_PARACONF_VERSION 1.0)
set(PDI_REQUIRED_PYBIND11_VERSION 2.9)
set(PDI_REQUIRED_PYTHON_VERSION   3.10)
set(PDI_REQUIRED_SPDLOG_VERSION   1.9)
set(PDI_REQUIRED_YAML_VERSION     0.2.2)

set(PDI_VENDORED_BENCHMARK_DIR  "vendor/benchmark-4ed29ae")
set(PDI_VENDORED_GOOGLETEST_DIR "vendor/googletest-56efe39")
set(PDI_VENDORED_HDF5_DIR       "vendor/hdf5-1.12.3")
set(PDI_VENDORED_JSON_DIR       "vendor/json-3.11.4")
set(PDI_VENDORED_NETCDF_DIR     "vendor/netcdf-c-4.9.3")
set(PDI_VENDORED_PARACONF_DIR   "vendor/paraconf-1.0.3")
set(PDI_VENDORED_PYBIND11_DIR   "vendor/pybind11-3.0.1")
set(PDI_VENDORED_SPDLOG_DIR     "vendor/spdlog-1.15.3")
set(PDI_VENDORED_YAML_DIR       "vendor/libyaml-0.2.5")
set(PDI_VENDORED_ZPP_DIR        "vendor/zpp-1.1.2")



### Build options

## Global options

set(DIST_PROFILE "User" CACHE STRING "Profile to use for PDI distribution build. Options are: User, Devel")
set_property(CACHE DIST_PROFILE PROPERTY STRINGS User Devel)

if(User STREQUAL "${DIST_PROFILE}")
	option(BUILD_DOCUMENTATION "Build documentation" OFF)
	option(BUILD_TESTING       "Build tests" OFF)
	option(BUILD_UNSTABLE      "Build all features by default including those not stable yet" OFF)
	set(DEFAULT_BUILD_TYPE "Release")
elseif(Devel STREQUAL "${DIST_PROFILE}")
	option(BUILD_DOCUMENTATION "Build documentation" ON)
	option(BUILD_TESTING       "Build tests" ON)
	option(BUILD_UNSTABLE      "Build all features by default including those not stable yet" ON)
	set(DEFAULT_BUILD_TYPE "Debug")
else()
	message(FATAL_ERROR "DIST_PROFILE should be set to one of: User, Devel")
endif()

set(USE_DEFAULT AUTO CACHE STRING "Default version of libraries to use; this can be 1) EMBEDDED to use the provided version, 2) SYSTEM to use an already installed version (you can use CMAKE_PREFIX_PATH to specify where to look, or 3) AUTO to use SYSTEM if available and EMBEDDED otherwise")

# With the superbuild on, this file builds the vendored dependencies and then re-invokes itself as a single ExternalProject with the flag off.
# With it off, this is a normal project, dependencies are expected to be already available and the components that joined it are added with
# add_subdirectory; the superbuild still builds the others as projects of their own.
option(PDI_SUPERBUILD "Build the vendored dependencies, then PDI itself as a sub-project" ON)
if("${PDI_SUPERBUILD}")
	message(STATUS " **Profile**: Superbuild mode is: `ON' (-DPDI_SUPERBUILD=ON)")
endif()

# Modules to build

option(BUILD_BENCHMARKING       "Build PDI benchmarks" ON)
option(BUILD_DECL_HDF5_PLUGIN   "Build Decl'HDF5 plug-in" ON)
option(BUILD_DECL_NETCDF_PLUGIN "Build Decl'NetCDF plug-in" ON)
option(BUILD_FORTRAN            "Build with Fortran support" ON)
option(BUILD_HDF5_PARALLEL      "Build Decl'HDF5 in parallel mode" ON)
option(BUILD_JSON_PLUGIN        "Build JSON plugin" OFF)
option(BUILD_MPI_PLUGIN         "Build MPI plug-in" ON)
option(BUILD_NETCDF_PARALLEL    "Build Decl'NetCDF in parallel mode" ON)
option(BUILD_PYCALL_PLUGIN      "Build Pycall plug-in" "${BUILD_UNSTABLE}")
option(BUILD_PYTHON             "Build with Python support" "${BUILD_UNSTABLE}")
option(BUILD_SET_VALUE_PLUGIN   "Build Set_value plug-in" ON)
option(BUILD_SERIALIZE_PLUGIN   "Build Serialize plug-in" ON)
option(BUILD_TIMER_PLUGIN       "Build Timer plugin" ON)
option(BUILD_TRACE_PLUGIN       "Build Trace plugin" ON)
option(BUILD_USER_CODE_PLUGIN   "Build User-code plugin" ON)
option(ENABLE_BENCHMARKING      "Activate benchmarks in the test suite" OFF)



### Default build type

if(NOT DEFINED CMAKE_BUILD_TYPE AND "${CMAKE_CONFIGURATION_TYPES}" STREQUAL "")
	set(CMAKE_BUILD_TYPE "${DEFAULT_BUILD_TYPE}" CACHE STRING "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel ...")
	set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS None Debug Release RelWithDebInfo MinSizeRel)
endif()
message(STATUS " **Profile**: Distribution profile is: `${DIST_PROFILE}' (-DDIST_PROFILE=${DIST_PROFILE})")
message(STATUS " **Profile**: Build type is: `${CMAKE_BUILD_TYPE}' (-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE})")
message(STATUS " **Profile**: Prefix path is: `${CMAKE_PREFIX_PATH}' (-DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH})")
message(STATUS " **Profile**: Default use is: `${USE_DEFAULT}' (-DUSE_DEFAULT=${USE_DEFAULT})")
message(STATUS " **Profile**: Unstable activation is: `${BUILD_UNSTABLE}' (-DBUILD_UNSTABLE=${BUILD_UNSTABLE})")
foreach(FEATURE FORTRAN HDF5_PARALLEL PYTHON TESTING)
	set(FEATURE_ENABLED "DISABLED")
	if(${BUILD_${FEATURE}})
		set(FEATURE_ENABLED "ENABLED ")
	endif()
	message(STATUS " **Feature**: ${FEATURE_ENABLED} ${FEATURE} (-DBUILD_${FEATURE}=${BUILD_${FEATURE}})")
endforeach()



### Include sub-projects find modules

list(APPEND CMAKE_MODULE_PATH
	"${CMAKE_CURRENT_SOURCE_DIR}/cmake"
	"${CMAKE_CURRENT_SOURCE_DIR}/pdi/cmake")
if("${BUILD_DECL_NETCDF_PLUGIN}")
	if(4.3 VERSION_GREATER "${CMAKE_VERSION}")
		# Workaround missing "Prefer h5hl<LANG> compilers if HDF5_FIND_HL is enabled" in cmake pre-4.1
		# Workaround for missing HDF5_IS_PARALLEL with HDF5-2.0.0+ in cmake pre-4.3
		list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/plugins/decl_netcdf/cmake/4.3")
	endif()
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_netcdf/cmake")
endif()
if("${BUILD_DECL_HDF5_PLUGIN}")
	if(4.3 VERSION_GREATER "${CMAKE_VERSION}")
		# Workaround missing "Prefer h5hl<LANG> compilers if HDF5_FIND_HL is enabled" in cmake pre-4.1
		# Workaround for missing HDF5_IS_PARALLEL with HDF5-2.0.0+ in cmake pre-4.3
		list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/plugins/decl_hdf5/cmake/4.3")
	endif()
endif()
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" CACHE STRING "" FORCE)



### Sanity check

if("${BUILD_PYCALL_PLUGIN}" AND NOT "${BUILD_PYTHON}")
	message(FATAL_ERROR
		"The Pycall plugin is enabled but Python support is disabled.\n"
		"    you have the following options:\n"
		"     * enable Python support => pass `-DBUILD_PYTHON=ON' to cmake\n"
		"     * do not build the Pycall plugin => pass `-DBUILD_PYCALL_PLUGIN=OFF' to cmake"
	)
endif()

if("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_HDF5_PARALLEL}" AND NOT "${BUILD_MPI_PLUGIN}")
	message(FATAL_ERROR
		"Parallel support is enabled for the Decl'HDF5 plugin but the MPI plugin is disabled.\n"
		"    you have the following options:\n"
		"     * build the MPI plugin => pass `-DBUILD_MPI_PLUGIN=ON' to cmake\n"
		"     * build Decl'HDF5 without parallel support => pass `-DBUILD_HDF5_PARALLEL=OFF' to cmake\n"
		"     * do not build the Decl'HDF5 at all => pass `-DBUILD_DECL_HDF5_PLUGIN=OFF' to cmake"
	)
endif()

if("${BUILD_DECL_NETCDF_PLUGIN}" AND "${BUILD_NETCDF_PARALLEL}" AND NOT "${BUILD_DECL_HDF5_PLUGIN}")
	message(FATAL_ERROR
		"Parallel support is enabled for the Decl'NetCDF plugin but parallel support for HDF5 disabled.\n"
		"    you have the following options:\n"
		"     * enable HDF5 parallel support => pass `-DBUILD_HDF5_PARALLEL=ON' to cmake\n"
		"     * build Decl'NetCDF without parallel support => pass `-DBUILD_NETCDF_PARALLEL=OFF' to cmake\n"
		"     * do not build the Decl'NetCDF at all => pass `-DBUILD_DECL_NETCDF_PLUGIN=OFF' to cmake"
	)
endif()

if("${BUILD_DECL_NETCDF_PLUGIN}" AND "${BUILD_NETCDF_PARALLEL}" AND NOT "${BUILD_MPI_PLUGIN}")
	message(FATAL_ERROR
		"Parallel support is enabled for the Decl'NetCDF plugin but the MPI plugin is disabled.\n"
		"    you have the following options:\n"
		"     * build the MPI plugin => pass `-DBUILD_MPI_PLUGIN=ON' to cmake\n"
		"     * build Decl'NetCDF without parallel support => pass `-DBUILD_NETCDF_PARALLEL=OFF' to cmake\n"
		"     * do not build the Decl'NetCDF at all => pass `-DBUILD_DECL_NETCDF_PLUGIN=OFF' to cmake"
	)
endif()



### Languages

# This has to come after the build type is settled and before any try_compile or include(GNUInstallDirs).
enable_language(C)
enable_language(CXX)
if("${BUILD_FORTRAN}")
	enable_language(Fortran)
endif()



### Testing

if("${BUILD_TESTING}")
	include(CTest)
endif()



### Dependencies common to both modes

if("${BUILD_TESTING}")
	find_program(ZSH_PROGRAM zsh)
	if(EXISTS "${ZSH_PROGRAM}")
		set(ZSH_PROGRAM_FOUND ON)
	else()
		set(ZSH_PROGRAM_FOUND OFF)
	endif()
	option(ENABLE_ZSH "Enable test of ZSH for env script" "${ZSH_PROGRAM_FOUND}")
endif()

if("${BUILD_DOCUMENTATION}")
	find_package(Doxygen "${PDI_REQUIRED_DOXYGEN_VERSION}" REQUIRED OPTIONAL_COMPONENTS dot)
endif()

set(MPI_COMPONENTS)
if("${BUILD_TESTING}")
	list(APPEND MPI_COMPONENTS CXX C)
	if("${BUILD_FORTRAN}")
		list(APPEND MPI_COMPONENTS Fortran)
	endif()
endif()
if( ("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_HDF5_PARALLEL}") OR ("${BUILD_DECL_NETCDF_PLUGIN}" AND "${BUILD_NETCDF_PARALLEL}") )
	list(APPEND MPI_COMPONENTS CXX C)
	if("${BUILD_FORTRAN}")
		list(APPEND MPI_COMPONENTS Fortran)
	endif()
endif()
if("${BUILD_MPI_PLUGIN}")
	list(APPEND MPI_COMPONENTS CXX)
endif()
if(NOT "x${MPI_COMPONENTS}x" STREQUAL "xx")
	list(REMOVE_DUPLICATES MPI_COMPONENTS)
	find_package(MPI REQUIRED COMPONENTS ${MPI_COMPONENTS})
endif()

if("${BUILD_PYTHON}")
	find_package(Python3Path "${PDI_REQUIRED_PYTHON_VERSION}" REQUIRED COMPONENTS Interpreter Development)
elseif("${BUILD_FORTRAN}")
	find_package(Python3Path "${PDI_REQUIRED_PYTHON_VERSION}" REQUIRED COMPONENTS Interpreter)
endif()

if("${BUILD_TESTING}")
	find_package(Threads REQUIRED)
endif()



### Installation directories

include(GNUInstallDirs)
set(INSTALL_PDIDATADIR  "${CMAKE_INSTALL_DATADIR}/pdi" CACHE PATH "PDI data directory (DATADIR/pdi)")
set(INSTALL_PDICMAKEDIR "${INSTALL_PDIDATADIR}/cmake"  CACHE PATH "PDI cmake modules (PDIDATADIR/cmake)")
# GNUInstallDirs defaults DOCDIR to DATAROOTDIR/doc/PDI after PROJECT_NAME; replace only that default, not a value the user cached.
if("" STREQUAL "$CACHE{CMAKE_INSTALL_DOCDIR}")
	set(CMAKE_INSTALL_DOCDIR "${CMAKE_INSTALL_DATAROOTDIR}/doc/pdi")
endif()

if("${BUILD_FORTRAN}")
	# Expect compilers to provide compatible modules at the minor release level at least, hence the version truncated to its first two components.
	string(REGEX REPLACE "^([0-9]*\\.[0-9]*)\\..*$" "\\1" Fortran_COMPILER_MINOR_VERSION "${CMAKE_Fortran_COMPILER_VERSION}")
	set(INSTALL_FMODDIR "${CMAKE_INSTALL_LIBDIR}/pdi/finclude/${CMAKE_Fortran_COMPILER_ID}-${Fortran_COMPILER_MINOR_VERSION}"
		CACHE PATH "Fortran module directory (LIBDIR/pdi/finclude/<compiler>-<version>)")
endif()

set(INSTALL_PDIPLUGINDIR "${CMAKE_INSTALL_LIBDIR}/pdi/plugins_${PDI_VERSION}" CACHE PATH "PDI plugins (LIBDIR/pdi/plugins_<version>)")



### Direct mode, the part that acts as a normal CMake project

if(NOT "${PDI_SUPERBUILD}")

	## The vendored dependencies

	set(PARACONF_COMPONENTS C)
	if("${BUILD_FORTRAN}")
		list(APPEND PARACONF_COMPONENTS f90)
	endif()
	find_package(paraconf "${PDI_REQUIRED_PARACONF_VERSION}" REQUIRED COMPONENTS ${PARACONF_COMPONENTS})

	find_package(spdlog "${PDI_REQUIRED_SPDLOG_VERSION}" REQUIRED)

	if("${BUILD_PYTHON}")
		# Ensure pybind11 finds the same python as us.
		set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency")
		set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules")
		find_package(pybind11 "${PDI_REQUIRED_PYBIND11_VERSION}" REQUIRED)
	endif()

	if("${BUILD_FORTRAN}" OR "${BUILD_DOCUMENTATION}")
		add_subdirectory("${PDI_VENDORED_ZPP_DIR}" vendor/zpp)
	endif()

	if("${BUILD_TESTING}")
		option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
		add_subdirectory("${PDI_VENDORED_GOOGLETEST_DIR}" vendor/googletest EXCLUDE_FROM_ALL)
	endif()



	## The CMake modules

	include(CMakePackageConfigHelpers)

	include(GenerateExportHeader)

	if("${BUILD_FORTRAN}")
		include(DefaultKind)
	endif()

	if("${BUILD_TESTING}")
		include(GoogleTest)
	endif()



	## and now for the actual content

	message(STATUS " **Module**: ENABLED  PDI")
	add_subdirectory(pdi)

	set(PDI_PLUGIN_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/plugins")
	foreach(PDI_PLUGIN IN ITEMS SET_VALUE TRACE USER_CODE)
		if("${BUILD_${PDI_PLUGIN}_PLUGIN}")
			message(STATUS " **Module**: ENABLED  ${PDI_PLUGIN}_PLUGIN (-DBUILD_${PDI_PLUGIN}_PLUGIN=ON)")
			string(TOLOWER "${PDI_PLUGIN}" PDI_PLUGIN_DIR)
			add_subdirectory("plugins/${PDI_PLUGIN_DIR}")
		else()
			message(STATUS " **Module**: DISABLED ${PDI_PLUGIN}_PLUGIN (-DBUILD_${PDI_PLUGIN}_PLUGIN=OFF)")
		endif()
	endforeach()

	return()
endif()



### Dependencies

include(SuperBuild)
sbuild_get_env(SBUILD_PATH PATH)
sbuild_get_env(SBUILD_CPATH CPATH)
sbuild_get_env(SBUILD_LIBRARY_PATH LIBRARY_PATH)
sbuild_get_env(SBUILD_LD_LIBRARY_PATH LD_LIBRARY_PATH)


## Yaml

# if ( paraconf )
sbuild_add_dependency(yaml "${USE_DEFAULT}"
	EMBEDDED_PATH "${PDI_VENDORED_YAML_DIR}"
	CMAKE_CACHE_ARGS
		"-DBUILD_TESTING:BOOL=OFF"
		"-DCMAKE_POLICY_VERSION_MINIMUM:STRING=3.10" # needed until upstream libyaml is updated (https://github.com/yaml/libyaml/pull/314)
		"-DINSTALL_BIN_DIR:STRING=${CMAKE_INSTALL_BINDIR}"
		"-DINSTALL_CMAKE_DIR:STRING=share/yaml/cmake"
		"-DINSTALL_INCLUDE_DIR:STRING=${CMAKE_INSTALL_INCLUDEDIR}"
		"-DINSTALL_LIB_DIR:STRING=${CMAKE_INSTALL_LIBDIR}"
	VERSION "${PDI_REQUIRED_YAML_VERSION}"
)


## Paraconf

set(PARACONF_COMPONENTS "C")
if("${BUILD_FORTRAN}")
	list(APPEND PARACONF_COMPONENTS "f90")
endif()
# if ( PDI or ... )
sbuild_add_dependency(paraconf "${USE_DEFAULT}"
	EMBEDDED_PATH "${PDI_VENDORED_PARACONF_DIR}"
	COMPONENTS ${PARACONF_COMPONENTS}
	CMAKE_CACHE_ARGS
		"-DUSE_yaml:STRING=SYSTEM"
		"-DBUILD_TESTING:BOOL=OFF"
	DEPENDS "yaml"
	VERSION "${PDI_REQUIRED_PARACONF_VERSION}"
)


## spdlog

# if ( PDI or ... )
sbuild_add_dependency(spdlog "${USE_DEFAULT}"
	EMBEDDED_PATH "${PDI_VENDORED_SPDLOG_DIR}"
	CMAKE_CACHE_ARGS
		"-DSPDLOG_BUILD_BENCH:BOOL=OFF"
		"-DSPDLOG_BUILD_EXAMPLES:BOOL=OFF"
		"-DSPDLOG_BUILD_TESTS:BOOL=OFF"
		"-DSPDLOG_FMT_EXTERNAL:BOOL=OFF"
	VERSION "${PDI_REQUIRED_SPDLOG_VERSION}"
)


## HDF5

if("${BUILD_DECL_HDF5_PLUGIN}" OR "${BUILD_DECL_NETCDF_PLUGIN}")
	
	set(HDF5_CMAKE_CACHE_ARGS)
	
	if("${BUILD_HDF5_PARALLEL}")
		set(HDF5_PREFER_PARALLEL ON)
		list(APPEND HDF5_CMAKE_CACHE_ARGS "-DHDF5_ENABLE_PARALLEL:BOOL=ON")
	else()
		set(HDF5_PREFER_PARALLEL OFF)
		list(APPEND HDF5_CMAKE_CACHE_ARGS "-DHDF5_ENABLE_PARALLEL:BOOL=OFF")
	endif()
	
	if("${BUILD_DECL_NETCDF_PLUGIN}")
		set(HDF5_COMPONENTS C HL)
		list(APPEND HDF5_CMAKE_CACHE_ARGS "-DHDF5_BUILD_HL_LIB:BOOL=ON")
	else()
		set(HDF5_COMPONENTS C)
		list(APPEND HDF5_CMAKE_CACHE_ARGS "-DHDF5_BUILD_HL_LIB:BOOL=OFF")
	endif()
	
	sbuild_add_dependency(HDF5 "${USE_DEFAULT}"
		EMBEDDED_PATH "${PDI_VENDORED_HDF5_DIR}"
		COMPONENTS ${HDF5_COMPONENTS}
		MODULE_VARS HDF5_IS_PARALLEL HDF5_VERSION
		CMAKE_CACHE_ARGS
			-DBUILD_STATIC_LIBS:BOOL=OFF
			-DBUILD_TESTING:BOOL=OFF
			-DHDF5_BUILD_EXAMPLES:BOOL=OFF
			-DHDF5_BUILD_TOOLS:BOOL=ON
			-DHDF5_BUILD_UTILS:BOOL=OFF
			"-DHDF5_INSTALL_BIN_DIR:STRING=${CMAKE_INSTALL_BINDIR}"
			"-DHDF5_INSTALL_CMAKE_DIR:STRING=${CMAKE_INSTALL_DATADIR}/cmake/hdf5"
			"-DHDF5_INSTALL_DATA_DIR:STRING=${CMAKE_INSTALL_DATADIR}/hdf5"
			"-DHDF5_INSTALL_LIB_DIR:STRING=${CMAKE_INSTALL_LIBDIR}"
			${HDF5_CMAKE_CACHE_ARGS}
	)
	if("${HDF5_FOUND}")
		if("${HDF5_VERSION}" VERSION_LESS "${PDI_REQUIRED_HDF5_VERSION}")
			message(FATAL_ERROR "HDF5 version ${PDI_REQUIRED_HDF5_VERSION} at least required, HDF5 ${HDF5_VERSION} found.")
		endif()
		if("${BUILD_HDF5_PARALLEL}" AND NOT "${HDF5_IS_PARALLEL}")
			message(FATAL_ERROR
				"You requested a parallel HDF5 build (-DBUILD_HDF5_PARALLEL=ON) but a sequential SYSTEM version has been found\n"
				"    you have the following options:\n"
				"     * build with this sequential HDF5 => pass `-DBUILD_HDF5_PARALLEL=OFF' to cmake\n"
				"     * build the EMBEDDED HDF5 from PDI distribution => pass `-DUSE_HDF5=EMBEDDED' to cmake\n"
				"     * use another version of HDF5 from the system => pass `-DHDF5_ROOT=/path/to/hdf5/root/' to cmake"
			)
		endif()
	elseif(NOT "EMBEDDED" STREQUAL "${USE_HDF5}")
		message(FATAL_ERROR
			"HDF5 setup failed: A suitable SYSTEM version (>= ${PDI_REQUIRED_HDF5_VERSION}) could not be found.\n"
			"    you have the following options:\n"
			"     * build the EMBEDDED HDF5 from PDI distribution => pass `-DUSE_HDF5=EMBEDDED' to cmake\n"
			"     * use another version of HDF5 from the system => pass `-DHDF5_ROOT=/path/to/hdf5/root/' to cmake"
		)
	endif()
endif()


## NetCDF

if("${BUILD_DECL_NETCDF_PLUGIN}")
	
	if("${BUILD_HDF5_PARALLEL}")
		set(NETCDF_CC "${MPI_C_COMPILER}")
	else()
		set(NETCDF_CC "${CMAKE_C_COMPILER}")
	endif()
	
	set(NETCDF_CMAKE_CACHE_ARGS)
	if("${BUILD_NETCDF_PARALLEL}")
		list(APPEND NETCDF_CMAKE_CACHE_ARGS "-DENABLE_PARALLEL4:BOOL=ON")
	else()
		list(APPEND NETCDF_CMAKE_CACHE_ARGS "-DENABLE_PARALLEL4:BOOL=OFF")
	endif()
	
	# Workaround NetCDF failing to add ZLIB include path in H5Deflate plugin
	find_package(ZLIB QUIET)
	if("${ZLIB_FOUND}")
		string(REPLACE ";" " -I" NETCDF_CFLAGS "-I ${ZLIB_INCLUDE_DIRS}")
	endif()
	
	sbuild_add_dependency(NetCDF "${USE_DEFAULT}"
		EMBEDDED_PATH "${PDI_VENDORED_NETCDF_DIR}"
		DEPENDS HDF5
		MODULE_VARS NetCDF_FEATURES
		CMAKE_CACHE_ARGS
			"-DCMAKE_C_COMPILER:STRING=${NETCDF_CC}"
			-DBUILD_TESTING:BOOL=OFF
			-DBUILD_TESTSETS:BOOL=OFF
			"-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS} ${NETCDF_CFLAGS}"
			-DENABLE_BASH_SCRIPT_TESTING:BOOL=OFF
			-DNETCDF_BUILD_UTILITIES:BOOL=OFF
			-DNETCDF_ENABLE_BYTERANGE:BOOL=OFF
			-DNETCDF_ENABLE_DAP:BOOL=OFF
			-DNETCDF_ENABLE_DAP4:BOOL=OFF
			-DNETCDF_ENABLE_EXAMPLES:BOOL=OFF
			-DNETCDF_ENABLE_FILTER_TESTING:BOOL=OFF
			-DNETCDF_ENABLE_TESTS:BOOL=OFF
		VERSION "${PDI_REQUIRED_NETCDF_VERSION}"
	)
	if("${NetCDF_FOUND}")
		if("${BUILD_NETCDF_PARALLEL}" AND NOT "PARALLEL4" IN_LIST NetCDF_FEATURES)
			message(FATAL_ERROR
				"You requested a parallel NetCDF build (-DBUILD_NETCDF_PARALLEL=ON) but a sequential SYSTEM version has been found\n"
				"    you have the following options:\n"
				"     * build with this sequential NetCDF => pass `-DBUILD_NETCDF_PARALLEL=OFF' to cmake\n"
				"     * build the EMBEDDED NetCDF from PDI distribution => pass `-DUSE_NetCDF=EMBEDDED' to cmake\n"
				"     * use another version of NetCDF from the system => pass `-DNetCDF_CFGSCRIPT=/path/to/netcdf/bin/nc-config' to cmake\n"
				"       you may need to change the discovery order so the config script is tried first =>\n"
				"       pass `-DNetCDF_FIND_STRATEGIES=\"CFGSCRIPT\"' to cmake"
			)
		endif()
	elseif(NOT "EMBEDDED" STREQUAL "${USE_NetCDF}")
		message(FATAL_ERROR
			"NetCDF setup failed: A suitable SYSTEM version (>= ${PDI_REQUIRED_NETCDF_VERSION}) could not be found.\n"
			"    you have the following options:\n"
			"     * build the EMBEDDED NetCDF from PDI distribution => pass `-DUSE_NetCDF=EMBEDDED' to cmake\n"
			"     * use another version of NetCDF from the system => pass `-DNetCDF_CFGSCRIPT=/path/to/netcdf/bin/nc-config' to cmake\n"
			"       you may need to change the discovery order so the config script is tried first =>\n"
			"       pass `-DNetCDF_FIND_STRATEGIES=\"CFGSCRIPT\"' to cmake"
		)
	endif()
endif()


## PyBind11

if("${BUILD_PYTHON}")
	set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency" FORCE)
	set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules" FORCE)
	sbuild_add_dependency(pybind11 "${USE_DEFAULT}"
		EMBEDDED_PATH "${PDI_VENDORED_PYBIND11_DIR}"
		CMAKE_CACHE_ARGS
			"-DBUILD_TESTING:BOOL=OFF"
			"-DPYBIND11_TEST:BOOL=OFF"
			"-DPYBIND11_PYTHON_VERSION:STRING=${Python3_VERSION}"
		VERSION "${PDI_REQUIRED_PYBIND11_VERSION}"
	)
endif()


## JSON

if("${BUILD_JSON_PLUGIN}")
	sbuild_add_dependency(JSON "${USE_DEFAULT}"
		EMBEDDED_PATH "${PDI_VENDORED_JSON_DIR}"
		CMAKE_CACHE_ARGS
			"-DCMAKE_POLICY_VERSION_MINIMUM:STRING=3.10" # needed until upstream is updated
			"-DJSON_BuildTests:BOOL=OFF" # skip unit tests for json
		VERSION "${PDI_REQUIRED_JSON_VERSION}"
	)
endif()


### Own modules

set(PDI_DEPENDS_LIST paraconf spdlog)
if("${BUILD_PYTHON}")
	list(APPEND PDI_DEPENDS_LIST pybind11)
endif()
sbuild_add_module(PDI
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}"
	CMAKE_CACHE_ARGS "-DPDI_SUPERBUILD:BOOL=OFF"
	DEPENDS ${PDI_DEPENDS_LIST}
	SUBSTEPS test
)


sbuild_add_module(MPI_PLUGIN
	ENABLE_BUILD_FLAG BUILD_MPI_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/mpi"
	DEPENDS PDI
	SUBSTEPS test
)


sbuild_add_module(DECL_HDF5_PLUGIN
	ENABLE_BUILD_FLAG BUILD_DECL_HDF5_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_hdf5"
	DEPENDS PDI HDF5
	SUBSTEPS test
)

sbuild_add_module(DECL_NETCDF_PLUGIN
	ENABLE_BUILD_FLAG BUILD_DECL_NETCDF_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_netcdf"
	DEPENDS PDI NetCDF
	SUBSTEPS test
)


sbuild_add_module(JSON_PLUGIN
	ENABLE_BUILD_FLAG BUILD_JSON_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/json"
	DEPENDS PDI JSON
	SUBSTEPS test
)


sbuild_add_module(PYCALL_PLUGIN
	ENABLE_BUILD_FLAG BUILD_PYCALL_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/pycall"
	DEPENDS PDI
	SUBSTEPS test
)

sbuild_add_module(SERIALIZE_PLUGIN
	ENABLE_BUILD_FLAG BUILD_SERIALIZE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/serialize"
	DEPENDS PDI
	SUBSTEPS test
)


sbuild_add_module(PDI_EXAMPLE
	ENABLE_BUILD_FLAG BUILD_TESTING
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/example"
	DEPENDS PDI
	INSTALL_COMMAND ""
	SUBSTEPS test
)

sbuild_add_module(PDI_COMBINATION_TESTS
	ENABLE_BUILD_FLAG BUILD_TESTING
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/tests/combination_tests"
	DEPENDS PDI
	INSTALL_COMMAND ""
	SUBSTEPS test
)

sbuild_add_module(PDI_API_TESTS
	ENABLE_BUILD_FLAG BUILD_TESTING
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/tests/api_tests"
	DEPENDS PDI
	INSTALL_COMMAND ""
	SUBSTEPS test
)

sbuild_add_module(TIMER_PLUGIN
	ENABLE_BUILD_FLAG BUILD_TIMER_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/timer"
	DEPENDS PDI
	SUBSTEPS test
)

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



## Include path

# Every CMake module and helper script of the distribution lives here, whichever component uses it.
list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
if(4.3 VERSION_GREATER "${CMAKE_VERSION}")
	# Workaround missing "Prefer h5hl<LANG> compilers if HDF5_FIND_HL is enabled" in cmake pre-4.1
	# Workaround for missing HDF5_IS_PARALLEL with HDF5-2.0.0+ in cmake pre-4.3
	list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_SOURCE_DIR}/cmake/4.3")
endif()



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

include(PDIOptions)


## Global options

pdi_setting(DIST_PROFILE "Profile to use for PDI distribution build. Options are: User, Devel" "User")
set_property(CACHE PDI_DIST_PROFILE PROPERTY STRINGS User Devel)

if("User" STREQUAL "${PDI_DIST_PROFILE}")
	pdi_option(BUILD_DOCUMENTATION "Build documentation" OFF)
	pdi_option(BUILD_TESTING       "Build tests" OFF)
	pdi_option(BUILD_UNSTABLE      "Build all features by default including those not stable yet" OFF)
	set(DEFAULT_BUILD_TYPE "Release")
	# Defaults to ON (superbuild) when PDI is top level (called directly), but to OFF (direct mode) when added with `add_subdirectory` by a user.
	set(DEFAULT_SUPERBUILD "${PROJECT_IS_TOP_LEVEL}")
elseif("Devel" STREQUAL "${PDI_DIST_PROFILE}")
	pdi_option(BUILD_DOCUMENTATION "Build documentation" ON)
	pdi_option(BUILD_TESTING       "Build tests" ON)
	pdi_option(BUILD_UNSTABLE      "Build all features by default including those not stable yet" ON)
	set(DEFAULT_BUILD_TYPE "Debug")
	set(DEFAULT_SUPERBUILD "OFF")
else()
	message(FATAL_ERROR "PDI_DIST_PROFILE should be set to one of: User, Devel")
endif()

pdi_setting(USE_DEFAULT "Default version of libraries to use; this can be 1) EMBEDDED to use the provided version, 2) SYSTEM to use an already\
 installed version (you can use CMAKE_PREFIX_PATH to specify where to look, or 3) AUTO to use SYSTEM if available and EMBEDDED otherwise" AUTO)

# With the superbuild on, this file builds the vendored dependencies and then re-invokes itself as a single ExternalProject with the flag off.
# With it off, this is a normal project, dependencies are expected to be already available and every module is added with add_subdirectory.
option(PDI_SUPERBUILD "Build the vendored dependencies, then PDI itself as a sub-project" "${DEFAULT_SUPERBUILD}")
if("${PDI_SUPERBUILD}")
	message(STATUS " **Profile**: Superbuild mode is: `ON' (-DPDI_SUPERBUILD=ON)")
endif()

# Modules to build

pdi_option(BUILD_BENCHMARKING       "Build PDI benchmarks" ON)
pdi_option(BUILD_DECL_HDF5_PLUGIN   "Build Decl'HDF5 plug-in" ON)
pdi_option(BUILD_DECL_NETCDF_PLUGIN "Build Decl'NetCDF plug-in" ON)
pdi_option(BUILD_FORTRAN            "Build with Fortran support" ON)
pdi_option(BUILD_HDF5_PARALLEL      "Build Decl'HDF5 in parallel mode" ON)
pdi_option(BUILD_JSON_PLUGIN        "Build JSON plugin" OFF)
pdi_option(BUILD_MPI_PLUGIN         "Build MPI plug-in" ON)
pdi_option(BUILD_NETCDF_PARALLEL    "Build Decl'NetCDF in parallel mode" ON)
pdi_option(BUILD_PDI_CORE           "Build the PDI core library" ON)
pdi_option(BUILD_PYCALL_PLUGIN      "Build Pycall plug-in" "${PDI_BUILD_UNSTABLE}")
pdi_option(BUILD_PYTHON             "Build with Python support" "${PDI_BUILD_UNSTABLE}")
pdi_option(BUILD_SET_VALUE_PLUGIN   "Build Set_value plug-in" ON)
pdi_option(BUILD_SERIALIZE_PLUGIN   "Build Serialize plug-in" ON)
pdi_option(BUILD_TIMER_PLUGIN       "Build Timer plugin" ON)
pdi_option(BUILD_TRACE_PLUGIN       "Build Trace plugin" ON)
pdi_option(BUILD_USER_CODE_PLUGIN   "Build User-code plugin" ON)
pdi_option(ENABLE_BENCHMARKING      "Activate benchmarks in the test suite" OFF)



### Default build type

if("${PROJECT_IS_TOP_LEVEL}" AND NOT DEFINED CMAKE_BUILD_TYPE AND "${CMAKE_CONFIGURATION_TYPES}" STREQUAL "")
	set(CMAKE_BUILD_TYPE "${DEFAULT_BUILD_TYPE}" CACHE STRING "Choose the type of build, options are: None Debug Release RelWithDebInfo MinSizeRel ...")
	set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS None Debug Release RelWithDebInfo MinSizeRel)
endif()
message(STATUS " **Profile**: Distribution profile is: `${PDI_DIST_PROFILE}' (-DPDI_DIST_PROFILE=${PDI_DIST_PROFILE})")
message(STATUS " **Profile**: Build type is: `${CMAKE_BUILD_TYPE}' (-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE})")
message(STATUS " **Profile**: Prefix path is: `${CMAKE_PREFIX_PATH}' (-DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH})")
message(STATUS " **Profile**: Default use is: `${PDI_USE_DEFAULT}' (-DPDI_USE_DEFAULT=${PDI_USE_DEFAULT})")
message(STATUS " **Profile**: Unstable activation is: `${PDI_BUILD_UNSTABLE}' (-DPDI_BUILD_UNSTABLE=${PDI_BUILD_UNSTABLE})")
foreach(FEATURE FORTRAN HDF5_PARALLEL PYTHON TESTING)
	set(FEATURE_ENABLED "DISABLED")
	if(${PDI_BUILD_${FEATURE}})
		set(FEATURE_ENABLED "ENABLED ")
	endif()
	message(STATUS " **Feature**: ${FEATURE_ENABLED} ${FEATURE} (-DPDI_BUILD_${FEATURE}=${PDI_BUILD_${FEATURE}})")
endforeach()



### Sanity check

if("${PDI_BUILD_PYCALL_PLUGIN}" AND NOT "${PDI_BUILD_PYTHON}")
	message(FATAL_ERROR
		"The Pycall plugin is enabled but Python support is disabled.\n"
		"    you have the following options:\n"
		"     * enable Python support => pass `-DPDI_BUILD_PYTHON=ON' to cmake\n"
		"     * do not build the Pycall plugin => pass `-DPDI_BUILD_PYCALL_PLUGIN=OFF' to cmake"
	)
endif()

if("${PDI_BUILD_DECL_HDF5_PLUGIN}" AND "${PDI_BUILD_HDF5_PARALLEL}" AND NOT "${PDI_BUILD_MPI_PLUGIN}")
	message(FATAL_ERROR
		"Parallel support is enabled for the Decl'HDF5 plugin but the MPI plugin is disabled.\n"
		"    you have the following options:\n"
		"     * build the MPI plugin => pass `-DPDI_BUILD_MPI_PLUGIN=ON' to cmake\n"
		"     * build Decl'HDF5 without parallel support => pass `-DPDI_BUILD_HDF5_PARALLEL=OFF' to cmake\n"
		"     * do not build the Decl'HDF5 at all => pass `-DPDI_BUILD_DECL_HDF5_PLUGIN=OFF' to cmake"
	)
endif()

if("${PDI_BUILD_DECL_NETCDF_PLUGIN}" AND "${PDI_BUILD_NETCDF_PARALLEL}" AND NOT "${PDI_BUILD_DECL_HDF5_PLUGIN}")
	message(FATAL_ERROR
		"Parallel support is enabled for the Decl'NetCDF plugin but parallel support for HDF5 disabled.\n"
		"    you have the following options:\n"
		"     * enable HDF5 parallel support => pass `-DPDI_BUILD_HDF5_PARALLEL=ON' to cmake\n"
		"     * build Decl'NetCDF without parallel support => pass `-DPDI_BUILD_NETCDF_PARALLEL=OFF' to cmake\n"
		"     * do not build the Decl'NetCDF at all => pass `-DPDI_BUILD_DECL_NETCDF_PLUGIN=OFF' to cmake"
	)
endif()

if("${PDI_BUILD_DECL_NETCDF_PLUGIN}" AND "${PDI_BUILD_NETCDF_PARALLEL}" AND NOT "${PDI_BUILD_MPI_PLUGIN}")
	message(FATAL_ERROR
		"Parallel support is enabled for the Decl'NetCDF plugin but the MPI plugin is disabled.\n"
		"    you have the following options:\n"
		"     * build the MPI plugin => pass `-DPDI_BUILD_MPI_PLUGIN=ON' to cmake\n"
		"     * build Decl'NetCDF without parallel support => pass `-DPDI_BUILD_NETCDF_PARALLEL=OFF' to cmake\n"
		"     * do not build the Decl'NetCDF at all => pass `-DPDI_BUILD_DECL_NETCDF_PLUGIN=OFF' to cmake"
	)
endif()



### Languages

# This has to come after the build type is settled and before any try_compile or include(GNUInstallDirs).
enable_language(C)
enable_language(CXX)
if("${PDI_BUILD_FORTRAN}")
	enable_language(Fortran)
endif()



### Testing

if("${PDI_BUILD_TESTING}")
	set(BUILD_TESTING ON)
	include(CTest)
endif()



### Dependencies common to both modes

if("${PDI_BUILD_TESTING}")
	find_program(ZSH_PROGRAM zsh)
	if(EXISTS "${ZSH_PROGRAM}")
		set(ZSH_PROGRAM_FOUND ON)
	else()
		set(ZSH_PROGRAM_FOUND OFF)
	endif()
	pdi_option(ENABLE_ZSH "Enable test of ZSH for env script" "${ZSH_PROGRAM_FOUND}")
endif()

if("${PDI_BUILD_DOCUMENTATION}" AND NOT TARGET Doxygen::doxygen)
	find_package(Doxygen "${PDI_REQUIRED_DOXYGEN_VERSION}" REQUIRED OPTIONAL_COMPONENTS dot)
endif()

set(MPI_COMPONENTS)
if("${PDI_BUILD_TESTING}")
	list(APPEND MPI_COMPONENTS CXX C)
	if("${PDI_BUILD_FORTRAN}")
		list(APPEND MPI_COMPONENTS Fortran)
	endif()
endif()
if( ("${PDI_BUILD_DECL_HDF5_PLUGIN}" AND "${PDI_BUILD_HDF5_PARALLEL}") OR ("${PDI_BUILD_DECL_NETCDF_PLUGIN}" AND "${PDI_BUILD_NETCDF_PARALLEL}") )
	list(APPEND MPI_COMPONENTS CXX C)
	if("${PDI_BUILD_FORTRAN}")
		list(APPEND MPI_COMPONENTS Fortran)
	endif()
endif()
if("${PDI_BUILD_MPI_PLUGIN}")
	list(APPEND MPI_COMPONENTS CXX)
endif()
if(NOT "x${MPI_COMPONENTS}x" STREQUAL "xx")
	list(REMOVE_DUPLICATES MPI_COMPONENTS)
	# One target is not enough to skip the lookup here: whoever found MPI before us may have asked for fewer languages.
	set(MPI_MISSING_COMPONENTS)
	foreach(MPI_COMPONENT IN LISTS MPI_COMPONENTS)
		if(NOT TARGET "MPI::MPI_${MPI_COMPONENT}")
			list(APPEND MPI_MISSING_COMPONENTS "${MPI_COMPONENT}")
		endif()
	endforeach()
	if(NOT "x${MPI_MISSING_COMPONENTS}x" STREQUAL "xx")
		find_package(MPI REQUIRED COMPONENTS ${MPI_COMPONENTS})
	endif()
endif()

if("${PDI_BUILD_PYTHON}" AND NOT TARGET Python3::Module)
	find_package(Python3 "${PDI_REQUIRED_PYTHON_VERSION}" REQUIRED COMPONENTS Interpreter Development)
elseif("${PDI_BUILD_FORTRAN}" AND NOT TARGET Python3::Interpreter)
	find_package(Python3 "${PDI_REQUIRED_PYTHON_VERSION}" REQUIRED COMPONENTS Interpreter)
endif()

if("${PDI_BUILD_TESTING}" AND NOT TARGET Threads::Threads)
	find_package(Threads REQUIRED)
endif()

# PDI itself as a dependency when we are not the ones building it
if(NOT "${PDI_BUILD_PDI_CORE}" AND NOT TARGET PDI::PDI_C)
	message(STATUS " **Module**: DISABLED PDI (-DPDI_BUILD_PDI_CORE=OFF), looking for an installed one")
	set(PDI_FIND_COMPONENT_LIST C plugins)
	if("${PDI_BUILD_FORTRAN}")
		list(APPEND PDI_FIND_COMPONENT_LIST f90)
	endif()
	if("${PDI_BUILD_PYTHON}" OR "${PDI_BUILD_PYCALL_PLUGIN}")
		list(APPEND PDI_FIND_COMPONENT_LIST pysupport)
	endif()
	find_package(PDI REQUIRED COMPONENTS ${PDI_FIND_COMPONENT_LIST})
endif()



### Installation directories

include(GNUInstallDirs)
set(INSTALL_PDIDATADIR  "${CMAKE_INSTALL_DATADIR}/pdi" CACHE PATH "PDI data directory (DATADIR/pdi)")
set(INSTALL_PDICMAKEDIR "${INSTALL_PDIDATADIR}/cmake"  CACHE PATH "PDI cmake modules (PDIDATADIR/cmake)")
# GNUInstallDirs defaults DOCDIR to DATAROOTDIR/doc/PDI after PROJECT_NAME; replace only that default, not a value the user cached.
if("" STREQUAL "$CACHE{CMAKE_INSTALL_DOCDIR}")
	set(CMAKE_INSTALL_DOCDIR "${CMAKE_INSTALL_DATAROOTDIR}/doc/pdi")
endif()

if("${PDI_BUILD_FORTRAN}")
	# Expect compilers to provide compatible modules at the minor release level at least, hence the version truncated to its first two components.
	string(REGEX REPLACE "^([0-9]*\\.[0-9]*)\\..*$" "\\1" Fortran_COMPILER_MINOR_VERSION "${CMAKE_Fortran_COMPILER_VERSION}")
	set(INSTALL_FMODDIR "${CMAKE_INSTALL_LIBDIR}/pdi/finclude/${CMAKE_Fortran_COMPILER_ID}-${Fortran_COMPILER_MINOR_VERSION}"
		CACHE PATH "Fortran module directory (LIBDIR/pdi/finclude/<compiler>-<version>)")
endif()

if("${PDI_BUILD_PYTHON}")
	include(Python3InstallDirs)
endif()

if("${PDI_BUILD_PDI_CORE}")
	set(INSTALL_PDIPLUGINDIR "${CMAKE_INSTALL_LIBDIR}/pdi/plugins_${PDI_VERSION}" CACHE PATH "PDI plugins (LIBDIR/pdi/plugins_<version>)")
else()
	set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
endif()



### Direct mode, the part that acts as a normal CMake project

if(NOT "${PDI_SUPERBUILD}")

	## The vendored dependencies

	if(("${PDI_BUILD_PDI_CORE}" OR "${PDI_BUILD_TESTING}") AND NOT TARGET paraconf::paraconf)
		set(PARACONF_COMPONENTS C)
		if("${PDI_BUILD_FORTRAN}")
			list(APPEND PARACONF_COMPONENTS f90)
		endif()
		find_package(paraconf "${PDI_REQUIRED_PARACONF_VERSION}" REQUIRED COMPONENTS ${PARACONF_COMPONENTS})
	endif()

	if("${PDI_BUILD_PDI_CORE}" AND NOT TARGET spdlog::spdlog)
		find_package(spdlog "${PDI_REQUIRED_SPDLOG_VERSION}" REQUIRED)
	endif()

	if("${PDI_BUILD_PYTHON}" AND NOT TARGET pybind11::pybind11)
		# Ensure pybind11 finds the same python as us.
		set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency")
		set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules")
		find_package(pybind11 "${PDI_REQUIRED_PYBIND11_VERSION}" REQUIRED)
	endif()

	if("${PDI_BUILD_DECL_HDF5_PLUGIN}" AND NOT TARGET hdf5::hdf5)
		set(HDF5_USE_STATIC_LIBRARIES OFF)
		set(HDF5_PREFER_PARALLEL "${PDI_BUILD_HDF5_PARALLEL}")
		find_package(HDF5 REQUIRED COMPONENTS C)
		if("${HDF5_VERSION}" VERSION_LESS "${PDI_REQUIRED_HDF5_VERSION}")
			message(FATAL_ERROR "HDF5 version ${PDI_REQUIRED_HDF5_VERSION} at least required, HDF5 ${HDF5_VERSION} found.")
		endif()
		if("${PDI_BUILD_HDF5_PARALLEL}" AND NOT "${HDF5_IS_PARALLEL}")
			message(FATAL_ERROR "Parallel HDF5 required, sequential HDF5 only found. Please set -DPDI_BUILD_HDF5_PARALLEL=OFF to disable parallel HDF5")
		endif()
	endif()

	if("${PDI_BUILD_DECL_NETCDF_PLUGIN}" AND NOT TARGET NetCDF::NetCDF)
		find_package(NetCDF "${PDI_REQUIRED_NETCDF_VERSION}" REQUIRED)
		if("${PDI_BUILD_NETCDF_PARALLEL}" AND NOT "PARALLEL4" IN_LIST NetCDF_FEATURES)
			message(FATAL_ERROR "Parallel NetCDF required, sequential NetCDF only found. Please set -DPDI_BUILD_NETCDF_PARALLEL=OFF to disable parallel NetCDF")
		endif()
	endif()

	if("${PDI_BUILD_JSON_PLUGIN}" AND NOT TARGET nlohmann_json::nlohmann_json)
		find_package(nlohmann_json "${PDI_REQUIRED_JSON_VERSION}" REQUIRED)
	endif()

	# parallel HDF5 or NetCDF still needs MPI, even if we didn't ask for parallel support. Look for it if we didn't before.
	if(("${HDF5_IS_PARALLEL}" OR "PARALLEL4" IN_LIST NetCDF_FEATURES) AND NOT TARGET MPI::MPI_C)
		list(APPEND MPI_COMPONENTS C CXX)
		if("${PDI_BUILD_TESTING}" AND "${PDI_BUILD_FORTRAN}")
			list(APPEND MPI_COMPONENTS Fortran)
		endif()
		find_package(MPI REQUIRED COMPONENTS ${MPI_COMPONENTS})
	endif()

	if("${PDI_BUILD_PDI_CORE}" AND ("${PDI_BUILD_FORTRAN}" OR "${PDI_BUILD_DOCUMENTATION}") AND NOT DEFINED ZPP_EXECUTABLE)
		add_subdirectory("${PDI_VENDORED_ZPP_DIR}" vendor/zpp)
	endif()

	if(("${PDI_BUILD_TESTING}" OR ("${PDI_BUILD_DECL_HDF5_PLUGIN}" AND "${PDI_BUILD_BENCHMARKING}")) AND NOT TARGET GTest::gtest)
		option(INSTALL_GTEST "Enable installation of googletest. (Projects embedding googletest may want to turn this OFF.)" OFF)
		add_subdirectory("${PDI_VENDORED_GOOGLETEST_DIR}" vendor/googletest EXCLUDE_FROM_ALL)
	endif()

	if("${PDI_BUILD_DECL_HDF5_PLUGIN}" AND "${PDI_BUILD_BENCHMARKING}" AND NOT TARGET benchmark::benchmark)
		option(BENCHMARK_ENABLE_TESTING "Enable testing of the benchmark library." OFF)
		option(BENCHMARK_ENABLE_WERROR "Build Release candidates with -Werror." OFF)
		option(BENCHMARK_ENABLE_INSTALL "Enable installation of benchmark. (Projects embedding benchmark may want to turn this OFF.)" OFF)
		add_subdirectory("${PDI_VENDORED_BENCHMARK_DIR}" vendor/benchmark EXCLUDE_FROM_ALL)
	endif()



	## The CMake modules

	include(CMakePackageConfigHelpers)

	include(GenerateExportHeader)

	if("${PDI_BUILD_PDI_CORE}" AND "${PDI_BUILD_FORTRAN}")
		include(DefaultKind)
	endif()

	if("${PDI_BUILD_TESTING}")
		include(GoogleTest)
	endif()

	if("${PDI_BUILD_TESTING}" OR ("${PDI_BUILD_DECL_HDF5_PLUGIN}" AND "${PDI_BUILD_BENCHMARKING}"))
		include(RuntestDir)
	endif()

	if("${PDI_BUILD_TESTING}" AND "${PDI_BUILD_FORTRAN}")
		include(MPIWithMod)
	endif()



	## and now for the actual content

	if("${PDI_BUILD_PDI_CORE}")
		message(STATUS " **Module**: ENABLED  PDI (-DPDI_BUILD_PDI_CORE=ON)")
		add_subdirectory(pdi)
	endif()

	set(PDI_PLUGIN_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/plugins")
	foreach(PDI_PLUGIN IN ITEMS DECL_HDF5 DECL_NETCDF JSON MPI PYCALL SERIALIZE SET_VALUE TIMER TRACE USER_CODE)
		if("${PDI_BUILD_${PDI_PLUGIN}_PLUGIN}")
			message(STATUS " **Module**: ENABLED  ${PDI_PLUGIN}_PLUGIN (-DPDI_BUILD_${PDI_PLUGIN}_PLUGIN=ON)")
			string(TOLOWER "${PDI_PLUGIN}" PDI_PLUGIN_DIR)
			add_subdirectory("plugins/${PDI_PLUGIN_DIR}")
		else()
			message(STATUS " **Module**: DISABLED ${PDI_PLUGIN}_PLUGIN (-DPDI_BUILD_${PDI_PLUGIN}_PLUGIN=OFF)")
		endif()
	endforeach()

	if("${PDI_BUILD_TESTING}")
		message(STATUS " **Module**: ENABLED  the example and test modules (-DPDI_BUILD_TESTING=ON)")
		add_subdirectory(example)
		add_subdirectory(tests/api_tests)
		add_subdirectory(tests/combination_tests)
	endif()

	return()
endif()



### Dependencies

include(SuperBuild)


if("${PDI_BUILD_PDI_CORE}" OR "${PDI_BUILD_TESTING}")
	sbuild_add_dependency(yaml "${PDI_USE_DEFAULT}"
		EMBEDDED_PATH "${PDI_VENDORED_YAML_DIR}"
		CMAKE_CACHE_ARGS
			"-DCMAKE_POLICY_VERSION_MINIMUM:STRING=3.10" # needed until upstream libyaml is updated (https://github.com/yaml/libyaml/pull/314)
			"-DINSTALL_BIN_DIR:STRING=${CMAKE_INSTALL_BINDIR}"
			"-DINSTALL_CMAKE_DIR:STRING=share/yaml/cmake"
			"-DINSTALL_INCLUDE_DIR:STRING=${CMAKE_INSTALL_INCLUDEDIR}"
			"-DINSTALL_LIB_DIR:STRING=${CMAKE_INSTALL_LIBDIR}"
		VERSION "${PDI_REQUIRED_YAML_VERSION}"
	)

	set(PARACONF_COMPONENTS "C")
	if("${PDI_BUILD_FORTRAN}")
		list(APPEND PARACONF_COMPONENTS "f90")
	endif()
	sbuild_add_dependency(paraconf "${PDI_USE_DEFAULT}"
		EMBEDDED_PATH "${PDI_VENDORED_PARACONF_DIR}"
		COMPONENTS ${PARACONF_COMPONENTS}
		DEPENDS "yaml"
		VERSION "${PDI_REQUIRED_PARACONF_VERSION}"
	)
endif()

if("${PDI_BUILD_PDI_CORE}")
	sbuild_add_dependency(spdlog "${PDI_USE_DEFAULT}"
		EMBEDDED_PATH "${PDI_VENDORED_SPDLOG_DIR}"
		CMAKE_CACHE_ARGS
			"-DSPDLOG_BUILD_BENCH:BOOL=OFF"
			"-DSPDLOG_BUILD_EXAMPLES:BOOL=OFF"
			"-DSPDLOG_BUILD_TESTS:BOOL=OFF"
			"-DSPDLOG_FMT_EXTERNAL:BOOL=OFF"
		VERSION "${PDI_REQUIRED_SPDLOG_VERSION}"
	)
endif()


## HDF5

if("${PDI_BUILD_DECL_HDF5_PLUGIN}" OR "${PDI_BUILD_DECL_NETCDF_PLUGIN}")
	
	set(HDF5_CMAKE_CACHE_ARGS)
	
	if("${PDI_BUILD_HDF5_PARALLEL}")
		set(HDF5_PREFER_PARALLEL ON)
		list(APPEND HDF5_CMAKE_CACHE_ARGS "-DHDF5_ENABLE_PARALLEL:BOOL=ON")
	else()
		set(HDF5_PREFER_PARALLEL OFF)
		list(APPEND HDF5_CMAKE_CACHE_ARGS "-DHDF5_ENABLE_PARALLEL:BOOL=OFF")
	endif()
	
	if("${PDI_BUILD_DECL_NETCDF_PLUGIN}")
		set(HDF5_COMPONENTS C HL)
		list(APPEND HDF5_CMAKE_CACHE_ARGS "-DHDF5_BUILD_HL_LIB:BOOL=ON")
	else()
		set(HDF5_COMPONENTS C)
		list(APPEND HDF5_CMAKE_CACHE_ARGS "-DHDF5_BUILD_HL_LIB:BOOL=OFF")
	endif()
	
	sbuild_add_dependency(HDF5 "${PDI_USE_DEFAULT}"
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
		if("${PDI_BUILD_HDF5_PARALLEL}" AND NOT "${HDF5_IS_PARALLEL}")
			message(FATAL_ERROR
				"You requested a parallel HDF5 build (-DPDI_BUILD_HDF5_PARALLEL=ON) but a sequential SYSTEM version has been found\n"
				"    you have the following options:\n"
				"     * build with this sequential HDF5 => pass `-DPDI_BUILD_HDF5_PARALLEL=OFF' to cmake\n"
				"     * build the EMBEDDED HDF5 from PDI distribution => pass `-DPDI_USE_HDF5=EMBEDDED' to cmake\n"
				"     * use another version of HDF5 from the system => pass `-DHDF5_ROOT=/path/to/hdf5/root/' to cmake"
			)
		endif()
	elseif(NOT "EMBEDDED" STREQUAL "${PDI_USE_HDF5}")
		message(FATAL_ERROR
			"HDF5 setup failed: A suitable SYSTEM version (>= ${PDI_REQUIRED_HDF5_VERSION}) could not be found.\n"
			"    you have the following options:\n"
			"     * build the EMBEDDED HDF5 from PDI distribution => pass `-DPDI_USE_HDF5=EMBEDDED' to cmake\n"
			"     * use another version of HDF5 from the system => pass `-DHDF5_ROOT=/path/to/hdf5/root/' to cmake"
		)
	endif()
endif()


## NetCDF

if("${PDI_BUILD_DECL_NETCDF_PLUGIN}")
	
	if("${PDI_BUILD_HDF5_PARALLEL}")
		set(NETCDF_CC "${MPI_C_COMPILER}")
	else()
		set(NETCDF_CC "${CMAKE_C_COMPILER}")
	endif()
	
	set(NETCDF_CMAKE_CACHE_ARGS)
	if("${PDI_BUILD_NETCDF_PARALLEL}")
		list(APPEND NETCDF_CMAKE_CACHE_ARGS "-DENABLE_PARALLEL4:BOOL=ON")
	else()
		list(APPEND NETCDF_CMAKE_CACHE_ARGS "-DENABLE_PARALLEL4:BOOL=OFF")
	endif()
	
	# Workaround NetCDF failing to add ZLIB include path in H5Deflate plugin
	find_package(ZLIB QUIET)
	if("${ZLIB_FOUND}")
		string(REPLACE ";" " -I" NETCDF_CFLAGS "-I ${ZLIB_INCLUDE_DIRS}")
	endif()
	
	sbuild_add_dependency(NetCDF "${PDI_USE_DEFAULT}"
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
		if("${PDI_BUILD_NETCDF_PARALLEL}" AND NOT "PARALLEL4" IN_LIST NetCDF_FEATURES)
			message(FATAL_ERROR
				"You requested a parallel NetCDF build (-DPDI_BUILD_NETCDF_PARALLEL=ON) but a sequential SYSTEM version has been found\n"
				"    you have the following options:\n"
				"     * build with this sequential NetCDF => pass `-DPDI_BUILD_NETCDF_PARALLEL=OFF' to cmake\n"
				"     * build the EMBEDDED NetCDF from PDI distribution => pass `-DPDI_USE_NetCDF=EMBEDDED' to cmake\n"
				"     * use another version of NetCDF from the system => pass `-DNetCDF_CFGSCRIPT=/path/to/netcdf/bin/nc-config' to cmake\n"
				"       you may need to change the discovery order so the config script is tried first =>\n"
				"       pass `-DNetCDF_FIND_STRATEGIES=\"CFGSCRIPT\"' to cmake"
			)
		endif()
	elseif(NOT "EMBEDDED" STREQUAL "${PDI_USE_NetCDF}")
		message(FATAL_ERROR
			"NetCDF setup failed: A suitable SYSTEM version (>= ${PDI_REQUIRED_NETCDF_VERSION}) could not be found.\n"
			"    you have the following options:\n"
			"     * build the EMBEDDED NetCDF from PDI distribution => pass `-DPDI_USE_NetCDF=EMBEDDED' to cmake\n"
			"     * use another version of NetCDF from the system => pass `-DNetCDF_CFGSCRIPT=/path/to/netcdf/bin/nc-config' to cmake\n"
			"       you may need to change the discovery order so the config script is tried first =>\n"
			"       pass `-DNetCDF_FIND_STRATEGIES=\"CFGSCRIPT\"' to cmake"
		)
	endif()
endif()


## PyBind11

if("${PDI_BUILD_PYTHON}")
	set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency" FORCE)
	set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules" FORCE)
	sbuild_add_dependency(pybind11 "${PDI_USE_DEFAULT}"
		EMBEDDED_PATH "${PDI_VENDORED_PYBIND11_DIR}"
		CMAKE_CACHE_ARGS
			"-DPYBIND11_TEST:BOOL=OFF"
			"-DPYBIND11_PYTHON_VERSION:STRING=${Python3_VERSION}"
		VERSION "${PDI_REQUIRED_PYBIND11_VERSION}"
	)
endif()


## JSON

if("${PDI_BUILD_JSON_PLUGIN}")
	sbuild_add_dependency(JSON "${PDI_USE_DEFAULT}"
		EMBEDDED_PATH "${PDI_VENDORED_JSON_DIR}"
		CMAKE_CACHE_ARGS
			"-DCMAKE_POLICY_VERSION_MINIMUM:STRING=3.10" # needed until upstream is updated
			"-DJSON_BuildTests:BOOL=OFF" # skip unit tests for json
		VERSION "${PDI_REQUIRED_JSON_VERSION}"
	)
endif()


### The main PDI sub-project

sbuild_add_self()

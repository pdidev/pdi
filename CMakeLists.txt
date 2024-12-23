#=============================================================================
# Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.16...3.29)
project(PDI LANGUAGES C CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")


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



# Modules to build

option(BUILD_BENCHMARKING       "Build PDI benchmarks" ON)
option(BUILD_DECL_HDF5_PLUGIN   "Build Decl'HDF5 plug-in" ON)
option(BUILD_DECL_NETCDF_PLUGIN "Build Decl'NetCDF plug-in" ON)
option(BUILD_FORTRAN            "Build with Fortran support" ON)
option(BUILD_HDF5_PARALLEL      "Build Decl'HDF5 in parallel mode" ON)
option(BUILD_MPI_PLUGIN         "Build MPI plug-in" ON)
option(BUILD_NETCDF_PARALLEL    "Build Decl'NetCDF in parallel mode" ON)
option(BUILD_PYCALL_PLUGIN      "Build Pycall plug-in" "${BUILD_UNSTABLE}")
option(BUILD_PYTHON             "Build with Python support" "${BUILD_UNSTABLE}")
option(BUILD_SET_VALUE_PLUGIN   "Build Set_value plug-in" ON)
option(BUILD_SERIALIZE_PLUGIN   "Build Serialize plug-in" ON)
option(BUILD_SHARED_LIBS        "Build shared libraries rather than static ones" ON)
option(BUILD_TRACE_PLUGIN       "Build Trace plugin" ON)
option(BUILD_USER_CODE_PLUGIN   "Build User-code plugin" ON)
option(BUILD_JSON_PLUGIN        "Build JSON plugin" OFF)
option(BUILD_DEISA_PLUGIN   	"Build Deisa plug-in" OFF)




### Default build type

if(NOT "${CMAKE_BUILD_TYPE}")
	set(CMAKE_BUILD_TYPE "${DEFAULT_BUILD_TYPE}" CACHE STRING "Choose the type of build, options are: None Debug Release(default) RelWithDebInfo MinSizeRel ..." FORCE)
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



### Sanity check

if(NOT "${BUILD_SHARED_LIBS}")
	message(FATAL_ERROR "Building PDI as a static lib is not supported. Please add the flag `BUILD_SHARED_LIBS=ON` to the cmake command line.")
else()
	set(CMAKE_POSITION_INDEPENDENT_CODE ON CACHE BOOL "" FORCE)
	set(BUILD_SHARED_LIBS ON CACHE BOOL "" FORCE)
	mark_as_advanced(CMAKE_POSITION_INDEPENDENT_CODE BUILD_SHARED_LIBS)
endif()


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

if("${BUILD_DEISA_PLUGIN}" AND NOT "${BUILD_PYTHON}")
	message(FATAL_ERROR
			"Deisa plugin is enabled but Python support is disabled.\n"
			"    you have the following options:\n"
			"     * enable Python support => pass `-DBUILD_PYTHON=ON' to cmake\n"
			"     * do not build the Deisa plugin => pass `-DBUILD_DEISA_PLUGIN=OFF' to cmake"
	)
endif()


### Dependencies

set(_REAL_PROJECT_NAME "${PROJECT_NAME}")
set(PROJECT_NAME pdi)
include(GNUInstallDirs)
set(PROJECT_NAME "${_REAL_PROJECT_NAME}")
set(INSTALL_PDIDATADIR "${CMAKE_INSTALL_DATADIR}/pdi" CACHE PATH "PDI data directory (DATADIR/pdi)")
set(INSTALL_CMAKEDIR   "${INSTALL_PDIDATADIR}/cmake"  CACHE PATH "cmake modules (PDIDATADIR/cmake)")
set(INSTALL_PDIPLUGINDIR  "${CMAKE_INSTALL_LIBDIR}/pdi/plugins_${PDI_VERSION}" CACHE PATH "cmake modules (LIBDIR/pdi/plugins_${PDI_VERSION})")
include(SuperBuild)

set(PDI_BUILD_TESTING "${BUILD_TESTING}")
set(BUILD_TESTING OFF)


## Fortran

if("${BUILD_FORTRAN}") # and (paraconf or PDI or ...)
	enable_language(Fortran)
endif()


## Python3

if("${BUILD_PYTHON}")
	find_package(Python3 3.8.2 REQUIRED COMPONENTS Interpreter Development)
elseif("${BUILD_FORTRAN}")
	find_package(Python3 3.8.2 REQUIRED COMPONENTS Interpreter)
endif()


## Threads

if("${BUILD_TESTING}")
	find_package(Threads REQUIRED)
endif()


## MPI

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
if(NOT "x${MPI_COMPONENTS}x" STREQUAL xx)
	list(REMOVE_DUPLICATES MPI_COMPONENTS)
	find_package(MPI REQUIRED COMPONENTS ${MPI_COMPONENTS})
endif()


## Yaml

# if ( paraconf )

sbuild_add_dependency(yaml "${USE_DEFAULT}"
	SOURCE_DIR "vendor/libyaml-0.2.5"
	VERSION 0.2.2
)


## Paraconf

set(PARACONF_COMPONENTS "C")
if("${BUILD_FORTRAN}")
	list(APPEND PARACONF_COMPONENTS "f90")
endif()
# if ( PDI or ... )
sbuild_add_dependency(paraconf "${USE_DEFAULT}"
	SOURCE_DIR "vendor/paraconf-1.0.0/paraconf"
	VERSION 1.0.0
	FIND_PACKAGE_ARGS
	COMPONENTS ${PARACONF_COMPONENTS}
)


## spdlog

# if ( PDI or ... )
set(SPDLOG_BUILD_BENCH OFF)
set(SPDLOG_BUILD_EXAMPLES OFF)
set(SPDLOG_BUILD_TESTS OFF)
set(SPDLOG_FMT_EXTERNAL OFF)
sbuild_add_dependency(spdlog "${USE_DEFAULT}"
	SOURCE_DIR "vendor/spdlog-1.14.1"
	VERSION 1.5.0
)


## Doxygen

if("${BUILD_DOCUMENTATION}")
	find_package(Doxygen 1.8.17 REQUIRED)
endif()


## HDF5

if("${BUILD_DECL_HDF5_PLUGIN}" OR "${BUILD_DECL_NETCDF_PLUGIN}")
	
	if("${BUILD_HDF5_PARALLEL}")
		set(HDF5_PREFER_PARALLEL ON)
	else()
		set(HDF5_PREFER_PARALLEL OFF)
	endif()
	
	if("${BUILD_DECL_NETCDF_PLUGIN}")
		set(HDF5_COMPONENTS C HL)
		set(HDF5_BUILD_HL_LIB ON)
	else()
		set(HDF5_COMPONENTS C)
		set(HDF5_BUILD_HL_LIB OFF)
	endif()
	
	set(HDF5_BUILD_EXAMPLES OFF)
	set(HDF5_BUILD_TOOLS ON)
	set(HDF5_BUILD_UTILS OFF)
	
	find_package(ZLIB)
	
	sbuild_add_dependency(HDF5 "${USE_DEFAULT}"
		SOURCE_DIR "vendor/hdf5-1.12.3"
		VERSION 1.10.4
		FIND_PACKAGE_ARGS
		COMPONENTS ${HDF5_COMPONENTS}
	)
	
	if("${HDF5_FOUND}")
		if(1.10.4 VERSION_GREATER "${HDF5_VERSION}")
			message(FATAL_ERROR "HDF5 version ${HDF5_VERSION} found less than required 1.10.4")
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
	endif()
endif()


## NetCDF

if("${BUILD_DECL_NETCDF_PLUGIN}")
	
	if("${BUILD_NETCDF_PARALLEL}")
		set(ENABLE_PARALLEL4 ON)
	else()
		set(ENABLE_PARALLEL4 OFF)
	endif()
	
	set(BUILD_TESTSETS OFF)
	set(BUILD_UTILITIES OFF)
	set(ENABLE_BASH_SCRIPT_TESTING OFF)
	set(ENABLE_BYTERANGE OFF)
	set(ENABLE_DAP OFF)
	set(ENABLE_DAP4 OFF)
	set(ENABLE_EXAMPLES OFF)
	set(ENABLE_FILTER_TESTING OFF)
	set(ENABLE_TESTS OFF)
	sbuild_add_dependency(NetCDF "${USE_DEFAULT}"
		SOURCE_DIR "vendor/netcdf-c-4.9.2"
		VERSION 4.7.3
	)
	if("${NETCDF_FOUND}")
		if("${BUILD_NETCDF_PARALLEL}" AND NOT "PARALLEL4" IN_LIST NetCDF_FEATURES)
			message(FATAL_ERROR
				"You requested a parallel NetCDF build (-DBUILD_NETCDF_PARALLEL=ON) but a sequential SYSTEM version has been found\n"
				"    you have the following options:\n"
				"     * build with this sequential NetCDF => pass `-DBUILD_NETCDF_PARALLEL=OFF' to cmake\n"
				"     * build the EMBEDDED NetCDF from PDI distribution => pass `-DUSE_NETCDF=EMBEDDED' to cmake\n"
				"     * use another version of NetCDF from the system => pass `-DNetCDF_CFGSCRIPT=/path/to/netcdf/bin/nc-config' to cmake"
			)
		endif()
	endif()
endif()


## PyBind11

if("${BUILD_PYTHON}")
	set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency" FORCE)
	set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules" FORCE)
	set(PYBIND11_TEST OFF)
	sbuild_add_dependency(pybind11 "${USE_DEFAULT}"
		SOURCE_DIR "vendor/pybind11-2.13.1"
		VERSION 2.4.3
	)
endif()


## ZPP

if("${BUILD_FORTRAN}")
	add_subdirectory("vendor/zpp-1.0.16/" EXCLUDE_FROM_ALL)
endif()


### Own modules

set(BUILD_TESTING "${PDI_BUILD_TESTING}")


# Version

file(READ VERSION PDI_VERSION)
string(STRIP "${PDI_VERSION}" PDI_VERSION)
string(REGEX MATCH "([0-9A-Za-z]+)\\.([0-9A-Za-z]+)\\.([0-9A-Za-z]+)(-([0-9A-Za-z]+))?" "\\1;\\2;\\3" "${PDI_VERSION}")
set(PDI_VERSION_MAJOR "${CMAKE_MATCH_1}")
set(PDI_VERSION_MINOR "${CMAKE_MATCH_2}")
set(PDI_VERSION_PATCH "${CMAKE_MATCH_3}")
set(PDI_VERSION_MODIF "${CMAKE_MATCH_5}")
if("git" STREQUAL "${PDI_VERSION_MODIF}" OR "alpha" STREQUAL "${PDI_VERSION_MODIF}")
	execute_process(COMMAND "${PDI_SOURCE_DIR}/cmake/version-uid" "${PDI_VERSION_MODIF}" WORKING_DIRECTORY "${PDI_SOURCE_DIR}" OUTPUT_VARIABLE PDI_VERSION_MODIF OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()
set(PDI_VERSION "${PDI_VERSION_MAJOR}.${PDI_VERSION_MINOR}.${PDI_VERSION_PATCH}")
set(PDI_VERSION "${PDI_VERSION}${PDI_VERSION_MODIF}")

# pdirun

file(COPY scripts/pdirun_intree DESTINATION "${CMAKE_BINARY_DIR}/")
add_executable(pdirun IMPORTED)
add_executable(PDI::pdirun IMPORTED)
set_target_properties(pdirun PDI::pdirun PROPERTIES IMPORTED_LOCATION "${CMAKE_BINARY_DIR}/pdirun_intree")
install(PROGRAMS scripts/pdirun
		DESTINATION "${CMAKE_INSTALL_BINDIR}"
		COMPONENT Runtime
)

configure_file(scripts/env.sh.in env.sh @ONLY)
configure_file(scripts/env.xsh env.bash @ONLY)
configure_file(scripts/env.xsh env.zsh @ONLY)
install(FILES
			"${PDI_BINARY_DIR}/env.bash"
			"${PDI_BINARY_DIR}/env.sh"
			"${PDI_BINARY_DIR}/env.zsh"
		DESTINATION "${INSTALL_PDIDATADIR}"
		COMPONENT Runtime
)


# pdi

add_subdirectory(src)

sbuild_add_module(BENCHMARKING
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/benchmarks"
)

sbuild_add_module(DOCUMENTATION
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/docs"
)

sbuild_add_module(MPI_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/mpi"
)

sbuild_add_module(DECL_HDF5_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_hdf5"
)

sbuild_add_module(DECL_NETCDF_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_netcdf"
)


sbuild_add_module(JSON_PLUGIN
	ENABLE_BUILD_FLAG BUILD_JSON_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/json"
	DEPENDS PDI JSON
	SUBSTEPS indent test
)


sbuild_add_module(PYCALL_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/pycall"
)

sbuild_add_module(SET_VALUE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/set_value"
)

sbuild_add_module(SERIALIZE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/serialize"
)

sbuild_add_module(TRACE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/trace"
)

sbuild_add_module(USER_CODE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/user_code"
)

sbuild_add_module(DEISA_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/deisa"
)

sbuild_add_module(PDI_EXAMPLE
	ENABLE_BUILD_FLAG BUILD_TESTING
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/example"
)

sbuild_add_module(PDI_TESTS
	ENABLE_BUILD_FLAG BUILD_TESTING
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/tests"
)

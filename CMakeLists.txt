#=============================================================================
# Copyright (C) 2015-2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.10...3.25)
project(PDI_DIST LANGUAGES C CXX)



### Build options

## Global options

set(DIST_PROFILE "User" CACHE STRING "Profile to use for PDI distribution build. Options are: User, Devel")
set_property(CACHE DIST_PROFILE PROPERTY STRINGS User Devel)

if(User STREQUAL "${DIST_PROFILE}")
	option(BUILD_DOCUMENTATION "Build documentation" OFF)
	option(BUILD_INDENT        "Enable automatic code indentation" OFF)
	option(BUILD_TESTING       "Build tests" OFF)
	option(BUILD_UNSTABLE      "Build all features by default including those not stable yet" OFF)
	set(DEFAULT_BUILD_TYPE "Release")
elseif(Devel STREQUAL "${DIST_PROFILE}")
	option(BUILD_DOCUMENTATION "Build documentation" ON)
	option(BUILD_INDENT        "Enable automatic code indentation" ON)
	option(BUILD_TESTING       "Build tests" ON)
	option(BUILD_UNSTABLE      "Build all features by default including those not stable yet" ON)
	set(DEFAULT_BUILD_TYPE "Debug")
else()
	message(FATAL_ERROR "DIST_PROFILE should be set to one of: User, Devel")
endif()

set(USE_DEFAULT AUTO CACHE STRING "Default version of libraries to use; this can be 1) EMBEDDED to use the provided version, 2) SYSTEM to use an already installed version (you can use CMAKE_PREFIX_PATH to specify where to look, or 3) AUTO to use SYSTEM if available and EMBEDDED otherwise")



# Modules to build

option(BUILD_BENCHMARKING       "Build PDI benchmarks" ON)
option(BUILD_CFG_VALIDATOR      "Build config validation script" "${BUILD_UNSTABLE}")
option(BUILD_DECL_HDF5_PLUGIN   "Build Decl'HDF5 plug-in" ON)
option(BUILD_DECL_NETCDF_PLUGIN "Build Decl'NetCDF plug-in" "${BUILD_UNSTABLE}")
option(BUILD_DECL_SION_PLUGIN   "Build Decl'SION plug-in" "${BUILD_UNSTABLE}")
option(BUILD_FLOWVR_PLUGIN      "Build FlowVR plug-in" "${BUILD_UNSTABLE}")
option(BUILD_FORTRAN            "Build with Fortran support" ON)
option(BUILD_FTI_PLUGIN         "Build FTI plug-in." "${BUILD_UNSTABLE}")
option(BUILD_HDF5_PARALLEL      "Build Decl'HDF5 in parallel mode" ON)
option(BUILD_MPI_PLUGIN         "Build MPI plug-in" ON)
option(BUILD_NETCDF_PARALLEL    "Build Decl'NetCDF in parallel mode" ON)
option(BUILD_PYCALL_PLUGIN      "Build Pycall plug-in" "${BUILD_UNSTABLE}")
option(BUILD_PYTHON             "Build with Python support" "${BUILD_UNSTABLE}")
option(BUILD_SET_VALUE_PLUGIN   "Build Set_value plug-in" ON)
option(BUILD_SERIALIZE_PLUGIN   "Build Serialize plug-in" ON)
option(BUILD_SHARED_LIBS        "Build shared libraries rather than static ones" ON)
option(BUILD_TEST_PLUGIN        "Build Test plugin" "${BUILD_UNSTABLE}")
option(BUILD_TRACE_PLUGIN       "Build Trace plugin" ON)
option(BUILD_USER_CODE_PLUGIN   "Build User-code plugin" ON)



### Default build type

if(NOT "${CMAKE_BUILD_TYPE}")
	set(CMAKE_BUILD_TYPE "${DEFAULT_BUILD_TYPE}" CACHE STRING "Choose the type of build, options are: None Debug Release(default) RelWithDebInfo MinSizeRel ..." FORCE)
endif()
message(STATUS " **Profile**: Distribution profile is: `${DIST_PROFILE}' (-DDIST_PROFILE=${DIST_PROFILE})")
message(STATUS " **Profile**: Build type is: `${CMAKE_BUILD_TYPE}' (-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE})")
message(STATUS " **Profile**: Prefix path is: `${CMAKE_PREFIX_PATH}' (-DCMAKE_PREFIX_PATH=${CMAKE_PREFIX_PATH})")
message(STATUS " **Profile**: Default use is: `${USE_DEFAULT}' (-DUSE_DEFAULT=${USE_DEFAULT})")
message(STATUS " **Profile**: Unstable activation is: `${BUILD_UNSTABLE}' (-DBUILD_UNSTABLE=${BUILD_UNSTABLE})")
foreach(FEATURE FORTRAN HDF5_PARALLEL INDENT PYTHON TESTING)
	set(FEATURE_ENABLED "DISABLED")
	if(${BUILD_${FEATURE}})
		set(FEATURE_ENABLED "ENABLED ")
	endif()
	message(STATUS " **Feature**: ${FEATURE_ENABLED} ${FEATURE} (-DBUILD_${FEATURE}=${BUILD_${FEATURE}})")
endforeach()



### Include sub-projects find modules

list(APPEND CMAKE_MODULE_PATH
	"${CMAKE_CURRENT_SOURCE_DIR}/cmake"
	"${CMAKE_CURRENT_SOURCE_DIR}/pdi/cmake"
	"${CMAKE_CURRENT_SOURCE_DIR}/plugins/test/cmake"
	"${CMAKE_CURRENT_SOURCE_DIR}/plugins/trace/cmake"
	"${CMAKE_CURRENT_SOURCE_DIR}/plugins/user_code/cmake")
if("${BUILD_CFG_VALIDATOR}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/tools/pdicfg_validator/cmake")
endif()
if("${BUILD_DECL_HDF5_PLUGIN}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_hdf5/cmake")
endif()
if("${BUILD_DECL_NETCDF_PLUGIN}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_netcdf/cmake")
endif()
if("${BUILD_DECL_SION_PLUGIN}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_sion/cmake")
endif()
if("${BUILD_FLOWVR_PLUGIN}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/plugins/flowvr/cmake")
endif()
if("${BUILD_FTI_PLUGIN}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/plugins/fti/cmake")
endif()
if("${BUILD_MPI_PLUGIN}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/plugins/mpi/cmake")
endif()
if("${BUILD_PYCALL_PLUGIN}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/plugins/pycall/cmake")
endif()
if("${BUILD_TESTING}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/example/cmake")
endif()



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



### Subtargets

if("${BUILD_INDENT}")
	add_custom_target(indent)
endif()



### Dependencies

include(GNUInstallDirs)
include(SuperBuild)
sbuild_get_env(SBUILD_PATH PATH)
sbuild_get_env(SBUILD_CPATH CPATH)
sbuild_get_env(SBUILD_LIBRARY_PATH LIBRARY_PATH)
sbuild_get_env(SBUILD_LD_LIBRARY_PATH LD_LIBRARY_PATH)


## Fortran

if("${BUILD_FORTRAN}") # and (paraconf or PDI or ...)
	enable_language(Fortran)
endif()


## Python3

if("${BUILD_PYTHON}")
	find_package(Python3Path 3.6 REQUIRED COMPONENTS Interpreter Development)
elseif("${BUILD_CFG_VALIDATOR}" OR "${BUILD_FLOWVR_PLUGIN}" OR "${BUILD_FORTRAN}")
	find_package(Python3Path 3.6 REQUIRED COMPONENTS Interpreter)
endif()


## Zpp

if("${BUILD_FORTRAN}")
	sbuild_add_dependency(Zpp EMBEDDED
		BUILD_DEPENDENCY
		EMBEDDED_PATH "vendor/zpp-1.0.15"
		VERSION 1.0.15
		BUILD_IN_SOURCE ON
		CONFIGURE_COMMAND "${Python3_EXECUTABLE}" "-m" "venv" "<INSTALL_DIR>/lib/zpp-venv"
		BUILD_COMMAND "<INSTALL_DIR>/lib/zpp-venv/bin/python" "<SOURCE_DIR>/setup.py" build
		INSTALL_COMMAND "<INSTALL_DIR>/lib/zpp-venv/bin/python" "<SOURCE_DIR>/setup.py" install "--install-data=<INSTALL_DIR>"
	)
endif()


## Threads

if("${BUILD_TESTING}")
	sbuild_add_dependency(Threads SYSTEM)
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
if("${BUILD_FTI_PLUGIN}" OR "${BUILD_MPI_PLUGIN}" OR "${BUILD_DECL_SION_PLUGIN}")
	list(APPEND MPI_COMPONENTS CXX)
endif()
if(NOT "x${MPI_COMPONENTS}x" STREQUAL xx)
	list(REMOVE_DUPLICATES MPI_COMPONENTS)
	sbuild_add_dependency(MPI SYSTEM
		COMPONENTS ${MPI_COMPONENTS}
		MODULE_VARS MPI_C_COMPILER
	)
endif()


## Yaml

# if ( paraconf or PyYAML )
sbuild_add_dependency(yaml "${USE_DEFAULT}"
	EMBEDDED_PATH "vendor/libyaml-0.2.2"
	CMAKE_CACHE_ARGS
		"-DBUILD_TESTING:BOOL=OFF"
		"-DINSTALL_LIB_DIR:STRING=${CMAKE_INSTALL_LIBDIR}"
		"-DINSTALL_BIN_DIR:STRING=${CMAKE_INSTALL_BINDIR}"
		"-DINSTALL_INCLUDE_DIR:STRING=${CMAKE_INSTALL_INCLUDEDIR}"
		"-DINSTALL_CMAKE_DIR:STRING=share/yaml/cmake"
	VERSION 0.1.7
)


## Paraconf

set(PARACONF_COMPONENTS "C")
if("${BUILD_FORTRAN}")
	list(APPEND PARACONF_COMPONENTS "f90")
endif()
# if ( PDI or ... )
sbuild_add_dependency(paraconf "${USE_DEFAULT}"
	EMBEDDED_PATH "vendor/paraconf-1.0.0"
	COMPONENTS ${PARACONF_COMPONENTS}
	SOURCE_SUBDIR "paraconf"
	CMAKE_CACHE_ARGS
		"-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=TRUE"
		"-DUSE_yaml:STRING=SYSTEM"
		"-DBUILD_TESTING:BOOL=OFF"
	DEPENDS "yaml"
	VERSION 1.0.0
)


## spdlog

# if ( PDI or ... )
sbuild_add_dependency(spdlog "${USE_DEFAULT}"
	EMBEDDED_PATH "vendor/spdlog-1.5.0"
	CMAKE_CACHE_ARGS
		"-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON"
		"-DSPDLOG_BUILD_BENCH:BOOL=OFF"
		"-DSPDLOG_BUILD_EXAMPLES:BOOL=OFF"
		"-DSPDLOG_BUILD_TESTS:BOOL=OFF"
		"-DSPDLOG_FMT_EXTERNAL:BOOL=OFF"
	VERSION 1.5.0
)


## Doxygen

if("${BUILD_DOCUMENTATION}")
	sbuild_add_dependency(Doxygen "${USE_DEFAULT}"
		BUILD_DEPENDENCY
		EMBEDDED_PATH "vendor/doxygen-1.8.15"
		VERSION 1.8.12
		OPTIONAL_COMPONENTS dot
	)
endif()


## Astyle

if("${BUILD_INDENT}")
	sbuild_add_dependency(Astyle "${USE_DEFAULT}"
		BUILD_DEPENDENCY
		EMBEDDED_PATH "vendor/astyle"
		PATCH_COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${CMAKE_CURRENT_SOURCE_DIR}/vendor/astyle/CMakeLists.txt" "<SOURCE_DIR>/CMakeLists.txt"
		CMAKE_CACHE_ARGS "-DBUILD_SHARED_LIBS:BOOL=OFF"
		VERSION 3.1
	)
endif()


## GTest

if(${BUILD_TESTING})
	# find_package(GMock REQUIRED)
	sbuild_add_dependency(GTest EMBEDDED
		EMBEDDED_PATH "vendor/googletest-release-1.8.0"
		BUILD_DEPENDENCY
		CMAKE_CACHE_ARGS
			"-DBUILD_TESTING:BOOL=OFF"
			# set BUILD_SHARED_LIBS=OFF to workaround https://github.com/google/googletest/issues/930
			"-DBUILD_SHARED_LIBS:BOOL=OFF"
		VERSION 1.8.0
	)
endif()


# Google benchmark
if(${BUILD_BENCHMARKING})
	sbuild_add_dependency(benchmark EMBEDDED
		EMBEDDED_PATH "vendor/benchmark-1.5.5"
		BUILD_DEPENDENCY
		CMAKE_CACHE_ARGS
			"-DCMAKE_BUILD_TYPE:STRING=Release"
			"-DBENCHMARK_BUILD_32_BITS:BOOL=OFF"
			"-DBENCHMARK_ENABLE_EXCEPTIONS:BOOL=ON"
			"-DBENCHMARK_ENABLE_INSTALL:BOOL=ON"
			"-DBENCHMARK_ENABLE_LIBPFM:BOOL=OFF"
			"-DBENCHMARK_ENABLE_LTO:BOOL=OFF"
			"-DBENCHMARK_ENABLE_TESTING:BOOL=OFF"
			"-DBENCHMARK_USE_LIBCXX:BOOL=OFF"
		VERSION 1.5.0
	)
	set(BENCHMARKING_DEPENDENCY benchmark)
endif()


## OpenGL

if("${BUILD_FLOWVR_PLUGIN}" AND "${BUILD_TESTING}")
	set(OpenGL_GL_PREFERENCE LEGACY)
	sbuild_add_dependency(OpenGL SYSTEM)
	sbuild_add_dependency(GLUT SYSTEM)
endif()


## FTI

if("${BUILD_FTI_PLUGIN}")
	sbuild_add_dependency(FTI "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/fti-1.6"
		CMAKE_CACHE_ARGS
			"-DENABLE_FORTRAN:BOOL=OFF"
			"-DENABLE_EXAMPLES:BOOL=OFF"
			"-DENABLE_TESTS:BOOL=OFF"
			"-DENABLE_DOCU:BOOL=OFF"
		VERSION 1.6
	)
endif()


## FlowVR

if("${BUILD_FLOWVR_PLUGIN}")
	sbuild_add_dependency(FlowVR "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/flowvr-ex-v2.3.2"
		CMAKE_CACHE_ARGS
			"-DENABLE_EXAMPLES:BOOL=OFF"
			"-DENABLE_TESTS:BOOL=OFF"
			"-DENABLE_DOCU:BOOL=OFF"
			"-DOpenGL_GL_PREFERENCE:STRING=LEGACY"
		# VERSION 2.3.2 Finding FlowVR version doesn't work with 2.3
	)
endif()


## HDF5

if("${BUILD_DECL_HDF5_PLUGIN}" OR "${BUILD_DECL_NETCDF_PLUGIN}")
	set(HDF5_CONFIGURE_OPTS)
	set(HDF5_BUILD_ENV)
	if("${CMAKE_BUILD_TYPE}" STREQUAL "Debug")
		list(APPEND HDF5_CONFIGURE_OPTS "--enable-build-mode=debug")
	else()
		list(APPEND HDF5_CONFIGURE_OPTS "--enable-build-mode=production")
	endif()
	if("${BUILD_HDF5_PARALLEL}")
		set(HDF5_PREFER_PARALLEL "ON")
		list(APPEND HDF5_CONFIGURE_OPTS "--enable-parallel")
		set(HDF5_BUILD_ENV "env" "CC=${MPI_C_COMPILER}")
	else()
		set(HDF5_PREFER_PARALLEL "OFF")
		list(APPEND HDF5_CONFIGURE_OPTS "--disable-parallel")
	endif()
	if("${BUILD_DECL_NETCDF_PLUGIN}")
		set(HDF5_COMPONENTS C HL)
		list(APPEND HDF5_CONFIGURE_OPTS "--enable-hl")
	else()
		set(HDF5_COMPONENTS C)
		list(APPEND HDF5_CONFIGURE_OPTS "--disable-hl")
	endif()
	
	sbuild_add_dependency(HDF5 "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/hdf5-1.12.0"
		COMPONENTS ${HDF5_COMPONENTS}
		MODULE_VARS HDF5_IS_PARALLEL HDF5_C_INCLUDE_DIRS HDF5_C_LIBRARIES HDF5_VERSION
		BUILD_IN_SOURCE 1
		CONFIGURE_COMMAND ${HDF5_BUILD_ENV}
			"<SOURCE_DIR>/configure"
			#TODO:  prefix should point to the final install dir, we should use a staging mechanism
			"--prefix=<INSTALL_DIR>"
			"--libdir=<INSTALL_DIR>/${CMAKE_INSTALL_LIBDIR}"
			"--disable-tests"
			"--disable-static"
			"--disable-tools"
			"--disable-sharedlib-rpath"
			${HDF5_CONFIGURE_OPTS}
	)
	if("${HDF5_FOUND}")
		if(1.10 VERSION_GREATER "${HDF5_VERSION}")
			message(FATAL_ERROR "HDF5 version ${HDF5_VERSION} found less than required 1.10.0")
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
	if("${BUILD_HDF5_PARALLEL}")
		set(NETCDF_CC "${MPI_C_COMPILER}")
	else()
		set(NETCDF_CC "${CMAKE_C_COMPILER}")
	endif()
	if("${BUILD_NETCDF_PARALLEL}")
		set(NETCDF_MIN_VERSION "4.6.2")
	else()
		set(NETCDF_MIN_VERSION "4.6.0")
	endif()
	set(NETCDF_CPATH "${SBUILD_CPATH}")
	set(NETCDF_LIBRARY_PATH "${SBUILD_LIBRARY_PATH}")
	if("${HDF5_FOUND}")
		string(REPLACE ";" ":" NETCDF_CPATH "${SBUILD_CPATH}:${HDF5_C_INCLUDE_DIRS}")
		foreach(LIB IN LISTS HDF5_C_LIBRARIES)
			get_filename_component(LIB_DIR "${LIB}" DIRECTORY)
			set(NETCDF_LIBRARY_PATH "${NETCDF_LIBRARY_PATH}:${LIB_DIR}")
		endforeach()
	endif()
	
	sbuild_add_dependency(NetCDF "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/netcdf-c-4.8.1"
		DEPENDS HDF5
		BUILD_IN_SOURCE 1
		ENV "CC=${NETCDF_CC}" "CPATH=${NETCDF_CPATH}" "LIBRARY_PATH=${NETCDF_LIBRARY_PATH}"
		CONFIGURE_COMMAND "<SOURCE_DIR>/configure"
			#TODO:  prefix should point to the final install dir, we should use a staging mechanism
			--prefix=<INSTALL_DIR>
			--disable-static
			--enable-netcdf-4 
			--disable-dap
			--disable-utilities
			--disable-testsets
		VERSION "${NETCDF_MIN_VERSION}"
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
	# Workaround pybind11 incompatibility with cmake<3.12 if Python3_FOUND is set
	set(_PDI_DIST_Python3_FOUND "${Python3_FOUND}")
	unset(Python3_FOUND)
	set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency" FORCE)
	set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules" FORCE)
	sbuild_add_dependency(pybind11 "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/pybind11-2.11.1"
		CMAKE_CACHE_ARGS
			"-DBUILD_TESTING:BOOL=OFF"
			"-DPYBIND11_TEST:BOOL=OFF"
			"-DPYBIND11_PYTHON_VERSION:STRING=${Python3_VERSION}"
		VERSION 2.11.1
	)
	set(Python3_FOUND "${_PDI_DIST_Python3_FOUND}")
endif()


## PyYAML

if("${BUILD_CFG_VALIDATOR}" OR "${BUILD_FLOWVR_PLUGIN}")
	execute_process(COMMAND "${Python3_EXECUTABLE}" -m pip install --system
		RESULT_VARIABLE PIP_INSTALL_ERROR
		OUTPUT_QUIET
		ERROR_QUIET)
	if("${PIP_INSTALL_ERROR}" EQUAL 0)
		set(PIP_SYSTEM "--system")
	endif()
	sbuild_get_env(CPATH CPATH)
	sbuild_get_env(LIBRARY_PATH LIBRARY_PATH)
	sbuild_add_dependency(PyYAML "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/PyYAML-6.0.1"
		CONFIGURE_COMMAND "true"
		BUILD_COMMAND "true"
		INSTALL_COMMAND env
			"CPATH=${CPATH}"
			"LIBRARY_PATH=${LIBRARY_PATH}"
			"LD_LIBRARY_PATH=${LIBRARY_PATH}"
			"${Python3_EXECUTABLE}" -m pip install
			${PIP_SYSTEM}
			--prefix "<INSTALL_DIR>"
			--no-cache-dir
			--ignore-installed
			install
			"<SOURCE_DIR>"
		DEPENDS yaml
	)
endif()


## SIONlib

if("${BUILD_DECL_SION_PLUGIN}")
	sbuild_add_dependency(SIONlib "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/sionlib-1.7.6"
		DEPENDS MPI
		BUILD_IN_SOURCE 1
		CONFIGURE_COMMAND "<SOURCE_DIR>/configure"
			--prefix=<INSTALL_DIR>
			--disable-fortran
			--disable-parutils
			--disable-cxx
		VERSION 1.7.6
	)
endif()



### Own modules

if("${BUILD_INDENT}")
	set(ASTYLE_DEPENDENCY Astyle)
endif()

set(PDI_DEPENDS_LIST paraconf spdlog "${ASTYLE_DEPENDENCY}" "${BENCHMARKING_DEPENDENCY}")
if("${BUILD_DOCUMENTATION}")
	list(APPEND PDI_DEPENDS_LIST Doxygen)
endif()
if("${BUILD_TESTING}")
	list(APPEND PDI_DEPENDS_LIST GTest)
endif()
if("${BUILD_FORTRAN}")
	list(APPEND PDI_DEPENDS_LIST Zpp)
endif()
if("${BUILD_PYTHON}")
	list(APPEND PDI_DEPENDS_LIST pybind11)
endif()
sbuild_add_module(PDI
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/pdi"
	DEPENDS ${PDI_DEPENDS_LIST}
	SUBSTEPS indent test
)


sbuild_add_module(PDICFG_VALIDATOR
	ENABLE_BUILD_FLAG BUILD_CFG_VALIDATOR
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/tools/pdicfg_validator"
	DEPENDS PyYAML
	SUBSTEPS test
)
if("${BUILD_CFG_VALIDATOR}")
	set(PDICFG_DEPENDENCY PDICFG_VALIDATOR)
endif()


sbuild_add_module(MPI_PLUGIN
	ENABLE_BUILD_FLAG BUILD_MPI_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/mpi"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)


sbuild_add_module(DECL_HDF5_PLUGIN
	ENABLE_BUILD_FLAG BUILD_DECL_HDF5_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_hdf5"
	DEPENDS PDI HDF5 ${ASTYLE_DEPENDENCY} ${PDICFG_DEPENDENCY} ${BENCHMARKING_DEPENDENCY}
	SUBSTEPS indent test
)


sbuild_add_module(DECL_SION_PLUGIN
	ENABLE_BUILD_FLAG BUILD_DECL_SION_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_sion"
	DEPENDS PDI SIONlib ${ASTYLE_DEPENDENCY} ${PDICFG_DEPENDENCY}
	SUBSTEPS indent test
)

sbuild_add_module(DECL_NETCDF_PLUGIN
	ENABLE_BUILD_FLAG BUILD_DECL_NETCDF_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_netcdf"
	DEPENDS PDI NetCDF ${ASTYLE_DEPENDENCY} ${PDICFG_DEPENDENCY}
	SUBSTEPS indent test
)

sbuild_add_module(FTI_PLUGIN
	ENABLE_BUILD_FLAG BUILD_FTI_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/fti"
	DEPENDS PDI FTI ${ASTYLE_DEPENDENCY} ${PDICFG_DEPENDENCY}
	SUBSTEPS indent test
)


sbuild_add_module(FLOWVR_PLUGIN
	ENABLE_BUILD_FLAG BUILD_FLOWVR_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/flowvr"
	DEPENDS PDI FlowVR PyYAML ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)


sbuild_add_module(PYCALL_PLUGIN
	ENABLE_BUILD_FLAG BUILD_PYCALL_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/pycall"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)

sbuild_add_module(SET_VALUE_PLUGIN
	ENABLE_BUILD_FLAG BUILD_SET_VALUE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/set_value"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)

sbuild_add_module(SERIALIZE_PLUGIN
	ENABLE_BUILD_FLAG BUILD_SERIALIZE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/serialize"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)

sbuild_add_module(TEST_PLUGIN
	ENABLE_BUILD_FLAG BUILD_TEST_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/test"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent
)


sbuild_add_module(TRACE_PLUGIN
	ENABLE_BUILD_FLAG BUILD_TRACE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/trace"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent
)


sbuild_add_module(USER_CODE_PLUGIN
	ENABLE_BUILD_FLAG BUILD_USER_CODE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/user_code"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)

if ("${BUILD_FLOWVR_PLUGIN}")
	set(FLOWVR_DEPENDENCY FlowVR)
endif()

sbuild_add_module(PDI_EXAMPLE
	ENABLE_BUILD_FLAG BUILD_TESTING
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/example"
	DEPENDS PDI ${FLOWVR_DEPENDENCY}
	INSTALL_COMMAND ""
	SUBSTEPS test
)

sbuild_add_module(PDI_TESTS
	ENABLE_BUILD_FLAG BUILD_TESTING
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/tests"
	DEPENDS PDI
	INSTALL_COMMAND ""
	SUBSTEPS indent test
)

#=============================================================================
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.5)
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

option(BUILD_CFG_VALIDATOR    "Build config validation script" "${BUILD_UNSTABLE}")
option(BUILD_DECL_HDF5_PLUGIN "Build Decl'HDF5 plug-in" ON)
option(BUILD_DECL_SION_PLUGIN "Build Decl'SION plug-in" "${BUILD_UNSTABLE}")
option(BUILD_FLOWVR_PLUGIN    "Build FlowVR plug-in" "${BUILD_UNSTABLE}")
option(BUILD_FORTRAN          "Build with Fortran support" ON)
option(BUILD_FTI_PLUGIN       "Build FTI plug-in." "OFF")
option(BUILD_HDF5_PARALLEL    "Build Decl'HDF5 in parallel mode" ON)
option(BUILD_MPI_PLUGIN       "Build MPI plug-in" ON)
option(BUILD_PYCALL_PLUGIN    "Build Pycall plug-in" "${BUILD_UNSTABLE}")
option(BUILD_PYTHON           "Build with Python support" "${BUILD_UNSTABLE}")
option(BUILD_SET_VALUE_PLUGIN "Build set_value plug-in" ON)
option(BUILD_SHARED_LIBS      "Build shared libraries rather than static ones" ON)
option(BUILD_TEST_PLUGIN      "Build Test plugin" ON)
option(BUILD_TRACE_PLUGIN     "Build Trace plugin" ON)
option(BUILD_USER_CODE_PLUGIN "Build User-code plugin" ON)



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
if("${BUILD_DECL_SION_PLUGIN}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_sion/cmake")
endif()
if("${BUILD_FLOWVR_PLUGIN}")
	list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/plugins/flowvr/cmake")
endif()
if("${BUILD_FTI_PLUGIN}")
	message(WARNING "The FTI plugin is currently disabled as its stability is problematic")
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
endif()


if("${CMAKE_VERSION}" VERSION_LESS "3.10")
	if("${BUILD_TESTING}")
		message(FATAL_ERROR
			"Testing is enabled but the cmake version is less than 3.10.\n"
			"    you have the following options:\n"
			"     * install and use a more recent version of cmake (3.10 at least)"
			"     * disable testing => pass `-DBUILD_TESTING=OFF' to cmake\n"
		)
	endif()
	if("${BUILD_DOCUMENTATION}")
		message(FATAL_ERROR
			"Documentation building is enabled but the cmake version is less than 3.10.\n"
			"    you have the following options:\n"
			"     * install and use a more recent version of cmake (3.10 at least)"
			"     * disable documentation build => pass `-DBUILD_DOCUMENTATION=OFF' to cmake\n"
		)
	endif()
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



### Subtargets

if("${BUILD_INDENT}")
	add_custom_target(indent)
endif()



### Dependencies

include(GNUInstallDirs)
include(SuperBuild)


## Fortran

if("${BUILD_FORTRAN}") # and (Bpp or paraconf or PDI or ...)
	enable_language(Fortran)
endif()


## Bpp

if("${BUILD_FORTRAN}") # and PDI
	sbuild_add_dependency(Bpp EMBEDDED
		BUILD_DEPENDENCY
		EMBEDDED_PATH "vendor/bpp-v0.3.0"
		VERSION 0.3.0
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
if("${BUILD_DECL_HDF5_PLUGIN}" AND "${BUILD_HDF5_PARALLEL}")
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
	)
endif()


## Yaml

# if ( paraconf or Py2YAML or Py3YAML )
sbuild_add_dependency(yaml "${USE_DEFAULT}"
	EMBEDDED_PATH "vendor/libyaml-0.2.2"
	CMAKE_CACHE_ARGS
		"-DBUILD_TESTING:BOOL=OFF"
		"-DINSTALL_LIB_DIR:STRING=${CMAKE_INSTALL_LIBDIR}"
		"-DINSTALL_BIN_DIR:STRING=${CMAKE_INSTALL_BINDIR}"
		"-DINSTALL_INCLUDE_DIR:STRING=${CMAKE_INSTALL_INCLUDEDIR}"
		"-DINSTALL_CMAKE_DIR:STRING=share/yaml/cmake"
	VERSION 0.1
)


## Paraconf

set(PARACONF_COMPONENTS "C")
if("${BUILD_FORTRAN}")
	list(APPEND PARACONF_COMPONENTS "f90")
endif()
# if ( PDI or ... )
sbuild_add_dependency(paraconf "${USE_DEFAULT}"
	EMBEDDED_PATH "vendor/libparaconf-0.4.6"
	COMPONENTS ${PARACONF_COMPONENTS}
	SOURCE_SUBDIR "paraconf"
	CMAKE_CACHE_ARGS
		"-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=TRUE"
		"-DUSE_yaml:STRING=SYSTEM"
		"-DBUILD_TESTING:BOOL=OFF"
	DEPENDS "yaml"
	VERSION 0.4.0
)


## spdlog

# if ( PDI or ... )
sbuild_add_dependency(spdlog "${USE_DEFAULT}"
	EMBEDDED_PATH "vendor/spdlog-1.3.1"
	CMAKE_CACHE_ARGS
		"-DSPDLOG_BUILD_TESTS:BOOL=OFF"
		"-DSPDLOG_BUILD_EXAMPLES:BOOL=OFF"
		"-DSPDLOG_BUILD_BENCH:BOOL=OFF"
	VERSION 1.3.1
)


## Doxygen

if("${BUILD_DOCUMENTATION}")
	sbuild_add_dependency(Doxygen "${USE_DEFAULT}"
		BUILD_DEPENDENCY
		EMBEDDED_PATH "vendor/doxygen-1.8.15"
		VERSION 1.8.13
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


## Python2

if("${BUILD_FLOWVR_PLUGIN}")
	sbuild_add_dependency(Python2 SYSTEM COMPONENTS Interpreter)
endif()


## Python3

if("${BUILD_PYTHON}")
	sbuild_add_dependency(Python3Path SYSTEM COMPONENTS Interpreter Development)
elseif("${BUILD_CFG_VALIDATOR}")
	sbuild_add_dependency(Python3Path SYSTEM COMPONENTS Interpreter)
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


## OpenGL

if("${BUILD_FLOWVR_PLUGIN}" AND "${BUILD_TESTING}")
	set(OpenGL_GL_PREFERENCE LEGACY)
	sbuild_add_dependency(OpenGL SYSTEM)
	sbuild_add_dependency(GLUT SYSTEM)
endif()


## FTI

if("${BUILD_FTI_PLUGIN}")
	sbuild_add_dependency(FTI "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/fti-1.3+fix-cmake"
		CMAKE_CACHE_ARGS
			"-DENABLE_FORTRAN:BOOL=OFF"
			"-DENABLE_EXAMPLES:BOOL=OFF"
			"-DENABLE_TESTS:BOOL=OFF"
			"-DENABLE_DOCU:BOOL=OFF"
		VERSION 1.3
	)
endif()


## FlowVR

if("${BUILD_FLOWVR_PLUGIN}")
	sbuild_add_dependency(FlowVR "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/flowvr-ex-02074ec43f05e94201f51a45fac7976b55800d3b"
		CMAKE_CACHE_ARGS
			"-DENABLE_EXAMPLES:BOOL=OFF"
			"-DENABLE_TESTS:BOOL=OFF"
			"-DENABLE_DOCU:BOOL=OFF"
			"-DOpenGL_GL_PREFERENCE:STRING=LEGACY"
	)
endif()


## HDF5

if("${BUILD_DECL_HDF5_PLUGIN}")
	set(HDF5_PREFER_PARALLEL "${BUILD_HDF5_PARALLEL}")
	sbuild_add_dependency(HDF5 "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/hdf5-1.10.5"
		COMPONENTS C
		MODULE_VARS HDF5_IS_PARALLEL
		CMAKE_CACHE_ARGS
			"-DHDF5_ENABLE_PARALLEL:BOOL=${BUILD_HDF5_PARALLEL}"
			"-DHDF5_BUILD_CPP_LIB:BOOL=OFF"
			"-DHDF5_BUILD_EXAMPLES:BOOL=OFF"
			"-DHDF5_BUILD_HL_LIB:BOOL=OFF"
			"-DHDF5_BUILD_TOOLS:BOOL=OFF"
			"-DBUILD_TESTING:BOOL=OFF"
		VERSION 1.8.0
	)
	if("${HDF5_FOUND}")
		if("${BUILD_HDF5_PARALLEL}" AND NOT "${HDF5_IS_PARALLEL}")
			message(FATAL_ERROR
				"You requested a parallel HDF5 build (-DBUILD_HDF5_PARALLEL=ON) but a sequential SYSTEM version has been found\n"
				"    you have the following options:\n"
				"     * build with this sequential HDF5 => pass `-DBUILD_HDF5_PARALLEL=OFF' to cmake\n"
				"     * build the EMBEDDED HDF5 from PDI distribution => pass `-DUSE_HDF5=EMBEDDED' to cmake\n"
				"     * use another version of HDF5 from the system => pass `-DHDF5_ROOT=/path/to/hdf5/root/' to cmake"
			)
		endif()
	elseif("${CMAKE_VERSION}" VERSION_LESS 3.10)
		message(FATAL_ERROR
			"The EMBEDDED version of HDF5 is requested but the cmake version is less than 3.10.\n"
			"    you have the following options:\n"
			"     * install and use a more recent version of cmake (3.10 at least)"
			"     * use a SYSTEM version of HDF5 => pass `-DUSE_HDF5=SYSTEM' to cmake\n"
			"     * disable the Decl'HDF5 plugin => pass `-DBUILD_DECL_HDF5_PLUGIN=OFF' to cmake"
		)
	endif()
endif()


## PyBind11

if("${BUILD_PYTHON}")
	find_package(Python3 QUIET REQUIRED COMPONENTS Interpreter Development)
	set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency")
	set(PYBIND11_PYTHON_VERSION "${Python3_VERSION}" CACHE STRING "Python version to use for compiling modules")
	sbuild_add_dependency(pybind11 "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/pybind11-2.3.0"
		CMAKE_CACHE_ARGS
			"-DBUILD_TESTING:BOOL=OFF"
			"-DPYBIND11_TEST:BOOL=OFF"
			"-DPYBIND11_PYTHON_VERSION:STRING=${Python3_VERSION}"
		VERSION 2.3.0
	)
endif()


## Py2YAML

sbuild_get_env(CPATH CPATH)
sbuild_get_env(LIBRARY_PATH LIBRARY_PATH)
if("${BUILD_FLOWVR_PLUGIN}")
	#TODO: installing python libraries doesn't work ATM, installing to a prefix is not supported, virtualenv or such should be used
	sbuild_add_dependency(Py2YAML SYSTEM #"${USE_DEFAULT}"
# 		EMBEDDED_PATH "vendor/PyYAML-5.1"
# 		CONFIGURE_COMMAND "${Python2_EXECUTABLE}" "<SOURCE_DIR>/setup.py"
# 			--without-libyaml
# 			build_ext
# 			-I "${CPATH}"
# 			-L "${LIBRARY_PATH}"
# 		BUILD_IN_SOURCE ON
# 		BUILD_COMMAND "${Python2_EXECUTABLE}" "<SOURCE_DIR>/setup.py"
# 			--without-libyaml
# 			build
# 		INSTALL_COMMAND "${Python2_EXECUTABLE}" "<SOURCE_DIR>/setup.py"
# 			--without-libyaml
# 			install
# 			--prefix "<INSTALL_DIR>"
# 			--install-purelib "<INSTALL_DIR>/${Python2_INSTALL_SITELIBDIR}"
# 			--install-platlib "<INSTALL_DIR>/${Python2_INSTALL_SITEARCHDIR}"
	)
endif()


## Py3YAML

if("${BUILD_CFG_VALIDATOR}")
# 	if("${BUILD_PYTHON}")
# 		set(Py3YAML_WITH_LIBYAML "--with-libyaml")
# 		set(Py3YAML_DEPENDS DEPENDS yaml)
# 	else()
# 		set(Py3YAML_WITH_LIBYAML "--without-libyaml")
# 		set(Py3YAML_DEPENDS)
# 	endif()
	#TODO: installing python libraries doesn't work ATM, installing to a prefix is not supported, virtualenv or such should be used
	sbuild_add_dependency(Py3YAML SYSTEM #"${USE_DEFAULT}"
# 		EMBEDDED_PATH "vendor/PyYAML-5.1"
# 		CONFIGURE_COMMAND "${Python3_EXECUTABLE}" "<SOURCE_DIR>/setup.py"
# 			"${Py3YAML_WITH_LIBYAML}"
# 			build_ext
# 			-I "${CPATH}"
# 			-L "${LIBRARY_PATH}"
# 		BUILD_IN_SOURCE ON
# 		BUILD_COMMAND "${Python3_EXECUTABLE}" "<SOURCE_DIR>/setup.py"
# 			"${Py3YAML_WITH_LIBYAML}"
# 			build
# 		INSTALL_COMMAND "${Python3_EXECUTABLE}" "<SOURCE_DIR>/setup.py"
# 			"${Py3YAML_WITH_LIBYAML}"
# 			install
# 			--prefix "<INSTALL_DIR>"
# 			--install-purelib "<INSTALL_DIR>/${Python3Path_INSTALL_SITELIBDIR}"
# 			--install-platlib "<INSTALL_DIR>/${Python3Path_INSTALL_SITEARCHDIR}"
# 		${Py3YAML_DEPENDS}
	)
endif()


## SIONlib

if("${BUILD_DECL_SION_PLUGIN}")
	sbuild_add_dependency(SIONlib "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/sionlib-1.7.6"
		BUILD_IN_SOURCE 1
		CONFIGURE_COMMAND "<SOURCE_DIR>/configure"
			--prefix="<INSTALL_DIR>"
			--disable-fortran
			--disable-parutils
			--disable-cxx
		VERSION 1.7.2
	)
endif()



### Own modules

if("${BUILD_INDENT}")
	set(ASTYLE_DEPENDENCY Astyle)
endif()

set(PDI_DEPENDS_LIST paraconf spdlog "${ASTYLE_DEPENDENCY}")
if("${BUILD_DOCUMENTATION}")
	list(APPEND PDI_DEPENDS_LIST Doxygen)
endif()
if("${BUILD_TESTING}")
	list(APPEND PDI_DEPENDS_LIST GTest)
endif()
if("${BUILD_FORTRAN}")
	list(APPEND PDI_DEPENDS_LIST Bpp)
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
	DEPENDS Py3YAML
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
	DEPENDS PDI HDF5 ${ASTYLE_DEPENDENCY} ${PDICFG_DEPENDENCY}
	SUBSTEPS indent test
)


sbuild_add_module(DECL_SION_PLUGIN
	ENABLE_BUILD_FLAG BUILD_DECL_SION_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_sion"
	DEPENDS PDI SIONlib ${ASTYLE_DEPENDENCY} ${PDICFG_DEPENDENCY}
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
	DEPENDS PDI FlowVR ${ASTYLE_DEPENDENCY} #TODO: Py2YAML
	SUBSTEPS indent test
)


sbuild_add_module(PYCALL_PLUGIN
	ENABLE_BUILD_FLAG BUILD_PYCALL_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/pycall"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)

sbuild_add_module(SET_VALUE_PLUGIN
	ENABLE_BUILD "${BUILD_SET_VALUE_PLUGIN}"
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/set_value"
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


sbuild_add_module(PDI_EXAMPLE
	ENABLE_BUILD_FLAG BUILD_TESTING
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/example"
	DEPENDS PDI
	INSTALL_COMMAND ""
	SUBSTEPS test
)

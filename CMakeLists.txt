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
option(BUILD_DECL_HDF5_PLUGIN "Build decl'HDF5 plug-in" ON)
option(BUILD_DECL_SION_PLUGIN "Build decl'SION plug-in" "${BUILD_UNSTABLE}")
option(BUILD_FLOWVR_PLUGIN    "Build FlowVR plug-in" "${BUILD_UNSTABLE}")
option(BUILD_FORTRAN          "Enable Fortran support" ON)
option(BUILD_FTI_PLUGIN       "Build FTI plug-in." OFF)
option(BUILD_HDF5_PARALLEL    "Enable HDF5 parallel build" ON)
option(BUILD_MPI_PLUGIN       "Build MPI plug-in" ON)
option(BUILD_PYCALL_PLUGIN    "Build Pycall plug-in" "${BUILD_UNSTABLE}")
option(BUILD_PYTHON           "Enable Python support" "${BUILD_UNSTABLE}")
option(BUILD_SHARED_LIBS      "Build shared libraries rather than static ones" ON)



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


if("${BUILD_FTI_PLUGIN}")
	message(FATAL_ERROR "The FTI plugin is disabled until the new-style plugin becomes available")
endif()

if("${BUILD_TESTING}" OR "${BUILD_DOCUMENTATION}")
	if("${CMAKE_VERSION}" VERSION_LESS "3.10")
		message(FATAL_ERROR "Minimum CMake version to build tests is 3.10. Please update CMake or turn off testing by adding the flag `-DBUILD_TESTING=OFF` to the cmake command line.")
	endif()
endif()

if("${BUILD_PYCALL_PLUGIN}" AND NOT "${BUILD_PYTHON}")
	message(FATAL_ERROR "The Pycall plugin requires Python support, please either pass -DBUILD_PYTHON=ON or -DBUILD_PYCALL_PLUGIN=OFF to cmake")
endif()



### Subtargets

if("${BUILD_INDENT}")
	add_custom_target(indent)
endif()



### Dependencies

include(GNUInstallDirs)
include(SuperBuild)


## Fortran

if("${BUILD_FORTRAN}") # and (BPP or paraconf or PDI or ...)
	enable_language(Fortran)
endif()


## BPP

if("${BUILD_FORTRAN}") # and (paraconf or PDI)
	sbuild_add_dependency(BPP EMBEDDED
		BUILD_DEPENDENCY
		EMBEDDED_PATH "vendor/bpp-v0.3.0"
		CMAKE_CACHE_ARGS
			"-DBUILD_TESTING:BOOL=OFF"
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
	VERSION 0.2.2
)


## Paraconf

set(PARACONF_DEPENDS_LIST "yaml")
set(PARACONF_COMPONENTS "C")
if("${BUILD_FORTRAN}")
	list(APPEND PARACONF_DEPENDS_LIST "BPP")
	list(APPEND PARACONF_COMPONENTS "f90")
endif()
# if ( PDI or ... )
sbuild_add_dependency(paraconf "${USE_DEFAULT}"
	EMBEDDED_PATH "vendor/libparaconf-0.4.0"
	COMPONENTS ${PARACONF_COMPONENTS}
	SOURCE_SUBDIR "paraconf"
	CMAKE_CACHE_ARGS
		"-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=TRUE"
		"-DUSE_yaml:STRING=SYSTEM"
		"-DBUILD_TESTING:BOOL=OFF"
	DEPENDS ${PARACONF_DEPENDS_LIST}
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
elseif("${BUILD_FORTRAN}" OR "${BUILD_CFG_VALIDATOR}")
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
		EMBEDDED_PATH "vendor/fti-1.1"
		CMAKE_CACHE_ARGS
			"-DENABLE_EXAMPLES:BOOL=OFF"
			"-DENABLE_TESTS:BOOL=OFF"
			"-DENABLE_DOCU:BOOL=OFF"
		VERSION 1.1
	)
endif()


## FlowVR

if("${BUILD_FLOWVR_PLUGIN}")
	sbuild_add_dependency(FlowVR "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/flowvr-ex"
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
		CMAKE_CACHE_ARGS
			"-DHDF5_ENABLE_PARALLEL:BOOL=${BUILD_HDF5_PARALLEL}"
			"-DHDF5_BUILD_CPP_LIB:BOOL=OFF"
			"-DHDF5_BUILD_EXAMPLES:BOOL=OFF"
			"-DHDF5_BUILD_HL_LIB:BOOL=OFF"
			"-DHDF5_BUILD_TOOLS:BOOL=OFF"
			"-DBUILD_TESTING:BOOL=OFF"
		VERSION 1.8.0
	)
endif()


## PyBind11

if("${BUILD_PYTHON}")
	find_package(Python3 QUIET REQUIRED COMPONENTS Interpreter Development)
	set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency")
	sbuild_add_dependency(pybind11 "${USE_DEFAULT}"
		EMBEDDED_PATH "vendor/pybind11-2.3.0"
		CMAKE_CACHE_ARGS
			"-DBUILD_TESTING:BOOL=OFF"
			"-DPYBIND11_TEST:BOOL=OFF"
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
		EMBEDDED_PATH "vendor/sionlib-1.7.2"
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
	list(APPEND PDI_DEPENDS_LIST BPP)
endif()
if("${BUILD_PYTHON}")
	list(APPEND PDI_DEPENDS_LIST pybind11)
endif()
sbuild_add_module(PDI
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/pdi"
	DEPENDS ${PDI_DEPENDS_LIST}
	SUBSTEPS indent test
)


sbuild_add_module(TEST_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/test"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent
)


sbuild_add_module(TRACE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/trace"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent
)


sbuild_add_module(USER_CODE_PLUGIN
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/user_code"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)


sbuild_add_module(MPI_PLUGIN
	ENABLE_BUILD "${BUILD_MPI_PLUGIN}"
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/mpi"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)


sbuild_add_module(DECL_HDF5_PLUGIN
	ENABLE_BUILD "${BUILD_DECL_HDF5_PLUGIN}"
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_hdf5"
	DEPENDS PDI HDF5 ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)


sbuild_add_module(DECL_SION_PLUGIN
	ENABLE_BUILD "${BUILD_DECL_SION_PLUGIN}"
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/decl_sion"
	DEPENDS PDI SIONlib ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)


# sbuild_add_module(FTI_PLUGIN
# 	ENABLE_BUILD "${BUILD_FTI_PLUGIN}"
# 	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/fti"
# 	DEPENDS PDI FTI "${ASTYLE_DEPENDENCY}"
# 	SUBSTEPS indent
# )


sbuild_add_module(FLOWVR_PLUGIN
	ENABLE_BUILD "${BUILD_FLOWVR_PLUGIN}"
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/flowvr"
	DEPENDS PDI FlowVR "${ASTYLE_DEPENDENCY}" #TODO: Py2YAML
	SUBSTEPS indent test
)


sbuild_add_module(PYCALL_PLUGIN
	ENABLE_BUILD "${BUILD_PYCALL_PLUGIN}"
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/plugins/pycall"
	DEPENDS PDI ${ASTYLE_DEPENDENCY}
	SUBSTEPS indent test
)


sbuild_add_module(PDI_EXAMPLE
	ENABLE_BUILD "${BUILD_TESTING}"
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/example"
	DEPENDS PDI
	INSTALL_COMMAND ""
	SUBSTEPS test
)


sbuild_add_module(PDICFG_VALIDATOR
	ENABLE_BUILD "${BUILD_CFG_VALIDATOR}"
	SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/tools/pdicfg_validator"
	DEPENDS Py3YAML
	SUBSTEPS test
)

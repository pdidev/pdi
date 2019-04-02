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

cmake_minimum_required(VERSION 3.5)


# The header project

project(PDISUPER LANGUAGES)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")


# Build options

option(ENABLE_UNSTABLE                         "Build all features by default including those not stable yet" OFF)
set(USE_DEFAULT  SYSTEM           CACHE STRING "Default version of libraries to use (SYSTEM or EMBEDDED)")

option(BUILD_CFG_VALIDATOR                     "Build config validation script" ON)
option(BUILD_DECL_HDF5_PLUGIN                  "Build decl'HDF5 plug-in" ON)
option(BUILD_DECL_SION_PLUGIN                  "Build decl'SION plug-in" "${ENABLE_UNSTABLE}")
option(BUILD_DOCUMENTATION                     "Build documentation" ON)
option(BUILD_FLOWVR_PLUGIN                     "Build FlowVR plug-in" "${ENABLE_UNSTABLE}")
option(BUILD_FTI_PLUGIN                        "Build FTI plug-in." OFF)
option(BUILD_MPI_PLUGIN                        "Build MPI plug-in" ON)
option(BUILD_PYCALL_PLUGIN                     "Build Pycall plug-in" "${ENABLE_UNSTABLE}")
option(BUILD_SHARED_LIBS                       "Build shared libraries rather than static ones" ON)
option(BUILD_TESTING                           "Build tests" ON)
option(ENABLE_FORTRAN                          "Enable Fortran support" ON)
option(ENABLE_HDF5_PARALLEL                    "Enable HDF5 parallel build" ON)
option(ENABLE_PYTHON                           "Enable Python support" "${ENABLE_UNSTABLE}")

set(USE_BPP      EMBEDDED         CACHE STRING "version of BPP to use, SYSTEM and EMBEDDED can be used")
set(USE_FTI      "${USE_DEFAULT}" CACHE STRING "version of FTI to use, SYSTEM and EMBEDDED can be used")
set(USE_FLOWVR   "${USE_DEFAULT}" CACHE STRING "version of FlowVR to use, SYSTEM and EMBEDDED can be used")
set(USE_GTEST    EMBEDDED         CACHE STRING "version of Gtest to use, SYSTEM and EMBEDDED can be used")
set(USE_HDF5 	 "${USE_DEFAULT}" CACHE STRING "version of HDF5 to use, SYSTEM and EMBEDDED can be used")
set(USE_PARACONF "${USE_DEFAULT}" CACHE STRING "version of paraconf to use, SYSTEM and EMBEDDED can be used")
set(USE_PYBIND11 "${USE_DEFAULT}" CACHE STRING "version of pybind11 to use, SYSTEM and EMBEDDED can be used")
set(USE_PYYAML   "${USE_DEFAULT}" CACHE STRING "version of PyYAML to use, SYSTEM and EMBEDDED can be used")
set(USE_SIONLIB  "${USE_DEFAULT}" CACHE STRING "version of SIONlib to use, SYSTEM and EMBEDDED can be used")
set(USE_SPDLOG   "${USE_DEFAULT}" CACHE STRING "version of spdlog to use, SYSTEM and EMBEDDED can be used")
set(USE_YAML     "${USE_DEFAULT}" CACHE STRING "version of yaml to use, SYSTEM and EMBEDDED can be used")


## Options handling

if("${BUILD_TESTING}" AND "${CMAKE_VERSION}" VERSION_LESS "3.10") 
	message(FATAL_ERROR "Minimum CMake version to build tests is 3.10. Please update CMake or turn off testing by adding the flag `-DBUILD_TESTING=OFF` to the cmake command line.")
endif()
if("${BUILD_DOCUMENTATION}" AND "${CMAKE_VERSION}" VERSION_LESS "3.10") 
	message(FATAL_ERROR "Minimum CMake version to build documentation is 3.10. Please update CMake or turn off documentation generation by adding the flag `-DBUILD_DOCUMENTATION=OFF` to the cmake command line.")
endif()
if(NOT "${BUILD_SHARED_LIBS}") 
	message(FATAL_ERROR "Building PDI as a static lib is not supported. Please add the flag `BUILD_SHARED_LIBS=ON` to the cmake command line.")
endif()

if("${USE_BPP}" STREQUAL EMBEDDED)
	set(BPP_PATH "${PDISUPER_SOURCE_DIR}/vendor/bpp-v0.2.11")
else()
	set(BPP_PATH "${USE_BPP}")
endif()
if("${USE_FTI}" STREQUAL EMBEDDED)
	set(FTI_PATH "${PDISUPER_SOURCE_DIR}/vendor/fti-1.1")
else()
	set(FTI_PATH "${USE_FTI}")
endif()
if("${USE_FLOWVR}" STREQUAL EMBEDDED)
	set(FLOWVR_PATH "${PDISUPER_SOURCE_DIR}/vendor/flowvr-ex")
else()
	set(FLOWVR_PATH "${USE_FLOWVR}")
endif()
if("${USE_GTEST}" STREQUAL EMBEDDED)
	set(GTEST_PATH "${PDISUPER_SOURCE_DIR}/vendor/googletest-release-1.8.0")
else()
	set(GTEST_PATH "${USE_GTEST}")
endif()
if("${USE_HDF5}" STREQUAL EMBEDDED)
	set(HDF5_PATH "${PDISUPER_SOURCE_DIR}/vendor/hdf5-1.10.5")
else()
	set(HDF5_PATH "${USE_HDF5}")
endif()
if("${USE_PARACONF}" STREQUAL EMBEDDED)
	set(PARACONF_PATH "${PDISUPER_SOURCE_DIR}/vendor/libparaconf-0.3.2")
else()
	set(PARACONF_PATH "${USE_PARACONF}")
endif()
if("${USE_PYBIND11}" STREQUAL EMBEDDED)
	set(PYBIND11_PATH "${PDISUPER_SOURCE_DIR}/vendor/pybind11-2.2.4")
else()
	set(PYBIND11_PATH "${USE_PYBIND11}")
endif()
if("${USE_PYYAML}" STREQUAL EMBEDDED)
	set(PYYAML_PATH "${PDISUPER_SOURCE_DIR}/vendor/PyYAML-3.13")
else()
	set(PYYAML_PATH "${USE_PYYAML}")
endif()
if("${USE_SIONLIB}" STREQUAL EMBEDDED)
	set(SIONLIB_PATH "${PDISUPER_SOURCE_DIR}/vendor/sionlib-1.7.2")
else()
	set(SIONLIB_PATH "${USE_SIONLIB}")
endif()
if("${USE_SPDLOG}" STREQUAL EMBEDDED)
	set(SPDLOG_PATH "${PDISUPER_SOURCE_DIR}/vendor/spdlog-1.3.1")
else()
	set(SPDLOG_PATH "${USE_SPDLOG}")
endif()
if("${USE_YAML}" STREQUAL EMBEDDED)
	set(YAML_PATH "${PDISUPER_SOURCE_DIR}/vendor/libyaml-0.2.1")
else()
	set(YAML_PATH "${USE_YAML}")
endif()


# Includes

include(ExternalProject)


# A list of all cache variables to forward to subprojects

include(GNUInstallDirs)
list(APPEND CMAKE_PREFIX_PATH "${PDISUPER_BINARY_DIR}/staging" "${PDISUPER_BINARY_DIR}/build-dep")
list(APPEND CMAKE_BUILD_RPATH "${PDISUPER_BINARY_DIR}/staging/${CMAKE_INSTALL_LIBDIR}" "${PDISUPER_BINARY_DIR}/staging/lib")
get_cmake_property(CACHE_VARIABLES CACHE_VARIABLES)
set(CMAKE_CACHE_ARGS)
foreach(CACHE_VARIABLE ${CACHE_VARIABLES})
	get_property(CACHE_VARIABLE_TYPE CACHE "${CACHE_VARIABLE}" PROPERTY TYPE)
	if(NOT "${CACHE_VARIABLE_TYPE}" STREQUAL STATIC AND NOT "${CACHE_VARIABLE_TYPE}" STREQUAL INTERNAL)
		list(APPEND CMAKE_CACHE_ARGS "-D${CACHE_VARIABLE}:${CACHE_VARIABLE_TYPE}=${${CACHE_VARIABLE}}")
	endif()
endforeach()
list(APPEND CMAKE_CACHE_ARGS 
		"-DCMAKE_BUILD_RPATH:PATH=${CMAKE_BUILD_RPATH}"
		"-DCMAKE_STAGING_PREFIX:PATH=<INSTALL_DIR>"
		"-DCMAKE_PREFIX_PATH:PATH=${CMAKE_PREFIX_PATH}")

set(LIBRARY_PATH "${PDISUPER_BINARY_DIR}/staging/${CMAKE_INSTALL_LIBDIR}:${PDISUPER_BINARY_DIR}/build-dep/${CMAKE_INSTALL_LIBDIR}:${PDISUPER_BINARY_DIR}/staging/lib:${PDISUPER_BINARY_DIR}/build-dep/lib${LIBRARY_PATH}")
set(LD_LIBRARY_PATH "${LIBRARY_PATH}")

if(NOT "x$ENV{LD_LIBRARY_PATH}x" STREQUAL "xx")
	set(LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:$ENV{LD_LIBRARY_PATH}")
endif()
if(NOT "x$ENV{LIBRARY_PATH}x" STREQUAL "xx")
	set(LIBRARY_PATH "${LIBRARY_PATH}:$ENV{LIBRARY_PATH}")
	set(LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:$ENV{LIBRARY_PATH}")
endif()

set(CPATH "${PDISUPER_BINARY_DIR}/staging/${CMAKE_INSTALL_INCLUDEDIR}:${PDISUPER_BINARY_DIR}/build-dep/${CMAKE_INSTALL_INCLUDEDIR}:${PDISUPER_BINARY_DIR}/staging/include:${PDISUPER_BINARY_DIR}/build-dep/include${CPATH}")
if(NOT "x$ENV{CPATH}x" STREQUAL "xx")
	set(CPATH "${CPATH}:$ENV{CPATH}")
endif()

if("${BUILD_CFG_VALIDATOR}" OR "${ENABLE_PYTHON}")
	set(Python_ADDITIONAL_VERSIONS 3.7 3.6 3.5 3.4)
	find_package(PythonPath REQUIRED)
endif()


# Testing

if("${BUILD_TESTING}")
	enable_testing()
	set_property(DIRECTORY PROPERTY TEST_INCLUDE_FILE "${PDISUPER_BINARY_DIR}/SubTests.cmake")
	file(WRITE "${PDISUPER_BINARY_DIR}/SubTests.cmake" 
#TODO: replace these path by the actual path where stuff is installed
"set(ADDPATH [=[${PDISUPER_BINARY_DIR}/staging/${CMAKE_INSTALL_LIBDIR}:${PDISUPER_BINARY_DIR}/staging/lib]=])\n"
"set(PYPATH  [=[${PDISUPER_BINARY_DIR}/staging/${PythonPath_INSTALL_SITEARCHDIR}:${PDISUPER_BINARY_DIR}/staging/${PythonPath_INSTALL_SITELIBDIR}]=])\n"
[===[
set(LD_LIBRARY_PATH "$ENV{LD_LIBRARY_PATH}")
if("x${LD_LIBRARY_PATH}x" STREQUAL xx)
	set(ENV{LD_LIBRARY_PATH} "${ADDPATH}")
else()
	set(ENV{LD_LIBRARY_PATH} "${ADDPATH}:${LD_LIBRARY_PATH}")
endif()
set(PYTHONPATH "$ENV{PYTHONPATH}")
if("x${PYTHONPATH}x" STREQUAL xx)
	set(ENV{PYTHONPATH} "${PYPATH}")
else()
	set(ENV{PYTHONPATH} "${PYPATH}:${PYTHONPATH}")
endif()
]===])
endif()
function(add_testing EP_NAME)
	ExternalProject_Get_Property("${EP_NAME}" BINARY_DIR)
	if("${BUILD_TESTING}")
		file(APPEND "${PDISUPER_BINARY_DIR}/SubTests.cmake" "subdirs([=[${BINARY_DIR}]=])\n")
	endif()
endfunction()


# Indentation

add_custom_target(indent)
function(add_indent_step EP_NAME)
	if("${CMAKE_GENERATOR}" MATCHES "Make") #< Use recursive make.
		set(INDENT_COMMAND "\$(MAKE)" indent)
	else() #< Drive the project with "cmake --build".
		set(INDENT_COMMAND "${CMAKE_COMMAND}" --build "." --target indent)
		get_property(_isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
		if(_isMultiConfig)
			list(APPEND INDENT_COMMAND --config $<CONFIG>)
		endif()
	endif()
	ExternalProject_Get_Property("${EP_NAME}" BINARY_DIR)
	ExternalProject_Add_StepTargets("${EP_NAME}" configure)
	add_custom_command(TARGET indent POST_BUILD
			COMMAND ${INDENT_COMMAND}
			WORKING_DIRECTORY "${BINARY_DIR}"
			COMMENT "Indenting '${EP_NAME}'")
	add_dependencies(indent "${EP_NAME}-configure")
endfunction()


# Documentation

add_custom_target(doc)
function(add_doc_step EP_NAME)
	if("${CMAKE_GENERATOR}" MATCHES "Make") #< Use recursive make.
		set(DOC_COMMAND "\$(MAKE)" doc)
	else() #< Drive the project with "cmake --build".
		set(DOC_COMMAND "${CMAKE_COMMAND}" --build "." --target doc)
		get_property(_isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
		if(_isMultiConfig)
			list(APPEND DOC_COMMAND --config $<CONFIG>)
		endif()
	endif()
	ExternalProject_Get_Property("${EP_NAME}" BINARY_DIR)
	ExternalProject_Add_StepTargets("${EP_NAME}" doc)
	add_custom_command(TARGET doc POST_BUILD
			COMMAND ${DOC_COMMAND}
			WORKING_DIRECTORY "${BINARY_DIR}"
			COMMENT "Generating documentation for '${EP_NAME}'")
	add_dependencies(doc "${EP_NAME}-configure")
endfunction()


# Main project

set(PDI_BUILD_COMMAND "${CMAKE_COMMAND}" -E env "LD_LIBRARY_PATH=${LD_LIBRARY_PATH}")
if("${CMAKE_GENERATOR}" MATCHES "Make") #< Use recursive make.
	list(APPEND PDI_BUILD_COMMAND "\$(MAKE)")
else() #< Drive the project with "cmake --build".
	list(APPEND PDI_BUILD_COMMAND "${CMAKE_COMMAND}" --build ".")
	get_property(_isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
	if(_isMultiConfig)
		list(APPEND PDI_BUILD_COMMAND --config $<CONFIG>)
	endif()
endif()
ExternalProject_Add(PDI
		PREFIX "${PDISUPER_BINARY_DIR}/pdi"
		SOURCE_DIR "${PDISUPER_SOURCE_DIR}/src" BUILD_ALWAYS TRUE
		CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
		BUILD_COMMAND ${PDI_BUILD_COMMAND}
		INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging")
add_indent_step(PDI)
add_doc_step(PDI)
add_testing(PDI)


## Main project dependancies

if("${BUILD_TESTING}" AND NOT "${USE_GTEST}" STREQUAL SYSTEM)
	file(MD5 "${GTEST_PATH}" HASH) #< trick to prevent no-hash warning
	ExternalProject_Add(GTEST
			PREFIX "${PDISUPER_BINARY_DIR}/gtest"
			URL "${GTEST_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS} "-DBUILD_TESTING:BOOL=OFF" "-DBUILD_SHARED_LIBS:BOOL=OFF" #< set BUILD_SHARED_LIBS=OFF to workaround https://github.com/google/googletest/issues/930
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/build-dep")
	ExternalProject_Add_StepDependencies(PDI configure GTEST)
endif()

if("${ENABLE_FORTRAN}" AND NOT "${USE_BPP}" STREQUAL SYSTEM)
	file(MD5 "${BPP_PATH}" HASH) #< trick to prevent no-hash warning
	ExternalProject_Add(BPP
			PREFIX "${PDISUPER_BINARY_DIR}/bpp"
			URL "${BPP_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS} "-DBUILD_TESTING:BOOL=OFF"
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/build-dep")
	ExternalProject_Add_StepDependencies(PDI configure BPP)
endif()

if(NOT "${USE_PARACONF}" STREQUAL SYSTEM)
	file(MD5 "${PARACONF_PATH}" HASH) #< trick to prevent no-hash warning
	ExternalProject_Add(PARACONF
			PREFIX "${PDISUPER_BINARY_DIR}/paraconf"
			URL "${PARACONF_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS} "-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=TRUE" "-DUSE_YAML:STRING=SYSTEM" "-DBUILD_TESTING:BOOL=OFF"
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging")
	ExternalProject_Add_StepDependencies(PDI configure PARACONF)
	if(NOT "${USE_YAML}" STREQUAL SYSTEM)
		file(MD5 "${YAML_PATH}" HASH) #< trick to prevent no-hash warning
		ExternalProject_Add(YAML
				PREFIX "${PDISUPER_BINARY_DIR}/yaml"
				URL "${YAML_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
				CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS} "-DBUILD_TESTING:BOOL=OFF" "-DINSTALL_LIB_DIR:STRING=${CMAKE_INSTALL_LIBDIR}" "-DINSTALL_BIN_DIR:STRING=${CMAKE_INSTALL_BINDIR}" "-DINSTALL_INCLUDE_DIR:STRING=${CMAKE_INSTALL_INCLUDEDIR}" "-DINSTALL_CMAKE_DIR:STRING=share/yaml/cmake"
				BUILD_COMMAND ${PDI_BUILD_COMMAND}
				INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging")
		add_dependencies(PARACONF YAML)
	endif()
endif()

if("${ENABLE_PYTHON}" AND NOT "${USE_PYBIND11}" STREQUAL SYSTEM)
	file(MD5 "${PYBIND11_PATH}" HASH) #< trick to prevent no-hash warning
	ExternalProject_Add(PYBIND11
			PREFIX "${PDISUPER_BINARY_DIR}/pybind11"
			URL "${PYBIND11_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS} "-DBUILD_TESTING:BOOL=OFF" "-DPYBIND11_TEST:BOOL=OFF"
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging")
	ExternalProject_Add_StepDependencies(PDI configure PYBIND11)
endif()

if(NOT "${USE_SPDLOG}" STREQUAL SYSTEM)
	file(MD5 "${SPDLOG_PATH}" HASH) #< trick to prevent no-hash warning
	ExternalProject_Add(SPDLOG
			PREFIX "${PDISUPER_BINARY_DIR}/spdlog"
			URL "${SPDLOG_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS} "-DBUILD_TESTING:BOOL=OFF" "-DSPDLOG_BUILD_EXAMPLES:BOOL=OFF" "-DSPDLOG_BUILD_BENCH:BOOL=OFF"
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging")
	ExternalProject_Add_StepDependencies(PDI configure SPDLOG)
endif()


# The plugins

ExternalProject_Add(TEST_PLUGIN
		PREFIX "${PDISUPER_BINARY_DIR}/pdi_plugin-test"
		SOURCE_DIR "${PDISUPER_SOURCE_DIR}/plugins/test" BUILD_ALWAYS TRUE
		CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
		BUILD_COMMAND ${PDI_BUILD_COMMAND}
		INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
		DEPENDS PDI)
add_indent_step(TEST_PLUGIN)

ExternalProject_Add(USER_CODE_PLUGIN
		PREFIX "${PDISUPER_BINARY_DIR}/pdi_plugin-user_code"
		SOURCE_DIR "${PDISUPER_SOURCE_DIR}/plugins/user_code" BUILD_ALWAYS TRUE
		CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
		BUILD_COMMAND ${PDI_BUILD_COMMAND}
		INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
		DEPENDS PDI)
add_testing(USER_CODE_PLUGIN)
add_indent_step(USER_CODE_PLUGIN)

if("${BUILD_MPI_PLUGIN}")
	ExternalProject_Add(MPI_PLUGIN
			PREFIX "${PDISUPER_BINARY_DIR}/pdi_plugin-mpi"
			SOURCE_DIR "${PDISUPER_SOURCE_DIR}/plugins/mpi" BUILD_ALWAYS TRUE
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
			DEPENDS PDI)
	add_testing(MPI_PLUGIN)
	add_indent_step(MPI_PLUGIN)
endif()

if("${BUILD_DECL_HDF5_PLUGIN}")
	ExternalProject_Add(DECL_HDF5_PLUGIN
			PREFIX "${PDISUPER_BINARY_DIR}/pdi_plugin-decl_hdf5"
			SOURCE_DIR "${PDISUPER_SOURCE_DIR}/plugins/decl_hdf5" BUILD_ALWAYS TRUE
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
			DEPENDS PDI)
	add_testing(DECL_HDF5_PLUGIN)
	add_indent_step(DECL_HDF5_PLUGIN)
	if(NOT "${USE_HDF5}" STREQUAL SYSTEM)
		if("${CMAKE_VERSION}" VERSION_LESS "3.10") 
			message(FATAL_ERROR "Minimum CMake version to build HDF5 is 3.10. Please update CMake or use system version of HDF5 by adding the flag `-DUSE_HDF5=SYSTEM` to the cmake command line.")
		endif()

		file(MD5 "${HDF5_PATH}" HASH) #< trick to prevent no-hash warning		
		ExternalProject_Add(HDF5
				PREFIX "${PDISUPER_BINARY_DIR}/hdf5"
				URL "${HDF5_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
				CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS} "-DHDF5_ENABLE_PARALLEL:BOOL=${ENABLE_HDF5_PARALLEL}" "-DHDF5_BUILD_CPP_LIB:BOOL=OFF" 
				"-DHDF5_BUILD_EXAMPLES:BOOL=OFF" "-DHDF5_BUILD_HL_LIB:BOOL=OFF" "-DHDF5_BUILD_TOOLS:BOOL=OFF" "-DBUILD_TESTING:BOOL=OFF"
				INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging")
		add_dependencies(DECL_HDF5_PLUGIN HDF5)
	endif()
endif()

if("${BUILD_DECL_SION_PLUGIN}")
	ExternalProject_Add(DECL_SION_PLUGIN
			PREFIX "${PDISUPER_BINARY_DIR}/pdi_plugin-decl_sion"
			SOURCE_DIR "${PDISUPER_SOURCE_DIR}/plugins/decl_sion" BUILD_ALWAYS TRUE
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
			DEPENDS PDI)
	add_testing(DECL_SION_PLUGIN)
	add_indent_step(DECL_SION_PLUGIN)
	if(NOT "${USE_SIONLIB}" STREQUAL SYSTEM)
		file(MD5 "${SIONLIB_PATH}" HASH) #< trick to prevent no-hash warning		
		ExternalProject_Add(SIONLIB
				PREFIX "${PDISUPER_BINARY_DIR}/sionlib"
				URL "${SIONLIB_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
				INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
				BUILD_IN_SOURCE 1
				CONFIGURE_COMMAND "<SOURCE_DIR>/configure" --prefix="<INSTALL_DIR>" --disable-fortran --disable-parutils --disable-cxx)
		add_dependencies(DECL_SION_PLUGIN SIONLIB)
	endif()
endif()

if("${BUILD_FTI_PLUGIN}")
	message(FATAL_ERROR "The FTI plugin is disabled until the new-style plugin becomes available")
	ExternalProject_Add(FTI_PLUGIN
			PREFIX "${PDISUPER_BINARY_DIR}/pdi_plugin-fti"
			SOURCE_DIR "${PDISUPER_SOURCE_DIR}/plugins/fti" BUILD_ALWAYS TRUE
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
			DEPENDS PDI)
	add_indent_step(FTI_PLUGIN)
	if(NOT "${USE_FTI}" STREQUAL SYSTEM)
		file(MD5 "${FTI_PATH}" HASH) #< trick to prevent no-hash warning
		ExternalProject_Add(FTI
				PREFIX "${PDISUPER_BINARY_DIR}/fti"
				URL "${FTI_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
				CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS} "-DENABLE_EXAMPLES:BOOL=OFF" "-DENABLE_TESTS:BOOL=OFF" "-DENABLE_DOCU:BOOL=OFF"
				BUILD_COMMAND ${PDI_BUILD_COMMAND}
				INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging")
		add_dependencies(FTI_PLUGIN FTI)
	endif()
endif()

if("${BUILD_FLOWVR_PLUGIN}")
	ExternalProject_Add(FLOWVR_PLUGIN
			PREFIX "${PDISUPER_BINARY_DIR}/pdi_plugin-flowvr"
			SOURCE_DIR "${PDISUPER_SOURCE_DIR}/plugins/flowvr" BUILD_ALWAYS TRUE
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
			DEPENDS PDI)
	add_testing(FLOWVR_PLUGIN)
	add_indent_step(FLOWVR_PLUGIN)
	if(NOT "${USE_FLOWVR}" STREQUAL SYSTEM)
		file(MD5 "${FLOWVR_PATH}" HASH) #< trick to prevent no-hash warning
		ExternalProject_Add(FLOWVR
				PREFIX "${PDISUPER_BINARY_DIR}/flowvr"
				URL "${FLOWVR_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
				CMAKE_ARGS -Wno-dev
				CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS} "-DENABLE_EXAMPLES:BOOL=OFF" "-DENABLE_TESTS:BOOL=OFF" "-DENABLE_DOCU:BOOL=OFF"
				BUILD_COMMAND ${PDI_BUILD_COMMAND}
				INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging")
		add_dependencies(FLOWVR_PLUGIN FLOWVR)
	endif()
	if(NOT "${USE_PYYAML}" STREQUAL SYSTEM)
		find_package (Python2 COMPONENTS Interpreter)
		string(SUBSTRING "${Python2_SITELIB}" 5 -1 Python2_INSTALL_SITELIBDIR) # cut the "/usr/" prefix
		string(SUBSTRING "${Python2_SITEARCH}" 5 -1 Python2_INSTALL_SITEARCHDIR) # cut the "/usr/" prefix
		file(MD5 "${PYYAML_PATH}" HASH) #< trick to prevent no-hash warning
		ExternalProject_Add(PYYAML2.7
				PREFIX "${PDISUPER_BINARY_DIR}/pyyaml27"
				URL "${PYYAML_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
				CONFIGURE_COMMAND "${Python2_EXECUTABLE}" "<SOURCE_DIR>/setup.py" --with-libyaml build_ext -I "${CPATH}" -L "${LIBRARY_PATH}"
				BUILD_IN_SOURCE ON
				BUILD_COMMAND "${Python2_EXECUTABLE}" "<SOURCE_DIR>/setup.py" --with-libyaml build
				INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
				INSTALL_COMMAND "${Python2_EXECUTABLE}"
						"<SOURCE_DIR>/setup.py"
						--with-libyaml
						install
						--prefix "<INSTALL_DIR>"
						--install-purelib "<INSTALL_DIR>/${Python2_INSTALL_SITELIBDIR}"
						--install-platlib "<INSTALL_DIR>/${Python2_INSTALL_SITEARCHDIR}")
		add_dependencies(FLOWVR_PLUGIN PYYAML2.7)
		add_dependencies(PYYAML2.7 YAML)
	endif()
endif()

if("${BUILD_PYCALL_PLUGIN}")
	if(NOT "${ENABLE_PYTHON}")
		message(FATAL_ERROR "the Pycall plugin requires Python support, please pass -DENABLE_PYTHON=ON to cmake")
	endif()
	ExternalProject_Add(PYCALL_PLUGIN
			PREFIX "${PDISUPER_BINARY_DIR}/pdi_plugin-pycall"
			SOURCE_DIR "${PDISUPER_SOURCE_DIR}/plugins/pycall" BUILD_ALWAYS TRUE
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
			DEPENDS PDI)
	add_testing(PYCALL_PLUGIN)
	add_indent_step(PYCALL_PLUGIN)
endif()


# Examples

if("${BUILD_TESTING}")
	ExternalProject_Add(PDI_EXAMPLE
			PREFIX "${PDISUPER_BINARY_DIR}/pdi_example"
			SOURCE_DIR "${PDISUPER_SOURCE_DIR}/example" BUILD_ALWAYS TRUE
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_COMMAND ""
			DEPENDS PDI)
	add_testing(PDI_EXAMPLE)
endif()


# Configuration validation

if ("${BUILD_CFG_VALIDATOR}")
	ExternalProject_Add(PDICFG_VALIDATOR
			PREFIX "${PDISUPER_BINARY_DIR}/pdicfg_validator"
			SOURCE_DIR "${PDISUPER_SOURCE_DIR}/tools/pdicfg_validator" BUILD_ALWAYS TRUE
			CMAKE_CACHE_ARGS ${CMAKE_CACHE_ARGS}
			BUILD_COMMAND ${PDI_BUILD_COMMAND}
			INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging")
	add_testing(PDICFG_VALIDATOR)
	if(NOT "${USE_PYYAML}" STREQUAL SYSTEM)
		file(MD5 "${PYYAML_PATH}" HASH) #< trick to prevent no-hash warning
		ExternalProject_Add(PYYAML
				PREFIX "${PDISUPER_BINARY_DIR}/pyyaml"
				URL "${PYYAML_PATH}" URL_HASH "MD5=${HASH}" EXCLUDE_FROM_ALL 1
				CONFIGURE_COMMAND "${PYTHON_EXECUTABLE}" "<SOURCE_DIR>/setup.py" --with-libyaml build_ext -I "${CPATH}" -L "${LIBRARY_PATH}"
				BUILD_IN_SOURCE ON
				BUILD_COMMAND "${PYTHON_EXECUTABLE}" "<SOURCE_DIR>/setup.py" --with-libyaml build
				INSTALL_DIR "${PDISUPER_BINARY_DIR}/staging"
				INSTALL_COMMAND "${PYTHON_EXECUTABLE}"
						"<SOURCE_DIR>/setup.py"
						--with-libyaml
						install
						--prefix "<INSTALL_DIR>"
						--install-purelib "<INSTALL_DIR>/${PythonPath_INSTALL_SITELIBDIR}"
						--install-platlib "<INSTALL_DIR>/${PythonPath_INSTALL_SITEARCHDIR}")
		add_dependencies(PDICFG_VALIDATOR PYYAML)
		add_dependencies(PYYAML YAML)
	endif()
endif()


# Installation

install(DIRECTORY "${PDISUPER_BINARY_DIR}/staging/" DESTINATION "." USE_SOURCE_PERMISSIONS)

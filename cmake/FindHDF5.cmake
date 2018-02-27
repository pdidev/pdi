# FindHDF5
# --------
#
# Find HDF5, a library for reading and writing self describing array data.
#
#
#
# The module will optionally accept the COMPONENTS argument.  If no
# COMPONENTS are specified, then the find module will default to finding
# only the HDF5 C library.  If one or more COMPONENTS are specified, the
# module will attempt to find the language bindings for the specified
# components.  The only valid components are C, CXX, Fortran, HL, and
# Fortran_HL.  If the COMPONENTS argument is not given, the module will
# attempt to find only the C bindings.
#
# On UNIX systems, this module will read the variable
# HDF5_USE_STATIC_LIBRARIES to determine whether or not to prefer a
# static link to a dynamic link for HDF5 and all of it's dependencies.
# To use this feature, make sure that the HDF5_USE_STATIC_LIBRARIES
# variable is set before the call to find_package.
#
# The serial version of HDF5 is preferentially selected. This behavior
# can be reversed by setting the variable HDF5_PREFER_PARALLEL to true.
#
# In addition to finding the includes and libraries required to compile
# an HDF5 client application, this module also makes an effort to find
# tools that come with the HDF5 distribution that may be useful for
# regression testing.
#
# This module will define the following variables:
#
# ::
#
#   HDF5_FOUND - true if HDF5 was found on the system
#   HDF5_VERSION - HDF5 version in format Major.Minor.Release
#   HDF5_INCLUDE_DIRS - Location of the hdf5 includes
#   HDF5_INCLUDE_DIR - Location of the hdf5 includes (deprecated)
#   HDF5_DEFINITIONS - Required compiler definitions for HDF5
#   HDF5_LIBRARIES - Required libraries for all requested bindings
#   HDF5_HL_LIBRARIES - Required libraries for the HDF5 high level API for all
#                       bindings, if the HL component is enabled
#
# Available components are: C CXX Fortran and HL.  For each enabled language
# binding, a corresponding HDF5_${LANG}_LIBRARIES variable will be defined.
# If the HL component is enabled, then an HDF5_${LANG}_HL_LIBRARIES will
# also be defined.  With all components enabled, the following variables will be defined:
#
# ::
#
#   HDF5_C_LIBRARIES - Required libraries for the HDF5 C bindings
#   HDF5_CXX_LIBRARIES - Required libraries for the HDF5 C++ bindings
#   HDF5_Fortran_LIBRARIES - Required libraries for the HDF5 Fortran bindings
#   HDF5_C_HL_LIBRARIES - Required libraries for the high level C bindings
#   HDF5_CXX_HL_LIBRARIES - Required libraries for the high level C++ bindings
#   HDF5_Fortran_HL_LIBRARIES - Required libraries for the high level Fortran
#                               bindings.
#
#   HDF5_IS_PARALLEL - Whether or not HDF5 was found with parallel IO support
#   HDF5_C_COMPILER_EXECUTABLE - the path to the HDF5 C wrapper compiler
#   HDF5_CXX_COMPILER_EXECUTABLE - the path to the HDF5 C++ wrapper compiler
#   HDF5_Fortran_COMPILER_EXECUTABLE - the path to the HDF5 Fortran wrapper compiler
#   HDF5_C_COMPILER_EXECUTABLE_NO_INTERROGATE - path to the primary C compiler
#                                               which is also the HDF5 wrapper
#   HDF5_CXX_COMPILER_EXECUTABLE_NO_INTERROGATE - path to the primary C++
#                                                 compiler which is also
#                                                 the HDF5 wrapper
#   HDF5_Fortran_COMPILER_EXECUTABLE_NO_INTERROGATE - path to the primary
#                                                     Fortran compiler which
#                                                     is also the HDF5 wrapper
#   HDF5_DIFF_EXECUTABLE - the path to the HDF5 dataset comparison tool


#=============================================================================
# Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#=============================================================================


cmake_minimum_required(VERSION 3.3)

# include this to handle the QUIETLY and REQUIRED arguments
include(${CMAKE_CURRENT_LIST_DIR}/FindPackageHandleStandardArgs.cmake)

if("${HDF5_USE_EMBEDDED}")
	
	if(NOT TARGET hdf5-static AND NOT TARGET hdf5-shared)
		
		set(HDF5_EXTERNALLY_CONFIGURED 1)
		set(HDF5_BUILD_EXAMPLES OFF CACHE BOOL "Build HDF5 Library Examples" FORCE)
		set(HDF5_DISABLE_COMPILER_WARNINGS ON CACHE BOOL "Disable compiler warnings" FORCE)
		set(BUILD_TESTING OFF CACHE BOOL "Build HDF5 Unit Testing" FORCE)
		if(NOT DEFINED CMAKE_Fortran_MODULE_DIRECTORY)
			set(CMAKE_Fortran_MODULE_DIRECTORY "${PROJECT_BINARY_DIR}")
		endif()
		set(HDF5_EXPORTED_TARGETS "hdf5-exports")
		
		set(BUILD_SHARED_LIBS ON CACHE BOOL "Build Shared Libraries" FORCE)
		if("${HDF5_USE_STATIC_LIBRARIES}")
			set(BUILD_SHARED_LIBS OFF CACHE BOOL "Build Shared Libraries" FORCE)
		endif()
		
		set(HDF5_ENABLE_PARALLEL OFF CACHE BOOL "Enable parallel build (requires MPI)" FORCE)
		if(${HDF5_PREFER_PARALLEL})
			set(HDF5_ENABLE_PARALLEL ON CACHE BOOL "Enable parallel build (requires MPI)" FORCE)
		endif()
		
		set(HDF5_BUILD_FORTRAN OFF CACHE BOOL "Build FORTRAN support" FORCE)
		set(HDF5_BUILD_HL_LIB  OFF CACHE BOOL "Build HIGH Level HDF5 Library" FORCE)
		set(HDF5_BUILD_CPP_LIB OFF CACHE BOOL "Build HDF5 C++ Library" FORCE)
		# The only valid components are C, CXX, Fortran, HL, and Fortran_HL.
		foreach(HDF5_COMPONENT ${HDF5_FIND_COMPONENTS})
			if("${HDF5_COMPONENT}" STREQUAL "C")
			elseif("${HDF5_COMPONENT}" STREQUAL "Fortran")
				set(HDF5_BUILD_FORTRAN ON CACHE BOOL "Build FORTRAN support" FORCE)
			elseif("${HDF5_COMPONENT}" STREQUAL "HL")
				set(HDF5_BUILD_HL_LIB  ON CACHE BOOL "Build HIGH Level HDF5 Library" FORCE)
			elseif("${HDF5_COMPONENT}" STREQUAL "Fortran_HL")
				set(HDF5_BUILD_FORTRAN ON CACHE BOOL "Build FORTRAN support" FORCE)
				set(HDF5_BUILD_HL_LIB  ON CACHE BOOL "Build HIGH Level HDF5 Library" FORCE)
			elseif("${HDF5_COMPONENT}" STREQUAL "CXX")
				set(HDF5_BUILD_CPP_LIB ON CACHE BOOL "Build HDF5 C++ Library" FORCE)
			else()
				message(AUTHOR_WARNING "Invalid component for FindHDF5: ${HDF5_COMPONENT}")
			endif()
		endforeach()
		
		add_subdirectory(../ext/hdf5-1.8.18/ hdf5-1.8.18 EXCLUDE_FROM_ALL)
		set(HDF5_VERSION "1.8.18")
		set(EXALL) # EXCLUDE_FROM_ALL is only supported from 2.6 in install(EXPORT)
		if("${CMAKE_VERSION}" VERSION_GREATER 3.6)
			set(EXALL "EXCLUDE_FROM_ALL")
		endif()
		install(EXPORT hdf5-exports DESTINATION "${CMAKE_CURRENT_BINARY_DIR}/trash" ${EXALL})
		
		if(TARGET hdf5-shared)
			target_include_directories(hdf5-shared INTERFACE "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/hdf5-1.8.18>") # Workaround missing modules in target
			set(HDF5_C_LIBRARIES hdf5-shared)
		endif()
		
		if(TARGET hdf5_hl-shared)
			target_include_directories(hdf5_hl-shared INTERFACE "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../ext/hdf5-1.8.18/hl/src/>") # Workaround missing modules in target
			set(HDF5_C_HL_LIBRARIES hdf5_hl-shared)
		endif()
		
		if(TARGET hdf5_fortran-shared)
			target_include_directories(hdf5_fortran-shared INTERFACE "$<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}/shared>") # Workaround missing modules in target
			set(HDF5_Fortran_LIBRARIES hdf5_fortran-shared)
		endif()
		
		if(TARGET hdf5_hl_fortran-shared)
			set(HDF5_Fortran_HL_LIBRARIES hdf5_hl_fortran-shared)
		endif()
		
		if(TARGET hdf5_cpp-shared)
			target_include_directories(hdf5_cpp-shared INTERFACE "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/hdf5-1.8.18>") # Workaround missing modules in target
			set(HDF5_CXX_LIBRARIES hdf5_cpp-shared)
		endif()
		
		if("${HDF5_USE_STATIC_LIBRARIES}")
			if(TARGET hdf5-static)
				target_include_directories(hdf5-static INTERFACE "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/hdf5-1.8.18>") # Workaround missing modules in target
				set(HDF5_C_LIBRARIES hdf5-static)
			endif()
		
			if(TARGET hdf5_hl-static)
				target_include_directories(hdf5_hl-static INTERFACE "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../ext/hdf5-1.8.18/hl/src/>") # Workaround missing modules in target
				set(HDF5_C_HL_LIBRARIES hdf5_hl-static)
			endif()
			
			if(TARGET hdf5_fortran-static)
				target_include_directories(hdf5_fortran-static INTERFACE "$<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}/static>") # Workaround missing modules in target
				set(HDF5_Fortran_LIBRARIES hdf5_fortran-static)
			endif()
			
			if(TARGET hdf5_hl_fortran-static)
# 				target_include_directories(hdf5_hl_fortran-static INTERFACE "$<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}/static>") # Workaround missing modules in target
				set(HDF5_Fortran_HL_LIBRARIES hdf5_hl_fortran-static)
			endif()
			
			if(TARGET hdf5_cpp-static)
				target_include_directories(hdf5_cpp-static INTERFACE "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/hdf5-1.8.18>") # Workaround missing modules in target
				set(HDF5_CXX_LIBRARIES hdf5_cpp-static)
			endif()
			
		endif()
		
		if(TARGET "${HDF5_C_LIBRARIES}")
			set(HDF5_C_FOUND TRUE)
		endif()
		
		if(TARGET "${HDF5_Fortran_LIBRARIES}")
			set(HDF5_Fortran_FOUND TRUE)
		endif()
		
		if(TARGET "${HDF5_C_HL_LIBRARIES}")
			set(HDF5_HL_FOUND TRUE)
		endif()
		
		if(TARGET "${HDF5_Fortran_HL_LIBRARIES}")
			set(HDF5_Fortran_HL_FOUND TRUE)
		endif()
		
		set(HDF5_LIBRARIES    ${HDF5_C_LIBRARIES} ${HDF5_Fortran_LIBRARIES} ${HDF5_CXX_LIBRARIES})
		set(HDF5_HL_LIBRARIES ${HDF5_C_HL_LIBRARIES} ${HDF5_Fortran_HL_LIBRARIES})
		
		set(HDF5_IS_PARALLEL "${H5_HAVE_PARALLEL}")
		
		# Force building h5diff that is exported by the lib
		add_custom_target(hdf5_tools ALL)
		add_dependencies(hdf5_tools h5diff)
		set(HDF5_DIFF_EXECUTABLE h5diff)
		
	endif()
	
	set(HDF5_C_COMPILER_EXECUTABLE HDF5_C_COMPILER_EXECUTABLE-NOTFOUND)
	set(HDF5_CXX_COMPILER_EXECUTABLE HDF5_CXX_COMPILER_EXECUTABLE-NOTFOUND)
	set(HDF5_Fortran_COMPILER_EXECUTABLE HDF5_Fortran_COMPILER_EXECUTABLE-NOTFOUND)
	set(HDF5_C_COMPILER_EXECUTABLE_NO_INTERROGATE HDF5_C_COMPILER_EXECUTABLE_NO_INTERROGATE-NOTFOUND)
	set(HDF5_CXX_COMPILER_EXECUTABLE_NO_INTERROGATE HDF5_CXX_COMPILER_EXECUTABLE_NO_INTERROGATE-NOTFOUND)
	set(HDF5_Fortran_COMPILER_EXECUTABLE_NO_INTERROGATE HDF5_Fortran_COMPILER_EXECUTABLE_NO_INTERROGATE-NOTFOUND)
	find_package_handle_standard_args(HDF5
		REQUIRED_VARS HDF5_C_LIBRARIES
		VERSION_VAR HDF5_VERSION
		HANDLE_COMPONENTS
		)
else()
	
	include(${CMAKE_CURRENT_LIST_DIR}/FindHDF5_ORIG.cmake)
	
	if("${HDF5_IS_PARALLEL}")
		find_package(MPI)
		set(HDF5_C_LIBRARIES ${HDF5_C_LIBRARIES} mpi)
		set(HDF5_Fortran_LIBRARIES ${HDF5_Fortran_LIBRARIES} mpi_f90)
	endif()
	
	# Compatibility with our old FindHDF5...
	set(HDF5_C_LIBRARIES  ${HDF5_C_LIBRARIES})
	set(HDF5_Fortran_LIBRARIES  ${HDF5_Fortran_LIBRARIES})
	set(HDF5_C_INCLUDE_PATH  "${HDF5_INCLUDE_DIRS}")
	set(HDF5_C_COMPILE_FLAGS "${HDF5_DEFINITIONS}")
	set(HDF5_Fortran_INCLUDE_PATH  "${HDF5_INCLUDE_DIRS}")
	set(HDF5_Fortran_COMPILE_FLAGS "${HDF5_DEFINITIONS}")
	set(HDF5_SHARED_C_INCLUDE_PATH  "${HDF5_INCLUDE_DIRS}")
	set(HDF5_SHARED_C_COMPILE_FLAGS "${HDF5_DEFINITIONS}")
	set(HDF5_SHARED_Fortran_INCLUDE_PATH  "${HDF5_INCLUDE_DIRS}")
	set(HDF5_SHARED_Fortran_COMPILE_FLAGS "${HDF5_DEFINITIONS}")
	
endif()

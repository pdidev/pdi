# - Find HDF5, a library for reading and writing self describing array data.
#
# === Variables ===
#
# This module accepts the following optional variable:
#   HDF5_PARALLEL                     can be one of
#                                     * "PAR": looks for the parallel version
#                                     * "SEQ": looks for the sequential version
#                                     * "ANY" or "": prefer the sequential
#                                       version but fallback on the parallel
#                                       version if not found
#
# This module will set the following variables per language in your project,
# where <lang> is one of C, CXX, or Fortran:
#   HDF5_<lang>_COMPILER              HDF5 Compiler wrapper for <lang>
#   HDF5_<lang>_COMPILE_FLAGS         Compilation flags for HDF5 programs
#   HDF5_<lang>_INCLUDE_PATH          Include path(s) for HDF5 header
#   HDF5_<lang>_LINK_FLAGS            Linking flags for HDF5 programs
#   HDF5_<lang>_LIBRARIES             All libraries to link HDF5 programs
#                                     against
#   HDF5_SHARED_<lang>_COMPILE_FLAGS  Compilation flags for HDF5 programs
#   HDF5_SHARED_<lang>_INCLUDE_PATH   Include path(s) for HDF5 header
#   HDF5_SHARED_<lang>_LINK_FLAGS     Linking flags for HDF5 programs
#   HDF5_SHARED_<lang>_LIBRARIES      All libraries to link HDF5 programs
#                                     against
# Additionally, FindHDF5 sets the following variables:
#   HDF5_FOUND                        TRUE if FindHDF5 found HDF5 flags for
#                                     all enabled languages
#   H5DIFF                            the path to the HDF5 dataset comparison
#                                     tool
#
# === Usage ===
#
# To use this module, simply call FindHDF5 from a CMakeLists.txt file, or
# run find_package(HDF5), then run CMake.  If you are happy with the auto-
# detected configuration for your language, then you're done.  If not, you
# have two options:
#   1. Set HDF5_<lang>_COMPILER to the HDF5 wrapper (hdf5cc, etc.) of your
#      choice and reconfigure.  FindHDF5 will attempt to determine all the
#      necessary variables using THAT compiler's compile and link flags.
#   2. If this fails, or if your HDF5 implementation does not come with
#      a compiler wrapper, then set both HDF5_<lang>_LIBRARIES and
#      HDF5_<lang>_INCLUDE_PATH.  You may also set any other variables
#      listed above, but these two are required.  This will circumvent
#      autodetection entirely.
# When configuration is successful, HDF5_<lang>_COMPILER will be set to the
# compiler wrapper for <lang>, if it was found.  HDF5_<lang>_FOUND and other
# variables above will be set if any HDF5 implementation was found for <lang>,
# regardless of whether a compiler was found.

#=============================================================================
# Copyright 2013-2016 CEA, Julien Bigot <julien.bigot@cea.fr>
# Copyright 2001-2011 Kitware, Inc.
# Copyright 2010-2011 Todd Gamblin tgamblin@llnl.gov
# Copyright 2009 Kitware, Inc.
# Copyright 2001-2009 Dave Partyka
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
# * Neither the names of Kitware, Inc., the Insight Software Consortium, nor
#   the names of their contributors may be used to endorse or promote products
#   derived from this software without specific prior written  permission.
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


# include this to handle the QUIETLY and REQUIRED arguments
include(FindPackageHandleStandardArgs)
include(GetPrerequisites)


# interrogate_hdf5_compiler(lang try_libs shared_hdf5)
#
# Attempts to extract compiler and linker args from an HDF5 compiler. The arguments set
# by this function are:
#
#   HDF5<shared>_<lang>_INCLUDE_PATH    HDF5<shared>_<lang>_LINK_FLAGS     HDF5<shared>_<lang>_FOUND
#   HDF5<shared>_<lang>_COMPILE_FLAGS   HDF5<shared>_<lang>_LIBRARIES
#
# HDF5_<lang>_COMPILER must be set beforehand to the absolute path to an HDF5 compiler for
# <lang>.  Additionally, HDF5_<lang>_INCLUDE_PATH and HDF5_<lang>_LIBRARIES may be set
# to skip autodetection.
#
# If try_libs is TRUE, this will also attempt to find plain HDF5 libraries in the usual
# way.  In general, this is not as effective as interrogating the compilers, as it
# ignores language-specific flags and libraries.
#
function (interrogate_hdf5_compiler lang try_libs shared_hdf5 parallel_hdf5)
	# HDF5_${lang}_NO_INTERROGATE will be set to a compiler name when the *regular* compiler was
	# discovered to be the HDF5 compiler. If the user force-sets another HDF5 compiler,
	# HDF5_${lang}_COMPILER won't be equal to HDF5_${lang}_NO_INTERROGATE, and we'll inspect
	# that compiler anew.  This allows users to set new compilers w/o rm'ing cache.
	string(COMPARE NOTEQUAL "${HDF5_${lang}_NO_INTERROGATE}" "${HDF5_${lang}_COMPILER}" interrogate)

	# If HDF5 is set already in the cache, don't bother with interrogating the compiler.
	if (interrogate AND ((NOT HDF5_${lang}_INCLUDE_PATH) OR (NOT HDF5_${lang}_LIBRARIES)))
		if (HDF5_${lang}_COMPILER)

			set(H5COMPILER_PARAM)
			if (${shared_hdf5})
				set(H5COMPILER_PARAM "-shlib")
			endif()

			execute_process(
				COMMAND ${HDF5_${lang}_COMPILER} -show ${H5COMPILER_PARAM}
				OUTPUT_VARIABLE  HDF5_CMDLINE OUTPUT_STRIP_TRAILING_WHITESPACE
				ERROR_VARIABLE   HDF5_CMDLINE ERROR_STRIP_TRAILING_WHITESPACE
				RESULT_VARIABLE  HDF5_COMPILER_RETURN)

			if (NOT "${HDF5_COMPILER_RETURN}" EQUAL 0)
				message(STATUS "Unable to determine HDF5 from HDF5 driver ${HDF5_${lang}_COMPILER}")
				set(HDF5_CMDLINE)
			endif()

			# Here, we're done with the interrogation part, and we'll try to extract args we care
			# about from what we learned from the compiler wrapper scripts.

			# If interrogation came back with something, extract our variable from the HDF5 command line
			if (HDF5_CMDLINE)
				# Extract compile flags from the compile command line.
				# It used to contain -f.* options, but this took some invalid options on the way => removed.
				string(REGEX MATCHALL "(^| )-D([^\" ]+|\"[^\"]+\")" HDF5_ALL_COMPILE_FLAGS "${HDF5_CMDLINE}")
				set(HDF5_COMPILE_FLAGS_WORK)

				foreach(FLAG ${HDF5_ALL_COMPILE_FLAGS})
					if (HDF5_COMPILE_FLAGS_WORK)
						set(HDF5_COMPILE_FLAGS_WORK "${HDF5_COMPILE_FLAGS_WORK} ${FLAG}")
					else()
						set(HDF5_COMPILE_FLAGS_WORK ${FLAG})
					endif()
				endforeach()

				# Extract include paths from compile command line
				string(REGEX MATCHALL "(^| )-I([^\" ]+|\"[^\"]+\")" HDF5_ALL_INCLUDE_PATHS "${HDF5_CMDLINE}")
				foreach(IPATH ${HDF5_ALL_INCLUDE_PATHS})
					string(REGEX REPLACE "^ ?-I" "" IPATH ${IPATH})
					string(REGEX REPLACE "//" "/" IPATH ${IPATH})
					list(APPEND HDF5_INCLUDE_PATH_WORK ${IPATH})
				endforeach()

				# If all else fails, just search for hdf5.h in the normal include paths.
				if (NOT HDF5_INCLUDE_PATH_WORK)
					set(HDF5_HEADER_PATH "HDF5_HEADER_PATH-NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
					if (lang STREQUAL Fortran)
						find_path(HDF5_HEADER_PATH hdf5.mod
							HINTS ${_HDF5_BASE_DIR}
							PATH_SUFFIXES include)
						set(HDF5_INCLUDE_PATH_WORK ${HDF5_HEADER_PATH})
					else ()
						find_path(HDF5_HEADER_PATH hdf5.h
							HINTS ${_HDF5_BASE_DIR}
							PATH_SUFFIXES include)
						set(HDF5_INCLUDE_PATH_WORK ${HDF5_HEADER_PATH})
					endif()
				endif()

				# Extract linker paths from the link command line
				string(REGEX MATCHALL "(^| |-Wl,)-L([^\" ]+|\"[^\"]+\")" HDF5_ALL_LINK_PATHS "${HDF5_CMDLINE}")
				set(HDF5_LINK_PATH)
				foreach(LPATH ${HDF5_ALL_LINK_PATHS})
					string(REGEX REPLACE "^(| |-Wl,)-L" "" LPATH ${LPATH})
					string(REGEX REPLACE "//" "/" LPATH ${LPATH})
					list(APPEND HDF5_LINK_PATH ${LPATH})
				endforeach()
				# add the compiler implicit directories because some compilers
				# such as the intel compiler have libraries that show up
				# in the show list that can only be found in the implicit
				# link directories of the compiler.
				if (DEFINED CMAKE_${lang}_IMPLICIT_LINK_DIRECTORIES)
					list(APPEND HDF5_LINK_PATH "${CMAKE_${lang}_IMPLICIT_LINK_DIRECTORIES}")
				endif ()

				# Extract linker flags from the link command line
				string(REGEX MATCHALL "(^| )-Wl,([^\" ]+|\"[^\"]+\")" HDF5_ALL_LINK_FLAGS "${HDF5_CMDLINE}")
				set(HDF5_LINK_FLAGS_WORK)
				foreach(FLAG ${HDF5_ALL_LINK_FLAGS})
					if (HDF5_LINK_FLAGS_WORK)
						set(HDF5_LINK_FLAGS_WORK "${HDF5_LINK_FLAGS_WORK} ${FLAG}")
					else()
						set(HDF5_LINK_FLAGS_WORK ${FLAG})
					endif()
				endforeach()

				# Extract the set of libraries to link against from the link command
				# line
				string(REGEX MATCHALL "(^| )(-l([^\" ]+|\"[^\"]+\")|[^ ]*\\.(a|so))( |$)" HDF5_LIBNAMES "${HDF5_CMDLINE}")

				# Determine full path names for all of the libraries that one needs
				# to link against in an HDF5 program
				foreach(LIB ${HDF5_LIBNAMES})
					string(REGEX REPLACE "^ +" "" LIB ${LIB})
					string(REGEX REPLACE "^-l *" "" LIB ${LIB})
					string(REGEX REPLACE " $" "" LIB ${LIB})
					# HDF5_LIB is cached by find_library, but we don't want that.  Clear it first.
					set(HDF5_LIB "HDF5_LIB-NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
					if(EXISTS "${LIB}")
						set(HDF5_LIB "${LIB}")
					else()
						find_library(HDF5_LIB NAMES ${LIB} HINTS ${HDF5_LINK_PATH})
					endif()

					if (HDF5_LIB)
						list(APPEND HDF5_LIBRARIES_WORK ${HDF5_LIB})
					elseif (NOT HDF5_FIND_QUIETLY)
						message(WARNING "Unable to find HDF5 library ${LIB}")
					endif()
				endforeach()

			endif()

		elseif(try_libs)
			# If we didn't have an HDF5 compiler script to interrogate, attempt to find everything
			# with plain old find functions.
			unset(HDF5_LIBRARIES_WORK)
			unset(HDF5_INCLUDE_PATH_WORK)

			find_path(HDF5_HEADER_PATH hdf5.h
				HINTS ${_HDF5_BASE_DIR}
				PATH_SUFFIXES include)
			set(HDF5_INCLUDE_PATH_WORK ${HDF5_HEADER_PATH})

			unset(HDF5_LIB CACHE)
			find_library(HDF5_LIB
				NAMES         hdf5
				HINTS         ${_HDF5_BASE_DIR}
				PATH_SUFFIXES lib)
			if (${HDF5_LIB})
				list(APPEND HDF5_LIBRARIES_WORK "${HDF5_LIB}")
			endif()
			unset(HDF5_LIB CACHE)
			find_library(HDF5_LIB
				NAMES         hdf5_hl
				HINTS         ${_HDF5_BASE_DIR}
				PATH_SUFFIXES lib)
			if (${HDF5_LIB})
				list(APPEND HDF5_LIBRARIES_WORK "${HDF5_LIB}")
			endif()

			if (${lang} STREQUAL CXX)
				unset(HDF5_LIB CACHE)
				find_library(HDF5_LIB
					NAMES         hdf5_cpp
					HINTS         ${_HDF5_BASE_DIR}
					PATH_SUFFIXES lib)
				unset(HDF5_LIB)
				if (${HDF5_LIB})
					list(APPEND HDF5_LIBRARIES_WORK "${HDF5_LIB}")
				endif()
			elseif (${lang} STREQUAL Fortran)
				unset(HDF5_LIB CACHE)
				find_library(HDF5_LIB
					NAMES         hdf5_fortran
					HINTS         ${_HDF5_BASE_DIR}
					PATH_SUFFIXES lib)
				if (${HDF5_LIB})
					list(APPEND HDF5_LIBRARIES_WORK "${HDF5_LIB}")
				endif()
				unset(HDF5_LIB CACHE)
				find_library(HDF5_LIB
					NAMES         hdf5hl_fortran
					HINTS         ${_HDF5_BASE_DIR}
					PATH_SUFFIXES lib)
				if (${HDF5_LIB})
					list(APPEND HDF5_LIBRARIES_WORK "${HDF5_LIB}")
				endif()
			endif()

			if (NOT HDF5_LIBRARIES_WORK)
				set(HDF5_LIBRARIES_WORK "HDF5_${lang}_LIBRARIES-NOTFOUND")
			endif()
		endif()

		set(H5SHARED)
		if (${shared_hdf5})
			set(H5SHARED _SHARED)
		endif()
		# If we found HDF5, set up all of the appropriate cache entries
		if ("${parallel_hdf5}")
			find_package(MPI)
			set(HDF5_COMPILE_FLAGS_WORK "${HDF5_COMPILE_FLAGS_WORK} ${MPI_${lang}_COMPILE_FLAGS}")
			set(HDF5_INCLUDE_PATH_WORK "${HDF5_INCLUDE_PATH_WORK}" "${MPI_${lang}_INCLUDE_PATH}")
			set(HDF5_LINK_FLAGS_WORK "${HDF5_LINK_FLAGS_WORK} ${MPI_${lang}_LINK_FLAGS}")
			set(HDF5_LIBRARIES_WORK "${HDF5_LIBRARIES_WORK}" "${MPI_${lang}_LIBRARIES}")
		endif()
		set(HDF5${H5SHARED}_${lang}_COMPILE_FLAGS ${HDF5_COMPILE_FLAGS_WORK} CACHE STRING "${H5SHARED} HDF5 ${lang} compilation flags"         FORCE)
		set(HDF5${H5SHARED}_${lang}_INCLUDE_PATH  ${HDF5_INCLUDE_PATH_WORK}  CACHE STRING "${H5SHARED} HDF5 ${lang} include path"              FORCE)
		set(HDF5${H5SHARED}_${lang}_LINK_FLAGS    ${HDF5_LINK_FLAGS_WORK}    CACHE STRING "${H5SHARED} HDF5 ${lang} linking flags"             FORCE)
		set(HDF5${H5SHARED}_${lang}_LIBRARIES     ${HDF5_LIBRARIES_WORK}     CACHE STRING "${H5SHARED} HDF5 ${lang} libraries to link against" FORCE)
		mark_as_advanced(HDF5${H5SHARED}_${lang}_COMPILE_FLAGS HDF5${H5SHARED}_${lang}_INCLUDE_PATH HDF5${H5SHARED}_${lang}_LINK_FLAGS HDF5${H5SHARED}_${lang}_LIBRARIES)

		# clear out our temporary lib/header detectionv variable here.
		set(HDF5_LIB         "HDF5_LIB-NOTFOUND"         CACHE INTERNAL "Scratch variable for HDF5 lib detection"    FORCE)
		set(HDF5_HEADER_PATH "HDF5_HEADER_PATH-NOTFOUND" CACHE INTERNAL "Scratch variable for HDF5 header detection" FORCE)
	endif()

	# finally set a found variable for each HDF5 language
	if (HDF5_${lang}_INCLUDE_PATH AND HDF5_${lang}_LIBRARIES)
		set(HDF5_${lang}_FOUND TRUE PARENT_SCOPE)
	else()
		set(HDF5_${lang}_FOUND FALSE PARENT_SCOPE)
	endif()
endfunction()

# Find the compilers and sends them off for interrogation.
function(hdf5_try_find parallel_hdf5)
	# HDF5 compiler names
	if("${parallel_hdf5}")
		set(_HDF5_C_COMPILER_NAMES         h5pcc)
		set(_HDF5_CXX_COMPILER_NAMES       h5pc++)
		set(_HDF5_Fortran_COMPILER_NAMES   h5pfc)
	else()
		set(_HDF5_C_COMPILER_NAMES         h5cc)
		set(_HDF5_CXX_COMPILER_NAMES       h5c++)
		set(_HDF5_Fortran_COMPILER_NAMES   h5fc)
	endif()
	unset(_HDF5_COMPILERS)
	foreach (lang C CXX Fortran)
		if (CMAKE_${lang}_COMPILER_WORKS)
			# If the user supplies a compiler *name* instead of an absolute path, assume that we need to find THAT compiler.
			if (HDF5_${lang}_COMPILER)
				is_file_executable(HDF5_${lang}_COMPILER HDF5_COMPILER_IS_EXECUTABLE)
				if (NOT HDF5_COMPILER_IS_EXECUTABLE)
					# Get rid of our default list of names and just search for the name the user wants.
					set(_HDF5_${lang}_COMPILER_NAMES ${HDF5_${lang}_COMPILER})
					set(HDF5_${lang}_COMPILER "HDF5_${lang}_COMPILER-NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
					# If the user specifies a compiler, we don't want to try to search libraries either.
					set(try_libs FALSE)
				endif()
			else()
				set(try_libs TRUE)
			endif()

			find_program(HDF5_${lang}_COMPILER
				NAMES         ${_HDF5_${lang}_COMPILER_NAMES}
				HINTS         ${_HDF5_BASE_DIR}
				PATH_SUFFIXES bin)
			mark_as_advanced(HDF5_${lang}_COMPILER)
			interrogate_hdf5_compiler(${lang} ${try_libs} ON ${parallel_hdf5})
			interrogate_hdf5_compiler(${lang} ${try_libs} OFF ${parallel_hdf5})
			unset(_HDF5_${lang}_COMPILER_NAMES)
			if(HDF5_${lang}_COMPILER)
				list(APPEND _HDF5_COMPILERS "${HDF5_${lang}_COMPILER}")
			else()
				list(APPEND _HDF5_COMPILERS ${HDF5_${lang}_LIBRARIES})
			endif()
		endif()
	endforeach()
	unset(_HDF5_BASE_DIR)
	set(HDF5_COMPILERS "${_HDF5_COMPILERS}" PARENT_SCOPE)
endfunction()

# End definitions, commence real work here.

find_program(H5DIFF NAMES h5diff PATH_SUFFIXES bin DOC "HDF5 file diff tool.")
mark_as_advanced(H5DIFF)

# call get_filename_component twice to remove h5diff and the directory it exists in (typically bin).
# This gives us a fairly reliable base directory to search for /bin /lib and /include from.
get_filename_component(_HDF5_BASE_DIR "${H5DIFF}" PATH)
get_filename_component(_HDF5_BASE_DIR "${_HDF5_BASE_DIR}" PATH)

if( "${HDF5_PARALLEL}" STREQUAL "PAR" )
	hdf5_try_find(TRUE)
elseif( "${HDF5_PARALLEL}" STREQUAL "SEQ" )
	hdf5_try_find(FALSE)
else()
	hdf5_try_find(FALSE)
	if( NOT "${HDF5_COMPILERS}" )
		hdf5_try_find(TRUE)
	endif()
endif()

find_package_handle_standard_args(HDF5 DEFAULT_MSG HDF5_COMPILERS)
# unset these vars to cleanup namespace
unset(_HDF5_COMPILERS)

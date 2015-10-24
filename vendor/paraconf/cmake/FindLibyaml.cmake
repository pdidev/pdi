# - Finds Libyaml
#
# === Variables ===
#
# This module will set the following variables in your project:
#   LibYaml_FOUND           TRUE if FindLibYaml found LibYaml
#   LibYaml_COMPILE_FLAGS   Compilation flags for LibYaml programs
#   LibYaml_INCLUDE_PATH    Include path(s) for LibYaml header
#   LibYaml_LINK_FLAGS      Linking flags for LibYaml programs
#   LibYaml_LIBRARIES       All libraries to link LibYaml programs against
#
# === Usage ===
#
# To use this module, simply run find_package(LibYaml) from a CMakeLists.txt.
# If you are happy with the auto-detected configuration, then you're done.
# If not, set both LibYaml_<lang>_LIBRARIES and  LibYaml_<lang>_INCLUDE_PATH.
# You may also set any other variables listed above, but these two are
# required. This will circumvent autodetection entirely.

#=============================================================================
# Copyright 2015 CEA, Julien Bigot <julien.bigot@cea.fr>
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

# include this to handle the QUIETLY and REQUIRED arguments
include(FindPackageHandleStandardArgs)
include(GetPrerequisites)

#
# interrogate_mpi_compiler(lang try_libs)
#
# Attempts to extract compiler and linker args from an LibYaml compiler. The arguments set
# by this function are:
#
#   LibYaml_<lang>_INCLUDE_PATH    LibYaml_<lang>_LINK_FLAGS     LibYaml_<lang>_FOUND
#   LibYaml_<lang>_COMPILE_FLAGS   LibYaml_<lang>_LIBRARIES
#
# LibYaml_<lang>_COMPILER must be set beforehand to the absolute path to an LibYaml compiler for
# <lang>.  Additionally, LibYaml_<lang>_INCLUDE_PATH and LibYaml_<lang>_LIBRARIES may be set
# to skip autodetection.
#
# If try_libs is TRUE, this will also attempt to find plain LibYaml libraries in the usual
# way.  In general, this is not as effective as interrogating the compilers, as it
# ignores language-specific flags and libraries.  However, some LibYaml implementations
# (Windows implementations) do not have compiler wrappers, so this approach must be used.
#
function (interrogate_mpi_compiler lang try_libs)
  # LibYaml_${lang}_NO_INTERROGATE will be set to a compiler name when the *regular* compiler was
  # discovered to be the LibYaml compiler.  This happens on machines like the Cray XE6 that use
  # modules to set cc, CC, and ftn to the LibYaml compilers.  If the user force-sets another LibYaml
  # compiler, LibYaml_${lang}_COMPILER won't be equal to LibYaml_${lang}_NO_INTERROGATE, and we'll
  # inspect that compiler anew.  This allows users to set new compilers w/o rm'ing cache.
  string(COMPARE NOTEQUAL "${LibYaml_${lang}_NO_INTERROGATE}" "${LibYaml_${lang}_COMPILER}" interrogate)

  # If LibYaml is set already in the cache, don't bother with interrogating the compiler.
  if (interrogate AND ((NOT LibYaml_${lang}_INCLUDE_PATH) OR (NOT LibYaml_${lang}_LIBRARIES)))
    if (LibYaml_${lang}_COMPILER)
      # Check whether the -showme:compile option works. This indicates that we have either OpenMPI
      # or a newer version of LAM-LibYaml, and implies that -showme:link will also work.
      execute_process(
        COMMAND ${LibYaml_${lang}_COMPILER} -showme:compile
        OUTPUT_VARIABLE  LibYaml_COMPILE_CMDLINE OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_VARIABLE   LibYaml_COMPILE_CMDLINE ERROR_STRIP_TRAILING_WHITESPACE
        RESULT_VARIABLE  LibYaml_COMPILER_RETURN)

      if (LibYaml_COMPILER_RETURN EQUAL 0)
        # If we appear to have -showme:compile, then we should
        # also have -showme:link. Try it.
        execute_process(
          COMMAND ${LibYaml_${lang}_COMPILER} -showme:link
          OUTPUT_VARIABLE  LibYaml_LINK_CMDLINE OUTPUT_STRIP_TRAILING_WHITESPACE
          ERROR_VARIABLE   LibYaml_LINK_CMDLINE ERROR_STRIP_TRAILING_WHITESPACE
          RESULT_VARIABLE  LibYaml_COMPILER_RETURN)

        if (LibYaml_COMPILER_RETURN EQUAL 0)
          # We probably have -showme:incdirs and -showme:libdirs as well,
          # so grab that while we're at it.
          execute_process(
            COMMAND ${LibYaml_${lang}_COMPILER} -showme:incdirs
            OUTPUT_VARIABLE  LibYaml_INCDIRS OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_VARIABLE   LibYaml_INCDIRS ERROR_STRIP_TRAILING_WHITESPACE)

          execute_process(
            COMMAND ${LibYaml_${lang}_COMPILER} -showme:libdirs
            OUTPUT_VARIABLE  LibYaml_LIBDIRS OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_VARIABLE   LibYaml_LIBDIRS ERROR_STRIP_TRAILING_WHITESPACE)

        else()
          # reset things here if something went wrong.
          set(LibYaml_COMPILE_CMDLINE)
          set(LibYaml_LINK_CMDLINE)
        endif()
      endif ()

      # Older versions of LAM-LibYaml have "-showme". Try to find that.
      if (NOT LibYaml_COMPILER_RETURN EQUAL 0)
        execute_process(
          COMMAND ${LibYaml_${lang}_COMPILER} -showme
          OUTPUT_VARIABLE  LibYaml_COMPILE_CMDLINE OUTPUT_STRIP_TRAILING_WHITESPACE
          ERROR_VARIABLE   LibYaml_COMPILE_CMDLINE ERROR_STRIP_TRAILING_WHITESPACE
          RESULT_VARIABLE  LibYaml_COMPILER_RETURN)
      endif()

      # MVAPICH uses -compile-info and -link-info.  Try them.
      if (NOT LibYaml_COMPILER_RETURN EQUAL 0)
        execute_process(
          COMMAND ${LibYaml_${lang}_COMPILER} -compile-info
          OUTPUT_VARIABLE  LibYaml_COMPILE_CMDLINE OUTPUT_STRIP_TRAILING_WHITESPACE
          ERROR_VARIABLE   LibYaml_COMPILE_CMDLINE ERROR_STRIP_TRAILING_WHITESPACE
          RESULT_VARIABLE  LibYaml_COMPILER_RETURN)

        # If we have compile-info, also have link-info.
        if (LibYaml_COMPILER_RETURN EQUAL 0)
          execute_process(
            COMMAND ${LibYaml_${lang}_COMPILER} -link-info
            OUTPUT_VARIABLE  LibYaml_LINK_CMDLINE OUTPUT_STRIP_TRAILING_WHITESPACE
            ERROR_VARIABLE   LibYaml_LINK_CMDLINE ERROR_STRIP_TRAILING_WHITESPACE
            RESULT_VARIABLE  LibYaml_COMPILER_RETURN)
        endif()

        # make sure we got compile and link.  Reset vars if something's wrong.
        if (NOT LibYaml_COMPILER_RETURN EQUAL 0)
          set(LibYaml_COMPILE_CMDLINE)
          set(LibYaml_LINK_CMDLINE)
        endif()
      endif()

      # MPICH just uses "-show". Try it.
      if (NOT LibYaml_COMPILER_RETURN EQUAL 0)
        execute_process(
          COMMAND ${LibYaml_${lang}_COMPILER} -show
          OUTPUT_VARIABLE  LibYaml_COMPILE_CMDLINE OUTPUT_STRIP_TRAILING_WHITESPACE
          ERROR_VARIABLE   LibYaml_COMPILE_CMDLINE ERROR_STRIP_TRAILING_WHITESPACE
          RESULT_VARIABLE  LibYaml_COMPILER_RETURN)
      endif()

      if (LibYaml_COMPILER_RETURN EQUAL 0)
        # We have our command lines, but we might need to copy LibYaml_COMPILE_CMDLINE
        # into LibYaml_LINK_CMDLINE, if we didn't find the link line.
        if (NOT LibYaml_LINK_CMDLINE)
          set(LibYaml_LINK_CMDLINE ${LibYaml_COMPILE_CMDLINE})
        endif()
      else()
        message(STATUS "Unable to determine LibYaml from LibYaml driver ${LibYaml_${lang}_COMPILER}")
        set(LibYaml_COMPILE_CMDLINE)
        set(LibYaml_LINK_CMDLINE)
      endif()

      # Here, we're done with the interrogation part, and we'll try to extract args we care
      # about from what we learned from the compiler wrapper scripts.

      # If interrogation came back with something, extract our variable from the LibYaml command line
      if (LibYaml_COMPILE_CMDLINE OR LibYaml_LINK_CMDLINE)
        # Extract compile flags from the compile command line.
        string(REGEX MATCHALL "(^| )-[Df]([^\" ]+|\"[^\"]+\")" LibYaml_ALL_COMPILE_FLAGS "${LibYaml_COMPILE_CMDLINE}")
        set(LibYaml_COMPILE_FLAGS_WORK)

        foreach(FLAG ${LibYaml_ALL_COMPILE_FLAGS})
          if (LibYaml_COMPILE_FLAGS_WORK)
            set(LibYaml_COMPILE_FLAGS_WORK "${LibYaml_COMPILE_FLAGS_WORK} ${FLAG}")
          else()
            set(LibYaml_COMPILE_FLAGS_WORK ${FLAG})
          endif()
        endforeach()

        # Extract include paths from compile command line
        string(REGEX MATCHALL "(^| )-I([^\" ]+|\"[^\"]+\")" LibYaml_ALL_INCLUDE_PATHS "${LibYaml_COMPILE_CMDLINE}")
        foreach(IPATH ${LibYaml_ALL_INCLUDE_PATHS})
          string(REGEX REPLACE "^ ?-I" "" IPATH ${IPATH})
          string(REGEX REPLACE "//" "/" IPATH ${IPATH})
          list(APPEND LibYaml_INCLUDE_PATH_WORK ${IPATH})
        endforeach()

        # try using showme:incdirs if extracting didn't work.
        if (NOT LibYaml_INCLUDE_PATH_WORK)
          set(LibYaml_INCLUDE_PATH_WORK ${LibYaml_INCDIRS})
          separate_arguments(LibYaml_INCLUDE_PATH_WORK)
        endif()

        # If all else fails, just search for mpi.h in the normal include paths.
        if (NOT LibYaml_INCLUDE_PATH_WORK)
          set(LibYaml_HEADER_PATH "LibYaml_HEADER_PATH-NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
          find_path(LibYaml_HEADER_PATH mpi.h
            HINTS ${_LibYaml_BASE_DIR} ${_LibYaml_PREFIX_PATH}
            PATH_SUFFIXES include)
          set(LibYaml_INCLUDE_PATH_WORK ${LibYaml_HEADER_PATH})
        endif()

        # Extract linker paths from the link command line
        string(REGEX MATCHALL "(^| |-Wl,)-L([^\" ]+|\"[^\"]+\")" LibYaml_ALL_LINK_PATHS "${LibYaml_LINK_CMDLINE}")
        set(LibYaml_LINK_PATH)
        foreach(LPATH ${LibYaml_ALL_LINK_PATHS})
          string(REGEX REPLACE "^(| |-Wl,)-L" "" LPATH ${LPATH})
          string(REGEX REPLACE "//" "/" LPATH ${LPATH})
          list(APPEND LibYaml_LINK_PATH ${LPATH})
        endforeach()

        # try using showme:libdirs if extracting didn't work.
        if (NOT LibYaml_LINK_PATH)
          set(LibYaml_LINK_PATH ${LibYaml_LIBDIRS})
          separate_arguments(LibYaml_LINK_PATH)
        endif()

        # add the compiler implicit directories because some compilers
        # such as the intel compiler have libraries that show up
        # in the show list that can only be found in the implicit
        # link directories of the compiler.
        if (DEFINED CMAKE_${lang}_IMPLICIT_LINK_DIRECTORIES)
          list(APPEND LibYaml_LINK_PATH "${CMAKE_${lang}_IMPLICIT_LINK_DIRECTORIES}")
        endif ()
        
        # Extract linker flags from the link command line
        string(REGEX MATCHALL "(^| )-Wl,([^\" ]+|\"[^\"]+\")" LibYaml_ALL_LINK_FLAGS "${LibYaml_LINK_CMDLINE}")
        set(LibYaml_LINK_FLAGS_WORK)
        foreach(FLAG ${LibYaml_ALL_LINK_FLAGS})
          if (LibYaml_LINK_FLAGS_WORK)
            set(LibYaml_LINK_FLAGS_WORK "${LibYaml_LINK_FLAGS_WORK} ${FLAG}")
          else()
            set(LibYaml_LINK_FLAGS_WORK ${FLAG})
          endif()
        endforeach()

        # Extract the set of libraries to link against from the link command
        # line
        string(REGEX MATCHALL "(^| )(-l([^\" ]+|\"[^\"]+\")|[^ ]*\\.(a|so))( |$)" LibYaml_LIBNAMES "${LibYaml_LINK_CMDLINE}")

        # Determine full path names for all of the libraries that one needs
        # to link against in an LibYaml program
        foreach(LIB ${LibYaml_LIBNAMES})
          string(REGEX REPLACE "^ +" "" LIB ${LIB})
          string(REGEX REPLACE "^-l *" "" LIB ${LIB})
          string(REGEX REPLACE " $" "" LIB ${LIB})
          # LibYaml_LIB is cached by find_library, but we don't want that.  Clear it first.
          set(LibYaml_LIB "LibYaml_LIB-NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
          if(EXISTS "${LIB}")
            set(LibYaml_LIB "${LIB}")
          else()
            find_library(LibYaml_LIB NAMES ${LIB} HINTS ${LibYaml_LINK_PATH})
          endif()

          if (LibYaml_LIB)
            list(APPEND LibYaml_LIBRARIES_WORK ${LibYaml_LIB})
          elseif (NOT LibYaml_FIND_QUIETLY)
            message(WARNING "Unable to find LibYaml library ${LIB}")
          endif()
        endforeach()

        # Sanity check LibYaml_LIBRARIES to make sure there are enough libraries
        list(LENGTH LibYaml_LIBRARIES_WORK LibYaml_NUMLIBS)
        list(LENGTH LibYaml_LIBNAMES LibYaml_NUMLIBS_EXPECTED)
        if (NOT LibYaml_NUMLIBS EQUAL LibYaml_NUMLIBS_EXPECTED)
          set(LibYaml_LIBRARIES_WORK "LibYaml_${lang}_LIBRARIES-NOTFOUND")
        endif()
      endif()

    elseif(try_libs)
      # If we didn't have an LibYaml compiler script to interrogate, attempt to find everything
      # with plain old find functions.  This is nasty because LibYaml implementations have LOTS of
      # different library names, so this section isn't going to be very generic.  We need to
      # make sure it works for MS LibYaml, though, since there are no compiler wrappers for that.
      find_path(LibYaml_HEADER_PATH mpi.h
        HINTS ${_LibYaml_BASE_DIR} ${_LibYaml_PREFIX_PATH}
        PATH_SUFFIXES include Inc)
      set(LibYaml_INCLUDE_PATH_WORK ${LibYaml_HEADER_PATH})

      # Decide between 32-bit and 64-bit libraries for Microsoft's LibYaml
      if("${CMAKE_SIZEOF_VOID_P}" EQUAL 8)
        set(MS_LibYaml_ARCH_DIR amd64)
      else()
        set(MS_LibYaml_ARCH_DIR i386)
      endif()

      set(LibYaml_LIB "LibYaml_LIB-NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
      find_library(LibYaml_LIB
        NAMES         mpi mpich mpich2 msmpi
        HINTS         ${_LibYaml_BASE_DIR} ${_LibYaml_PREFIX_PATH}
        PATH_SUFFIXES lib lib/${MS_LibYaml_ARCH_DIR} Lib Lib/${MS_LibYaml_ARCH_DIR})
      set(LibYaml_LIBRARIES_WORK ${LibYaml_LIB})

      # Right now, we only know about the extra libs for C++.
      # We could add Fortran here (as there is usually libfmpich, etc.), but
      # this really only has to work with MS LibYaml on Windows.
      # Assume that other LibYaml's are covered by the compiler wrappers.
      if (${lang} STREQUAL CXX)
        set(LibYaml_LIB "LibYaml_LIB-NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
        find_library(LibYaml_LIB
          NAMES         mpi++ mpicxx cxx mpi_cxx
          HINTS         ${_LibYaml_BASE_DIR} ${_LibYaml_PREFIX_PATH}
          PATH_SUFFIXES lib)
        if (LibYaml_LIBRARIES_WORK AND LibYaml_LIB)
          list(APPEND LibYaml_LIBRARIES_WORK ${LibYaml_LIB})
        endif()
      endif()

      if (NOT LibYaml_LIBRARIES_WORK)
        set(LibYaml_LIBRARIES_WORK "LibYaml_${lang}_LIBRARIES-NOTFOUND")
      endif()
    endif()
    
    if (${lang} STREQUAL Fortran)
      message(STATUS "Checking whether LibYaml Fortran module is available")
      set(test_file "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/cmake_mpimodule_test.F90")
      file(WRITE "${test_file}" "
program test_mpimodule
    use mpi
    integer:: ierr
    call mpi_init(ierr)
    ierr = LibYaml_BOTTOM
    call mpi_finalize(ierr)
endprogram test_mpimodule
")
      try_compile(compile_result "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}" "${test_file}"
        COMPILE_DEFINITIONS ${LibYaml_COMPILE_FLAGS_WORK}
        LINK_LIBRARIES ${LibYaml_LIBRARIES_WORK}
        CMAKE_FLAGS "-DINCLUDE_DIRECTORIES:STRING=${LibYaml_INCLUDE_PATH_WORK}"
        OUTPUT_VARIABLE compile_output)
      
      if (NOT compile_result)
        file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
            "LibYaml Fortran module detection failed to compile with the following output:\n"
            "${compile_output}\n")
        set(LibYaml_Fortran_HAS_MODULE FALSE)
        message(STATUS "Checking whether LibYaml Fortran module is available -- no")
      else()
        set(LibYaml_Fortran_HAS_MODULE TRUE)
        message(STATUS "Checking whether LibYaml Fortran module is available -- yes")
      endif()
      set(LibYaml_Fortran_HAS_MODULE "${LibYaml_Fortran_HAS_MODULE}" CACHE BOOLEAN "Whether LibYaml Fortran supports the use of the LibYaml module")
      mark_as_advanced(LibYaml_Fortran_HAS_MODULE)
      
    endif()
    
    # If we found LibYaml, set up all of the appropriate cache entries
    set(LibYaml_${lang}_COMPILE_FLAGS ${LibYaml_COMPILE_FLAGS_WORK} CACHE STRING "LibYaml ${lang} compilation flags"         FORCE)
    set(LibYaml_${lang}_INCLUDE_PATH  ${LibYaml_INCLUDE_PATH_WORK}  CACHE STRING "LibYaml ${lang} include path"              FORCE)
    set(LibYaml_${lang}_LINK_FLAGS    ${LibYaml_LINK_FLAGS_WORK}    CACHE STRING "LibYaml ${lang} linking flags"             FORCE)
    set(LibYaml_${lang}_LIBRARIES     ${LibYaml_LIBRARIES_WORK}     CACHE STRING "LibYaml ${lang} libraries to link against" FORCE)
    mark_as_advanced(LibYaml_${lang}_COMPILE_FLAGS LibYaml_${lang}_INCLUDE_PATH LibYaml_${lang}_LINK_FLAGS LibYaml_${lang}_LIBRARIES)

    # clear out our temporary lib/header detectionv variable here.
    set(LibYaml_LIB         "LibYaml_LIB-NOTFOUND"         CACHE INTERNAL "Scratch variable for LibYaml lib detection"    FORCE)
    set(LibYaml_HEADER_PATH "LibYaml_HEADER_PATH-NOTFOUND" CACHE INTERNAL "Scratch variable for LibYaml header detection" FORCE)
  endif()

  # finally set a found variable for each LibYaml language
  if (LibYaml_${lang}_INCLUDE_PATH AND LibYaml_${lang}_LIBRARIES)
    set(LibYaml_${lang}_FOUND TRUE PARENT_SCOPE)
  else()
    set(LibYaml_${lang}_FOUND FALSE PARENT_SCOPE)
  endif()
endfunction()


# This function attempts to compile with the regular compiler, to see if LibYaml programs
# work with it.  This is a last ditch attempt after we've tried interrogating mpicc and
# friends, and after we've tried to find generic libraries.  Works on machines like
# Cray XE6, where the modules environment changes what LibYaml version cc, CC, and ftn use.
function(try_regular_compiler lang success)
  set(scratch_directory ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY})
  if (${lang} STREQUAL Fortran)
    set(test_file ${scratch_directory}/cmake_mpi_test.f90)
    file(WRITE ${test_file}
      "program hello\n"
      "include 'mpif.h'\n"
      "integer ierror\n"
      "call LibYaml_INIT(ierror)\n"
      "call LibYaml_FINALIZE(ierror)\n"
      "end\n")
  else()
    if (${lang} STREQUAL CXX)
      set(test_file ${scratch_directory}/cmake_mpi_test.cpp)
    else()
      set(test_file ${scratch_directory}/cmake_mpi_test.c)
    endif()
    file(WRITE ${test_file}
      "#include <mpi.h>\n"
      "int main(int argc, char **argv) {\n"
      "  LibYaml_Init(&argc, &argv);\n"
      "  LibYaml_Finalize();\n"
      "}\n")
  endif()
  try_compile(compiler_has_mpi ${scratch_directory} ${test_file})
  if (compiler_has_mpi)
    set(LibYaml_${lang}_NO_INTERROGATE ${CMAKE_${lang}_COMPILER} CACHE STRING "Whether to interrogate LibYaml ${lang} compiler" FORCE)
    set(LibYaml_${lang}_COMPILER       ${CMAKE_${lang}_COMPILER} CACHE STRING "LibYaml ${lang} compiler"                        FORCE)
    set(LibYaml_${lang}_COMPILE_FLAGS  ""                        CACHE STRING "LibYaml ${lang} compilation flags"               FORCE)
    set(LibYaml_${lang}_INCLUDE_PATH   ""                        CACHE STRING "LibYaml ${lang} include path"                    FORCE)
    set(LibYaml_${lang}_LINK_FLAGS     ""                        CACHE STRING "LibYaml ${lang} linking flags"                   FORCE)
    set(LibYaml_${lang}_LIBRARIES      ""                        CACHE STRING "LibYaml ${lang} libraries to link against"       FORCE)
  endif()
  set(${success} ${compiler_has_mpi} PARENT_SCOPE)
  unset(compiler_has_mpi CACHE)
endfunction()

# End definitions, commence real work here.

# call get_filename_component twice to remove mpiexec and the directory it exists in (typically bin).
# This gives us a fairly reliable base directory to search for /bin /lib and /include from.
get_filename_component(_LibYaml_BASE_DIR "${MPIEXEC}" PATH)
get_filename_component(_LibYaml_BASE_DIR "${_LibYaml_BASE_DIR}" PATH)

mark_as_advanced(MPIEXEC)

unset(_LibYaml_COMPILERS)
# This loop finds the compilers and sends them off for interrogation.
foreach (lang C CXX Fortran)
  if (CMAKE_${lang}_COMPILER_WORKS)
    # If the user supplies a compiler *name* instead of an absolute path, assume that we need to find THAT compiler.
    if (LibYaml_${lang}_COMPILER)
      is_file_executable(LibYaml_${lang}_COMPILER LibYaml_COMPILER_IS_EXECUTABLE)
      if (NOT LibYaml_COMPILER_IS_EXECUTABLE)
        # Get rid of our default list of names and just search for the name the user wants.
        set(_LibYaml_${lang}_COMPILER_NAMES ${LibYaml_${lang}_COMPILER})
        set(LibYaml_${lang}_COMPILER "LibYaml_${lang}_COMPILER-NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
        # If the user specifies a compiler, we don't want to try to search libraries either.
        set(try_libs FALSE)
      endif()
    else()
      set(try_libs TRUE)
    endif()

    find_program(LibYaml_${lang}_COMPILER
      NAMES  ${_LibYaml_${lang}_COMPILER_NAMES}
      PATHS  "${LibYaml_HOME}/bin" "$ENV{LibYaml_HOME}/bin" ${_LibYaml_PREFIX_PATH})
    interrogate_mpi_compiler(${lang} ${try_libs})
    mark_as_advanced(LibYaml_${lang}_COMPILER)

    # last ditch try -- if nothing works so far, just try running the regular compiler and
    # see if we can create an LibYaml executable.
    set(regular_compiler_worked 0)
    if (NOT LibYaml_${lang}_LIBRARIES OR NOT LibYaml_${lang}_INCLUDE_PATH)
      try_regular_compiler(${lang} regular_compiler_worked)
    endif()

    list(APPEND _LibYaml_COMPILERS "${LibYaml_${lang}_COMPILER}")
  endif()
endforeach()

find_package_handle_standard_args(LibYaml DEFAULT_MSG _LibYaml_COMPILERS)
# unset these vars to cleanup namespace
unset(_LibYaml_COMPILERS)
unset(_LibYaml_PREFIX_PATH)
unset(_LibYaml_BASE_DIR)
foreach (lang C CXX Fortran)
  unset(_LibYaml_${lang}_COMPILER_NAMES)
endforeach()

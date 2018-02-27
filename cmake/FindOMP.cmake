# - Finds OMP support
# This module can be used to detect OMP support in a compiler.
# If the compiler supports OMP, the flags required to compile with
# openmp support are set.
#
# This module will set the following variables per language in your project,
# where <lang> is one of C, CXX, or Fortran:
#   OMP_<lang>_COMPILE_FLAGS  Compilation flags for OpenMP programs
#   OMP_<lang>_LINK_FLAGS     Linking flags for OpenMP programs
# Additionally, FindOMP sets the following variable:
#   OMP_FOUND                 TRUE if FindOMP found OpenMP flags for all
#                             enabled languages
# Supported compilers can be found at http://openmp.org/wp/openmp-compilers/

#=============================================================================
# Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# Copyright 2009 Kitware, Inc.
# Copyright 2008-2009 André Rigland Brodtkorb <Andre.Brodtkorb@ifi.uio.no>
# Copyright 2012 Rolf Eike Beer <eike@sf-mail.de>
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

include(FindPackageHandleStandardArgs)

function(_OMP_flag_candidates RESULT LANG)
  set(FLAGS_CANDIDATES
    " "
    "-fopenmp"
    "/openmp"
    "-qopenmp"
    "-Qopenmp"
    "-qopenmp"
    "-openmp"
    "-xopenmp"
    "+Oopenmp"
    "-qsmp=omp"
    "-qsmp"
    "-mp"
  )

  set(FLAGS_GNU "-fopenmp")
  set(FLAGS_HP "+Oopenmp")
  set(FLAGS_Intel "-qopenmp" "-Qopenmp" "-openmp")
  set(FLAGS_MIPSpro "-mp")
  set(FLAGS_MSVC "/openmp")
  set(FLAGS_PathScale "-openmp")
  set(FLAGS_PGI "-mp")
  set(FLAGS_SunPro "-xopenmp")
  set(FLAGS_XL "-qsmp=omp" "-qsmp")

  # Move the flag that matches the compiler to the head of the list,
  # this is faster and doesn't clutter the output that much. If that
  # flag doesn't work we will still try all.
  if(FLAGS_${CMAKE_${LANG}_COMPILER_ID})
    list(REMOVE_ITEM FLAGS_CANDIDATES ${FLAGS_${CMAKE_${LANG}_COMPILER_ID}})
    list(INSERT FLAGS_CANDIDATES 0 ${FLAGS_${CMAKE_${LANG}_COMPILER_ID}})
  endif()

  set(${RESULT} "${FLAGS_CANDIDATES}" PARENT_SCOPE)
endfunction()

function(_OMP_find_flag LANG)
  _OMP_flag_candidates(FLAGS_CANDIDATES ${LANG})
  foreach(FLAG ${FLAGS_CANDIDATES})
    if(LANG STREQUAL Fortran)
      set(test_file "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/cmake_omp_test.F90")
      file(WRITE "${test_file}" "
        program test_omp
          use omp_lib
          integer ii
          ii = omp_get_thread_num()
        end
        ")
    else()
      if(LANG STREQUAL C)
        set(test_file "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/cmake_omp_test.c")
      elseif(LANG STREQUAL CXX)
        set(test_file "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/cmake_omp_test.cpp")
      endif()
      file(WRITE "${test_file}" "
        #include <omp.h>
        int main() {
          int ii;
          ii = omp_get_thread_num();
        #ifdef _OPENMP
          return 0; 
        #else
          breaks on purpose
        #endif
        }
        ")
    endif()
    try_compile(flag_works "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}" "${test_file}"
      CMAKE_FLAGS -DCOMPILE_DEFINITIONS:STRING=${FLAG}
      OUTPUT_VARIABLE test_output)
    
    if(flag_works)
      set(OMP_${LANG}_COMPILE_FLAGS " ${FLAG}" CACHE STRING "${LANG} compile flags for OMP parallization")
      set(OMP_${LANG}_LINK_FLAGS    " ${FLAG}" CACHE STRING "${LANG} link flags for OMP parallization")
      mark_as_advanced(OMP_${LANG}_COMPILE_FLAGS OMP_${LANG}_LINK_FLAGS)
      break()
    else()
      file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
        "Fortran OMP flag ${FLAG} for language ${LANG} test failed with the following output:\n"
        "${test_output}\n")
    endif()
  endforeach()
endfunction()

foreach(LANG C CXX Fortran)
  if(CMAKE_${LANG}_COMPILER_WORKS)
    if(NOT OMP_${LANG}_COMPILE_FLAGS)
      _OMP_find_flag(${LANG})
    endif()
    list(APPEND _OMP_vars OMP_${LANG}_COMPILE_FLAGS OMP_${LANG}_LINK_FLAGS)
  endif()
endforeach()

find_package_handle_standard_args(OMP DEFAULT_MSG ${_OMP_vars})
unset(_OMP_vars)
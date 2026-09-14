#=============================================================================
# Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================

# Some MPI implementations ship no F90 `mpi' module, only the F77 `mpif.h' header.
# This offers a MPI_with_mod target that provides the module either way, so that Fortran sources can
# simply `use mpi'.
# It is a portability workaround, not a part of PDI, and is only meant for the tests and examples.
#
# The target is defined by the first directory that includes this module, and shared by every other.

include_guard(GLOBAL)

# Whoever found MPI with Fortran already did it with every language it needs, and a Fortran-only lookup would recompute its results.
if(NOT TARGET MPI::MPI_Fortran)
	find_package(MPI REQUIRED COMPONENTS Fortran)
endif()

if("${MPI_Fortran_HAVE_F90_MODULE}")
	add_library(MPI_with_mod INTERFACE)
	target_link_libraries(MPI_with_mod INTERFACE MPI::MPI_Fortran)
elseif("${MPI_Fortran_HAVE_F77_HEADER}")
	set(_MPI_WITH_MOD_DIR "${CMAKE_CURRENT_BINARY_DIR}/mpi_with_mod")
	file(WRITE "${_MPI_WITH_MOD_DIR}/mpi.F90" [=[
module mpi
implicit none
include "mpif.h"
end module
]=])
	add_library(MPI_with_mod STATIC "${_MPI_WITH_MOD_DIR}/mpi.F90")
	target_link_libraries(MPI_with_mod PUBLIC MPI::MPI_Fortran)
	# CMake does not propagate Fortran module directories through target_link_libraries and every
	# consumer sits in another directory, so the directory holding mpi.mod has to be published by
	# hand for `use mpi' to resolve there.
	set_target_properties(MPI_with_mod PROPERTIES Fortran_MODULE_DIRECTORY "${_MPI_WITH_MOD_DIR}/mod")
	target_include_directories(MPI_with_mod INTERFACE "${_MPI_WITH_MOD_DIR}/mod")
	unset(_MPI_WITH_MOD_DIR)
else()
	message(FATAL_ERROR "Unable to compile a MPI program either with F90 module or F77 include")
endif()

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

cmake_minimum_required(VERSION 3.10)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")


if("${BUILD_FORTRAN}")
	if(NOT "${MPI_Fortran_HAVE_F90_MODULE}")
		if("${MPI_Fortran_HAVE_F77_HEADER}")
			file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/mpi.F90" [=[
module mpi
implicit none
include "mpif.h"
end module
]=])
			add_library(MPI_with_mod STATIC "${CMAKE_CURRENT_BINARY_DIR}/mpi.F90")
			target_link_libraries(MPI_with_mod MPI::MPI_Fortran)
		else()
			message(FATAL_ERROR "Unable to compile a MPI program either with F90 module or F77 include")
		endif()
	else()
		add_library(MPI_with_mod INTERFACE)
		target_link_libraries(MPI_with_mod INTERFACE MPI::MPI_Fortran)
	endif()
endif()


# Testing export of Metatadata and files creation

add_executable(decl_sion_01_c decl_sion_test_01.c)
target_link_libraries(decl_sion_01_c PDI::pdi MPI::MPI_C)
add_test(NAME decl_sion_01_c COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_sion_01_c>" ${MPIEXEC_POSTFLAGS})

if("${BUILD_FORTRAN}")
add_executable(decl_sion_01_F decl_sion_test_01.F90)
target_link_libraries(decl_sion_01_F PDI::pdi_f90 MPI_with_mod)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_sion_01_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_sion_01_F COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_sion_01_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_01.yml")
endif("${BUILD_FORTRAN}")


# Testing writer/reader with one file per variable

add_executable(decl_sion_02_c decl_sion_test_02.c)
target_link_libraries(decl_sion_02_c PDI::pdi MPI::MPI_C)
add_test(NAME decl_sion_02_c COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_sion_02_c>" ${MPIEXEC_POSTFLAGS})

if("${BUILD_FORTRAN}")
add_executable(decl_sion_02_F decl_sion_test_02.F90)
target_link_libraries(decl_sion_02_F PDI::pdi_f90 MPI_with_mod)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_sion_02_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_sion_02_F COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_sion_02_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_02.yml")
endif("${BUILD_FORTRAN}")


# Testing writer/reader using the event based mechanism 

add_executable(decl_sion_03_c decl_sion_test_03.c)
target_link_libraries(decl_sion_03_c PDI::pdi MPI::MPI_C)
add_test(NAME decl_sion_03_c COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_sion_03_c>" ${MPIEXEC_POSTFLAGS})

if("${BUILD_FORTRAN}")
add_executable(decl_sion_03_F decl_sion_test_03.F90)
target_link_libraries(decl_sion_03_F PDI::pdi_f90 MPI_with_mod)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(decl_sion_03_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME decl_sion_03_F COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 2 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:decl_sion_03_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_test_03.yml")
endif("${BUILD_FORTRAN}")


# Config tests

if("${BUILD_CFG_VALIDATOR}")
find_package(PDIcfg REQUIRED)

# Positive tests
add_test(NAME pos_val_decl_sion_test_01 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_sion_test_01.yml")
add_test(NAME pos_val_decl_sion_test_02 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_sion_test_02.yml")
add_test(NAME pos_val_decl_sion_test_03 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_decl_sion_test_03.yml")

# Negative tests
add_test(NAME neg_val_decl_sion_test_01 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_sion_test_01.yml")
add_test(NAME neg_val_decl_sion_test_02 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_sion_test_02.yml")
add_test(NAME neg_val_decl_sion_test_03 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_sion_test_03.yml")
add_test(NAME neg_val_decl_sion_test_04 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_sion_test_04.yml")
add_test(NAME neg_val_decl_sion_test_05 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_sion_test_05.yml")
add_test(NAME neg_val_decl_sion_test_06 COMMAND PDIcfg::pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_decl_sion_test_06.yml")
set_tests_properties( 
	neg_val_decl_sion_test_01
	neg_val_decl_sion_test_02
	neg_val_decl_sion_test_03
	neg_val_decl_sion_test_04
	neg_val_decl_sion_test_05
	neg_val_decl_sion_test_06
	PROPERTIES
	WILL_FAIL TRUE)

endif()


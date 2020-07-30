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
project(pdi_examples LANGUAGES C)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

set(MPI_COMPONENTS C)
set(PDI_COMPONENTS C)

if("${BUILD_FORTRAN}")
	enable_language(Fortran)
	list(APPEND MPI_COMPONENTS Fortran)
	list(APPEND PDI_COMPONENTS f90)
endif()

if("${BUILD_PYTHON}")
	find_package(Python3 REQUIRED COMPONENTS Interpreter)
	list(APPEND PDI_COMPONENTS python)
endif()

# Includes
include(CTest)
find_package(MPI REQUIRED COMPONENTS ${MPI_COMPONENTS})
find_package(PDI REQUIRED COMPONENTS ${PDI_COMPONENTS})
find_library(LIB_M m DOC "The math library")

# Default standard in the project is C99 & C++14
set(CMAKE_C_STANDARD 99)
set(CMAKE_C_STANDARD_REQUIRED TRUE)

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


add_executable(PDI_example_C example.c)
target_link_libraries(PDI_example_C PDI::PDI_C MPI::MPI_C m)

if("${BUILD_FORTRAN}")
add_executable(PDI_example_F example.F90)
target_link_libraries(PDI_example_F PDI::PDI_f90 MPI_with_mod m)
endif("${BUILD_FORTRAN}")

set(RUNTEST_DIR "${CMAKE_SOURCE_DIR}/cmake/runtest-dir")

if("${BUILD_DECL_HDF5_PLUGIN}")
add_test(NAME PDI_example_decl_hdf5_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_C>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5.yml")
set_property(TEST PDI_example_decl_hdf5_C PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_hdf5_C PROPERTY PROCESSORS 3)

if("${BUILD_FORTRAN}")
add_test(NAME PDI_example_decl_hdf5_F COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5.yml")
set_property(TEST PDI_example_decl_hdf5_F PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_hdf5_F PROPERTY PROCESSORS 3)
endif("${BUILD_FORTRAN}")

if("${BUILD_PYTHON}")
add_test(NAME PDI_example_decl_hdf5_P COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI::pdirun>" "${Python3_EXECUTABLE}" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/example.py" "${CMAKE_CURRENT_SOURCE_DIR}/decl_hdf5.yml")
set_property(TEST PDI_example_decl_hdf5_P PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_hdf5_P PROPERTY PROCESSORS 3)
set_property(TEST PDI_example_decl_hdf5_P PROPERTY DISABLED TRUE) #TODO: python tests randomly deadlock currently
endif("${BUILD_PYTHON}")
endif("${BUILD_DECL_HDF5_PLUGIN}")

if("${BUILD_DECL_NETCDF_PLUGIN}")
add_test(NAME PDI_example_decl_netcdf_C_serial COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_C>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf_serial.yml")
set_property(TEST PDI_example_decl_netcdf_C_serial PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_netcdf_C_serial PROPERTY PROCESSORS 1)

add_test(NAME PDI_example_decl_netcdf_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_C>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_netcdf.yml")
set_property(TEST PDI_example_decl_netcdf_C PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_netcdf_C PROPERTY PROCESSORS 3)
set_property(TEST PDI_example_decl_netcdf_C PROPERTY DISABLED TRUE) #TODO: netcdf parallel #define NC_HAS_PARALLEL4

endif("${BUILD_DECL_NETCDF_PLUGIN}")


if("${BUILD_DECL_SION_PLUGIN}")
add_test(NAME PDI_example_decl_sion_variables_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_C>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_variables.yml")
set_property(TEST PDI_example_decl_sion_variables_C PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_sion_variables_C PROPERTY PROCESSORS 3)

if("${BUILD_FORTRAN}")
add_test(NAME PDI_example_decl_sion_variables_F COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_variables.yml")
set_property(TEST PDI_example_decl_sion_variables_F PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_sion_variables_F PROPERTY PROCESSORS 3)
endif("${BUILD_FORTRAN}")

if("${BUILD_PYTHON}")
add_test(NAME PDI_example_decl_sion_variables_P COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI::pdirun>" "${Python3_EXECUTABLE}" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/example.py" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_variables.yml")
set_property(TEST PDI_example_decl_sion_variables_P PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_sion_variables_P PROPERTY PROCESSORS 3)
set_property(TEST PDI_example_decl_sion_variables_P PROPERTY DISABLED TRUE) #TODO: python tests randomly deadlock currently
endif("${BUILD_PYTHON}")

add_test(NAME PDI_example_decl_sion_files_C COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_C>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_files.yml")
set_property(TEST PDI_example_decl_sion_files_C PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_sion_files_C PROPERTY PROCESSORS 3)

if("${BUILD_FORTRAN}")
add_test(NAME PDI_example_decl_sion_files_F COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_files.yml")
set_property(TEST PDI_example_decl_sion_files_F PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_sion_files_F PROPERTY PROCESSORS 3)
endif("${BUILD_FORTRAN}")

if("${BUILD_PYTHON}")
add_test(NAME PDI_example_decl_sion_files_P COMMAND "${RUNTEST_DIR}" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI::pdirun>" "${Python3_EXECUTABLE}" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/example.py" "${CMAKE_CURRENT_SOURCE_DIR}/decl_sion_files.yml")
set_property(TEST PDI_example_decl_sion_files_P PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_decl_sion_files_P PROPERTY PROCESSORS 3)
set_property(TEST PDI_example_decl_sion_files_P PROPERTY DISABLED TRUE) #TODO: python tests randomly deadlock currently
endif("${BUILD_PYTHON}")
endif("${BUILD_DECL_SION_PLUGIN}")

if("${BUILD_FTI_PLUGIN}")
add_test(NAME PDI_example_fti_C COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config.ini" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 6 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_C>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/fti.yml")
set_property(TEST PDI_example_fti_C PROPERTY TIMEOUT 180)
set_property(TEST PDI_example_fti_C PROPERTY PROCESSORS 6)

if("${BUILD_FORTRAN}")
add_test(NAME PDI_example_fti_F COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config.ini" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 6 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/fti_transtype.yml")
set_property(TEST PDI_example_fti_F PROPERTY TIMEOUT 180)
set_property(TEST PDI_example_fti_F PROPERTY PROCESSORS 6)
endif("${BUILD_FORTRAN}")

if("${BUILD_PYTHON}")
add_test(NAME PDI_example_fti_P COMMAND "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/fti_config.ini" "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 6 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI::pdirun>" "${PYTHON_EXECUTABLE}" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/example.py" "${CMAKE_CURRENT_SOURCE_DIR}/fti_transtype.yml")
set_property(TEST PDI_example_fti_P PROPERTY TIMEOUT 180)
set_property(TEST PDI_example_fti_P PROPERTY PROCESSORS 6)
set_property(TEST PDI_example_fti_P PROPERTY DISABLED TRUE) #TODO: python tests randomly deadlock currently
endif("${BUILD_PYTHON}")
endif("${BUILD_FTI_PLUGIN}")

if("${BUILD_FLOWVR_PLUGIN}" AND NOT "$ENV{MPI_LIB}" MATCHES "mpich") #disable MPI FlowVR with mpich for now
find_package(FlowVR REQUIRED)

set(OpenGL_GL_PREFERENCE LEGACY)
find_package(OpenGL REQUIRED)
find_package(GLUT REQUIRED)
add_executable(flowvr_visu flowvr_visu.c)
target_link_libraries(flowvr_visu PDI::PDI_C ${OPENGL_LIBRARIES} ${GLUT_LIBRARIES})

add_test(NAME PDI_example_flowvr_C COMMAND PDI::pdirun "${RUNTEST_DIR}" "--runtest-dir-copy-file" "${CMAKE_CURRENT_SOURCE_DIR}/flowvr_test.sh" "./flowvr_test.sh" "${CMAKE_CURRENT_SOURCE_DIR}" "$<TARGET_FILE:PDI_example_C>" "$<TARGET_FILE:flowvr_visu>" "${FLOWVR_PREFIX}")
set_property(TEST PDI_example_flowvr_C PROPERTY TIMEOUT 60)
set_property(TEST PDI_example_flowvr_C PROPERTY PROCESSORS 4)
set_property(TEST PDI_example_flowvr_C PROPERTY RESOURCE_LOCK flowvrd)
set_property(TEST PDI_example_flowvr_C PROPERTY DISABLED TRUE)
endif("${BUILD_FLOWVR_PLUGIN}" AND NOT "$ENV{MPI_LIB}" MATCHES "mpich")

add_test(NAME PDI_example_trace_C COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_C>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/trace.yml")
set_property(TEST PDI_example_trace_C PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_trace_C PROPERTY PROCESSORS 3)

if("${BUILD_FORTRAN}")
add_test(NAME PDI_example_trace_F COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI_example_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/trace.yml")
set_property(TEST PDI_example_trace_F PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_trace_F PROPERTY PROCESSORS 3)
endif("${BUILD_FORTRAN}")

if("${BUILD_PYTHON}")
add_test(NAME PDI_example_trace_P COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 3 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:PDI::pdirun>" "${Python3_EXECUTABLE}" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/example.py" "${CMAKE_CURRENT_SOURCE_DIR}/trace.yml")
set_property(TEST PDI_example_trace_P PROPERTY TIMEOUT 5)
set_property(TEST PDI_example_trace_P PROPERTY PROCESSORS 3)
set_property(TEST PDI_example_trace_P PROPERTY DISABLED TRUE) #TODO: python tests randomly deadlock currently
endif("${BUILD_PYTHON}")
